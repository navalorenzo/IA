:- use_module(library(draw_world)).
:- use_module(library(strips_planner)).
:- use_module(mondi).
:- use_module(geometria).
import_type(geometria, [direzione/0,
			point/0]).

:- style_check(-discontiguous).

raccoglitore_strips_help
:- maplist(write, [
'\n***********************************************************',
'\nRaccoglitore pianificatore,  provare:\n',
'\n?- set_strategy(astar).',
'\n?- set_strategy(ps(closed)).',
'\n?- load_world(cw(1)).',
'\n?- raccolta(cw(1), point(2,2),[point(2,9), point(8,3)],point(3,2),Pl,C).',
'\n***********************************************************\n']).
:- raccoglitore_strips_help.

%===============================================================
%  Agente "Raccoglitore" pianificatore,  usa strips_planner.
%==============================================================

type([in(point), deve_prendere(punto), deposito(punto), fine]:fluent).
%   in(P):  l'agente si trova in P
%   deve_prendere(X): l'agente deve prendere un oggetto in X
%   deposito(Q):  deve depositare gli oggetti in Q
%   fine:  ha finito

type([va(point,point), raccoglie, deposita]:azione).
%  va(P1,P2) : avanza da P1 a P2
%  raccoglie:  raccoglie l'oggetto
%  deposita: deposita gli oggetti
%


strips_planner:add_del(va(P1,P2), St, [in(P2)], [in(P1)], Cost) :-
	member(in(P1), St),
	not(member(deve_prendere(P1),St)),
	step(Dir,P1,P2), % P2 raggiungibile in un passo in una qualche direzione Dir
	current_world(W),
	navigable(W,P2),
	length_step(Dir,Cost).  %distanza nella direzione di spostamento
strips_planner:add_del(raccoglie, St, [], [deve_prendere(P)], 0.5) :-
	member(in(P), St),
	member(deve_prendere(P), St).
strips_planner:add_del(deposita, St, [fine], [deposito(P)], 0.5) :-
	member(in(P), St),
	member(deposito(P), St),
	not(member(deve_prendere(_), St)).

strips_planner:h(St, 0) :-
	member(fine,St), !.
strips_planner:h(St, H) :-
	%  conto il numero di oggetti da raccogliere;
	%  euristica da migliorare
	setof(X, member(deve_prendere(X), St), Pos) ->
	    length(Pos,H)
	;   H = 0.





%=================================================================
%   Per far partire l'agente in un mondo è necessario aver caricato il
%   mondo, aver posto l'agente in una posizione e direzione iniziale,
%   l'oggetto in posizione P1 e aver fissato la psizione di trasporto
%   P2
%   ===================================================================
:- dynamic(current_world/1).
pred(raccolta(atom, point, list(point), point, list(action), number)).
%  get_plan(W, P, Raccolta, Q, Plan, Cost) : il mondo W è stato
%  caricato, l'agente si trova inzialmente in P, deve passare per i
%  punti Raccolta e depositare in Q. MODO (+,+,+,+) semidet, FALLISCE se
%  W non è caricato
%
raccolta(W, Pos, Racc, Q, Plan, Cost) :-
	world(W,_, _),!,
	% il mondo W è stato caricato o generato;
	% lo pongo come mondo corrente e lo disegno
	retractall(current_world(_)),
	assert(current_world(W)),
	draw_loaded_world(W,20),
	draw_fig(W, circ(15,[col(green)]), Pos),
	forall(member(PR,Racc), draw_fig(W, box(15,[col(blue)]), PR)),
        draw_fig(W, circ(15,[col(yellow)]), Q),
	setof(deve_prendere(X), member(X, Racc), Raccolta),
	%  [in(Pos), deposito(Q) | Raccolta] sono i fluenti veri nello stato iniziale
	plan(start_with([in(Pos), deposito(Q) | Raccolta]), goal_state, _, Plan, Cost).

raccolta(W,_,_,_,_,_) :-
	writeln(W:' non caricato, usa load_world o gen_world'),
	fail.

start_with(X,X).
goal_state(St) :-
	member(fine,St).
