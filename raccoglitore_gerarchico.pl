:- use_module(library(draw_world)).
:- use_module(two_level_planner).
:- use_module(mondi).
:- use_module(geometria).
import_type(geometria, [direzione/0,
			point/0]).

:- style_check(-discontiguous).

raccoglitore_gerarchico_help :- maplist(write, [
'\n***********************************************************',
'\nRaccoglitore gerarchico,  provare:\n',
'\n?- load_world(cw(1)).',
'\n?- raccolta(cw(1), point(2,2),[point(2,9), point(8,3)],point(3,2)).',
'\n?- action_plan(Stato, Azione, Piano, Costo).',
'\n***********************************************************\n']).
:- raccoglitore_gerarchico_help.

%===============================================================
%  Agente "Raccoglitore".  Un esempio di implementazione di un agente
%  pianificatore gerarchico su una gerarchia di 2 livelli
%==============================================================

type([in(point), deve_prendere(punto), deposito(punto), fine]:fluent).
%   in(P):  l'agente si trova in P
%   deve_prendere(X): l'agente deve prendere un oggetto in X
%   deposito(Q):  deve depositare gli oggetti in Q
%   fine:  ha finito

type([va(point,point), va_da_a(point,point), raccoglie, deposita]:azione).
%  va(P1,P2) : avanza da P1 a P2 adiacente
%  va_da_a(P1,P2) : va da P1 a P2 non adiacente, macro-azione
%  raccoglie:  raccoglie l'oggetto
%  deposita: deposita gli oggetti

%================================================
%  Predicati dinamici che rappresentano lo stato di
%  mondo+agente
%=================================================
%
pred(in(point)).
%  in(P) :  l'agente si trova in P
:- dynamic(deve_prendere/1).

pred(deve_prendere(point)).
%  deve_prendere(P) :  deve prendere un oggetto in P
:- dynamic(deve_prendere/1).

pred(deposito(point)).
%  deposito(P) :  l'agente deve depositare in P
:- dynamic(deposito/1).

pred(current_world(any)).
%  current_world(W):  W è il mondo in cui si trova l'agente
:- dynamic(current_world/1).

%==================================================
%  Implemento i predicati aperti di two_level_planner
%===================================================
%=====================================
%    AZIONI DI LIVELLO 0
%=====================================

two_level_planner:add_del(0, deposita, St, [fine], [deposito(P)], 0.5) :-
	not(member(deve_prendere(_), St)),
	member(deposito(P), St),
	member(in(P), St).
two_level_planner:add_del(0,va_da_a(P1,P2), St, [in(P2)], [in(P1)], Cost) :-
	not(member(deve_prendere(_), St)),
	member(deposito(P2), St),
	member(in(P1), St),
	P1 \= P2,
	get_action_plan(St, va_da_a(P1,P2), _Plan, Cost).
two_level_planner:add_del(0,va_da_a(P1,P2), St, [in(P2)], [in(P1)], Cost) :-
	member(deve_prendere(P2),St),
	member(in(P1), St),
	P1 \= P2,
	get_action_plan(St, va_da_a(P1,P2), _Plan, Cost).
two_level_planner:add_del(0,raccoglie, St, [], [deve_prendere(P)], 0.5) :-
	member(deve_prendere(P), St),
	member(in(P), St).

%=====================================
%    AZIONI DI LIVELLO 1
%=====================================
two_level_planner:add_del(1,va(P1,P2), St, [in(P2)], [in(P1)], Cost) :-
	member(in(P1), St),
	step(Dir,P1,P2), % P2 raggiungibile in un passo in una direzione Dir
	current_world(W),
	not(content(W,P2,ostacolo)),
	length_step(Dir,Cost).  %distanza nella direzione di spostamento


%=====================================
%    EURISTICA DI LIVELLO 0
%=====================================

two_level_planner:h(0, St, 0) :-
	member(fine,St), !.
two_level_planner:h(0, St, H) :-
	%  conto il numero di oggetti da raccogliere;
	%  euristica da migliorare, così se gli oggetti da
	%  raccogliere sono distanti si ha out of stack
	setof(X, member(deve_prendere(X), St), Pos) ->
	    length(Pos,H)
	;   H = 0.

%=====================================
%    EURISTICA DI LIVELLO 1
%=====================================
two_level_planner:h(1, St, H) :-
	member(target(P2), St),
	member(in(P1),St),
	distance(diagonal, P1,P2,H).

%=======================================
%   PREDICATI DI START E  GOAL LIVELLO 0
%=======================================
%
two_level_planner:starting_state([P1, PuntiRaccolta, P2],
		                 [in(P1), deposito(P2)| FluentiRaccolta]) :-
	setof(deve_prendere(X), member(X, PuntiRaccolta), FluentiRaccolta).
	%  [in(P1), deposito(P2) | FluentiRaccolta] sono i fluenti veri nello stato iniziale

two_level_planner:goal_state(_Param, G) :-
	member(fine, G).

%=======================================
%   PREDICATI DI START E  GOAL LIVELLO 1
%=======================================

two_level_planner:action_starting_state(_St, va_da_a(P1,P2), [in(P1), target(P2)]).
%  negli stati per pianificare va_da_a(P1,P2) mi bastano in(P) e il
%  target(P2)

two_level_planner:action_final_state(_St, va_da_a(_,_), G) :-
%  ho terminato il piano per va_da_a quando raggiungo il target
	member(target(P),G),
	member(in(P),G).

%=========================================
%  PREDICATI APERTI DI ESECUZIONE PIANO a 2 livelli
%=========================================

two_level_planner:macro_azione(va_da_a(_,_)).
%  unica macro-azione

two_level_planner:esegui_azione_base(va(P1,P2)) :-
	retract(in(P1)),
	assert(in(P2)),
	simula(va(P1,P2)).
two_level_planner:esegui_azione_base(deposita) :-
	simula(deposita).
two_level_planner:esegui_azione_base(raccoglie) :-
	in(P),
	retract(deve_prendere(P)),
	simula(raccogli(P)).
two_level_planner:action_starting_state(va_da_a(P1,P2), [in(P1), target(P2)]).
%  trovo lo stato strips di inizio azione in esecuzione nel mondo
%  necessario per ottenere il piano di esecuzione per quello stato


%=================================================
%  RICERCA DI UN PIANO PER IL RACCOGLITORE
%  USA get_plan e ottiene un piano di livello 0 e memorizza
%  i piani di esecuzione (livello 1) delle macro-azioni
%===================================================


pred(raccolta(atom, point, list(point), point, list(action), number)).
%  raccolta(W, P, Raccolta, Q, Plan, Cost) : il mondo W è stato
%  caricato, l'agente si trova inzialmente in P, deve passare per i
%  punti Raccolta e depositare in Q. Plan è il piano di raccolta con
%  costo Cost.
%  MODO (+,+,+,+) semidet, FALLISCE se W non è caricato
%
raccolta(W, Pos, Racc, Q) :-
	% preparo lo stato iniziale del mondo+agente
	maplist(retractall, [in(_), deposito(_), deve_prendere(_)]),
	assert(in(Pos)),
	assert(deposito(Q)),
	forall(member(P,Racc), assert(deve_prendere(P))),

	%  disegno il mondo e il suo stato
	set_current_world(W),
	draw_world_state(W),

	% fisso la strategia di default
	set_strategy(astar),
	set_strategy(ps(closed)),
	step(['Avvio con strategia astar e potatura chiusi']),!,

	%  calcolo il piano  di 2 livelli
	get_plan([Pos,Racc,Q], Plan, Cost),
	step(['Calcolato il piano ',Plan, '\ncon costo ', Cost]),!,

	%  eseguo il piano di 2 livelli
	esegui(Plan).


set_current_world(W) :-
	%  disegno la parte statica del mondo
	load_world(W),
	retractall(current_world(_)),
	assert(current_world(W)),
	draw_loaded_world(W,20).

draw_world_state(W) :-
	% disegno lo stato corrente del mondo
	in(Pos),
	draw_fig(W, circ(15,[col(green)]), Pos),
	forall(deve_prendere(PR), draw_fig(W, box(15,[col(blue)]), PR)),
	deposito(Q),
        draw_fig(W, circ(15,[col(yellow)]), Q).






%========================================================
%
%pred(simula(evento)).
%  simula(E):  mostra l'evento E graficamente e/o con messaggio
%  di spiegazione;  per alcuni eventi resta in attesa di un
%  input dell'utente (usando il predicato step(Msg))
%
pred(step(list(term))).
%  step(Msg) :  stampa Msg e chiede un input;
%  se l'input è t, avvia trace e manda avanti l'esecuzione, se è n
%  arresta trace e manda avanti l'esecuzione, se è a abortisce; negli
%  altri casi manda avanti l'esecuzione senza far nulla
%.

simula(va(P1,P2)) :- !,
	current_world(W),
	move_fig(W,circ(_,_),P1,P2),
	step([]).
simula(raccogli(P)) :-
	current_world(W),
	del_fig(W, box(_,_), P),
	step(['Raccolto oggetto']).
simula(deposita) :-
	in(P),
	step(['Depositati gli oggetti in ',P]),
	current_world(W),
	destroy(W).

step(Msg) :-
	maplist(write, Msg),
	nl,
	readln(L),
	(   L=[t|_] -> trace
	;   L=[a|_] -> abort
	;   L=[n|_] -> notrace
	;   true).




















