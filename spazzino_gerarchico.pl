:- use_module(library(draw_world)).
:- use_module(two_level_planner).
:- use_module(spazzino_mondi).
:- use_module(geometria).
import_type(geometria, [direzione/0,
			point/0]).

:- style_check(-discontiguous).

raccoglitore_gerarchico_help :- maplist(write, [
'\n***********************************************************',
'\nRaccoglitore gerarchico,  provare:\n',
'\n?- load_world(cw(1)).',
'\n  raccolta(mondo, punto di partenza, lista di cassonetti, punto di alta densita, punto di bassa densita, benzinaio, deposito, capienza serbatoio, capienza camion)',
'\n?- raccolta(cw(1), point(6,8),[point(2,9), point(8,3)],point(2,2), point(8,4),point(3,3),point(3,2),35,15).',
'\n?- action_plan(Stato, Azione, Piano, Costo).',
'\n***********************************************************\n']).
:- raccoglitore_gerarchico_help.

%===============================================================
%  Agente "Spazzino". Pianificatore gerarchico
%==============================================================

type([in(point), deve_prendere(punto), deposito(punto), benzinaio(punto), serbatoio(integer), carico(integer), fine]:fluent).
%   in(P):  l'agente si trova in P
%   deve_prendere(X): l'agente deve prendere un oggetto in X
%   deposito(Q):  deve depositare gli oggetti in Q
%   benzinaio(B):  deve fare rifornimento in B
%   serbatoio(S): quantità di benzina S nel serbatoio
%   carico(C): quantità C di rifiuti già raccolti
%   fine:  ha finito

type([va(point,point), va_da_a(point,point), raccoglie, deposita, rifornimento, scarica]:azione).
%  va(P1,P2) : avanza da P1 a P2 adiacente
%  va_da_a(P1,P2) : va da P1 a P2 non adiacente, macro-azione
%  raccoglie:  raccoglie l'oggetto
%  deposita: deposita gli oggetti e termina
%  rifornimento: fa il pieno all'agente
%	 scarica: scarica i rifiuti

%================================================
%  Predicati dinamici che rappresentano lo stato di
%  mondo+agente
%=================================================
%
type([alta,bassa]:densita).

pred(serbatoio(integer)).
%  serbatoio(S) :- livello di benzina S
:- dynamic(serbatoio/1).

pred(capienza_serbatoio(integer)).
pred(capienza_camion(integer)).

pred(in(point)).
%  in(P) :  l'agente si trova in P
:- dynamic(deve_prendere/1).

pred(deve_prendere(point)).
%  deve_prendere(P) :  deve prendere un oggetto in P
:- dynamic(deve_prendere/1).

pred(deposito(point)).
%  deposito(P) :  l'agente deve depositare in P
:- dynamic(deposito/1).

pred(benzinaio(punto)).
%  benzinaio(B) : l'agente deve rifornirsi in B
:- dynamic(benzinaio/1).

pred(current_world(any)).
%  current_world(W):  W � il mondo in cui si trova l'agente
:- dynamic(current_world/1).

pred(cassonetto(point,densita,integer)).
%  MODO(++,++, --)semidet;
%  cassonetto(P,D, I) : il cassonetto si trova in P e contiene una quantità I
%  di rifiuti dipendente dalla densita D

pred(upper_bound(densita, integer)).
% MODO(++, --) det; MODO(++, --) det;
% upper_bound(D,Q) : Q limite di spazzatura nei cassonetti di aree com densità D
upper_bound(alta, 8).
upper_bound(bassa,4).

pred(inizializza_cassonetto(point, point, point)).
%  inizializza_cassonetto(P,Da, Db) assegna a un cassonetto nel punto P una quantità di
%  rifiuti casuale dipendenti dall'area di appartenenza

inizializza_cassonetto(P, Da, Db) :-
	distance(diagonal,P,Da,A),
	distance(diagonal,P,Db,B),
	A @> B,
	upper_bound(alta, Ub),
	random_between(4, Ub, R),
	assert(cassonetto(P,alta,R)).

inizializza_cassonetto(P, Da, Db) :-
	distance(diagonal,P,Da,A),
	distance(diagonal,P,Db,B),
	upper_bound(bassa, Ub),
	random_between(1, Ub, R),
	assert(cassonetto(P,bassa,R)).

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

two_level_planner:add_del(0, scarica, St, [carico(0)], [carico(C)], 0.5) :-
	member(deve_prendere(_), St),
	member(deposito(P), St),
	member(in(P), St),
	member(carico(C), St).

two_level_planner:add_del(0,va_da_a(P1,P2), St, [in(P2), serbatoio(Y)], [in(P1), serbatoio(X)], Cost) :-
	%controllo carico
	member(carico(Ca), St),
	capienza_camion(Cap),
	cassonetto(P2,Dens,_),
	upper_bound(Dens, Ub),
	K is Cap - Ub,
	Ca @=<  K,
	%piano
	not(member(deve_prendere(_), St)),
	member(deposito(P2), St),
	member(in(P1), St),
	P1 \= P2,
	get_action_plan([in(P1), tager(P2)], va_da_a(P1,P2), _Plan, Cost),
	%controllo serbatoio
	member(serbatoio(X), St),
	C is round(Cost),
	C @< X,
	Y is X - C.

two_level_planner:add_del(0,va_da_a(P1,P2), St, [in(P2), serbatoio(Y)], [in(P1), serbatoio(X)], Cost) :-
	%controllo capienza
	member(carico(Ca), St),
	capienza_camion(Cap),
	cassonetto(P2,Dens,_),
	upper_bound(Dens, Ub),
	K is Cap - Ub,
	Ca @=< K,
	%piano
	member(deve_prendere(P2),St),
	member(in(P1), St),
	P1 \= P2,
	get_action_plan([in(P1), tager(P2)], va_da_a(P1,P2), _Plan, Cost),
	%controllo serbatoio
	member(serbatoio(X), St),
	C is round(Cost),
	C @< X,
	Y is X - C.

%benzinaio
two_level_planner:add_del(0,va_da_a(P1,P2), St, [in(P2), serbatoio(Y)], [in(P1), serbatoio(X)], Cost) :-
	%piano
	member(in(P1), St),
	member(benzinaio(P2), St),
	P1 \= P2,
	get_action_plan([in(P1), tager(P2)], va_da_a(P1,P2), _Plan, Cost),
	% controllo serbatoio
	member(serbatoio(X), St),
	C is round(Cost),
	Y is X - C,
	Y @>= 0.

%svuota carico
two_level_planner:add_del(0,va_da_a(P1,P2), St, [in(P2), serbatoio(Y)], [in(P1), serbatoio(X)], Cost) :-
	member(deposito(P2),St),
	member(in(P1), St),
	P1 \= P2,
	get_action_plan([in(P1), tager(P2)], va_da_a(P1,P2), _Plan, Cost),
	%controllo serbatoio
	member(serbatoio(X), St),
	C is round(Cost),
	C @< X,
	Y is X - C.


two_level_planner:add_del(0,raccoglie, St, [carico(Y)], [deve_prendere(P),carico(X)], 0.5) :-
	member(deve_prendere(P), St),
	member(in(P), St),
	member(carico(X), St),
	cassonetto(P,_,Q),
	Y is X + Q.

two_level_planner:add_del(0,rifornimento, St, [serbatoio(C)], [serbatoio(X)], 0.5) :-
	member(serbatoio(X), St),
	member(in(P), St),
	member(benzinaio(P), St),
	capienza_serbatoio(C).

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

pred(distanza(list(any), point, integer)).
% distanza(L, P, I) : I =somma delle distanze tra P e i punti della lista L
% Modo(++,++,--)det
distanza([], P, 0).
distanza([H], P, I):- distance(diagonal,H,P,I).
distanza([H|T], P, I):- distanza(T, P, It), distance(diagonal,H,P,Ih), I is It + Ih.

two_level_planner:h(0, St, 0) :-
	member(fine,St), !.
two_level_planner:h(0, St, H) :-
	%  L'euristica viene calcolata stimando la somma delle distanze di andata e ritorno
	%  tra ogni cassonetto e il deposito
	setof(X, member(deve_prendere(X), St), Pos) ->
		length(Pos, NumCassonetti),
		deposito(D),
		distanza(Pos,D, V),
		H is  V * 2;
		H = 0.

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
two_level_planner:starting_state([P1, PuntiRaccolta, P2, P3],
		                 [in(P1), deposito(P2), benzinaio(P3), carico(0), serbatoio(X) | FluentiRaccolta]) :-
	setof(deve_prendere(X), member(X, PuntiRaccolta), FluentiRaccolta), serbatoio(X).
	%  [in(P1), deposito(P2), benzinaio(P3) | FluentiRaccolta] sono i fluenti veri nello stato iniziale

two_level_planner:goal_state(_Param, G) :-
	member(fine, G).

%=======================================
%   PREDICATI DI START E  GOAL LIVELLO 1
%=======================================

two_level_planner:action_starting_state(_St, va_da_a(P1,P2), [in(P1), target(P2)]).
% è usato durante la fase di PIANIFICAZIONE
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
	serbatoio(S),
	N_S is S - 1,
	retract(serbatoio(S)),
	assert(serbatoio(N_S)),
	simula(va(P1,P2)).

two_level_planner:esegui_azione_base(deposita) :-
	retract(carico(C)),
	assert(carico(0)),
	simula(deposita).


two_level_planner:esegui_azione_base(scarica) :-
	retract(carico(C)),
	assert(carico(0)),
	simula(scarica).

two_level_planner:esegui_azione_base(rifornimento) :-
	benzinaio(P3),
	in(P3),
	serbatoio(Se),
	step(['Serbatoio: ' , Se]),
	retract(serbatoio(S)),
	capienza_serbatoio(C),
	step(['Capienza; ', C]),
	assert(serbatoio(C)),
	simula(rifornimento).

two_level_planner:esegui_azione_base(raccoglie) :-
	in(P),
	retract(deve_prendere(P)),
	carico(C),
	cassonetto(P,A,X),
	N_C is C + X,
	retract(carico(C)),
	assert(carico(N_C)),
	retract(cassonetto(P, A, X)),
	assert(cassonetto(P, A, 0)),
	simula(raccogli(P)).

two_level_planner:action_starting_state(va_da_a(P1,P2), [in(P1), target(P2)]).
%  è chimato durante l'esecuzione del piano
%  trovo lo stato strips di inizio azione in esecuzione nel mondo
%  necessario per ottenere il piano di esecuzione per quello stato


%=================================================
%  RICERCA DI UN PIANO PER IL RACCOGLITORE
%  USA get_plan e ottiene un piano di livello 0 e memorizza
%  i piani di esecuzione (livello 1) delle macro-azioni
%===================================================

pred(raccolta(atom, point, list(point), point, point, point, point, integer, integer)).
%  raccolta(W, P, Raccolta, Q, B, Plan, Cost) : il mondo W � stato
%  caricato, l'agente si trova inzialmente in P, deve passare per i
%  punti Raccolta e depositare in Q. Può fare rifornimento in B Plan � il piano di raccolta con
%  costo Cost.
%  MODO (+,+,+,+,+) semidet, FALLISCE se W non � caricato
%
raccolta(W, Pos, Racc, Da, Db, B, Q, C, A) :-
	% preparo lo stato iniziale del mondo+agente
	maplist(retractall, [in(_), deposito(_), deve_prendere(_), benzinaio(_), serbatoio(_), carico(_),
	 							cassonetto(_,_,_), capienza_camion(_),capienza_serbatoio(_)]),
	assert(in(Pos)),
	assert(deposito(Q)),
	assert(benzinaio(B)),
	assert(serbatoio(C)),
	assert(carico(0)),
	forall(member(P,Racc), assert(deve_prendere(P))),
	forall(member(P,Racc), inizializza_cassonetto(P, Da, Db)),
	assert(capienza_serbatoio(C)),
	assert(capienza_camion(A)),
	step(['Capienza ' , A]),

	%  disegno il mondo e il suo stato
	set_current_world(W),
	draw_world_state(W),

	% fisso la strategia di default
	set_strategy(astar),
	set_strategy(ps(closed)),
	step(['Avvio con strategia astar e potatura chiusi']),!,

	%  calcolo il piano  di 2 livelli
	get_plan([Pos,Racc,Q,B], Plan, Cost),
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
	forall(deve_prendere(PR),
		(cassonetto(PR, alta, _) ->
			% i cassonetti appartenenti all'area ad alta densità sono blu
		 	draw_fig(W, box(15,[col(blue)]), PR)
		 	;
			% i cassonetti appartenenti all'area a bassa densità sono viola
		 	draw_fig(W, box(15,[col(purple)]), PR))),
	deposito(Q),
        draw_fig(W, box(15,[col(orange)]), Q),
	benzinaio(B),
		draw_fig(W, box(15,[col(brown)]), B).

%========================================================
%
%pred(simula(evento)).
%  simula(E):  mostra l'evento E graficamente e/o con messaggio
%  di spiegazione;  per alcuni eventi resta in attesa di un
%  input dell'utente (usando il predicato step(Msg))
%
pred(step(list(term))).
%  step(Msg) :  stampa Msg e chiede un input;
%  se l'input � t, avvia trace e manda avanti l'esecuzione, se � n
%  arresta trace e manda avanti l'esecuzione, se � a abortisce; negli
%  altri casi manda avanti l'esecuzione senza far nulla
%.

simula(va(P1,P2)) :- !,
	current_world(W),
	move_fig(W,circ(_,_),P1,P2),
	step([]).

simula(raccogli(P)) :-
	current_world(W),
	del_fig(W, box(_,_), P),
	carico(C),
	serbatoio(S),
	step(['Raccolti rifiuti, carico ', C]),
	step(['\nLivello benzina ', S]).

simula(deposita) :-
	in(P),
	step(['Depositati gli oggetti in ',P]),
	current_world(W),
	destroy(W).

simula(scarica) :-
	current_world(W),
	step(['Scarico oggetti']).


simula(rifornimento) :-
	current_world(W),
	step(['Rifornimento eseguito']).

step(Msg) :-
	maplist(write, Msg),
	nl,
	readln(L),
	(   L=[t|_] -> trace
	;   L=[a|_] -> abort
	;   L=[n|_] -> notrace
	;   true).
