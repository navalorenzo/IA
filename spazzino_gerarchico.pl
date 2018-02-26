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
'\n  raccolta(mondo, punto di partenza, lista di cassonetti, lista_aree, lista_upperbound, benzinaio, deposito, capienza serbatoio, capienza camion)',
'\n?- raccolta(cw(1), point(2,2),[point(2,9), point(8,3)], [point(1,7), point(9,4)], [3,8], point(3,3),point(3,2),35,15).',
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



pred(serbatoio(integer)).
%  serbatoio(S) : benzina S attualmente disponibile
:- dynamic(serbatoio/1).

pred(capienza_serbatoio(integer)).
% capienza_serbatoio(C) : capienza del serbatoio del camion
pred(capienza_camion(integer)).
% capienza_camino(C) : quantità massima di rifiuti trasportabili C

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

pred(cassonetto(point,point,integer)).
%  MODO(++,++, --)semidet;
%  cassonetto(P,A, I) : il cassonetto si trova in P e contiene una quantità I
%  di rifiuti dipendente dall'area A


pred(info_area(point,integer)).
:- dynamic(info_area/2).

pred(make_area(list(point),list(integer))).
%	date due liste inizializza le aree
% MODO(++,++)det
make_area([Ha], [Hu]) :- assert(info_area(Ha,Hu)).
make_area([Ha|Ta], [Hu|Tu]) :- assert(info_area(Ha,Hu)) , make_area(Ta, Tu).


pred(inizializza_cassonetto(point, list(point), list(integer))).
%  inizializza_cassonetto(P,Lista_aree, Lista_upperbound) assegna a un cassonetto nel punto P una quantità di
%  rifiuti casuale dipendenti dall'area di appartenenza

inizializza_cassonetto(P, Lista_aree, Lista_upperbound) :-
% calcolo dell'area di appartenenza
	nearest(P, Lista_aree, Area),
%	generazione valore
	info_area(Area, Ub),
	random_between(1, Ub, R),
	assert(cassonetto(P,Area,3)).
%	Cambia il 3 con R finito il testing

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

% scarica
two_level_planner:add_del(0, scarica, St, [carico(0)], [carico(C)], 0.5) :-
% azione compiuta quando vi sono ancora cassonetti da svuotare ma non si ha
% spazio a sufficienza per portare a termine la raccolta

% Svuota il carico del camion
	member(deve_prendere(_), St),
	member(deposito(P), St),
	member(in(P), St),
	member(carico(C), St).


% vai al cassonetto
two_level_planner:add_del(0,va_da_a(P1,P2), St, [in(P2), serbatoio(Y)], [in(P1), serbatoio(X)], Cost) :-
% azione che permette di raggiungere un cassonetto del mondo decrementando
% il qunatitativo di benzona disponibile

% controllo che sia possibile svuotare interamente il cassonetto, controllando
% che la capienza del camion - l'upperbound dell'area del cassonetto sia <= al
% carico trasportabile residuo
	member(carico(Ca), St),
	capienza_camion(Cap),
	cassonetto(P2,Dens,_),
	info_area(Dens, Ub),
	K is Cap - Ub,
	Ca @=< K,
% generazione del piano
	member(deve_prendere(P2),St),
	member(in(P1), St),
	P1 \= P2,
	get_action_plan([in(P1), tager(P2)], va_da_a(P1,P2), _Plan, Cost),
% controllo che sia effettivamente possibile raggiungere il cassonetto
% con il livello di benzina attuale
	member(serbatoio(X), St),
	C is round(Cost),
	C @< X,
	Y is X - C.

%vai al benzinaio
two_level_planner:add_del(0,va_da_a(P1,P2), St, [in(P2), serbatoio(Y)], [in(P1), serbatoio(X)], Cost) :-
% azione che permette di raggiungere la stazione di rifornimento quando non è
% possibile raggiungere alcun cassonetto

% generazione del piano
	member(in(P1), St),
	member(benzinaio(P2), St),
	P1 \= P2,
	get_action_plan([in(P1), tager(P2)], va_da_a(P1,P2), _Plan, Cost),
% controllo che sia effettivamente possibile raggiungere il benzinaio
% con il livello di benzina attuale
	member(serbatoio(X), St),
	C is round(Cost),
	Y is X - C,
	Y @>= 0.

%vai al deposito
two_level_planner:add_del(0,va_da_a(P1,P2), St, [in(P2), serbatoio(Y)], [in(P1), serbatoio(X)], Cost) :-
% azione che permette di raggiungere il deposito quando non è possibile
% raggiungere alcun cassonetto
	member(deposito(P2),St),
	member(in(P1), St),
	P1 \= P2,
	get_action_plan([in(P1), tager(P2)], va_da_a(P1,P2), _Plan, Cost),
% controllo che sia effettivamente possibile raggiungere il deposito
% con il livello di benzina attuale
	member(serbatoio(X), St),
	C is round(Cost),
	C @< X,
	Y is X - C.

% raccogli cassonetto
two_level_planner:add_del(0,raccoglie, St, [carico(Y)], [deve_prendere(P),carico(X)], 0.5) :-
% azione che permette di raccogliere tutto il contenuto di ogni cassonetto
	member(deve_prendere(P), St),
	member(in(P), St),
	member(carico(X), St),
	cassonetto(P,_,Q),
	Y is X + Q.

% rifornimento
two_level_planner:add_del(0,rifornimento, St, [serbatoio(C)], [serbatoio(X)], 0.5) :-
% azione che permette di riempire il serbatoio del camion
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

%==================================
%			P_MERGE_SORT
%==================================

% funzionamento analogo al merge_sort usuale ma con ordinamento rispetto a
% un punto dato (Pivot)

pred(split(list(point), list(point), list(point))).
% divido in due una lista data MODO(++,--,--)det.
% split(L,L1,L2) : L viene divisa in L1 e L2
split([], [], []).
split([X|Y], [X|Xs], Ys) :- split(Y,Ys,Xs).

pred(p_merge(point, list(point), list(point), list(point))).
% p_merge(Pivot, L1, L2, L):
% fonde ordinatamente due liste ordinate L1 e L2 in L rispetto al Pivot
% MODO(++,++,++,--)det.
p_merge(Pivot, X, [], X).
p_merge(Pivot, [], X, X).
p_merge(Pivot, [X|Tx], [Y|Ty], [X|Tz]) :-
							distance(diagonal, Pivot, X, Dx),
							distance(diagonal, Pivot, Y, Dy),
							Dx @=< Dy, !,
							p_merge(Pivot, Tx,[Y|Ty], Tz).

p_merge(Pivot, [X|Tx], [Y|Ty], [Y|Tz]) :-
							p_merge(Pivot, [X|Tx], Ty, Tz).


pred(p_merge_sort(point, list(point), list(point))).
% p_merge_sort(Pivot, L1, L2) :- ordina L1 in L2 rispetto al Pivot
% MODO(++,++,--)det
p_merge_sort(Pivot, [], []).
p_merge_sort(Pivot, [H], [H]).
p_merge_sort(Pivot, [X, Y| T], R) :-
							split(T, T1, T2),
							p_merge_sort(Pivot, [X|T1], L1) ,
							p_merge_sort(Pivot, [Y|T2], L2),
							p_merge(Pivot, L1, L2, R).

%==================================
%			LONGEST & NEAREST
%==================================

pred(longest(point,list(point), point)).
% MODO(++,++,--)det
% longest(P,L,N) : N è il punto appartente alla lista L più lontano da P
longest(P,[X], X).
longest(P, [X,Y|T], N) :-
		distance(diagonal,X,P,Dx),
		distance(diagonal,Y,P,Dy),
		Dx @> Dy, !,
		longest(P,[X|T], N).

longest(P, [X,Y|T], N) :-
		longest(P,[Y|T], N).


pred(nearest(point,list(point), point)).
% MODO(++,++,--)det
% nearest(P,L,N) : N è il punto appartente alla lista L più vicino a P
nearest(P,[X], X).
nearest(P, [X,Y|T], N) :-
		distance(diagonal,X,P,Dx),
		distance(diagonal,Y,P,Dy),
		Dx @< Dy, !,
		nearest(P,[X|T], N).

nearest(P, [X,Y|T], N) :-
		nearest(P,[Y|T], N).

%========================================
%    DISTANZA_ES (ES = euristica sottostimata)
%========================================

pred(distanza_es(list(any), point, integer)).
% distanza_es(L, P, I) : I = somma della distanza relativa tra i punti della
% lista L partendo da P
% Modo(++,++,--)det
		distanza_es([], P, 0).
		distanza_es([H], P, I):- distance(diagonal,H,P,I).
		distanza_es([H|T], P, I):- distanza_es(T, H, It), distance(diagonal,H,P,Ih), I is It + Ih.

%========================================
%    DISTANZA_ENS (ENS = euristica non sottostimata)
%========================================

pred(distanza_ens(list(any), point, integer)).
% distanza_es(L, P, I) : I = somma delle distanze tra P e ogni punto della lista L
% Modo(++,++,--)det
		distanza_ens([], P, 0).
		distanza_ens([H], P, I):- distance(diagonal,H,P,I).
		distanza_ens([H|T], P, I):- distanza_ens(T, P, It), distance(diagonal,H,P,Ih), I is It + Ih.


%==================================
%			ADD
%==================================

pred(add(list(point), point, list(point))).
% MODO(++,++,--)det.
% add(L,P, R) : R è [R|P] aggiunge in coda ad Lil punto P
add(X,P,[P|X]).

%=====================================
%    EURISTICHE DI LIVELLO 0
%=====================================

two_level_planner:h(0, St, 0) :-
	member(fine,St), !.

%******************************************************************************
% EURISTICA ES_lenta sottstimata con branching factor troppo elevato
%two_level_planner:h(0, St, H) :-
% L'euristica viene calcolata sommando la distnza dal punto punto di partenza al cassonetto
% più lontano con la distanza tra il cassonetto più lontano e il deposito
%	setof(X, member(deve_prendere(X), St), Pos) ->
%		in(P),
%		deposito(D),
%		longest(P,Pos,L),
%		distance(diagonal,P,L, Andata),
%		distance(diagonal,L,D, Ritorno),
%		H is Ritorno +  Andata
%		;
%  	H = 0.


%******************************************************************************
% EURISTICA ES_veloce sottostimata ma con branching factor accettabile
two_level_planner:h(0, St, H) :-
% L'euristica viene calcolata trovando il cammino minimo per raggiungere tutti i cassonetti
% e il benzinaio partendo dal punto di partenza e terminando nel deposito
	setof(X, member(deve_prendere(X), St), Pos) ->
		in(P),
		deposito(D),
		p_merge_sort(P,Pos, Ord),
		add(Ord, D, Punti),
		distanza_es(Punti ,P, H)
		;
		H = 0.


%******************************************************************************

% EURISTICA ENS non sottostimata con branching factor molto basso
% L'euristica somma la distanza tra ogni cassonetto che deve raccogliere +
% punto di partenza e il deposito
%two_level_planner:h(0, St, H) :-
%		setof(X, member(deve_prendere(X), St), Pos) ->
%		deposito(D),
%		in(P),
%		add(Pos,P, Res),
%		distanza_ens(Res,D, V),
%		H is  V * 2
%		;
%		H = 0.


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
	%  [in(P1), deposito(P2), benzinaio(P3), carico(0), serbatoio(X) | FluentiRaccolta] sono i fluenti veri nello stato iniziale

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
%	aggiornamento della posizione dell'agente
	retract(in(P1)),
	assert(in(P2)),
%	aggiornamento del serbatoio
	serbatoio(S),
	N_S is S - 1,
	retract(serbatoio(S)),
	assert(serbatoio(N_S)),
	simula(va(P1,P2)).

two_level_planner:esegui_azione_base(deposita) :-
%	aggiornamento del carico quando non vi sono
% più cassonetti da svuotare
	retract(carico(C)),
	assert(carico(0)),
	simula(deposita).


two_level_planner:esegui_azione_base(scarica) :-
% aggiornamento del carico quando ci sono ancora
%	cassonetti da raccogliere
	retract(carico(C)),
	assert(carico(0)),
	simula(scarica).

two_level_planner:esegui_azione_base(rifornimento) :-
	benzinaio(P3),
	in(P3),
%	aggiornamento del serbatoio
	serbatoio(Se),
	step(['Serbatoio prima del rifornimento: ' , Se]),
	retract(serbatoio(S)),
	capienza_serbatoio(C),
	assert(serbatoio(C)),
	simula(rifornimento).

two_level_planner:esegui_azione_base(raccoglie) :-
	in(P),
	retract(deve_prendere(P)),
%	aggiornamento del carico
	carico(C),
	cassonetto(P,A,X),
	step(['Cassonetto di area ', A]),
	N_C is C + X,
	retract(carico(C)),
	assert(carico(N_C)),
% aggionemento del cassonetto
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

pred(raccolta(atom, point, list(point), list(point), list(integer), point, point, integer, integer)).
%  raccolta(W, P, Raccolta, Lista_aree, Lista_upperbound, B, Q, C, A) : il mondo W � stato
%  caricato, l'agente si trova inzialmente in P, deve passare per i
%  punti Raccolta e depositare in Q. Può fare rifornimento in B.
%  Lista_aree  lista contenente i punti che rappresentano le diverse distribuzioni di densita nel mondo
%	 Lista_upperbound lista che rappresenta gli upperbound corrispondenti ai punti contenuti in Lista_aree
%  C è la capienza_serbatoio e A è la capienza_camion
%  MODO (+,+,+,+,+,+,+,+,+) semidet, FALLISCE se W non � caricato


raccolta(W, Pos, Racc, Lista_aree, Lista_upperbound, B, Q, C, A) :-
% preparo lo stato iniziale del mondo+agente
	maplist(retractall, [in(_), deposito(_), deve_prendere(_), benzinaio(_), serbatoio(_), carico(_),
	 							cassonetto(_,_,_), capienza_camion(_),capienza_serbatoio(_), info_area(_,_)]),
	length(Lista_aree, La),
	length(Lista_upperbound, Lu),
	% le due liste devono essere lunghe uguali
	La == Lu,
	make_area(Lista_aree, Lista_upperbound),
	assert(in(Pos)),
	assert(deposito(Q)),
	assert(benzinaio(B)),
	assert(serbatoio(C)),
	assert(carico(0)),
	forall(member(P,Racc), assert(deve_prendere(P))),
	forall(member(P,Racc), inizializza_cassonetto(P, Lista_aree, Lista_upperbound)),
	assert(capienza_serbatoio(C)),
	assert(capienza_camion(A)),

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
					draw_fig(W, box(15,[col(blue)]), PR) ),
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
