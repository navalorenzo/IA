
:- style_check(-discontiguous).
:- use_module(library(draw_world)).
:- use_module(mondi).
:- use_module(geometria).
:- use_module(library(strips_planner)).

:- maplist(write, [
'\n***********************************************************',
'\nAgente delivery   -  delivery_help',
'\n***********************************************************\n']).

%============================================================
%   SCHEMA DI AGENTE CON DECISIONE E PIANIFICAZIONE
%==========================================================

type(open:stato_interno).
%  stato_interno:   lo stato interno dell'agente, che dipende
%  dalla storia precedente e influisce sul comportamento
%
type(open:obiettivo).
%   l'obiettivo generale dell'agente
%
type(open:decisione).
%  le possibili decisioni dell'agente
%
type(open:azione).
%  le azioni dell'agente, che cambiano lo stato del mondo+agente
%

pred(agente(stato_interno, obiettivo)).
%  agente(S, Goal) :  schema di comportamento di un agente
%  con decisioni e pianificazione
%  Genera ed esegue la sequenza di decisioni e azioni dell'agente a
%  partire dallo stato_interno S e dallo stato iniziale del
%  mondo+agente, in base alla implementazione dei predicati aperti
%  MODO (+,+) genera e visualizza un comportamento
%
pred(esegui(list(azione), stato_interno, stato_interno)).
%   esegui(P, S1,S2) : esegue il piano P, passando dallo stato S1 allo
%   stato S2.     MODO  (+,+,-) det
%
open_pred(fine(stato_interno, obiettivo)).
%   fine(S, G) : l'obiettivo G è raggiunto (e l'agente termina)
%   MODO (+,+) semidet
%
open_pred(decidi(stato_interno, obiettivo, decisione)).
%  decidi(S, G, D) : nello stato_interno S e con goal G
%  l'agente prende la decisione D
%  MODO:   (+,+,-) det
%
open_pred(pianifica(stato_interno, decisione, list(azione))).
%  pianifica(S, D, P) : nello stato_interno S l'agente
%  calcola il piano per portare a termine la decisione D
%  MODO: (+,+,-)  det
%
open_pred(esegui(azione, stato_interno, stato_interno)).
% esegui(A, S1,S2) : l'agente esegue A e passa da stato_interno S1
% a stato_interno S2
% MODO  (+,+,-) det  (in caso di azioni deterministiche)


agente(Stato, Obiettivo) :-
	fine(Stato, Obiettivo),!
	;
	decidi(Stato, Obiettivo, Decisione),
	pianifica(Stato, Decisione, Piano),!,
	esegui(Piano, Stato, NuovoStato),
	agente(NuovoStato, Obiettivo).

esegui([], Stato, Stato).
esegui([A|Piano], Stato1, Stato2) :-
	esegui_azione(A, Stato1, Stato),!,
	esegui(Piano, Stato, Stato2).


%==========================================================
%   IMPLEMENTAZIONE DEI PREDICATI APERTI PER UN AGENTE
%   CHE DISTRIBUISCE LA POSTA:
%   carica la posta da due caselle c0, c1, la distribuisce e
%   torna davanti a c0
%==========================================================

type([c0,c1]:casella).
%  le caselle della posta
%
type([p(integer)]:porta).
%  c0, c1 sono le due caselle
%  p(K): la porta della stanza s(K), per K < 7
%  p(2), p(7) le porte del laboratorio
%
type([{porta},{casella}]:luogo).
%  i luoghi davanti a cui passa il robot
%
type([s(integer), lab]:stanza).
%  le stanze in cui consegnare
%
type([preleva, va(luogo,luogo), consegna]:action).
%   preleva : si trova davanti a una casella e preleva la posta
%   va(L1,L2)  :  da davanti a L1 va davanti a L2
%   consegna : ha un plico da consegnare nella stanza davanti a cui si
%   trova
type([pl(stanza,integer)]:plico).
%  pl(S,K) : plico di K missive destinate alla stanza S
%
type([carica_0, %l'agente all'inizio deve caricare da c0
      caricata_0, % l'agente ha caricato c0
      carica_1, % l'agente ha caricatoc0 e deve caricare da c1
      distribuzione %  l'agente ha ultimato il carico e inizia a
             %  distribuire o sta distribuendo
      ]:stato_interno).
type([decido(action), distribuisco]:decisione).
%  decido(A):  ho dciso di eseguire l'azione A
%  distribuisco:   ho deciso di distribuire la posta caricata
%
type([posta_consegnata]:obiettivo).
%  c'è un solo obiettivo, aver cosegnato tutta la posta

pred(casa(point)).
%  casa(P) : P è la casa del robot
:- dynamic(casa/1).

pred(davanti(luogo)).
%  davanti(L) :  il robot è davanti al luogo L
:- dynamic(davanti/1).

pred(consegna(stanza,integer)).
%  consegna(S,K): il robot deve eseguire una consegna di K plichi nella
%  stanza S
:- dynamic(consegna/2).

pred(posta(casella, stanza, integer)).
%  posta(C, S, K) : la casella C contiene K lettere per la stanza S
:- dynamic(posta/3).

pred(porta(porta,stanza)).
%  porta(P,S) :  P porta della stanza S

pred(delivery(list(plico), list(plico))).
%  delivery(L1, L2) : avvia la simulazione con plichi di missive
%  L1 in casella 1 e L2 in casella2

%=======================================
%  Inizializzo e faccio partire l'agente
delivery(C1,C2) :-
	set_delivery_world(C1, C2), !,
	agente(carica_0,consegne_eseguite).

%==================================
%  Implemento i predicati aperti

fine(_,consegne_eseguite) :-
	davanti(c0),
	not(posta(_,_,_)),
	not(consegna(_,_)),
	simula(fine).

decidi(carica_0, _, decido(preleva)).
decidi(carica_1,_, decido(preleva)).
  % se deve caricare da una casella, decide di prelevare dalla casella
decidi(caricata_0,_,decido(va(c0,c1))).
  % prelevata la posta da c0, decide di andare alla casella c1
decidi(distribuzione, _, decido(va(c1,c0))) :-
	% è davanti a c1 ma non ha consegne da fare
	% decide di tornare a casa
	davanti(c1),
	not(consegna(_,_)).
decidi(distribuzione, _, distribuisco):-
  consegna(_,_).
  %  una volta carico, decide di distribuire la posta


pianifica(_, decido(A), [A]).
%  il piano per eseguire l'azione decisa A è [A]
pianifica(_, distribuisco, Piano) :-
	% il robot si trova davanti a c1 e ha caricato tutte le consegne
	% da eseguire, rappresentate dal predicato dinamico
	setof(consegna(L,K), consegna(L,K), Consegne),
	% Consegne è la lista di fluenti che contiene le
	% consegne da fare in questo momento, calcolo
	% un piano per effettuarle tutte:
	pianifica_con_strips(Consegne, Piano),
	next_step(['Calcolato Piano ', Piano]).

esegui_azione(preleva, carica_0, caricata_0) :-
	davanti(c0),
	posta(c0,_,_),
	forall(posta(c0,L,K), carica_posta(c0,L,K)),!,
	(   consegna(_,_) -> next_step(['Caricata la posta da casella 0'])
	;   next_step(['Non c''e'' posta in casella 0'])).
esegui_azione(va(c0,c1), caricata_0, carica_1) :-
	retract(davanti(c0)),
	assert(davanti(c1)),
	simula(va(c0,c1)),
	next_step(['Raggiunta casella 1']).
esegui_azione(preleva, carica_1, distribuzione) :-
	forall(posta(c1,L,K), carica_posta(c1,L,K)),
	consegna(_,_),!,
	next_step(['Caricata la posta da casella 1']).
esegui_azione(torna_a_casa, distribuzione, fine)  :-
	not(consegna(_,_)), !,
	retract(davanti(c1)),
	assert(davanti(c0)),
	simula(va(c1,c0)),
	next_step(['Non c''e'' posta, torno a casa']).
esegui_azione(va(L1,L2), distribuzione, distribuzione) :-
	retract(davanti(L1)),
	assert(davanti(L2)),
	simula(va(L1,L2)),
	next_step(['Raggiunta porta ', L2]).
esegui_azione(consegna, distribuzione, distribuzione) :-
	davanti(P),
	porta(P,L),
	retract(consegna(L,K)),
	simula(consegna(L)),
	next_step(['Consegnati ', K,' plichi in ', L]).

%  le porte delle stanze
porta(p(K), s(K)) :-
	between(1,8,K),
	K \= 2,
	K \= 7.
porta(p(2), lab).
porta(p(7), lab).

%  predicato di tipo:  i luoghi
luogo(p(K)) :-
	between(1,8,K).
luogo(c0).
luogo(c1).

%  ausiliario, l'azione di caricare un plico di K missive
%  per la stanza L agendo sui predicati dinamici posta e c
carica_posta(Casella,L,K) :-
	retract(posta(Casella,L,K)),
	% retract: le K missive per L non sono più in casella
	% le carico sul robot:
	(   retract(consegna(L,H)) ->
	   % se vale consegna(L,H) [il robot già porta H missive per L],
	   % con retract ho cancellato consegna(L,H); aggiungo ad H le
	   % K missive che sto caricando dalla casella
	   H1 is H+K,
	   % e con assert aggiorno il nuovo numero di missive per L
	   assert(consegna(L,H1))
	   ;
	   % se retract fallisce, non c'erano missive per L,
	   % ce ne sono K e pongo:
	   assert(consegna(L,K))
	).


%==========================================
%  Pianificazione con strips_planner
%============================================


type([consegna(stanza,integer), davanti(luogo)]:fluent).
%  consegna(L,P1): deve consegnare un plico di P1 missive in L
%  davanti(L) : si trova davanti a L

pred(pianifica_con_strips(list(fluent), list(action))).
% pianifica(Consegne, Piano) : il robot si trova davanti a c1
% e Consegne è la lista di consegne che deve fare; Piano è un
% piano che porta ad eseguire tutte le consegne partendo da c1
% e tornando vuoto davanti a c0

pianifica_con_strips(Consegne, Piano) :-
	%  uso plan/5 di strips_planner con predicati Start= start(Consegne)
	%  e Goal = goal, dove:
	% call(start(Consegne), S0)  fornisce lo stato iniziale,
	% call(goal, G)  riconosce il goal G; vedi start e goal qui sotto
	plan(start(Consegne), goal, _, Piano, _).

start(Consegne,[davanti(c1)|Consegne]).
%  Nello stato iniziale il robot si trova davanti(c1) e deve
%  eseguire le Consegne
goal([davanti(c0)]).
%  Nello stato goal si trova davanti(c0) e non ha consegne da
%  eseguire

%  CHIUDO i predicati aperti di strips_planner
strips_planner:add_del(va(L1,L2), St, [davanti(L2)], [davanti(L1)], Cost) :-
	member(davanti(L1), St),
	adiacente(L1,L2,Cost).
strips_planner:add_del(consegna, St, [], [consegna(S,K)], 1) :-
	member(davanti(L), St),
	porta(L,S),
	member(consegna(S,K),St).
strips_planner:h(Stato,H) :-
	member(davanti(L),Stato),
	tragitto(Stato, L, H),
	not((tragitto(Stato,L,H1), H1 > H)).

tragitto(Stato, L, Dist) :-
	member(consegna(St1,_), Stato),
	dist_stanza(L, St1, L1, D1),
	dist(L1, c0, D2),
	Dist is D1+D2.
tragitto(Stato, L, Dist) :-
	not(member(consegna(_,_), Stato)),
	dist(L, c0, Dist).

dist_stanza(L, lab, LL, D) :-!,
	% prendo la porta più vicina
	% delle due di accesso al lab
	porta(P1, lab),
	porta(P2, lab),
	P1 \= P2,
	dist(L,L1,D1),
	dist(L,L2,D2),
	(   D1 < D2 ->
	    LL=L1, D=D1
	;   LL=L2, D=D2).
dist_stanza(L, S, PS, D) :-
	porta(PS,S),
	dist(L,PS,D).

%=========================
%  Per l'euristica: calcolo della stanza più a destra rispetto a una
%  posizione

rightmost(St,[],St).
rightmost(St,[davanti(_)|L], Q) :-
	rightmost(St,L,Q).
rightmost(St,[consegna(S,_)|L], Q) :-
	leftside(St,S) ->
	rightmost(S,L,Q)
	;
	rightmost(St,L,Q).

leftside(s(1),S) :-
	member(S,[lab, s(3),s(4),s(5),s(6)]).
leftside(s(8),S) :-
        member(S,[lab, s(3), s(4), s(5), s(6)]).
leftside(lab,S) :-
	member(S,[s(3),s(4),s(5),s(6)]).
leftside(s(3),S) :-
        member(S,[s(4),s(5)]).
leftside(s(6),S) :-
        member(S,[s(4),s(5)]).

%===========================================================
%        SIMULAZIONE DEL COMPORTAMENTO DELL'AGENTE
%        Vengono mostrati passo passo i vari eventi
%===========================================================


pred(next_step(list(atom))).
%  next_step(Msg) :  stampa Msg e attende un input
%  dall'utente; se l'input è t avvia una trace, se a
%  abortisce; in ogni altro caso procede senza far nulla
%  MODO (++) det
next_step(Msg) :-
	maplist(write, Msg),
	nl,
	readln(L),
	(   L=[t|_] -> trace
	;   L=[a|_] -> abort
	;   true).

pred(simula(azione)).
%   simula(A) :  visulizza l'esecuzione di A anche a livello
%   grafico se opportuno
simula(va(St1,St2)) :-!,
	%  stampo l'azione di alto va(St1,St2)
	writeln(eseguo:va(St1,St2)),
	posto_davanti(St2, G),
	%  e ne simulo l'esecuzione quadretto per
	%  quadretto, implementando un agente reattivo
	%  che va dal quadretto in cui si trova, davanti a St1
	%  e raggiunge la posizione goal G davanti a St2
	reattivo(G).
simula(fine) :- !,
	next_step(['FINE']),
	destroy(delivery(1)).
simula(X) :- writeln(X).
       % per le altre azioni, mi limito a stamparle


pred(reattivo(point)).
%  reattivo(Goal) :  esecuzione passo passo e visualizzazione
%  grafica del comportamento di un agente reattivo che dalla
%  posizione in cui si trova raggiunge il punto Goal
%  MODO (++) det  [assumendo che non si incontrino ostacoli]
reattivo(G) :-
	content(delivery(1), P, agente),
	%  P è la posizione attuale dell'agente nel mondo delivery(1)
	(   G = P, !
	    % se P è già il goal G l'agente si arresta
	;   % altrimenti calcola il quadretto adiacente che
	    % più si avvicina a G:
	    next_pos(G, P, NextP),
	    retract(content(delivery(1), P, agente)),
	    assert(content(delivery(1), NextP, agente)),
	    %  cambia lo stato del mondo delivery(1) con retract e assert
	    %  e sposta nella grafica la figura che rappresenta l'agente
	    move_fig(delivery(1), circ(_,[col(green)]), P, NextP),
	    %  chiama next_step per l'esecuzione passo passo
	    next_step([]),
	    %  e procede ricorsivamente
	    reattivo(G)).

next_pos(point(GX, GY), point(X,Y), point(NX,NY)) :-
	DX is sign(GX-X),
	DY is sign(GY-Y),
	NX is X+DX,
	NY is Y+DY.

%===========================================================
%   GEOMETRIA SPECIFICA DEL MONDO delivery(1)
%============================================================


pred(adiacente(luogo,luogo,number)).
%  adiacente(P1,P2,D) : il percorso da P1 a P2 non passa in
%  prossimità di altri luoghi;  D è la lunghezza del percorso
%  MODO (++,++,--) det
%
pred(posto_davanti(luogo, point)).
%  posto_davanti(L,P) :  P è il waypoint davanti
%  al luogo L
%  MODO (++, --)  det

pred(dist(luogo,luogo,number)).
%  dist(X,Y,D) : D è la lunghezza del percorso dal posto davanti al
%  luogo X al posto davanti al luogo Y
%  MODO  (++,++,--) det

adiacente(c0,c1).
adiacente(c1,p(1)).
adiacente(p(I),p(J)) :-
	between(1,7,I),
	J is I+1.
adiacente(p(8),c0).
adiacente(p(2),p(7)).

adiacente(X,Y,D) :-
	(   adiacente(X,Y) ; adiacente(Y,X)  ),
	dist(X,Y,D).

posto_davanti(P, point(X1,Y1)) :-
	luogo(P),
	content(delivery(1), point(X,Y), P),
	rosa(Dir:Dir, DX, DY),
	X1 is X+DX,
	Y1 is Y+DY,
	content(delivery(1), point(X1,Y1), waypoint).

dist(X,Y,D) :-
	posto_davanti(X, PX),
	posto_davanti(Y, PY),
	distance(diagonal,PX,PY,D).


%=========================================================
%  INIZIALIZZAZIONE  MONDO delivery(1) e disegno grafico
%=========================================================

pred(set_delivery_world(list(plico), list(plico))).
%  set_delivery_world(Pl0, Pl1):   inizilizza e disegna un
%  mondo delivery(1) in cui il robot è a casa vuoto, la
%  casella c0 contiene i plichi Pl0 e c1 i plichi Pl1

set_delivery_world(Consegne1, Consegne2) :-
	%  Pulisco la base dati
	retractall(world(delivery(1),_,_)),
	retractall(content(delivery(1),_,_)),
	retractall(casa(_)),
	retractall(posta(_,_,_)),
	retractall(davanti(_)),
	retractall(consegna(_,_)),

	%  carico il mondo vuoto delivery(1) dal modulo mondi
	load_world(delivery(1)),!,
	content(delivery(1), point(CR,CC1), c0),
	CC is CC1+1,
	assert(casa(point(CR,CC))),
	assert(content(delivery(1), point(CR,CC), agente)),
	assert(davanti(c0)),
	forall(member(pl(L,K), Consegne1),
	       (   stanza(L,SL), assert(posta(c0,SL,K)))),
	forall(member(pl(L,K), Consegne2),
	       (   stanza(L,SL), assert(posta(c1,SL,K)))),

	% il mondo delivery(1) è stato caricato, compresa la posta e
	% la posizione inizale dell'agente e la casa; lo disegno
	draw_loaded_world(delivery(1), 30).

stanza(lab,lab).
stanza(I, s(I)) :-
	member(I,[1,3,4,5,6,8]).
stanza(I,_) :-
	not(member(I,[1,3,4,5,6,8,lab])),
	writeln(I:' non e'' un numero di stanza, ERRORE'),
	fail.


%=====================================
%  HELP
%=========================================

delivery_help
:- maplist(write, [
'\nAgente pianificatore delivery',
'\n   delivery(Pl0, Pl1) : nel mondo delivery l''agente preleva',
'\n   i plichi di posta Pl0 dalla casella c0 e Pl1 dalla casella',
'\n   c1 e li distribuisce',
'\nEsempio di esecuzione:',
'\n?- delivery([pl(1,3)], [pl(6,9), pl(lab,2)]).   Esegue le',
'\n    consegne a partire dallo stato iniziale in cui la casella',
'\n    c0 contiene un plico di 3 missive per la stanza 2 (pl(2,3)),',
'\n    e un plico di 2 missive per la stanza 4 (pl(4,2)); analogamente',
'\n    la casella c1 contiene 3 missive per la stanza 5 e 2 per il lab'
]).



