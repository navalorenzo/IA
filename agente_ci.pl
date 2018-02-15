:- use_module(mondi).
:- use_module(geometria).
:- use_module(library(draw_world)).
:- use_module(two_level_planner).
import_type(two_level_planner,
	    [action/0, fluent/0, strips_state/0]).


:- style_check(-discontiguous).

agente_ci_help :- maplist(write, [
'\n***********************************************************',
'\nRaccoglitore con conoscenza incompleta  (agente_ci_help)',
'\nProvare ripetutamente:',
'\n?- raccolta(cw(1), point(2,2),[point(5,10), point(8,3)],point(9,13)).',
'\nSi osservi che ad ogni esecuzione l''agente impara.   Con:',
'\n?- dimentica_tutto.',
'\ndimentica tutta la conoscenza acquisita',
'\n***********************************************************\n']).
:- agente_ci_help.

%============================================================
%   SCHEMA DI AGENTE CON DECISIONE E PIANIFICAZIONE
%   A DUE LIVELLI
%==========================================================

type(open:stato_agente).
%  stato_agente:   lo stato interno dell'agente, che dipende
%  dalla storia precedente e influisce sul comportamento
%
type(open:obiettivo).
%   gli obiettivi generali dell'agente
%
type(open:decisione_complessa).
% le decisioni che richiedono un piano
%
type([failed(list(action), any),
      failed(list(action), any, list(action))]:exception).
%  failed(Env, Cause) : interruzione azione, indica ambiente e cause
%  failed(Env, Cause, PianoRestante) : interruzione piano, indica
%  l'ambiente, le cause e il piano che resterebbe da eseguire.
%  L'ambiente è uno stack di una o due azioni: Env = [Az] : eccezione
%  sollevata da Az Env = [AzBase,Az] : eccezione sollevata da AzBase nel
%  corso della esecuzione di un piano per eseguire la macro-azione Az
%
type([esecuzione(action), piano(decisione_complessa)]:decisione).
%  la decisione presa, in base a cui pianificare
%  la decisione "esecuzione(A)" ha un piano molto semplice,[A]
%  la decisione "piano(D)" richiede il calcolo di un piano per portare a
%  termine la decisione complessa D
%
type([{stato_agente},{exception}]:stato_interno).
%  lo stato interno è uno stato "normale" dell'agente (stato_agente)
%  o una situazione imprevista (exception) dovuta a conoscenza
%  incompleta
%
open_pred(fine(stato_interno, obiettivo)).
%   fine(S, G) : l'obiettivo G è raggiunto e l'agente termina
%   MODO (+,+) semidet
%
open_pred(decidi(stato_interno, obiettivo, decisione)).
%  decidi(S, G, D) : nello stato_interno S e con goal G
%  l'agente prende la decisione D
%  MODO:   (+,+,-) det
%
open_pred(macro_azione(action)).
%  macro_azione(A) :  A è una macro-azione.  MODO:   (+) semidet
%
open_pred(esegui_azione_base(list(action),action, stato_interno, stato_interno)).
% esegui_azione_base(Env, A, S1, S2) : l'agente esegue A e passa da
% stato_interno S1 a stato_interno S2; in caso di fallimento lancia una
% eccezione di fallimento; Env = [] per esecuzione a livello 0, Env=[M]
% per esecuzione a livello 1 del piano per la macro-azione M
% MODO (+,+,+,-) det
%
open_pred(action_starting_state(action, strips_state)).
%  action_starting_state(M, St) : M è una macro-azione e St è lo
%  strips_state necessario per calcolare il piano di escecuzione di M
%  vero nello stato attuale del mondo+agente
%
local_pred(esegui_azione(list(action),action, stato_interno, stato_interno)).
% esegui_azione(Env, A, S1, S2) : l'agente esegue A e passa da
% stato_interno S1 a stato_interno S2; in caso di fallimento lancia una
% eccezione di fallimento; Env = [] per esecuzione a livello 0, Env=[M]
% per esecuzione a livello 1 del piano per la macro-azione M
% MODO (+,+,+,-) det
%
local_pred(esegui(list(action), stato_interno, stato_interno)).
% esegui(Piano, S1, S2) : l'agente esegue il Piano e passa da
% stato_interno S1 a stato_interno S2; in caso di fallimento S2 è una
% exception
% MODO (+,+,-) det
%
local_pred(pianifica(stato_interno, decisione, list(azione))).
%  pianifica(S, D, P) : nello stato_interno S l'agente
%  calcola il piano per portare a termine la decisione D
%  MODO: (+,+,-)  det
%
pred(agente(stato_interno, obiettivo)).
%  agente(S, Goal) :  schema di comportamento di un agente
%  con decisioni e pianificazione
%  MODO (+,+) genera e visualizza un comportamento


agente(Stato, Obiettivo) :-
	fine(Stato, Obiettivo),!
	;
	decidi(Stato, Obiettivo, Decisione),
	pianifica(Stato1, Decisione, Piano),!,
	%  esecuzione passo passo, aiuta nel debugging
	step(['Decisione ', Decisione, '\npiano ', Piano]),
	catch( esegui([],Piano, Stato1, NuovoStato),
	       failed(EnvA, Causa, PianoA),
	       % in caso di eccezione di fallimento passo
	       % in stato di fallimento
	       NuovoStato = failed(EnvA, Causa, PianoA)),
	agente(NuovoStato, Obiettivo).


pianifica(_Stato, esecuzione(A), [A]).
pianifica(Stato,  piano(Decisione), Piano) :-
	get_plan([Stato, Decisione], Piano, _Cost).


esegui(_Env, [], Stato, Stato).
%  Piano terminato con successo
esegui(Env,[A|Piano], Stato1, Stato2) :-
	% esecuzione azione e prosecuzione
	catch( esegui_azione(Env, A, Stato1, Stato),
	       failed(EnvA, Causa),
	       % in caso di eccezione di fallimento, la rilancio
	       % includendo l'informazione sul Piano ancora da eseguire
	       throw(failed(EnvA, Causa, Piano))),
	esegui(Env,Piano, Stato, Stato2).

esegui_azione(Env, A, Stato1, Stato2) :-
	macro_azione(A),!,
	action_starting_state(A, St0),
	% lo strips_state St0 rappresenta lo stato attuale del mondo,
	% .. per calcolare il piano per A a partire dallo stato attuale
	get_action_plan(St0,A,PianoAzione,_),!,
	esegui([A|Env], PianoAzione, Stato1, Stato2).
esegui_azione(Env, A, Stato1, Stato2) :-
	esegui_azione_base(Env, A, Stato1, Stato2),
	(   Stato2=failed(Causa) ->
	    % lo stato di fallimento interrompe il processo di esecuzione
	    throw(failed([A|Env], Causa))
	;   true).

%==================================================================
%  SCHEMA DI GESTIONE DELLA CONOSCENZA
%==================================================================
type(open:osservabile).
%  proprietà osservabile del mondo
%
type([{osservabile}, non(osservabile)]:osservazione).
%  una osservazione può essere positiva o negativa

open_pred(osserva(point, osservazione)).
%  osserva(P, Oss)  : l'agente osserva che in P vale Oss

open_pred(puo_assumere(point, osservazione)).
% puo_assumere(P, Oss) : l'agente può assumere che in P
% valga Oss, a meno che non abbia conoscenze contrarie

pred(negazione(osservazione,osservazione)).
%  NonOss è la negazione di Oss
%  MODO (++,--) det
%
pred(sa(point, osservazione)).
%  sa(P, Oss) :  l'agente sa che in P vale Oss
%  MODO (++,--) nondet
:- dynamic(sa/2).

pred(impara(point, osservazione)).
%  impara(P, Oss) : non sa(P, Oss) e lo impara, cioè
%  memorizza sa(P, Oss)
%  MODO (++,++) det

pred(sa_o_assume(point, osservazione)).
%  sa_o_assume(P, Oss) : l'agente sa o assume che in P valga Oss,
%  in assenza di conoscenza contraria
%  MODO (++,--) nondet


negazione(A, non(A)) :-
	A \= non(_).
negazione(non(A), A) :-
	A \= non(_).

impara(P, Oss) :-
	not(sa(P,Oss)),
	assert(sa(P,Oss)).

sa_o_assume(P, Oss) :-
	puo_assumere(P, Oss),
	negazione(Oss, NonOss),
	not(sa(P, NonOss)).

dimentica_tutto :- retractall(sa(_,_)).


%====================================================
%   A)  IMPLEMENTAZIONE DI TIPI E PREDICATI APERTI
%   DELLO SCHEMA
%=====================================================

type([raccolto]:obiettivo).
%  vi è un unico obiettivo, aver raccolto tutto
%
type([vai, fine]:stato_agente).
%  vai : stato esecuzione normale
%  fine : stato finale
%
type([in(point), deve_prendere(point), deposito(point)]:fluent).
%   in(P):  l'agente si trova in P
%   deve_prendere(X): l'agente deve prendere un oggetto in X
%   deposito(Q):  deve depositare gli oggetti in Q

type([va(point,point), va_da_a(point,point), raccoglie, deposita]:action).
%  va(P1,P2) : avanza da P1 a P2 adiacente
%  va_da_a(P1,P2) : va da P1 a P2 non adiacente, macro-azione
%  raccoglie:  raccoglie l'oggetto
%  deposita: deposita gli oggetti

%===============================================
%  Predicati di stato di mondo+agente,  dinamici
%===============================================
%
pred(in(point)).
% in(P):  l'agente si trova in P
:- dynamic(in/1).

pred(deve_prendere(point)).
%  deve_prendere(P) :  deve prendere un oggetto in P
:- dynamic(deve_prendere/1).

pred(deposito(point)).
%  deposito(P) :  il deposito è in P
:- dynamic(deposito/1).

pred(current_world(any)).
%  current_world(W):  W è il mondo in cui si trova l'agente
:- dynamic(current_world/1).

pred(aggira_ostacolo(point, point)).
%  aggira_ostacolo(P,G): da P raggiunge G con comportamento
%  reattivo basato sull'aggiramento antiorario.
%  Si assume che non vi siano "trappole" dovute a ostacoli concavi;
%  altrimenti bisognerebbe ripianificare usando la nuova conscenza

%=======================================================
%  IMPLEMENTAZIONE DEI PREDICATI APERTI di agente
%=======================================================

fine(fine,_).

%----------------

decidi(vai, _, esecuzione(deposita)) :-
	not(deve_prendere(_)),
	in(Q),
	deposito(Q).
decidi(vai, _, piano(raccogli(P, OggettiDaPrendere, Q))) :-
	in(P),
	setof(deve_prendere(X), deve_prendere(X), OggettiDaPrendere),
	deposito(Q).
decidi(failed([va(P,Q),va_da_a(_P1,P2)], ostacolo_in(Q), _Piano), _,
                     esecuzione(aggira_ostacolo(P,P2))).
%----------------------

macro_azione(va_da_a(_,_)).

%------------------------

esegui_azione_base(_Env, va(P1,P2), vai, vai) :-
	sa(P2, non(ostacolo)),
        fai_un_passo(P1,P2).
esegui_azione_base(_Env, va(P1,P2), vai, failed(ostacolo_in(P2))) :-
	sa(P2, ostacolo),
	simula(failed(va(P1,P2), ostacolo_in(P2))).
esegui_azione_base(_Env, raccoglie, vai, vai) :-
	in(P),
	retract(deve_prendere(P)),
	simula(raccogli(P)).
esegui_azione_base(_Env, deposita, vai, fine) :-
	in(P),
	simula(deposita(P)).
esegui_azione_base(_Env, aggira_ostacolo(P,G), _Failed, vai) :-
	%  l'azione aggira_ostacolo presa in stato di failure
	%  fa passare allo stato normale
	aggira_ostacolo(P,G).

aggira_ostacolo(G,G) :- !.
aggira_ostacolo(P,G) :-
	next_pos(G, P, NextP),
	fai_un_passo(P,NextP),
	aggira_ostacolo(NextP,G).

%----------------------------------

action_starting_state(va_da_a(P1,P2), [in(P1), target(P2)]).

%==================================
%  AUSILIARII
%=================================

fai_un_passo(P1,P2) :-
	% eseguo e simulo il passo da P1 a P2 adiacente
	retract(in(P1)),
	assert(in(P2)),
	simula(va(P1,P2)),
	osserva_in(P2).
	%  così so già le posizioni adiacenti libere o occupate
	%  della prossima mossa, da P2

next_pos(G, P, NextP) :-
	direzione(G,P,Dir),!,
	% GoalDir è la direzione verso il goal point(GX, GY) a
	% partire dalla posizione corrente point(X,Y)
	% cerco la prima posizione adiacente libera ruotando in
	% senso antiorario a partire da GoalDir:
	left_dir(Dir,NextDir),
	step(NextDir,P, NextP),
	osserva(NextP, non(ostacolo)).

%=========================================================
%  IMPLEMENTO I PREDICATI APERTI DI GESTIONE CONOSCENZA
%=========================================================

type([ostacolo]:osservabile).
%  l'agente è interessato solo agli ostacoli

osserva(P, Oss) :-
	current_world(W),
	(   content(W, P, ostacolo) ->
	    Oss=ostacolo
	;   Oss=non(ostacolo)).
puo_assumere(_, non(ostacolo)).
%  l'unica assunzione che l'agente può fare è che non ci sia un ostacolo

%  aggiungo:
pred(osserva_in(point)).
%  osserva_in(P):  l'agente osserva tutte le posizioni adiacenti a P
osserva_in(P) :-
	step([osservo]),
	forall(step(_Dir, P, Q),
	       ( sa(Q,_),!
	       ; osserva(Q,Oss),
		 impara(Q,Oss),
		 simula(impara(Q,Oss)))).

%==================================================
%  C)   PIANIFICAZIONE
%==================================================
%=====================================
%   C1) AZIONI DI LIVELLO 0
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
%  C2)  AZIONI DI LIVELLO 1
%=====================================
two_level_planner:add_del(1,va(P1,P2), St, [in(P2)], [in(P1)], Cost) :-
	member(in(P1), St),
	step(Dir,P1,P2), % P2 raggiungibile in un passo in una direzione Dir
	sa_o_assume(P2, non(ostacolo)),
	%  se non so che c'è un ostacolo, assumo non ci sia
	length_step(Dir,Cost).  %distanza nella direzione di spostamento


%=====================================
%  C3)  EURISTICA DI LIVELLO 0
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
%   C4) EURISTICA DI LIVELLO 1
%=====================================
two_level_planner:h(1, St, H) :-
	member(target(P2), St),
	member(in(P1),St),
	distance(diagonal, P1,P2,H).


%=================================================
%  C5)  PREDICATI DI START E GOAL livelli 0 e 1
%=================================================

two_level_planner:starting_state([vai, raccogli(P1, FluentiRaccolta, P2)],
		                 [in(P1), deposito(P2)| FluentiRaccolta]).
	%  [in(P1), deposito(P2) | FluentiRaccolta] sono i fluenti veri nello stato iniziale

two_level_planner:goal_state(_Param, G) :-
	member(fine, G).

two_level_planner:action_starting_state(_St, va_da_a(P1,P2), [in(P1), target(P2)]).
%  negli stati per pianificare va_da_a(P1,P2) mi bastano in(P) e il
%  target(P2)

two_level_planner:action_final_state(_St, va_da_a(_,_), G) :-
%  ho terminato il piano per va_da_a quando raggiungo il target
	member(target(P),G),
	member(in(P),G).


%=======================================================
%    D)  MAIN, fa partire l'agente
%========================================================


pred(raccolta(atom, point, list(point), point)).
%  raccolta(W, P, Raccolta, Q) : il mondo W è stato
%  caricato, l'agente si trova inzialmente in P, deve passare per i
%  punti Raccolta e depositare in Q.
%  MODO (+,+,+,+) semidet, FALLISCE se W non è caricato
%
raccolta(W, Pos, Racc, Q) :-
	% preparo lo stato iniziale del mondo+agente
	maplist(retractall, [in(_), deposito(_), deve_prendere(_)]),
	assert(in(Pos)),
	assert(sa(Pos,non(ostacolo))),
	assert(deposito(Q)),
	assert(sa(Q,non(ostacolo))),
	forall(member(P,Racc),
	       (   assert(deve_prendere(P)), assert(sa(P,non(ostacolo))))),

	%  disegno il mondo e il suo stato
	load_current_world(W),
	draw_agent_state(W),

	% fisso la strategia di default
	set_strategy(astar),
	set_strategy(ps(closed)),

	% faccio le osservazioni iniziali
	osserva_in(Pos),
	simula(avvio),!,

	% faccio partire l'agente
	agente(vai, raccolto).

raccolto :-
	%  predicato obiettivo, non usato; dato solo come esempio
	not(deve_prendere(_)),
	in(D),
	deposito(D).

load_current_world(W) :-
	%  disegno la parte statica del mondo
	load_world(W),
	retractall(current_world(_)),
	assert(current_world(W)),
	%  disegno la conoscenza del mondo W, passando
	%  come parametro draw_knowledge(W)
	draw_loaded_world(W,20,draw_knowledge(W)).

draw_knowledge(W, P, Size) :-
	% disegno la conscenza di una casella
	sa(P,non(ostacolo)) ->
	% caselle che so libere bianche
	true
	;
	sa(P,ostacolo) ->
	%  caselle che so occupate nere
	draw_fig(W, box(Size,[col(black)]), P)
	;
	%  caselle che non conosco grige
	%  in pianificazione le assumo libere
	draw_fig(W, box(Size,[col(grey)]), P).


draw_agent_state(W) :-
	% "disegno" i predicati dinamici di stato del mondo+agente
	in(Pos),
	draw_fig(W, circ(15,[col(green)]), Pos),
	forall(deve_prendere(PR), draw_fig(W, box(15,[col(blue)]), PR)),
	deposito(Q),
        draw_fig(W, circ(15,[col(yellow)]), Q).

%========================================================
%  E)  SIMULAZIONE
%========================================================

pred(simula(evento)).
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

simula(avvio) :- !,
	findall(sa(P,F), sa(P,F), So),
	step(['Parto con astar e potatura chiusi\n', 'Conoscenza iniziale: ', So]).
simula(va(P1,P2)) :- !,
	current_world(W),
	move_fig(W,circ(_,_),P1,P2),
	step([]).
simula(raccogli(P)) :-!,
	current_world(W),
	del_fig(W, box(_,_), P),
	step(['Raccolto oggetto']).
simula(deposita(P)) :-!,
	step(['Depositati gli oggetti in ',P]),
	current_world(W),
	destroy(W).
simula(impara(P,ostacolo)) :-!,
	current_world(W),
	del_fig(W, box(Size,[col(grey)]), P),
	draw_fig(W, box(Size,[col(black)]), P),
	step(['imparato ostacolo in ', P]).
simula(impara(P, non(ostacolo))) :-!,
	current_world(W),
	del_fig(W, box(_,[col(grey)]), P),
	step(['imparato ', P, ' libera']).
simula(X) :-
	step([X]).

step(Msg) :-
	maplist(write, Msg),
	nl,
	readln(L),
	(   L=[t|_] -> trace
	;   L=[a|_] -> abort
	;   L=[n|_] -> notrace
	;   true).














