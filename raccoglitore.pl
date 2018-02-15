:- use_module(library(draw_world)).
:- use_module(mondi).
:- use_module(geometria).
import_type(geometria, [direzione/0,
			point/0]).

:- style_check(-discontiguous).

:- maplist(write, [
'\n***********************************************************',
'\nRaccoglitore, agente reattivo con stato,  raccoglitore_help',
'\n***********************************************************\n']).

raccoglitore_help :- maplist(write, [
'\nstart(W, P1, P2) : nel mondo W l''agente va da posizione P1 a',
'\n   posizione P2, dove raccoglie un oggetto e lo porta in P3;',
'\n il mondo va prima caricato, vedi mondi.pl',
'\nEsempio di esecuzione con successo:',
'\n?- load_world(cw(1)).',
'\n?- start(cw(1), point(2,2), point(2,14), point(5,2)).'
]).

%============================================================
%   SCHEMA DI AGENTE REATTIVO CON STATO INTERNO
%===========================================================

type(open:stato_interno).
%  stato_interno: stato interno dell'agente, dipende alla storia precedente e influisce sul comportamento
type(open:obiettivo).
%   l'obiettivo generale dell'agente
type(open:azione).
%  le azioni dell'agente, che cambiano lo stato del mondo+agente

pred(agente(stato_interno, obiettivo)).
%  agente(S, Goal) :  genera ed esegue la sequenza di azioni dell'agente a partire dallo stato_interno S e dallo stato
%  del mondo+agente, in base alla implementazione dei predicati aperti decidi_azione ed esegui
%  MODO (+,+)
open_pred(fine(stato_interno, obiettivo)).
%  fine(S, G) :  l'obiettivo G è raggiunto (e l'agente termina)
%  MODO  (+,+) semidet
open_pred(decidi_azione(stato_interno, obiettivo, azione)).
%  decidi_azione(S, G, A) : nello stato_interno S e con goal G l'agente decide di fare l'azione A
%  MODO:   (+,+,-) det (nondet per agenti non deterministici)
open_pred(esegui(azione, stato_interno, stato_interno)).
% esegui(A, S1,S2) : l'agente esegue A e passa da stato_interno S1 a stato_interno S2
% MODO  (+,+,-) det  (in caso di azioni deterministiche)

agente(Stato, Obiettivo) :-
	fine(Stato, Obiettivo),!
	;
	decidi_azione(Stato, Obiettivo, Azione),!,
	esegui(Azione, Stato, NuovoStato),
	agente(NuovoStato, Obiettivo).



%===============================================================
%  Agente "Raccoglitore".  Un esempio di implementazione di un agente
%  reattivo con stato che si muove in un ambiente con ostacoli e va a
%  raccogliere un oggetto in una posizione e lo trasporta in
%  un'altra posizione.  Itipi e predicati aperti sono implementati come
%  segue
%==============================================================

type([presa, trasporto, fine]:stato_interno).
%   presa:  l'agente va a prendere l'oggetto
%   trasporto: l'agente trasporta l'oggetto
%   fine:  ha depositatol'oggetto
type([trasporta(point, point)]:obiettivo).
%  trasporta(P1,P2) : raccogli l'oggetto O in P1 e
%  portalo in P2
type([va_in(direzione, point), raccogli, deposita]:azione).
%  va(Dir,P) : avanza di un passo in direzione Dir e prossimo punto P
%  raccogli: raccogli l'oggetto che si trova nella posizione in cui sei
%  deposita: deposita l'oggetto che stai trasportando

pred(in(direzione,point)).
%  in(Dir, P) : predicato che riguarda lo stato del mondo+agente,
%  l'agente si trova in posizione P voltato in direzione Dir
:- dynamic(in/2).

pred(fine(stato_interno, goal)).
%  fine(fine, _): finisce quando ha raggiunto lo stato
%  interno fine.
%
pred(decidi_azione(stato_interno, goal, azione)).
% decidi_azione(Stato, trasporto(P1,P2), Azione):
% a) Se l'agente è nello Stato=presa e in posizione P diversa da P1,
%    l'azione è di avvicinarsi a P1
% b) Se l'agente è nello Stato=presa i in posizione P1, l'azione è
% raccogli
% c) Se l'agente è nello Stato=trasporto e in posizione P diversa da P2,
%    l'azione è di avvicinarsi a P2
% d) Se l'agente è nello Stato=trasporto e in posizione P2, l'azione è
% deposita
%
pred(esegui(azione, stato_interno, stato_interno)).
%  esegui(va_in(Dir, P), ,Stato, Stato):  l'agente si sposta in P con
%  direzione di avanzamento Dir e non cambia stato interno (casi a), c))
%  esegui(raccogli, presa, trasporto):  l'agente raccoglie l'oggetto e
%  passa allo stato di trasprto (caso b))
%  esegui(deposita, trasporto, fine): l'agente deposita l'oggetto
%  e passa allo stato fine(caso d))
%
pred(next_point(point, point, point, direzione)).
%  next_point(Goal, P, NextP, NextDir) : NextDir è la prima direzione
%  che porta in una posizione NextP libera, andando in senso antiorario
%  a partire dalla direzione da P verso Goal
%

fine(fine,_) :-
	simula(stop).

decidi_azione(presa, trasporta(P1,_P2), va_in(NewDir,NewP)) :-
	in(_Dir,P),
	P \= P1,
	next_point(P1, P, NewP, NewDir),
	simula(deciso(va_in(NewDir, NewP))).
decidi_azione(presa, trasporta(P1,_P2), raccogli) :-
	in(_Dir,P1),
	simula(deciso(raccogli)).
decidi_azione(trasporto, trasporta(_P1,P2), va_in(NewDir,NewP)) :-
	in(_Dir,P),
	P \= P2,
	next_point(P2, P, NewP, NewDir),
	simula(deciso(va_in(NewDir, NewP))).
decidi_azione(trasporto, trasporta(_P1,P2), deposita) :-
	in(_Dir,P2),
	simula(deciso(deposita)).

esegui(va_in(Dir2,R2), Stato, Stato) :-
	member(Stato, [presa, trasporto]),
	retract(in(_,R1)),
	assert(in(Dir2,R2)),
	simula(mossa(R1,R2)).
esegui(raccogli, presa, trasporto) :-
	current_world(W),
	in(_, P),
	retract(content(W, P, oggetto)),
	simula(raccogli).
esegui(deposita, trasporto, fine) :-
	current_world(W),
	in(_, P),
	assert(content(W, P, oggetto)),
	simula(deposita).

next_point(point(GX, GY), point(X,Y), NextP, NextDir) :-
	DX is sign(GX-X),
	DY is sign(GY-Y),
	rosa(GoalDir, DX, DY),!,
	% GoalDir è la direzione verso il goal point(GX, GY) a
	% partire dalla posizione corrente point(X,Y)
	% cerco la prima posizione adiacente libera ruotando in
	% senso antiorario a partire da GoalDir:
	left_dir(GoalDir,NextDir),
	step(NextDir,point(X,Y), NextP),
	libera_da_ostacoli(NextP).


%=================================================================
%   Per far partire l'agente in un mondo è necessario aver caricato il
%   mondo, aver posto l'agente in una posizione e direzione iniziale,
%   l'oggetto in posizione P1 e aver fissato la psizione di trasporto
%   P2
%   ===================================================================
:- dynamic(current_world/1).
pred(start(atom, point, point, point)).
%  start(W, P,  P1, P2) : il mondo W è stato caricato, l'agente
%  si trova inzialmente in P e ha goal trasporta(P1,P2)
%  MODO (+,+,+,+) semidet   FALLISCE se W non è caricato
%
start(W, Pos, P1,P2 ) :-
	world(W,_, _),!,
	% il mondo W è stato caricato o generato;
	% lo pongo come mondo corrente e lo disegno
	retractall(current_world(_)),
	assert(current_world(W)),
	draw_loaded_world(W,20),

	%  per default, l'agente ha direzione iniziale nord:nord;
	%  asserisco lo stato iniziale e lo disegno
	retractall(in(_,_)),
	assert(in(nord:nord,Pos)),
	assert(content(W,P1,oggetto)),
	draw_fig(W, circ(15,[col(green)]), Pos),
	draw_fig(W, box(15,[col(blue)]), P1),
	draw_fig(W, circ(15,[col(yellow)]), P2),

	step(['stato iniziale con posizione iniziale verde e goal giallo']),!,
	% messaggio iniziale e poi parte l'esecuzione dell'agente:
	agente(presa,trasporta(P1,P2)).
start(W,_,_,_) :-
	writeln(W:' non caricato, usa load_world o gen_world'),
	fail.

%==========================================================
%    VISUALIZZAZIONE PASSO PASSO degli eventi rilevanti
%    mediante il predicato simula
%=========================================================

type([deciso(action), % presa decisione
      mossa(point,point), % movimento
      raccogli,
      deposita,
      stop % raggiunto il goal
     ]:evento).
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
simula(deciso(D)) :-
	maplist(write, ['decisione ', D, '\n']).
simula(mossa(P1,P2)) :-
	current_world(W),
	move_fig(W,P1,P2),
	step([]).
simula(raccogli) :-
	in(_,P),
	current_world(W),
	% l'oggetto è rappresentato da box(15,[col(blue)]); siccome
	% viene raccolto scompare dalla posizione P in cui è raccolto
	del_fig(W,box(15,_),P),
	step([]).
simula(deposita) :-
	in(_,P),
	current_world(W),
	% l'oggetto ricompare nella posizione P dove viene depositato,
	% disegnando in P la figura box(15,[col(blue)]) che lo rappresenta
	draw_fig(W,box(15,[col(blue)]),P),
	step([]).
simula(stop) :-
	writeln('Fine'),
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


%================================================
%  Alcuni predicati ausiliarii
%===============================================

libera_da_ostacoli(P) :-
	%P è un punto del mondo corrente libero da ostacoli:
	current_world(W),
	point(W,P),
	not(content(W,P, ostacolo)).
point(W, point(X,Y)) :-
	world(W,Width,Height),
	between(1,Width,Y),
	between(1,Height,X).
