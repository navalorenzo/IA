

:- use_module(library(draw_world)).
:- use_module(mondi).
:- use_module(geometria).
import_type(geometria, [direzione/0,
			point/0]).

:- style_check(-discontiguous).

:- maplist(write, [
'\n***********************************************************',
'\nAgente reattivo aggiratore,  aggiratore_help',
'\n***********************************************************\n']).

aggiratore_help :- maplist(write, [
'\nstart(W, P1, P2) : nel mondo W l''agente va da posizione P1 a',
'\n   posizione P2;  il mondo va prima caricato, vedi mondi.pl',
'\nEsempio di esecuzione con successo:',
'\n?- load_world(r1).',
'\n?- start(r1,point(14,14), point(2,1)).',
'\nEsempio di esecuzione con trappola:',
'\n?- load_world(cw(2)).',
'\n?- start(cw(2),point(6,10), point(2,12)).',
'\n\nPer uscire dall''esecuzione con la trppola, digitare a (per abort):',
'\n.......',
'\ndecisione va_in(ovest:ovest,point(4,10))',
'\n|: a',
'% Execution Aborted']).

%============================================================
%   SCHEMA DI AGENTE REATTIVO
%===========================================================

type(open:obiettivo).
%   l'obiettivo generale dell'agente
%
type(open:azione).
%  le azioni dell'agente, che cambiano lo stato del mondo+agente
%

pred(agente(obiettivo)).
%  agente(Goal) :  schema di comportamento di un agente
%  puramente reattivo.
%  Genera ed esegue la sequenza di azioni dell'agente a partire stato
%  iniziale del mondo+agente, in base alla implementazione dei predicati
%  aperti decidi_azione ed esegui
%  MODO (+) genera e visualizza un comportamento
%
open_pred(fine(obiettivo)).
%  fine(G) :  l'obiettivo G è raggiunto (e l'agente termina)
%  MODO  (+) semidet
%
open_pred(decidi_azione(obiettivo, azione)).
%  decidi_azione(G, A) : l'agente decide di fare l'azione A
%  MODO:   (+,+,-) det
%
open_pred(esegui(azione)).
% esegui(A) : l'agente esegue A
% MODO  (+) det  (in caso di azioni deterministiche)
%
agente(Obiettivo) :-
	fine(Obiettivo),!
	;
	decidi_azione(Obiettivo, Azione),!,
	esegui(Azione),
	agente(Obiettivo).


%===============================================================
%  Agente "Aggira Ostacoli".  Un esempio di implementazione di un agente
%  puramente reattivo che si muove in un ambiente con ostacoli da una
%  posizione di partenza ad una di arrivo (l'obiettivo) e aggira gli
%  ostacoli. I tipi e predicati aperti sono implementati come segue
%==============================================================

type([target(point)]:obiettivo).
%  l'obiettivo è raggiungere il punto target
type([va_in(direzione, point)]:azione).
%  una sola azione, va(Dir,P) : avanza di un passo
%  in direzione Dir e vai nel punto P

pred(in(direzione,point)).
%  in(Dir, P) : predicato che riguarda lo stato del mondo+agente,
%  l'agente si trova in posizione P voltato in direzione Dir
:- dynamic(in/2).

pred(fine(goal)).
%  fine(target(G)) :  vero se vale in(Dir, G),
%  cioè l'agente è in G
%
pred(decidi_azione(goal, azione)).
% decidi_azione(target(G), va_in(Dir, P)) : Dir è la prima direzione che
% porta a un punto adiacente P libero, ruotando a sinistra a partire
% dalla direzione dalla posizione correteverso G
% MODO  (++,--) det
% Usa next_point per trovare Dir e P
%
pred(esegui(azione, stato_interno, stato_interno)).
%  esegui(va_in(Dir, P), empty,emmpty):  l'agente si sposta in P con
%  direzione di avanzamento Dir
%
pred(next_point(point, point, point, direzione)).
%  next_point(Goal, P, NextP, NextDir) : NextDir è la prima direzione
%  che porta in una posizione NextP libera, andando in senso antiorario
%  a partire dalla direzione da P verso Goal
%

fine(target(G)) :-
	in(_,G),
	simula(stop(G)).

decidi_azione(target(Q), va_in(NewDir,NewP)) :-
	in(_Dir,P),
	next_point(Q, P, NewP, NewDir),
	simula(deciso(va_in(NewDir, NewP))).

esegui(va_in(Dir2,R2)) :-
	retract(in(_,R1)),
	assert(in(Dir2,R2)),
	simula(mossa(R1,R2)).

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
%   Per far partire l'agente "Aggira Ostacoli" in un mondo è necessario
%   aver caricato il mondo (con il modulo mondi.pl) e aver posto
%   l'agente in una posizione e direzione iniziale.
%   Il predicato start assume che il mondo sia stato caricato, pone
%   l'agente nella sua posizione iniziale e lo fa partire con la
%   posizione Goal indicata
%   ===================================================================
pred(start(atom, point, point)).
%  start(W, P0,  G) : il mondo W è stato caricato, l'agente
%  si trova inzialmente in P0 e deve andare in G
%  MODO (+,+,+) semidet   FALLISCE se W non è caricato

:- dynamic(current_world/1).
start(W, Pos, Goal ) :-
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
	draw_fig(W, circ(15,[col(green)]), Pos),
	draw_fig(W, circ(15,[col(yellow)]), Goal),

	step(['stato iniziale con posizione iniziale verde e goal giallo']),!,
	% messaggio iniziale e poi parte l'esecuzione dell'agente:
	agente(target(Goal)).
start(W,_,_) :-
	writeln(W:' non caricato, usa load_world o gen_world'),
	fail.



%==========================================================
%    VISUALIZZAZIONE PASSO PASSO degli eventi rilevanti
%    mediante il predicato simula
%=========================================================

type([deciso(action), % presa decisione
      mossa(point,point), % movimento
      stop(point) % raggiunto il goal
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
simula(deciso(va_in(NewDir,R))) :-
	maplist(write, ['decisione ', va_in(NewDir,R), '\n']).
simula(mossa(P1,P2)) :-
	current_world(W),
	move_fig(W,P1,P2),
	step([]).
simula(stop(P)) :-
	step(['Fine, raggiunta la posizione ', P, '\n']),
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
