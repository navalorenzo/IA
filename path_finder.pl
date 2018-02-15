
:- maplist(write, [
'\n******************************************************',
'\n     Agente path_finder,  pianificatore,    path_finder_help',
'\n******************************************************']).

:- style_check(-discontiguous).

:- use_module(library(draw_world)).
import_type(draw_world, [point/0]).
:- use_module(geometria, [step/3,
			 length_step/2,
			 distance/4 ]).
:- use_module(mondi, [draw_loaded_world/2,
		      draw_content/4,
		      navigable/2,
		      load_world/1,
		      gen_world/3,
		      world/3]).
:- use_module(library(search), [solve/3,
				set_strategy/1,
			        ds_on/0,
			        ds_off/0]).

:- dynamic(current_world/1).

type([va(point,point)]:action).
% va(P1, P2): azione di spostamento di un quadretto da P1
% a P2, in una direzione verticale, orizzontale o diagonale
% La posizione P2 deve essere "navigabile", cioè libera da ostacoli.

pred(in(point)).
% in(P):  l'agente si trova nel punto P
:- dynamic(in/1).

pred(esegui(va(point,point))).
% esegui(va(P1,P2)) :  possibile se l'agente è in P1
% e P2 è navigabile; sposta l'agente in P2 con retract e assert
% MODO (+) det
pred(start(any,point,point)).
% start(W, P,G) : calcola un cammino e lo segue, per andare
% da P a G nel mondo caricato W
% MODO (++,++,++) semidet
pred(segui_cammino(list(point))).
%  segui_cammino(Path) :  simula l'agente che segue
%  il cammino Path pianificato
%  MODO (++) det

esegui(va(P1,P2)) :-
	retract(in(P1)),
	assert(in(P2)),
	simula(va(P1,P2)).

segui_cammino([P1,P2|Path]) :-
	esegui(va(P1,P2)),
	segui_cammino([P2|Path]).
segui_cammino([P]) :-
	simula(raggiunto(P)).

start(W, P, G) :-
	world(W,_,_),!,
	% pongo W come mondo corrente e lo disegno
	retractall(current_world(_)),
	assert(current_world(W)),
	draw_loaded_world(W,20),

	%  pongo l'agente nello stato iniziale e lo disegno
	%  in verde, assieme al nodo goal G in giallo
	retractall(in(_)),
	assert(in(P)),
	draw_fig(W, circ(15,[col(green)]), P),
        draw_fig(W, circ(15,[col(yellow)]), G),

	%  calcolo il piano
	get_path(P, G, Path),!,
	step(['Calcolato un piano per andare da ', P, ' (verde) a ', G,
	      ' (giallo)\n']),

	% e lo eseguo
	segui_cammino(Path).
start(W,_,_) :-
	writeln(W:' non caricato, usa load_world o gen_world').

%===================================================
% I nodi di pianificazione e il grafo di ricerca
%===================================================

type([{point}]:nodo_pianificazione).
%  i punti navigabili sono i nodi dello spazio di ricerca; un punto
%  navigabile rappresenta una posizione possibile dell'agente
%
pred(arc(nodo_pianificazione, nodo_pianificazione,number)).
%   arc(P1,P2,C) : l'azione va(P1,P2) è ipoteticamente possibile
%   se P1, P2 sono punti navigabili adiacenti un una delle 8 direzioni
%   possibili; il costo è la lunghezza in quella direzione (1 in
%   orizzontale e verticale, 1.42 in diagonale)
%
arc(P1, P2, C) :-
       step(Dir,P1,P2),
       current_world(W),
       navigable(W,P1),
       navigable(W,P2),
       length_step(Dir,C).

%Implemento i predicati aperti di path_search in library(search)
%
path_search:neighbours(S, V) :-
	setof(X, C^arc(S,X,C), V), ! ; V=[].

path_search:cost(S1,S2,C) :-
	arc(S1,S2,C).

path_search:heuristic(P,H) :-
	selected_distance(Dist) ->
	% se ho scelto una distanza l'euristica è la distanza dal target
	target(G),
	distance(Dist,P,G,H)
	;
	% altrimenti è 0
	H=0.

%  uso solve per ottenere un cammino; con strategia A* è un ottimale
%  usando predicati dinamici per start e goal:
:- dynamic(start_node/1). %  nodo di start
:- dynamic(target/1).	  %  nodo goal
get_path(P,G,Path) :-
	retractall(start_node(_)),
	assert(start_node(P)),
	retractall(target(_)),
	assert(target(G)),
	solve(path_finder:start_node, path_finder:target, sol(_,Path,_Cost)).

%=========================================================
%          INTERFACCIA
%=========================================================

%==========================================================
%    VISUALIZZAZIONE PASSO PASSO degli eventi rilevanti
%    mediante il predicato simula
%=========================================================

type([va(point,point), raggiunto(point)]:evento).

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

simula(va(P1,P2)) :-
	current_world(W),
	move_fig(W,P1,P2),
	step([]).
simula(raggiunto(P)) :-
	current_world(W),
	step(['Raggiunto il target ', P, '  FINE\n']),
	destroy(W).

step(Msg) :-
	maplist(write, Msg),
	nl,
	readln(L),
	(   L=[t|_] -> trace
	;   L=[a|_] -> abort
	;   L=[n|_] -> notrace
	;   true).


%===========================================================
%   Scelta distanza da usare nella euristica
%=============================================================

:- dynamic(selected_distance/1).
set_distance(diagonal) :-
	retractall(selected_distance(_)),
	assert(selected_distance(diagonal)).
set_distance(manhattan) :-
	retractall(selected_distance(_)),
	assert(selected_distance(manhattan)).
set_distance(euclid) :-
	retractall(selected_distance(_)),
	assert(selected_distance(euclid)).
clear_distance :-
	retractall(selected_distance(_)).

%=============================================================
%  help
%=============================================================

path_finder_help :- maplist(write,[
'\nVedere mondi_help per la gestione e caricamento dei mondi',
'\nstart(+Mondo, +P1, +P2) : agente che calcola un percorso da P1 a P2',
'\n     e lo segue',
'\nset_strategy(S) per scegliere la strategia S, vedi search_help',
'\nset_distance(Dist) per scegliere la distanza da usare nella euristica',
'\n      Le distanze sono diagonal, euclid, manhattan',
'\nclear_distance per passare ad euristica 0',
'\nprovare con strategia astar e ps(closed) [potatura chiusi] e diverse',
'\neuristiche (scegliendo la distanza da usare o lasciando euristica 0):',
'\n?- load_world(r1).',
'\n?- set_strategy(astar).',
'\n?- set_strategy(ps(closed)).',
'\n?- start(r1,point(21,21), point(2,16)).',
'\nCon ds_on attivare le statistiche e confrontare il fattore di branching effettivo',
'\ncon euristica 0 e con euristiche basate su diverse distanze',
'\n\nProvare poi  ?- start(r1,point(25,21), point(2,16)). Si noti che con euristica 0',
'\nnon si arriva ad ottenere una soluzione, mentre si arriva con le altre euristiche' ]).
