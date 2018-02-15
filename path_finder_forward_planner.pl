
:- style_check(-discontiguous).

:- use_module(geometria, [step/3,
			 length_step/2,
			 distance/4 ]).
:- use_module(mondi, [draw_loaded_world/2,
		      draw_content/3,
		      navigable/2,
		      load_world/1,
		      gen_world/3,
		      world/3]).
:- use_module(library(forward_planner), [plan/5]).

:- dynamic(current_world/1).

type([in(point)]:planning_state).
% in(P) rappresenta lo stato esterno  in(P), in cui l’agente si trova nel punto P
type([va(point,point)]:action).
% va(P1,P2) : l'agente si sposta da P1 a P2

% Implemento i predicati aperti di forward_planner
forward_planner:do_action(va(P1,P2), in(P1), in(P2), Cost) :-
        step(Dir,P1,P2),
       current_world(W),
       navigable(W,P1),
       navigable(W,P2),
       length_step(Dir,Cost).
forward_planner:h(in(P),H) :-
       target(in(G)),
       distance(diagonal,P,G,H).

% per ottenere il piano uso plan/5 esportato da forward_planner:
:- dynamic(start_node/1).
:- dynamic(target/1).
get_plan(W, P,G,Plan, Cost) :-
	world(r1,_,_),!,
	retractall(current_world(_)),
	assert(current_world(W)),
	retractall(start_node(_)), assert(start_node(in(P))),  % fisso il predicato di start
	retractall(target(_)), assert(target(in(G))), % fisso il predicato di goal
		plan(start_node, target, _,Plan,Cost).
get_plan(W, _P,_G,_Plan, _Cost) :-
	writeln(W:' non caricato'),
	fail.

path_finder_forward_planner_help :- maplist(write, [
'*****************************************************',
'path_finder_forward_planner_help:',
'\nPer provare il funzionento:',
'\n?- load_world(r1).',
'\nget_plan(r1,point(14,14),point(2,1),Plan,Cost).']).

:- path_finder_forward_planner_help.
