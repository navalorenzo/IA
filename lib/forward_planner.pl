:- module(forward_planner, [plan/5,
			    ds_on/0,
			    ds_off/0,
			    get_strategy/1,
			    set_strategy/1,
			    strategy_request/1]).
:- style_check(-discontiguous).
:- maplist(write, [
'\n\n*************************************************************\n',
' CARICATO FORWARD PLANNER: planner_help \n',
'***************************************************************']).

unit(forward_planner, []).
planner_help :-
	maplist(write, [
'A) forward_planner definisce:',
'\nplan(+StartPred, +GoalPred, -Plan, -Cost) : ',
'\n      StartPred:  call(StartPred, S) fornisce uno stato iniziale S',
'\n      GoalPred: call(GoalPred, G) stabilisce se G è uno stato goal',
'\n      Plan è un piano che dallo stato inziale S raggiunge il goal G',
'\n      Cost è il costo del piano\n',
'\nB) Necessita dei tipi e predicati aperti:',
'\nTipo planning_state,  tipo degli stati di pinificazione',
'\nh(+Stato,-H) :   H è il valore euristico h(Stato);',
'\n                 se omesso si assume H=0',
'\ndo_action(Act, St1, St2, Cost): ',
'\n    l''azione Act fa passare dal planning_state S1 al planning_stato S2',
'1n    con costo Cost\n',
'\nC) USA il modulo search per la ricerca del piano;  si consiglia di',
'\nimpostare la strategia astar con potatura dei chiusi.',
'\nUsare search_help per vedere come impostare le strategie e attivare',
'\nla visualizzazione delle statistiche e delle stampe di debug'
	       ]).

:- use_module(search, [get_strategy/1,
		       set_strategy/1,
		       strategy_request/1,
		       solve/3,
		       ds_on/0,
		       ds_off/0
		      ]).
import_type(search, []).

type(open:planning_state).
%   Gli stati nello spazio di ricerca del pianificatore,
%   da non confondere con gli stati del sistema agente+ambiente

open_pred(h(planning_state,number)).
%   h(St, H) :   H = h(St), dove h è l'euristica
%
open_pred(do_action(action, planning_state, planning_state, number)).
%   do_action(A, S1, S2, C) :  l'azione A fa passare da S1 a S2 con
%   costo C;   MODO (+,+,-,-) nondet
%
pred(plan(pred, pred, list(action), number)).
%   plan(Start, Goal, Plan, Cost) :
%   Plan è un piano di costoCost che fa passare da uno stato iniziale S
%   ottenuto con call(Start, S) a uno stato goal G riconosciuto con
%   call(Goal, G)
%   MODO (+,+,-,-) nondet
%   Start e Goal devono essere due predicati tali che
%   call(Start, -S)   fornisce uno stato iniziale S:planning_state
%   call(Goal, +G)  decide se G:planning_state è un goal

path_search:neighbours(S, V) :-
	setof(X, A^C^do_action(A,S,X,C), V), !
	;
	V=[].

path_search:cost(S1,S2,C) :-
	do_action(_, S1, S2, C).

path_search:heuristic(S,H) :-
	h(S,H).

plan(Start, Goal, Path, Plan, Cost) :-
	solve(Start,Goal,sol(_,Path,Cost)),
	states_to_actions(Path,Plan).

states_to_actions([S1,S2|States], [A|Actions]) :-
	do_action(A,S1,S2,_),
	states_to_actions([S2|States], Actions).
states_to_actions([_S], []).


