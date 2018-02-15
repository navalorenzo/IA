:- module(strips_planner, [plan/5,
			    ds_on/0,
			    ds_off/0,
			    get_strategy/1,
			    set_strategy/1,
			    strategy_request/1]).
:- style_check(-discontiguous).
:- maplist(write, [
'\n\n*************************************************************\n',
' CARICATO STRIPS PLANNER: strips_planner_help per una breve guida\n\n',
'***************************************************************\n']).

unit(strips_planner, []).
strips_planner_help :-  maplist(write, [
'A) strips_planner definisce:',
'\nplan(+StartPred, +GoalPred, -Plan, -Cost) : ',
'\n      StartPred:  call(StartPred, S) fornisce uno stato iniziale S',
'\n      GoalPred: call(GoalPred, G) stabilisce se G è uno stato goal',
'\n      Plan è un piano che dallo stato inziale S raggiunge il goal G',
'\n      Cost è il costo del piano\n',
'\nB) Necessita dei predicati aperti:',
'\nh(+Stato,-H) :   H è il valore euristico h(Stato);',
'\n                 se omesso si assume H=0',
'\nadd_del(Act, St, Add, Del, Cost): ',
'\n      Act azione alla STRIPS con costo Cost\n',
'\nC) USA il modulo strips per la definizione delle azioni;',
'\nusare strips_help per vedere come implementare add_del.\n',
'\nD) USA il modulo search per la ricerca del piano;  si consiglia di',
'\nimpostare la strategia astar con potatura dei chiusi.',
'\nUsare search_help per vedere come impostare le strategie e attivare',
'\nla visualizzazione delle statistiche e delle stampe di debug'
	       ]).


:- use_module(strips, [do_action/4,
		       states_to_actions/2]).
import_type(strips, [fluent/0, action/0,
		     strips_state/0]).
%ASSUNZIONE:  le azioni sono deterministiche e non vi è più di
%una azione che fa passare da uno stato S ad uno stato S

:- use_module(search, [get_strategy/1,
		       set_strategy/1,
		       strategy_request/1,
		       solve/3,
		       ds_on/0,
		       ds_off/0
		      ]).
import_type(search, []).

open_pred(h(strips_state,number)).
%   h(St, H) :   H = h(St), dove h è l'euristica
%
open_pred(add_del(action, strips_state, list(fluent), list(fluent), number)).
%   add_del(A, Add, Del, C) : l'azione A rende veri Add e falsi Del ed
%   ha costo C; MODO (++,++,--,--,--) nondet
%
pred(plan(pred, pred, list(strips_state), list(action), number)).
%   plan(Start, Goal, Path, Plan, Cost) :
%   Path è un cammino nello spazio degli stati con costo Cost da uno
%   stato iniziale S ottenuto con call(Start, S) a uno stato goal G
%   riconosciuto con call(Goal, G)
%   MODO (+,+,-,-) nondet Start e Goal devono essere due predicati tali
%   che call(Start, -S) fornisce uno stato iniziale S:strips_state
%   call(Goal, +G) decide se G:strips_state è un goal

path_search:neighbours(S, V) :-
	setof(X, A^C^do_action(A,S,X,C), V), !
	;
	V=[].

path_search:cost(S1,S2,C) :-
	do_action(_, S1, S2, C).

path_search:heuristic(S,H) :-
	strips_planner:h(S,H).

strips:add_del(A,S,Add,Del,Cost) :-
	strips_planner:add_del(A,S,Add,Del,Cost).

plan(Start, Goal, Path, Plan, Cost) :-
	call(Start,S),
	list_to_ord_set(S,OS),
	solve(strips_planner:ord_start(OS),Goal,sol(_,Path,Cost)),
	states_to_actions(Path,Plan).
ord_start(X,X).









