:- module(two_level_planner, [get_plan/3,
			      get_action_plan/4,
			      esegui/1,
			      set_strategy/1,
			      get_strategy/1,
			      ds_on/0,
			      ds_off/0,
			      two_level_planner_help/0]).
:- style_check(-discontiguous).
:- use_module(library(strips_planner)).

:- maplist(write,[
'\n*********************************************************',
'\nCARICATO two_level_planner;   two_level_planner_help',
'\n**********************************************************']).

two_level_planner_help :-
	maplist(write,[
'\nget_plan(+ParemeterList, -Plan, -Cost) :',
'   Plan piano per ottenere quanto richiesto in ParemeterList con costo Cost',
'\nget_action_plan(+St, +Act, -Plan, -Cost) : Plan e'' un piano che esegue',
'\n   la macro-azine Act partendo dallo strips_state St',
'\nesegui(+Plan) : esegue il piano a due livelli Plan',
'\n\nPredicati aperti per la pianificazione:',
'\n  add_del(+Livello, +Azione, +StripsState, --Add, --Del, --Cost)',
'\n  h(+Livello, +StripsState, --H), dove H � il valore euristico di StripsState',
'\n  starting_state(+Paremeters, --StripsState), stati iniziali di livello 0',
'\n  goal_state(+Paremeters, --StripsState), stati goal di livello 0',
'\n  action_starting_state(+St0, +Act, --St1): St1 strips_state iniziale di livello 1',
'\n    nell''esecuzione della macro-azione Act con stato iniziale St0 di livello 0',
'\n  action_final_state(+St0, +Act, +G): G e'' uno strips_state finale di livello 1',
'\n    nell''esecuzione della macro-azione Act con stato iniziale St0 di livello 0',
'\n\nPredicati aperti per esegui:',
'\n  macro_azione(+Act) : Act � una macro-azione di livello 0',
'\n  action_starting_state(+Act, -St1) : St1 � lo strips_state iniziale di Act',
'\n    che rappresenta lo stato attuale di mondo+agente',
'\n  esegui_azione_base(+A, +S1, -S2) : esecuzione di A nello stato interno S1',
'\n     con transizione a S2'
]).
%=======================================================================
%   SCHEMA LIVELLI DI PIANIFICAZIONE su 2 livelli
%   Azioni di livello 0 :  azioni possibilmente macro
%   Azioni di livello 1: azioni base usate per eseguire le macro-azioni
%   di livello 0
%=======================================================================


type(open:fluent).
%   i fluenti, usiamo strips_planner come pianificatore
%   di base
type(open:action).
%  le azioni di livello 0 e di livello 1

open_pred(add_del(integer,action,strips_state,list(fluent), list(fluent), number)).
%   add_del(K, Act, St, Add, Del, Cost) :  azioni di livello K
%
open_pred(h(integer, strips_state, number)).
%  h(K, St, H) : H = valore euristico per lo stato St di livello K
%
open_pred(starting_state(list(any), strips_state)).
%  starting_state(Paremeters, St) :  St � lo strips_state iniziale
%  per il piano di livello 0
%  MODO  (++,--) nondet
%
open_pred(goal_state(list(any), strips_state)).
%  goal_state(Parameters, St):  St � uno stato goal per il livello 0
%  MODO (++,+) semidet
%
open_pred(action_starting_state(strips_state, action, strips_state)).
%  action_starting_state(St, Act, St1) : St1 � lo strips_state iniziale
%  per il piano di livello 1 che esegue Act a partire da St
%  MODO (++,++,--) nondet
%
open_pred(action_final_state(strips_state, action, strips_state)).
%  action_final_state(Parameters, St, G): G � uno stato goal per il
%  piano di livello 1 che esegue Act a partire da St
%  MODO (++,++,+) semidet

pred(get_plan(list(any), list(action), number)).
%   get_plan(Input, Plan, Cost) :  Plan piano per ottenere
%   l'obiettivo descritto in Input, con costo Cost
%
pred(get_action_plan(strips_state, action, list(action), number)).
%   get_action_plan(St, Act, Plan, Cost) : Plan � un piano che esegue
%   la macro-azione Act partendo dallo strips_state St
%
local_pred(action_plan(strips_state, action, list(action), number)).
% action_plan(St, Act, Plan, Cost) : Plan � un piano di esecuzione
% della macro-azione Act a partire dallo stato St
:- dynamic(action_plan/4).
%  memorizza i piani di esecuzione delle macro-azioni gi� ottenuti

% uso level per gestire i livelli
:- dynamic(level/1).
set_level(K) :-
	retractall(level(_)),
	assert(level(K)).

%  definisco azioni e euristica per livelli
strips_planner:add_del(Act, St, Add, Del, Cost) :-
	level(K),
	add_del(K,Act, St, Add, Del, Cost).

strips_planner:h(St, H) :-
	level(K),
	h(K,St,H).

% =========================================================
%Livello 0: cerco un piano di alto livello
%==========================================================
get_plan(ParameterList, Plan, Cost) :-
	starting_state(ParameterList, St0), % starting state livello 0
	set_level(0),
	retractall(action_plan(_,_,_,_)),
	plan(two_level_planner:start_with(St0),
	     two_level_planner:goal_state(ParameterList), _, Plan, Cost).

%============================================================
%  Livello 1: cerco un piano di esecuzione di una macro-azione, a meno
%  che non l'abbia gi� calcolato
%  ===============================================================
get_action_plan(St, Action, Plan, Cost) :-
	%action_starting_state(St1, Action),
	%step(['Stato lv0: ', St, 'Stato lv1: ', St1]),
	% con St1 al posto di St calcola i piano molto più velocemente ma non controlla la benzina
	action_plan(St1, Action, Plan, Cost),!.
	%  piano gi� calcolato
get_action_plan(St, Action, Plan, Cost) :-
	set_level(1),
	% entro in livello 1
	action_starting_state(St, Action, St1), %starting state livello 1
	plan(two_level_planner:start_with(St1),
	     two_level_planner:action_final_state(St,Action), _, Plan, Cost),!,
	assert(action_plan(St, Action, Plan, Cost)),
	% esco e ripongo il livello a 0
	set_level(0).

%  Mi serve per passare lo stato di start a plan
start_with(X,X).

%============================================================
%   ESECUZIONE DI UN PIANO A 2 LIVELLI
%==========================================================


pred(esegui(list(action))).
%   esegui(P, S1,S2) : esegue il piano P.   MODO  (+) det
%
local_pred(esegui_azione(action, stato_interno, stato_interno)).
% esegui(A, S1,S2) : l'agente esegue A e passa da stato_interno S1
% a stato_interno S2
% Se A � macro, l'esecuzione avviene recuperando il piano per
% A.   MODO (+,+,-) det
%
open_pred(macro_azione(action)).
%  macro_azione(Act) : Act � una macro-azione
%  MODO (+) semidet
%
open_pred(action_starting_state(action, strips_state)).
%  action_starting_state(Act, St1) : St1 � lo strips_state iniziale
%  per il calcolo del piano di livello 1 che esegue Act a partire dallo
%  stato corrente del mondo+agente
%  MODO (++,--) nondet
%
open_pred(esegui_azione_base(azione, stato_interno, stato_interno)).
% esegui_azione_base(A, S1,S2) : l'agente esegue A base e passa da
% stato_interno S1 a stato_interno S2 MODO (+,+,-) det (in caso di
% azioni deterministiche)
%
open_pred(action_starting_state(action, strips_state)).
%  action_starting_state(A, S) :  i fluenti in S rappresentano
%  lo strips_state iniziale per il calcolo di un piano per A
%  a partire dallo stato corrente del mondo+agente.
%  MODO (++,--) det


esegui([]).
esegui([A|Piano]) :-
	esegui_azione(A),!,
	esegui(Piano).

esegui_azione(A) :-
	macro_azione(A),!,
	% se A � macro, calcolo il piano di esecuzione di A
	% a partire dallo stato attuale e lo eseguo
	action_starting_state(A, St0),
	get_action_plan(St0,A,PianoAzione,_),
	esegui(PianoAzione).
esegui_azione(A) :-
	%  se A non � macro, � un'azione base direttamente eseguibile
	esegui_azione_base(A).
