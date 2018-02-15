:- module(strips, [do_action/4, states_to_transitions/2, states_to_actions/2]).

unit(strips, []).
:- maplist(write, [
'\n\n*************************************************************\n',
' CARICATO STRIPS: strips_help per una breve guida\n\n',
'***************************************************************\n']).

strips_help :- maplist(write, ['strips fornisce:',
'\nIl tipo srips_state, implementato come ordset(fluent).',
'\nIl tipo transition: tr(S1,A,S2)transizione da S1 a S2 con azione A.',
'\ndo_action(?A, +S, ?S1, Cost) nondet:',
'     dato uno stato S trova le azioni A e stati S1 tali che',
'     A e'' eseguibile in S e fa passare a S1 con costo Cost',
'Tipi e predicati APERTI:',
'Tipo fluent  dei fluenti',
'Tipo action   delle azioni',
'Predicato add_del(+A, +S, -Add, -Del, -Cost) nondet:',
'      A eseguibile in S con costo Cost;  Add sono i fluenti',
'      che diventano veri, Del quello che diventano falsi'
		       ]).
/* -----------------------------------------------------------
STRIPS.   Una semplice implementazione di un linguaggio
di azioni alla STRIPS.
L'unità fornisce:

Il tipo state, implementato come ordset(fluent).

Il tipo transition,  con generatore tr(state, action, state)
tr(S1,A,S2)  è una transizione da S1 a S2 con azione A

Il predicato do_action(Azione, Stato, Stato), con modo:
(?, +, ?)
Dato uno stato S trova tutte le azioni A e stati S' tali
che A è eseguibile in S e fa passare a S'
ASSUNZIONE:  le azioni sono deterministiche e non vi è più di
una azione che fa passare da uno stato S ad uno statp S'

Il predicato states_to_transitions(list(State), list(transition))
che mappa una sequenza di stati [S1,S2,...,Sn] nella
sequenza [tr(S1,A1,S2), tr(S2,A2,S3), ..., tr(Sn-1,An-1,Sn)].

L'utente deve fornire:

Il tipo fluent dei fluenti;  i fluenti sono proprietà
del mondo che possono cambiare nel tempo.

Il tipo action delle azioni.

Il predicato  add_del(Act,St, Add, Del) che descrive l'effetto
di una azione Act possibile nello stato St tramite la lista Add
dei fluenti che diventano veri e Del di quelli che diventano falsi
------------------------------------------------------------------*/

%=====simboli_definiti.
% I tipi e i predicati definiti da actions. --------------------

type([{ordset(fluent)}]: strips_state).
%   stati come insiemi (ordinati) di fluenti

type([tr(strips_state,action,strips_state)]:transition).
%  tr(S1,A,S2) rappresenta una transizione S1->A->S2

pred(do_action(action, strips_state, strips_state)).
%  do_action(Act, S1, S2) significa: Act fa passare da S1 a S2
%  MODO:   (-,+,-) nondet

pred(states_to_transitions(list(strips_state), list(transition))).
%   states_transitions(L, T):  mappa una sequenza di stati
%   [S_1,S_2,....,S_n] in
%   [tr(S_1,A_1,S_2), tr(S_2,A_2,S_3),...,tr(S_n-1,A_n-1,S_n)]
%   dove A_k è l'azione che fa passare da S_k a S_k+1
%
pred(states_to_actions(list(strips_state), list(action))).
%   states_transitions(L, T):  mappa una sequenza di stati
%   [S_1,S_2,....,S_n] in
%   [tr(S_1,A_1,S_2), tr(S_2,A_2,S_3),...,tr(S_n-1,A_n-1,S_n)]
%   dove A_k è l'azione che fa passare da S_k a S_k+1
%

% I tipi e i predicati aperti di actions.

type(open:action).
%  le azioni dell'agente che provocano le transizioni di stato

type(open:fluent).
%  i fluenti

open_pred(add_del(action, strips_state, list(fluent), list(fluent), number)).
%   add_del(A, S, Add, Del, Cost) rappresenta l'effetto di una
%   azione A come segue:
%       1)   A è possibile nello stato S
%       2)   La sua esecuzione rende veri i fluenti in Add
%            e falsi quelli in Del
%  MODO:  (?, +, ?, ?) nondet.
%  Dato uno stato S,  add_del(A,S,Add,Del) trova le azioni
%  A possibili in S e il loro effetto Add e Del


% Implementazione non documentata, vedere il codice.
% --------------------

do_action(Act, S1, S2, Cost) :-
	add_del(Act, S1, Add, Del, Cost),
	%  Act è possibile in S1 con
	%  effetto Add, Del; tolgo Del
	%  e aggiungo Add
	del(Del, S1, S),
	add(Add,S,S2).

del([X|L], S, S1) :-
	ord_del_element(S,X,SX),
	del(L,SX,S1).
del([],S,S).

add([X|L], S, S1) :-
	ord_add_element(S,X,SX),
	add(L,SX,S1).
add([],S,S).

states_to_transitions([S0|States], [tr(start,S0)|Transitions]):-
	states_to_trans([S0|States], Transitions).
states_to_trans([S1,S2|States], [tr(C, A,S2)|Transitions]) :-
	do_action(A,S1,S2,C),
	states_to_trans([S2|States], Transitions).
states_to_trans([_S], []).


states_to_actions([S1,S2|States], [A|Actions]) :-
	do_action(A,S1,S2,_),!,
	states_to_actions([S2|States], Actions).
states_to_actions([_S], []).










