:- module(search, [solve/3,
		   ds_on/0,
		   ds_off/0,
		   set_strategy/1,
		   get_strategy/1,
		   strategy_request/1,
		   show_strategy/0,
		   get_solution/3,
		   search_help/0]).
:- style_check(-discontiguous).
:- maplist(write, [
'\n*******************************************************\n',
'   CARICATO SEARCH: search_help \n',
'**********************************************************']).
:- include(unit_checker).

unit(search,[]).
%  Unità che accede agli algoritmi di ricerca, per ora
%  limitata a path_search

:- use_module('ga/path_search', [solve/3,
				 ds_on/0,
				 ds_off/0
				]).
import_type(path_search, []).

:- use_module('ga/path_strategy_manager', [
		                  get_strategy/1,
		                  set_strategy/1,
				  strategy_request/2,
				  check_inf/0]).
import_type(path_strategy_manager, []).
:- check_inf.

pred(search_help).
%   stampa una breve descrizione delle funzionalità di search

pred(show_strategy).
%  stampa le strategie correnti

pred(write_strategy_request(strategy_request)).


search_help :-
	nl,
	maplist(writeln,
		[
		 'ALGORITMO DI RICERCA',
		 'search_help:  questo help',
		 'Il problema deve definire:',
		 '    path_search:cost(+N1,?N2,?C)   :  costo di un arco',
		 '    path_search:neighbours(+N,?L)  :  L lista vicini di L',
		 '    path_search:heuristic(+N,?H)   :  H euristica di N',
		 'Viene fornito il predicato:',
		 '    solve(+Start, +Goal, -Sol)   : ',
		 '           Start, Goal predicati unari chiamati con call',
		 '           Sol = pn(N, RevPath, Cost, Heur)',
		 '    get_solution(+Sol, -Cost, -Path) :  estrae cammino e soluzione',
		 'Per caricare una o più strategie usare:',
		 '   set_strategy(+S) carica le strategie S',
		 '   strategy_request(-S) mostra le strategie implementate',
		 '   show_strategy  mostra la strategia corrente'
		 ]),
	nl.

show_strategy :-
	get_strategy(L),
	(   L=[] ->  writeln('Nessuna strategia caricata')
	;   forall(member(S,L), write_strategy_request(S))).

write_strategy_request(S) :-
	strategy_request(R,S),
	writeln(R).

strategy_request(R) :-
	strategy_request(R,_).

get_solution(pn(N, RevPath, Cost, _), Cost, Path) :-
	reverse([N|RevPath], Path).






