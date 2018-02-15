
:- discontiguous([type/1,
		  pred/1,
		  import_type/2,
		  import_pred/2,
		  local_pred/1,
		  open_pred/1,
		  section/1]).
start_check :-
	use_module(library(units)),
	writeln('\n**********  Units checker, type units_help. for help **********\n').

units_help :- maplist(write, [
'FOR FINITE MODELS:\n',
'*  of_type(?X, ?T):\n    true if X is an element of type T\n',
'*  print_pred(+P/N):\n    prints the tuples of the predicate P/N\n',
'\nFOR UNIT CHECKING:\n',
'*  <unit_name>:start_check.  Loads the unit checker\n',
'*  check_unit(<unit>):\n     check a unit in a loaded module\n',
'*    <unit> may be  user:<file_name> or the name of a module\n',
'*  show_clauses(<unit>, <options>):\n     show the checked clauses;\n',
'     options:  Nth, all, wrong, multiple_c, open_c, [list of options]\n',
'*  type_declarations(<unit>):\n     show the checked type declarations\n',
'*  pred_declarations(<unit>):\n     show the checked pred declarations\n',
'*  type_generators(<unit>, <type>):\n     show the gererators of a given type\n'
		      ]).

'GEN'(call(P), X) :-!,
	call(P,X).
'GEN'({T}, X) :- !,
	of_type(X,T).
'GEN'(X,X) :-
	atom(X).

of_type(X, T) :-
	type(Gen:T),
	member(G,Gen),
	( G=call -> G1=call(T) %  deve essere def. T(X)
	; G1=G),
	'GEN'(G1,X).


print_pred(Req) :-
	Req=P/N,
	catch(functor(Pred,P,N),_,fail) ->
	forall(Pred, writeln(Pred))
	;
	writeln(Req:' richiesta errata, forma attesa:  P/N').

