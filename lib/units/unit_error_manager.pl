:- module(unit_error_manager, [
	      clear_unit_errors/0,
	      assert_unit_error/2,
	      assert_unit_warning/2,
	      write_unit_error/2,
	      write_unit_warning/2,
	      unit_error/2,
	      unit_warning/2

	  ]).
:- include('../unit_checker').

unit(unit_error_manager, []).

:- use_module(util).
import_type(util,[]).

:- use_module('../units',[]).
import_type(units, [input_decl/0,
		    input_symbol/0,
		    prototype/0,
		    symbol/0,
		    pattern/0,
		    type_def/0,
		    is_type/0,
		    import_directive/0,
		    typed_var/0]).

:- dynamic([
       unit_error/2,
       unit_warning/2
   ]).


type([unit(atom),
      clause(atom, integer, any, any),
      decl(input_decl, atom),
      project
     ]:error_context).

type([bad_decl,
      bad_symbols,
      unit_declaration_error(any),
      non_transparent,
      import_error(any),
      not_imported(symbol),
      required_use_module(atom, any),
      not_declared_types(list(any)),
      not_defined_import(any, atom),
      clause_type_error(list(typed_var), any,any,any)
     ]:unit_error).

type([call, open(any), {unit_error}]:unit_warning).
     % in some circumstances, unit errors may become warnings
unit_warning(_).


type([use_module(atom, list(symbol)),
      use_module(atom),
      reexport(atom, list(symbol)),
      reexport(atom)]:import_unit).

pred(unit_error(unit_error, error_context)).

pred(unit_warning(unit_warning, error_context)).

pred(clear_unit_errors).
clear_unit_errors :-
	retractall(unit_error(_,_)),
	retractall(unit_warning(_,_)).

pred(assert_unit_error(unit_error, error_context)).
assert_unit_error(E,C) :-
	unit_error(E1,C1),
       (E1,C1) =@= (E,C) , !
       ;
       assert(unit_error(E,C)).

pred(assert_unit_warning(unit_warning, error_context)).
assert_unit_warning(E,C) :-
	assert(unit_warning(E,C)).

%===============================  WRITING ERRORS

write_errors :-
	forall(unit_error(E,Err),
	       write_unit_error(E,Err)).


%===============================  SINGLE ERROR

pred(write_unit_error(unit_error, error_context)).
write_unit_error(unit_declaration_error(U), _) :- !,
	maplist(write, ['ERROR:  missing or wrong declaration for unit ',U,'\n']).
write_unit_error(bad_decl, decl(Decl, Unit)) :- !,
	maplist(write, ['ERROR:  bad decl.; ',
			Decl, ', in unit ', Unit, '\n']).
write_unit_error(bad_symbols, decl(Decl, Unit)) :- !,
	maplist(write, ['ERROR:  bad symbol list in; ',
			Decl, ', in unit ', Unit, '\n']).
write_unit_error(import_error(CL), unit(Unit)) :- !,
	maplist(write, ['ERROR: bad import: ',
			CL, ', in unit ', Unit, '\n']).
write_unit_error(not_defined_import(S, U1), unit(Unit)) :- !,
	maplist(write, ['ERROR: ', S, ' not specified by ', U1,
			', in unit ', Unit, '\n']).
write_unit_error(not_declared_types(L), decl(Decl, Unit)) :- !,
	maplist(write, ['ERROR: types ', L, ' not declared in ', Decl,
			', of unit ', Unit, '\n']).
write_unit_error(non_transparent, decl(Type, Unit)) :- !,
	maplist(write, ['ERROR: ', Type, ' non transparent type declaration in unit ',
			Unit, '\n']).
write_unit_error(not_imported(P), decl(PredImportDecl, Unit)) :- !,
	maplist(write, ['ERROR: not imported ', P, ' declared in ', PredImportDecl, ' in unit ',
			Unit, '\n']).
write_unit_error(clause_type_error(CNT, Pred, Args, Types), clause(Unit, K, _,_)) :- !,
	copy_term((CNT, Pred, Args, Types), (CC, P, AA, TT)),
	ground_vars('X', CC:P),
	maplist(write, ['ERRORS in ', CC:P, ' of clause ', K, ' of ', Unit, ':\n']),
	write_clause_type_error(Unit, K, AA, TT).
write_unit_error(Err, C) :-
	maplist(write, ['ERROR ', Err, ' in context ', C, '\n']).


write_clause_type_error(Unit, K, AA, TT) :-
	maplist(write, ['      type error: ', AA, ': ', TT,
			'\n      to see the clause: show_clauses(',Unit,',',K,')\n']).


%============================ WRITING WARNINGS

pred(write_unit_warning(unit_warning, error_context)).

write_unit_warning(open(Pred), clause(Unit, K, _,_)) :-!,
	copy_term(Pred, P),
	ground_vars('X', P),
	maplist(write, ['Warning: not specified ', P, ' in clause ', K:Unit, '\n']).
write_unit_warning(clause_type_error(_,_, _,_), _) :- !.  %IGNORED
write_unit_warning(W, C) :-
        maplist(write, ['Warning ', W, ' in context ', C, '\n']).


write_ignored(clause_type_error(CN, Pred, Args, Types), clause(Unit, K,_,_)) :- !,
	copy_term((CN, Pred, Args, Types), (CC, P, AA, TT)),
	ground_vars('X', CC:P),
	maplist(write, ['Warning in ', P, ' of clause ', K:Unit, ':\n']),
	write_clause_type_warning(AA, TT).

write_clause_type_warning(AA, TT) :-
	maplist(write, ['   ', AA, ': ', TT, ' checked\n']).


