:- module(unit_writer, [print_clause/2]).
unit(unit_writer,[]).
:- include('../unit_checker').

:- use_module(unit_error_manager,[
		  unit_error/2,
		  unit_warning/2,
		  write_unit_error/2,
		  write_unit_warning/2
	      ]).
import_type(unit_error_manager,[
		unit_error/0,
		unit_warning/0,
		error_context/0
	    ]).

import_type(units, [input_decl/0,
		    input_symbol/0,
		    prototype/0,
		    symbol/0,
		    pattern/0,
		    type_def/0,
		    is_type/0,
		    import_directive/0,
		    typed_var/0,
		    atomic_wff/0,
		    body/0
		   ]).

import_type(tc, [numbered_checked_clause/0,
		 checked_clause/0]).

pred(print_clause(atom, numbered_checked_clause)).
print_clause(U, K:wrong_clause(Head, Body)) :-
	copy_term(clause(Head, Body),
		  clause(Head1, Body1)),
	reset_gensym('X'),
	term_variables((Head1,Body1), Vars),
	maplist(ground_v('X'), Vars),
	nl,
	maplist(write, [K,': ERRORS IN:\n']),
	print_head_body(Head1, Body1),
	forall(unit_error(Error, clause(U, K, Head1, Body1)),
	       write_unit_error(Error, clause(U, K, Head1, Body1))),
	forall(unit_warning(W, clause(U, K, Head1, Body1)),
	       write_unit_warning(W, clause(U, K, Head1, Body1))).
print_clause(U, K:clause(Contexts, Head, Body)) :-
	forall(member(CC, Contexts),
	(   copy_term(clause(CC, Head, Body),
		  clause(Context1, Head1, Body1)),
	    reset_gensym('X'),
	    reset_gensym('T'),
	    maplist(mk_ground, Context1),
	    nl,
	    print_grounded_clause(K:clause(Context1, Head1, Body1)),
	    forall(unit_warning(W, clause(U, K, Head1, Body1)),
	       write_unit_warning(W, clause(U, K, Head1, Body1)))
	)).

print_grounded_clause(K:clause(Context, Head, Body)) :-
	writeln(K:Context),
	print_head_body(Head, Body).

print_head_body(Head, Body):-
	write(Head),
	write(' :-\n    '),
	writeln(Body).

mk_ground(X:T) :- !,
	ground_v('X', X),
	term_variables(T,VT),
	maplist(ground_v('T'), VT).

ground_v(Name, Var) :-
	var(Var), !,
	gensym(Name, W),
	Var=W.
ground_v(_Name, _Var).
