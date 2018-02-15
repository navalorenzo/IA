:- module(units, [
	      type_declarations/1,
	      pred_declarations/1,
	      type_generators/2,
	      pred_generators/1,
	      check_unit/1,
	      show_clauses/2
	  ]).

unit(units,[]).
%:- include(unit_checker).
:- discontiguous([type/1,
		  pred/1,
		  import_type/2,
		  import_pred/2,
		  local_pred/1,
		  open_pred/1,
		  section/1]).

/**  <module>
 *   Main module for unit checking (type-checking and
 *   module-checking)
 *   */


 %---------------------------------------------------
 %    GENERAL TYPES

 type([pred(prototype),
       import_pred(atom,list(symbol)),
       local_pred(prototype),
       open_pred(prototype),
       pred(input_symbol),
       type(input_symbol),
       import_type(atom,list(symbol)),
       local_type(input_symbol),
       open_type(input_symbol),
       ( :- import_directive)
      ]:input_decl).
 type([type_def:pattern,
       {symbol}
      ]:input_symbol).
 type([use_module(any),
       use_module(any,any),
       reexport(any),
       reexport(any,any)
      ]:import_directive).


 type([type(type_def, pattern),
       import_type(atom, list(symbol))
      ]:type_decl).
 type([pred(prototype),
       import_pred(atom, list(symbol))
      ]:pred_decl).
 type([{type_decl}, {pred_decl}]:declaration).

 type([pred(pattern, list(var), list(is_type))
      ]:pred_pattern).

 type([gen(pattern, list(var), list(is_type)),
       gen(var, list(var), list(is_type)),
       %open_type,
       generic,
       open,
       call(atom, any)
      ]:gen_pattern).

 type([call, generic, open, imported(atom), {list(prototype)}
      ]:type_def).
 type_def(call).

 type([atom/integer,
       atom:symbol]:symbol).
 %  Name/Arity symbols with possible qualifiers M:N/A

 type([pred(symbol),
       type(symbol)
      ]:decl_symbol).

 type([pred, open, generic, defined, local,
       imported(atom, atom)
      ]:decl_attribute).

 type([call, {decl_attribute}
      ]:comment_line).
 comment_line(_).

 type([module(atom, list(symbol))
      ]:module).

 type([call, {pattern}
      ]:is_type).
 is_type(_).

 type([call
      ]:pattern).
 pattern(P) :-
	catch(P=..[_T|Args],_,fail),
	distinct_vars(Args).
 type([call
      ]:prototype).
 prototype(P) :-
	catch(P=..[_T|_],_,fail).

 type([call, {prototype}
      ]:generator).
 generator(_).

 type([var:is_type, open(any)]:typed_var).

 type([open_c, multiple_c, wrong, all, {integer}]:selection).

 type([{pattern},
       call]:atomic_wff).

 type(open:body).

 atomic_wff(_).



% -----------------------------------------------------------------------
%   Checking

:- use_module('units/util', [
		  distinct_vars/1
	      ]).
import_type(util,[]).

:- use_module('units/loader',[
		  load_declarations/1,
		  load_user_module_declarations/1,
		  type_decl/3,
		  pred_decl/3,
		  type_gen/3,
		  pred_gen/2
	      ]).
import_type(loader,[]).

:- use_module('units/tc',[
		  check_clauses/1,
		  checked/2,
		  wrong_clause/1,
		  multiple_context_clause/1,
		  open_context_clause/2
	      ]).
import_type(tc,[checked_clause/0,
		 numbered_checked_clause/0
		]).

:- use_module('units/unit_error_manager', [
		  unit_error/2,
		  unit_warning/2,
		  write_unit_error/2,
		  write_unit_warning/2
	      ]).
import_type(unit_error_manager,[error_context/0,
			        unit_error/0,
			        unit_warning/0
			       ]).

:- use_module('units/unit_writer', [print_clause/2]).
import_type(unit_writer,[]).


pred(type_declarations(atom)).
type_declarations(Unit) :-
	maplist(write, ['Unit ', Unit, ' type declarations:\n']),
	forall(type_decl(Unit, Type, Attr),
	   write_decl(type_decl(Unit, Type, Attr))).

write_decl(type_decl(_Unit, Decl, Attr)):-
        maplist(write, ['   ', Attr, ':	 ', Decl, '\n']).

pred(pred_declarations(atom)).
pred_declarations(Unit) :-
	maplist(write, ['Unit ', Unit, ' pred declarations:\n']),
	forall(pred_decl(Unit, Pred, Attr),
	   write_decl(type_decl(Unit, Pred, Attr))).

pred(type_generators(atom)).
type_generators(Unit,Type) :-
	maplist(write, ['Unit ', Unit, ' generators of type ', Type, ':\n']),
	forall(type_gen(Gen, Type, Unit),
	   write_gen_type(Gen, Type)).

write_gen_type(Gen, Type) :-
	maplist(write, ['    ', Gen, ': ', Type, '\n']).

pred(pred_generators(atom)).
pred_generators(Unit) :-
	maplist(write, ['Unit ', Unit, ' pred generators:\n']),
	forall(pred_gen(Gen, Unit),
	  maplist(write, ['    ', Gen, '\n'])).

pred(check_unit(atom)).
check_unit(user:Name) :- !,
	(   file_name_extension(_,pl,Name), Path=Name
	;   file_name_extension(Name,pl,Path)),
	load_user_module_declarations(Path),!,
	check_clauses(user),
	forall(unit_error(E,Err),
	       write_unit_error(E,Err)),
	forall(unit_warning(W,C),
	       write_unit_warning(W,C)).
check_unit(Unit) :-
	load_declarations(Unit),!,
	check_clauses(Unit),
	forall(unit_error(E,Err),
	       write_unit_error(E,Err)),
	forall(unit_warning(W,C),
	       write_unit_warning(W,C)),
	forall((
		checked(Unit, K:Clause),
		open_context_clause(K:Clause, Terms)),
	          maplist(write, ['\nOPEN: ', Terms, ' in clause ', K])).

local_type([all,
	    wrong,
	    multiple_c,
	    open_c,
	    {integer},
	    {list(integer)}
	   ]:selection).
pred(show_clauses(atom, selection)).
show_clauses(Unit, multiple_c) :-
	forall((checked(Unit, Clause),
		multiple_context_clause(Clause)),
	       print_clause(Unit,Clause)).
show_clauses(Unit, open_c) :-
	forall((
	          checked(Unit, Clause),
		   open_context_clause(Clause, Terms)),
	       (   maplist(write, ['\nOPEN: ', Terms]),
	           print_clause(Unit,Clause)
	       )).
show_clauses(Unit, wrong) :-
	forall((checked(Unit, Clause),
		wrong_clause(Clause)),
	       print_clause(Unit,Clause)).
show_clauses(Unit, all) :-
	forall(checked(Unit, Clause),
	       print_clause(Unit,Clause)).
show_clauses(Unit, L) :-
	is_list(L),!,
	maplist(show_clauses(Unit), L).
show_clauses(Unit, K) :-
	integer(K),!,
	checked(Unit, K:CL),
	print_clause(Unit, K:CL).




