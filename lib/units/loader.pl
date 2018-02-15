:- module(loader, [load_declarations/1,
		   load_user_module_declarations/1,
		   sub_type_generator/3,
		   unit_clause/4,
		   type_decl/3,
		   type_gen/3,
		   pred_decl/3,
		   pred_gen/2,
		   decl_comment/3]).

:- include('../unit_checker').


unit(loader, []).

:- reexport(prolog_types).
import_type(prolog_types, []).

:- use_module('../units',[]).
import_type(units, [
		symbol/0,
		is_type/0,
		module/0,
		atomic_wff/0,
		body/0,
		decl_attribute/0,
		gen_pattern/0,
		prototype/0,
		pred_pattern/0,
		declaration/0,
		type_decl/0,
		type_def/0,
		input_symbol/0,
		input_decl/0,
		generator/0,
		decl_symbol/0,
		comment_line/0
	    ]).

:- use_module(unit_error_manager, [
	      clear_unit_errors/0,
	      assert_unit_error/2,
	      assert_unit_warning/2
	      ]).
import_type(unit_error_manager, [error_context/0,
				 unit_error/0,
				 import_unit/0]).

:- use_module(util, [
	      mem_var/2,
	      distinct_vars/1
	      ]).
import_type(util,[]).



:- dynamic([unit_export/2,
	    unit_import/2,
	    unit_open/2,
	    unit_clause/4,
	    unit_dcg/3,
	    type_decl/3,
	    type_gen/3,
	    pred_decl/3,
	    pred_gen/2,
	    decl_comment/3]).


pred(unit_export(atom, list(symbol))).
%!	unit_export(?U:atom, ?L:list(symbol))
%	L is the list of the exported pred symbols of U

pred(unit_import(atom, module)).
%!	unit_import(?U:atom, ?M:module)
%	M=module(U1, PredL, TypeL) is imported with import lists
%	PredL and TypeL

pred(unit_open(atom, list(symbol))).
%!	unit_open(?U:atom, ?PL:list(symbol))
%	PL is the lists of the open pred symbols required by U

pred(unit_clause(integer, atom, atomic_wff, body)).
%!     unit_clause(K, Unit, Head, Body)
%      Head and Body of the K-th clause of Unit

pred(unit_dcg(integer, atom, any)).
%!     unit_dcg(K, Unit, Rule)
%      the K-th clause of Unit corresponds to a DCG Rule

pred(type_decl(atom, type_decl, list(decl_attribute))).
%!	type_decl(?Unit:atom, ?TD:type_decl, ?Att:list(decl_attribute))
%       TD is a type declaration of Unit with attributes Att

pred(type_gen(gen_pattern, is_type, atom)).
%!	 type_gen(?Gen, ?Type, ?Unit)
%	 Gen is a generator pattern of a type Type in the scope of Unit

pred(pred_decl(atom, prototype, list(decl_attribute))).
%!	pred_decl(?Unit:atom, ?PD:pred_decl, ?Att:list(decl_attribute))
%       PD is a pred declaration of Unit with attributes Att

pred(pred_gen(pred_pattern, atom)).
%!	 pred_gen(?Gen, ?Unit)
%	 Gen is a generator pattern of a predicate in the scope of Unit

pred(decl_comment(atom, declaration, list(comment_line))).
%decl_comment(?U:atom, ?D:decl_symbol, ?Comment)
%       Comment is the comment of D in the unit U

local_pred(is_term(any, atom, list(any))).
%!	is_term(?Term:any, ?N:atom, ?Args:list(any))
%       =.. ,  fails if arguments are not sufficiently instantiated
is_term(Term, Name, Args) :-
	catch(Term =.. [Name|Args], _, fail).

pred(clear_declarations(atom)).
%!	clear_declarations(+Unit)
%	clears the declaration db for Unit
clear_declarations(U) :-
	retractall(unit_clause(_,U,_,_)),
	retractall(unit_export(U,_)),
	retractall(unit_open(U,_)),
	retractall(unit_import(U,_)),
	retractall(type_decl(U,_,_)),
	retractall(type_gen(_,_,U)),
	retractall(pred_decl(U,_,_)),
	retractall(pred_gen(_,U)),
	retractall(decl_comment(U,_,_)).

pred(clear_declarations).
%!	clear_declarations
%	clears the declaration db
clear_declarations :-
	retractall(unit_clause(_,_,_,_)),
	retractall(unit_dcg(_,_,_)),
	retractall(unit_export(_,_)),
	retractall(unit_open(_,_)),
	retractall(unit_import(_,_)),
	retractall(type_decl(_,_,_)),
	retractall(type_gen(_,_,_)),
	retractall(pred_decl(_,_,_)),
	retractall(pred_gen(_,_)),
	retractall(decl_comment(_,_,_)).

local_pred(xx).
xx :-
     assert_unit_error(bad_decl, decl(pred(xx), uu)).

local_pred(save_pred_gen(atom, prototype)).
save_pred_gen(U, Pred) :-
	catch((

	    is_term(Pred,P,Arity),
	    length(Arity,N),
	    check_dupl(pred(P/N), U),
	    length(Args,N),
	    is_term(Pattern, P, Args),
	    asserta(pred_gen(pred(Pattern, Args, Arity), U))

	),_, assert_unit_error(bad_decl, decl(pred(Pred), U))) ->
	true
	;
	assert_unit_error(bad_decl, decl(pred(Pred), U)).

local_pred(save_type_gen(atom, atom, pattern, generator)).
save_type_gen(U, U1, Type, call) :-!,
	asserta(type_gen(call(U1, Type), Type, U)).
save_type_gen(U, U1, Type, call(P)) :-!,
	asserta(type_gen(call(U1, P), Type, U)).
save_type_gen(U, U1, Type, {T}) :-!,
	assertz(type_gen(gen(X, [X], [T]), Type, U)),
       ( U==U1, !
	; functor(T,ST,M),
	  save_import_type(U, U1, ST/M)).
save_type_gen(U,U1, Type, G) :-
	%G \= {_},
	make_gen(G, GenPattern, Args, Arity),
	asserta(type_gen(gen(GenPattern, Args, Arity), Type, U)),
	maplist(close_type(U,U1), Arity).

close_type(U,U1,T) :-
	U1:import_type(U2, L),
	member(T/N,L),
	not(type_gen(_,T,U)), !,
	save_import_type(U,U2,T/N).
close_type(_U,_U1,_T).


make_gen(G, GenPattern, Args, Types) :-
	is_term(G,F,Arity),
	(   Arity = [X|_],
	    typed_arg(X) ->
	    maplist(split, Arity, Args, Types),
	    is_term(GenPattern,F,Args)
	;   same_length(Args,Arity),
	    Types = Arity,
	    is_term(GenPattern,F,Args)
	).
split(X:T, X, T).
typed_arg(X) :-
	not(var(X)),
	X = _:_.

local_pred(save_type_decl(atom, atom, input_symbol, symbol, decl_attribute)).
save_type_decl(U, _U1, generic:Type, T/N, generic) :-!,
	check_dupl(type(T/N), U),
	check_pattern(type(generic:Type), U),
	asserta(type_gen(generic, Type, U)).
save_type_decl(U, _U1, open:Type, T/N, open) :-!,
	check_dupl(type(T/N), U),
	check_pattern(type(open:Type), U),
	asserta(type_gen(open, Type, U)).
save_type_decl(U, _U1, imported(U2):Type, T/N, open) :-!,
	check_dupl(type(T/N), U),
	%check_pattern(type(imported(U2):Type), U),
	asserta(type_gen(imported(U2), Type, U)).
save_type_decl(U, U1, Gen:Type, T/N, defined) :-
	check_dupl(type(T/N), U),
	check_pattern(type(Gen:Type), U),
	check_type_vars(U, type(Gen:Type), Gen, Type),
	maplist(save_type_gen(U, U1, Type), Gen).


local_pred(save_import_type(atom, atom, symbol)).
save_import_type(_U, prolog_types, _) :- !.
save_import_type(U, _U1, T/N) :-
	functor(Type, T, N),
	type_decl(U, type(_,Type), _),!.
save_import_type(U, U1, T/N) :-
	functor(Type, T, N),

	(
	%catch(U1:type(Gen:Type),_,fail),
	declared_type(U1, Type, Gen),
	save_type_decl(U, U1, Gen:Type, T/N,_) ->
	assertz(type_decl(U, type(Gen, Type), [imported(U, U1)]))
	;
	assert_unit_error(not_defined_import(type(T/N),U1), unit(U))
	).

declared_type(U,T, G) :-
	catch(U:type(Gen:T),_,fail),
	G = Gen
	;
	catch((U:import_type(U1,L), functor(T,K,N), member(K/N,L)), _, fail),
	G=imported(U1).

close_import_list( _U1, [], CL, CL).
close_import_list(U1, [T/N|L], CL1, CL2) :-
	dependency_set(U1, T/N, D),
	union(CL1,D,CL),!,
	close_import_list(U1, L, CL, CL2).

local_pred(save_import_pred(atom, atom, symbol)).
save_import_pred(U, U1, P/N) :-
	functor(Pred, P, N),
	(
	catch(U1:pred(Pred),_,fail),
	save_pred_gen(U, Pred) ->
	assertz(pred_decl(U, Pred, [imported(U,U1)]))
	;
	assert_unit_error(not_defined_import(pred(P/N),U1), unit(U))
	).

save_unit_import(Unit, M, PL) :-
       catch(M:unit(_,_),_,fail) ->
	  (   maplist(save_import_pred(Unit,M), PL) ->
	      assertz(unit_import(Unit, module(M,PL)))
	  ;   assert_unit_error(import_error(use_module(M, PL)), unit(Unit))
	  )
       ;
	  assert_unit_warning(non_unit_import(M),unit(Unit)).

/*importing_module((:- use_module(M, PL)), M, PL).
importing_module((:- reexport(M, PL)), M, PL).
importing_module((:- use_module(M)), M).
importing_module((:- reexport(M)), M).*/

local_pred(save_decl(atom, input_decl, list(comment_line))).


save_decl(U, (:- use_module(PathM, PL)), _Comment) :-!,
	file_base_name(PathM, M),
	save_unit_import(U, M, PL).
save_decl(U, (:- use_module(PathM)), _Comment) :- !,
	file_base_name(PathM, M),
	module_property(M, exports(PL)),
	save_unit_import(U, M, PL).
save_decl(U, (:- reexport(PathM, PL)), _Comment) :- !,
	file_base_name(PathM, M),
	save_unit_import(U, M, PL).
save_decl(U, (:- reexport(PathM)), _Comment) :-
	file_base_name(PathM, M),
	module_property(M, exports(PL)),
	save_unit_import(U, M, PL).
save_decl(U, pred(Pred), Comment) :-
        save_pred_gen(U, Pred), !,
	assertz(pred_decl(U, Pred, [defined])),
	assertz(decl_comment(U, pred(Pred), [defined|Comment])).
save_decl(U, pred(Pred), _Comment) :- !,
	assert_unit_error(bad_decl, decl(pred(Pred), U)).
save_decl(U, open_pred(Pred), Comment) :-
	save_pred_gen(U, Pred),!,
	assertz(pred_decl(U, Pred,[open])),
	assertz(decl_comment(U, pred(Pred), [open|Comment])).
save_decl(U, open_pred(Pred), _Comment) :- !,
	assert_unit_error(bad_decl, decl(open_pred(Pred), U)).
save_decl(U, local_pred(Pred), Comment) :-
	save_pred_gen(U, Pred), !,
	assertz(pred_decl(U, Pred, [local])),
	assertz(decl_comment(U, pred(Pred), [local|Comment])).
save_decl(U, local_pred(Pred), _Comment) :- !,
	assert_unit_error(bad_decl, decl(local_pred(Pred), U)).
save_decl(U, type(generic:Type), Comment) :-
	check_pattern(type(generic:Type), U),
	functor(Type, T, N),
	check_dupl(type(T/N), U),!,
	assertz(type_decl(U, type([], Type), [generic])),
	assertz(decl_comment(U, type([],Type), [generic|Comment])).
save_decl(U, type(generic:Type), _Comment) :- !,
	assert_unit_error(bad_decl, decl(type(generic:Type), U)).
save_decl(U, type(Gen:Type), Comment) :-
	functor(Type, T, N),
	save_type_decl(U, U, Gen:Type, T/N, Attribute),!,
	assertz(type_decl(U, type(Gen, Type), [Attribute])),
	assertz(decl_comment(U, type(Gen,Type), [Attribute|Comment])).
save_decl(U, type(Type), _Comment) :- !,
	assert_unit_error(bad_decl, decl(type(Type), U)).
save_decl(U, import_type(U1, TL), Comment) :-
	check_symbol_list(import_type(U1,TL), U, TL),
	close_import_list(U1, TL, TL, CTL),
	maplist(save_import_type(U, U1), CTL),!,
	assert(decl_comment(U, import_type(U1, CTL), Comment)).
save_decl(U, import_type(U1, TL), _Comment) :- !,
	assert_unit_error(bad_decl, decl(import_type(U1, TL), U)).
save_decl(U, import_pred(U1, PL), Comment) :-
	check_symbol_list(import_pred(U1, PL), U, PL),
	(   U1 = user -> maplist(save_import_pred(U, user), PL)
	;   check_import_preds(U, U1, PL)),!,
	assertz(decl_comment(U, import_pred(U1, PL), Comment)).
save_decl(U, import_pred(U1, PL), _Comment) :- !,
	assert_unit_error(bad_decl, decl(import_pred(U1, PL), U)).
save_decl(U, Clause, _Comment) :-
	is_clause(Clause, Head, Body), !,
	nb_getval(clause_counter, K),
	assertz(unit_clause(K, U, Head, Body)),
	K1 is K+1,
	nb_setval(clause_counter, K1).
save_decl(U, Rule, _Comment) :-
	is_dcg(Rule, Head, Body), !,
	nb_getval(clause_counter, K),
	assertz(unit_clause(K, U, Head, Body)),
	assertz(unit_dcg(K, U, Rule)),
	K1 is K+1,
	nb_setval(clause_counter, K1).
save_decl(_,_,_).




pred(load_declarations(atom)).

load_declarations(Unit) :-
	is_unit(Unit, Def, Open, Path),!,
	clear_unit_errors,
	check_symbol_list(unit(Unit, Open), Unit, Open),
	clear_declarations(Unit),
	assertz(unit_export(Unit, Def)),
	assertz(unit_open(Unit,Open)),
	load_prolog(Unit),
	nb_setval(clause_counter, 0),
	open(Path,read, Stream),
	read_declarations(Unit, Stream),
	close(Stream),
	close_generators(Unit),
	forall(load_error(Err, W, Unit),
	       assert_unit_error(Err,W)).
load_declarations(Unit) :-
	clear_unit_errors,
	assert_unit_error(unit_declaration_error(Unit),project).

pred(load_user_module_declarations(atom)).

load_user_module_declarations(Path) :-
	clear_declarations(user),
	clear_unit_errors,
	load_prolog(user),
	nb_setval(clause_counter, 0),
	open(Path,read, Stream),
	read_declarations(user, Stream),
	close(Stream),
	close_generators(user),
	forall(load_error(Err, W, user),
	       assert_unit_error(Err,W)).

read_declarations(_,Stream) :-
	at_end_of_stream(Stream), !.
read_declarations(Unit, Stream) :-
	read_term(Stream, Prec, [comments(UnitComments)]),
	save_decl(Unit, unit(Unit), UnitComments),!,
	read_declarations(Unit, Prec, Stream).

read_declarations(_Unit, _Prec, Stream) :-
	at_end_of_stream(Stream), !.
read_declarations(Unit, Prec, Stream) :-
	read_term(Stream, Next, [comments(Comment)]),
	(   catch(save_decl(Unit, Prec, Comment),_,fail) -> true
	;   assert_unit_warning(unclear_decl, Unit:Prec)),!,
	read_declarations(Unit,Next,Stream).

local_pred(is_unit(atom, list(symbol), list(symbol), atom)).

is_unit(Unit, Def, Open, Path) :-
	catch(( module_property(Unit, file(Path)),
	        Unit:unit(Unit, Open),
		module_property(Unit, exports(Def) )), _, fail).


%===============================================
%     QUERY

pred(declared(atom, decl_symbol)).
%  declared(?Unit, +Symbol):
%  Symbol is a declared symbol of Unit
%  (i.e., in the scope of Unit)
declared(Unit, pred(P/N)) :- !,
	functor(Pred, P, N),
	pred_decl(Unit, Pred,_).
declared(Unit, type(P/N)) :- !,
	functor(Type, P, N),
	type_decl(Unit, type(_,Type),_).

pred(declared_by(atom, decl_symbol)).
%  declared(?Unit, +Symbol):
%  Symbol is a declared symbol of Unit
%  (i.e., in the scope of Unit)
declared_by(Unit, pred(P/N)) :- !,
	functor(Pred, P, N),
	pred_decl(Unit, Pred,Attr),
	not(member(imported(Unit,_), Attr)).
declared_by(Unit, type(P/N)) :- !,
	P \=any,
	functor(Type, P, N),
	type_decl(Unit, type(_,Type),Attr),
	not(member(imported(Unit,_), Attr)).

pred(declared_open_pred(atom, symbol)).
%  declared_open_pred(?Unit, ?U1:P/N):
%  U1:P/N belongs to the open preds declared by Unit
declared_open_pred(U, U1:P/N) :-
	catch(U:unit(U, OL),_,fail),
	member(U1:P/N, OL).

pred(generates(atom, is_type, is_type)).
%!	 generates(+U, ?T1, ?T2) :
%	 {T1} is a generator type of T2  declared in U
generates(U, T1, T2) :-
	type_decl(U, type(Gen, T2), _),
	member({T1}, Gen).

pred(sub_type_generator(is_type, is_type, atom)).
%  the reflexive and transitive closure of generates
sub_type_generator(T1, T2, U) :-
	var(T2), !,
	forward_sub_type_gen(T1,T2,U).
sub_type_generator(T1, T2, U) :-
	backward_sub_type_gen(T1,T2,U).

forward_sub_type_gen(T1, T2, U) :-
	T1=T2
	;
	generates(U, T1, T),
	sub_type_generator(T, T2, U).

backward_sub_type_gen(T1, T2, U) :-
	T1=T2
	;
	generates(U, T, T2),
	sub_type_generator(T1, T, U).


%======================================  CHECKS


is_clause((Head :- Body), Head, Body) :- !.
is_clause( Head, Head, true) :-
	Head \= (_ --> _),
	Head \= (:- _).

is_dcg((HR --> BR), Head, Body) :- !,
	dcg_translate_rule((HR --> BR), (Head :- Body)).



check_dupl(Decl, U) :-
	declared(U, Decl) ->
	assert_unit_warning(duplicated(Decl), decl(Decl, U))
	;
	true.

check_pattern(type(G:Pattern), U) :-
	catch(Pattern =..[_T|Param], _, fail),!,
	distinct_vars(Param)
	;
	assert_unit_error(bad_pattern(Pattern), decl(type(G:Pattern), U)).

local_pred(check_symbol_list(any, atom, any)).
check_symbol_list(Decl, U, TL) :-
	is_list(TL),
	forall(member(S,TL), is_symbol(S))
	;
	assert_unit_error(bad_symbols, decl(Decl, U)).
is_symbol(S/N) :-
	atom(S),
	integer(N),
	N >= 0.
is_symbol(U:S/N) :-
	atom(U),
	atom(S),
	integer(N),
	N >= 0.

local_pred(check_type_vars(atom, any, type_def, pattern)).
check_type_vars(U, TD, Gen, T) :-
	term_variables(Gen, Vars1),
	term_variables(T, Vars2),
	not((mem_var(X,Vars1), not(mem_var(X,Vars2)))), !
	;
	assert_unit_error(non_transparent, decl(TD, U)).


local_pred(check_import_preds(atom, atom, list(symbol))).
check_import_preds(U, U1, PL) :-
	unit_import(U, module(U1, PL1)) ->
	forall(member(P/N, PL),
	      (  member(P/N, PL1) -> true
	         ;
	         assert_unit_error(not_imported(P/N), decl(import_pred(U1, PL), U))
	      ))
	;
	assert_unit_error(required_use_module(U1, PL), decl(import_pred(U1, PL), U)).



%=====================================  ERRORS

a_declared_type_chk(_U, Type, L,L) :-
       var(Type), !.
a_declared_type_chk(_U, T,L,L) :-
	T==any, !.
a_declared_type_chk(U,  Type, L1, L2) :-
       (   type_decl(U, type(_, Type), _) ->
           L=L1
       ;   L=[Type|L1]),
       Type =.. [_T|Types],
       foldl(a_declared_type_chk(U), Types,L,L2).

load_error_(not_declared_types([T|TT]), decl(type(TN/N), U)) :-
	type_gen(gen(_GenPattern, _Args, Arity), Type, U),
	functor(Type, TN, N),
	foldl(a_declared_type_chk(U), Arity, [], [T|TT]).
load_error_(not_declared_types([T|TT]), decl(pred(PN/N), Unit)) :-
	pred_gen(pred(Pred, _Args, Arity),Unit),
	functor(Pred, PN, N),
	foldl(a_declared_type_chk(Unit), Arity, [], [T|TT]).
declaration(type(T/N), U) :-
	type_decl(U,type(_,Type),Attr),
	Type \= any,
	not(member(imported(U,_), Attr)),
	functor(Type, T,N).
declaration(pred(P/N), U) :-
	pred_decl(U,Pred,Attr),
	Pred \= (_ = _),
	not(member(imported(U,_), Attr)),
	functor(Pred, P,N).


local_pred(load_error(unit_error, error_context, atom)).
% load_error(-Error, -Context)  nondet:
%     Error is an error with context Context, where:
% -  Error = import_type_missing(M), Context=unit(U)
%    Unit U uses module M but the mandatory import_type declaration
%    is missing
% -  not_declared_types(TT),  Context a type or pred K
%    The type or pred generators of K contain not declared types TT
% -  not_declared_export(pred(P/N)), Context unit(U)
%    U exports P/N but P/N is not declared
% -  not_declared_open(pred(U1:P/N)), Context unit(U)
%    U1:P/N declared in a unit U1 used by U is not declared
%    as open by U and is not closed by U
% -  (defined_but_open(pred(U1:P/N)), context unit(Unit)
%    a predicate U1:P/N declared as open by U is also
%    closed or defined by U
%  - multiply_defined_open_pred(M1:P/N, M2:P/N), context unit(U)
%    an open pred P/N is "imported" from two different units
%
load_error(import_type_missing(M), unit(U), U) :-
	unit_import(U, module(M,_)),
	catch(M:unit(M,_), _, fail),
	not(catch(U:import_type(M,_),_,fail)).

load_error(not_declared_types([T|TT]), decl(Decl, U), U) :-
	declaration(Decl,U),
        load_error_(not_declared_types([T|TT]), decl(Decl, U)).

load_error(not_declared_export(pred(P/N)), unit(Unit), Unit) :-
	exported_pred(Unit, P/N),
	not(declared(Unit, pred(P/N))).

load_error(not_declared_open(pred(U1:P/N)), unit(Unit), Unit) :-
	open_pred_of(Unit,U1:P/N),
	U1 \= Unit,
	not(closes(Unit, U1:P/N)),
	not(declared_open_pred(Unit, U1:P/N)).

load_error(defined_but_open(pred(U1:P/N)), unit(Unit), Unit) :-
	declared_open_pred(Unit,U1:P/N),
	(   closes(Unit, U1:P/N)
	;   is_defined_pred(Unit, P/N,_)).
load_error(multiply_defined_open_pred(M1:P/N, M2:P/N), unit(U), U) :-
	unit_export(U,_),
	open_pred_of(U, M1:P/N),
	open_pred_of(U, M2:P/N),
	M1 \= M2.

imported_pred(Unit, P/N, IU) :-
	unit_import(Unit, module(IU,Preds)),
	member(P/N, Preds).
exported_pred(Unit, P/N) :-
	unit_export(Unit,Preds),
	member(P/N, Preds).

is_defined_pred(Unit, P/N, DU) :-
	imported_pred(Unit, P/N, DU)
	;
	functor(Pred, P, N),
	clause(Unit:Pred, _),
	DU=Unit.

%======================================  AUX


closes(U, U1:P/N) :-
	unit_clause(_,U,U1:Pred,_),
	functor(Pred, P, N).

open_pred_of(U,U1:P/N) :-
	catch(U:unit(U,OP),_,fail),
	member(U:P/N, OP),
	U1=U
	;
	catch(U:import_type(M, _),_,fail),
	catch(M:unit(M,OPM),_,fail),
	member(U1:P/N, OPM).

open_pred_of(U, Path, U1:P/N) :-
	open_pred_of(U, [], Path, U1:P/N).

open_pred_of(U, Path1, Path2, U1:P/N) :-
	catch(U:unit(U,OP),_,fail),
	member(U:P/N, OP),
	Path2 = Path1,
	U1=U
	;
	catch(U:import_type(M, _),_,fail),
	not(member(M, Path1)),
	open_pred_of(M, [U|Path1], Path2, U1:P/N).

load_prolog(Unit) :-
	forall(prolog_type(Gen:Type),
	       ( functor(Type, T, N),
	         save_type_decl(Unit, prolog_types, Gen:Type, T/N,_),!,
	         assertz(type_decl(Unit, type(Gen, Type), [imported(Unit,prolog)]))
	       )),
	forall(prolog_pred(Pred),
	       (   save_pred_gen(Unit, Pred),
		   assertz(pred_decl(Unit, Pred, [imported(Unit,prolog)]))
	       )).

close_generators(Unit) :-
	setof(Type, top_type(Unit, Type), TopTypes),!,
	maplist(make_any_generator(Unit), TopTypes)
	;
	true.

top_type(Unit, Type) :-
	type_decl(Unit, type(_, Type),_),
		     Type \= any,
		     not(generates(Type,_,Unit)).

make_any_generator(Unit, Type) :-
	assertz(type_decl(Unit, type([{Type}], any), [any_generator(Type)])),
	assertz(type_gen(gen(X, [X], [Type]), any, Unit)).


called_in(Var, _) :-
	var(Var),!,
	fail.
called_in(_M:Pred, P/N) :- !,
	called_in(Pred, P/N).
called_in(maplist(Pred,_L1), P/N) :-!,
	catch(functor(Pred, P, M),_,fail),
        N is M+1.
called_in(setof(_,Body,_), P/N) :-!,
	called_in(Body,P/N).
called_in(catch(B1,_,B2), P/N) :-!,
	called_in(B1, P/N)
	;
	called_in(B2, P/N).
called_in(Body, P/N) :-
	Body =.. [Op|BL],
        body_op(Op),!,
	called_in_list(BL, P/N).
called_in(Pred, P/N) :-
	catch(functor(Pred, P, N),_,fail), !.


called_in_list([H|T], P/N) :-
	called_in(H, P/N)
	;
	called_in_list(T, P/N).

body_op(Op) :-
	member(Op,[
		   ',',
		   ';',
		   '->',
		   '*->',
		   forall,
		   foreach,
		   not,
		   '+/'
	       ]).


depends1(U1, T1/N, T2/M) :-
	functor(Type1, T1, N),
	catch(U1:type(Gen:Type1),_,fail),
	member(G, Gen),
	catch(G =.. [_|ArgTypes],_,fail),
	contains_type(Type2, ArgTypes),
	declared_type(U1, Type2,_),
	%catch(U1:type(_:Type2),_,fail),
	functor(Type2, T2, M).

contains_type(Type, ArgTypes) :-
	member(T, ArgTypes),
	nonvar(T),
	functor(T, K, N),
	(   functor(Type, K, N)
	;   T=..[K|TA],
	    contains_type(Type, TA)).

depends(U, T1, T2) :-
	path(U, T1, T2, [], _).

path(_U, T/N, T/N, Path, Path).
path(U, T1/N,T2/M,Path1, Path2) :-
	setof(X, depends1(U,T1/N,X), Dep),
	member(TT/NN, Dep),
	not(member(TT/NN, Path1)),
	path(U, TT/NN, T2/M, [TT/NN|Path1], Path2).

dependency_set(U, T, L) :-
	setof(T1, depends(U,T,T1), L), !
	;
	L=[].





