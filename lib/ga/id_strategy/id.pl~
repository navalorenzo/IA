
% Unità che chiude i predicati di frontiera di ga
% e i predicati di gestione degli incrementi nel caso di path_search
% per implementare iterative deepening
% NB:  USA DEPTH FIRST !!


:- path_search:set_strategy(depth_first).

pred(increment(number)).
:- dynamic(increment/1).
% increment(Incr): si ha iterative deepening e Incr è l'incremento per
% iterazione corrente; prima della iterazione successiva Incr è
% azzerato, per prepararlo al calcolo dell'incremento successivo
% Modo	(-) det

ga:id_increment(BS, NewBS) :-
	retract(increment(K)),
	K > 0,
	increment_bounds(BS,K,NewBS),
	assert(increment(0)).

increment_bounds(id_limit(M,_Min,Max,UP), K, id_limit(M, Min1,Max1,UP)) :-
	Max1 is Max+K,
	Max1 < UP,!,
	Min1 is Max+1.

path_search:start_id :-
       retractall(increment(_)),
       assert(increment(0)).

path_search:update_increment(Incr1) :-
	retract(increment(Incr)),
	next_incr(Incr,Incr1,NextIncr),
	assert(increment(NextIncr)).

next_incr(0,X,Z) :- !,
	% incremento inziale X
	Z is X.
next_incr(X,Y,Z) :- Z is min(X,Y).
	% minimo fra incremento prec. e nuovo



