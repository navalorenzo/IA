:- module(geometria, [rosa/3,
		     step/3,
		     length_step/2,
		     distance/4,
		     left_dir/2,
		     direzione/3,
		     right_dir/2]).

:- style_check(-discontiguous).
:- use_module(library(draw_world)).
import_type(draw_world, [point/0]).
%  la geometria usa gli stessi punti della grafica
%  rappresentati da point(X,Y)

type([nord,ovest,sud,est]:punto_cardinale).
type([punto_cardinale:punto_cardinale]:direzione).
type([euclid, manhattan, diagonal]:distance).

pred(rosa(direzione, integer, integer)).
%  rosa(Dir, DX, DY) :	1 passo in direzione Dir
%  porta da point(X,Y) a point(X+DX, Y+DY)
%  MODO  (?,?,?)  nondet
%
pred(step(direzione, point,  point)).
%  step(Dir, P1, P2) :  un passo in direzione Dir porta da
%  P1 a P2
%  MODO  (+,+,-) det
%
pred(length_step(direzione, float)).
%  length_step(Dir, L) :  un passo in direzione Dir è
%  lungo L
%  MODO  (+,-) det
%
pred(left_dir(direzione, direzione)).
%  left_dir(Dir1, Dir2) :  Dir2 è raggiungibile da Dir1
%  in senso antiorario
%  MODO  (+,-)  nondet:	genera le direzioni in senso antiorario
%  di un semiquadrante per volta, periodicamente:
%  Ad es.  ?- left_dir(nord:est, Dir).  genera nell'ordine:
%  Dir = nord:est ;
%  Dir = nord:nord ;
%  Dir = nord:ovest ;
%  Dir = ovest:ovest ;
%  Dir = sud:ovest
%  ecc., ciclicamente
pred(rigth_dir(direzione, direzione)).
%  come left_dir ma in senso orario
%
pred(distance(distance, point, point, number)).
%  distance(Dist, P1, P2, D) :
%  D è la distanza fra P1 e P2,  con distanza Dist
%  che può essere euclid, manhattan o diagonal
%  MODO (++,++,++,--) det


rosa(nord:nord, -1,0).
rosa(nord:ovest, -1,-1).
rosa(nord:est, -1,1).
rosa(est:est, 0, 1).
rosa(sud:sud, 1, 0).
rosa(sud:est, 1, 1).
rosa(sud:ovest, 1, -1).
rosa(ovest:ovest, 0,-1).

direzione(point(GX, GY), point(X,Y), Dir) :-
	DX is sign(GX-X),
	DY is sign(GY-Y),
	rosa(Dir, DX, DY).


step(Dir, point(Row,Col),point(Row1, Col1)) :-
	rosa(Dir, DX,DY),
	Row1 is Row+DX,
	Col1 is Col+DY.

length_step(Dir:Dir, 1) :-
	rosa(Dir:Dir,_,_).
length_step(D1:D2, 1.42) :-
        rosa(D1:D2,_,_),
	D1 \= D2.


left_dir(Dir1, Dir2) :-
	Dir1 = Dir2
	;
	l_dir(Dir1,Dir),
	left_dir(Dir, Dir2).

right_dir(Dir1, Dir2) :-
	Dir1 = Dir2
	;
	l_dir(Dir,Dir1),
	right_dir(Dir, Dir2).

l_dir(nord:est, nord:nord).
l_dir(nord:nord, nord:ovest).
l_dir(nord:ovest, ovest:ovest).

l_dir(ovest:ovest, sud:ovest).

l_dir(sud:ovest, sud:sud).
l_dir(sud:sud, sud:est).
l_dir(sud:est, est:est).

l_dir(est:est,nord:est).



distance(manhattan, point(X1,Y1), point(X2,Y2), D) :-
	D is abs(X1-X2) + abs(Y1-Y2).

distance(euclid, point(X1,Y1), point(X2,Y2), D) :-
	D is sqrt(abs(X1-X2)^2 + abs(Y1-Y2)^2).

distance(diagonal, point(X1,Y1), point(X2,Y2), D) :-
	DX is abs(X1-X2),
	DY is abs(Y1-Y2),
	D is min(DX,DY) * 1.42 + abs(DX-DY).









