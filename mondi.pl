:- module(mondi, [gen_world/3,
		  load_world/1,
		  existing_world/1,
		  add_to_repository/1,
		  create_repository/1,
		  world/3,
		  content/3,
		  navigable/2,
		  draw_content/3,
		  draw_loaded_world/2,
		  draw_loaded_world/3,
		  mondi_help/0
		 ]).

:- style_check(-discontiguous).

:- maplist(write, [
'\n***********************************************************',
'\n   MONDI, generazione/archiviazione mondi - mondi_help',
'\n***********************************************************']).

:- use_module(library(draw_world)).
import_type(draw_world, [point/0, filling/0, fig/0]).
% si usano i punti e le figure della grafica

%========================================================
%   Rappresentazione di mondi quadrettati, in cui i punti
%   sono quadretti che sono vuoti o contengono ostacoli
%   o altri tipi di terreno/oggetti
%   Un mondo di nome W è rappresentato dai predicati
%   a)  world(W, NCol, NRow)  che ne indica le dimensioni
%   b)  content(W, P, C)  che indica il contenuto C di un
%   punto (quadretto) P
%=========================================================

mondi_help :- maplist(write, [
'GESTIONE MONDI, lista comandi:',
'\ngen_world(+W, +NCol, +NRow) :  generazione random di un mondo di nome W',
'\nexisting_world(W) : elenca i mondi "esistenti"',
'\nload_world(W) : carica un mondo "esistente" W',
'\ncreate_repository(+List) :  crea repository.pl e vi archivia la lista di mondi List',
'\n     errore se il repository gia'' esiste; attualmente esiste gia'', cancellare',
'\n     il file repository.pl prima di generarne uno nuovo',
'\nadd_to_repository(+List) :  aggiunge a repository.pl i mondi in List',
'\n     errore se il repository non esiste gia''',
'\nnavigable(+W,+P) :  P e'' un punto navigabile del mondo W',
'\ndraw_loaded_world(+W,+Size) : disegna il mondo W on quadrettatura di dimensione Size',
'\ndraw_content(W, P, Cnt, Size): disegna il contenuto Cnt nel punto P del mondo W',
'\n	   con quadrettatura Size'
		      ]).


:- use_module(geometria,[]).
import_type(geometria, [point/0]).
%  si usa il tipo point della geometria

type([ostacolo, agente, goal, waypoint]:predefined_content).
%  contenuti predefiniti di un punto (quadretto)
%  ostacolo    un ostacolo insormontabile
%  agente      l'agente
%  goal        la posizione obiettivo
%  waypoint    punti navigazione
type(open:user_content).
%  contenti utente
type([{predefined_conteny},{user_content}]:content).
%   Contenuto di un punto

pred(world(any, integer, integer)).
%  world(W, NCol, NRow) :
%  W è un mondo caricato in memoria dinamica con NCol colonne
%  (larghezza) e NRow righe (altezza)
%  MODO  (?,?,?) nondet
%  NOTA: non si fissa un tipo per i nomi dei mondi, si usa
%  il tipo any (qualsiasi termine ground prolog)
:- dynamic(world/3).

pred(content(any, point, content)).
%  content(W, P, C) :  il punto P di W contiene C
%  MODO (?,?,?) nondet
:-  dynamic(content/3).

pred(navigable(any,point)).
%  navigable(W, P) :  P punto in cui l'agente può passare
%  MODO  (+,+) semidet
%
open_pred(navigable_content(content)).
%   navigable_content(C) :  C navigabile
%

%=====================================================
%  Generazione random di mondi di ostacoli
%=====================================================

pred(gen_world(any, integer, integer)).
%  gen_world(Name, NCol, NRow) : genera in modo randon un mondo di
%  NCol colonne e NRow righe popolato da ostacoli
%  MODO  (+,+,+) det


%=====================================================
%  Caricamento di mondi rappresentati da mappe di caratteri
%  Vedi sotto due esempi di mappe
%=====================================================

pred(load_world(any)).
%  load_world(Name) :  carica il mondo Name definito da una mappa
%  di caratteri  (vedi più avanti) o memorizzato nel "repository"
%  Modo  (+) semidet  (fallisce se non c'è una mappa con quel nome)
%
pred(existing_world(any)).
%  existing_world(Name) :  Name è definito da una mappa
%  di caratteri  o memorizzato nel "repository"
%  MODO  (?) nondet


%=====================================================
% Salvataggio di mondi su file (repository)
% Un file usa r_world e r_content statici
%  =====================================================

pred(create_repository(list(atom))).
%   create_repository(WList) :
%   salva in 'repository.pl' i fatti relativi ai mondi in WList
%   ERRORE se 'repository.pl' gia' esiste
%
pred(add_to_repository(list(atom))).
%   add_to_repository(WList) :
%   aggiunge a 'repository.pl' i fatti relativi ai mondi in WList
%   ERRORE se 'repository.pl' non esiste

%=====================================================
%  Grafica
%=========================================================
pred(draw_loaded_world(any,number)).
%  draw_loaded_world(W,Size) : disegna il mondo W on quadrettatura di
%  dimensione Size
%  MODO (++,++) semidet
%  Errore se W non è stato caricato
%
pred(draw_content(any, point, content, number)).
% draw_content(W, P, Cnt, Size):  disegna il contenuto
% Cnt nel punto P del mondo W con quadrettatura Size
% MODO  (++,++,++,++) semidet

%=================  IMPLEMENTAZIONE

point_of(W, point(X,Y)) :-
	world(W, Ncol, Nrow),
	between(1,Nrow,X),
	between(1,Ncol,Y).

navigable(W, P) :-
	point_of(W, P),
	(
	not(content(W, P, _)), !
	;
	content(W, P, C),
	(   member(C,[goal, waypoint])
	;   navigable_content(C) )
	).

navigable_content(_) :- fail.
% di default solo goal e waypoint sono navigabili;
% per aggiungere altri contenuti navigabili aggiungere
% mondi:navigable_content(...).


gen_world(Name, Width, Heigth) :-
	retractall(content(Name,_,_)),
	retractall(world(Name,_,_)),
	forall(between(1,Width,Col),
	       forall(between(1,Heigth,Row),
		      (	  (Col,Row) \= (1,1),
			  random(10) > 5 ->
		          assert(content(Name, point(Row,Col), ostacolo))
		      ;	  true))),
	assert(world(Name, Width, Heigth)).

load_world(W) :-
	not(char_world(W, _)),!,
	consult(repository),
	r_world(W, DX, DY),
	retractall(world(W,_,_)),
	retractall(content(W,_,_)),
	assert(world(W,DX,DY)),
	forall(r_content(W,P,C),
	       assert(content(W,P,C))).
load_world(Name) :-
	char_world(Name, [R|Rows]),
	atom_length(R, Width),
	length([R|Rows],Heigth),
	retractall(content(Name,_,_)),
	retractall(world(Name,_,_)),
	load_rows(Name, 1, [R|Rows]),!,
	assert(world(Name, Width, Heigth)).
load_rows(K, RN, [R|Rows]) :-
	atom_chars(R, LR),
	load_row(K, RN, 1, LR),
	RN1 is RN+1,
	load_rows(K,RN1, Rows).
load_rows(_,_,[]).

load_row(K, RN, CN, [Ch|Chars]) :-
	(   represents(K,Ch, Cnt) ->
	    assert(content(K, point(RN,CN), Cnt))
	;   true),
	CN1 is CN+1,
	load_row(K, RN, CN1, Chars).
load_row(_K, _RN, _CN, []).

existing_world(W) :-
	not(char_world(W, _)),!,
	consult(repository),
	r_world(W, _,_)
	;
	char_world(W,_).

create_repository(LW) :-
	not(exists_file('repository.pl')), !,
	open('repository.pl', write, Stream),
	write(':- style_check(-discontiguous).\n'),
	forall(member(W,LW),
	       forall(fact(W,F),
		      maplist(write(Stream), [F, '.\n'])
		      )),
	close(Stream).
create_repository(_LW) :-
	writeln('repository.pl gia'' creato'),
	fail.

add_to_repository(LW) :-
	exists_file('repository.pl'),
	open('repository.pl', append, Stream),
	forall(member(W,LW),
	       forall(fact(W,F),
		      maplist(write(Stream), [F, '.\n'])
		      )),
	close(Stream).
add_to_repository(_LW) :-
	writeln('repository.pl non esiste, crealo'),
	fail.

% fact(W, F) : il fatto F è una proprietà del mondo W da memorizzare su
% file; world(W,DX,DY) viene memorizzato come r_world(W,DX,DY)
% content(W,Point,Cnt) viene memorizzato come r_content(W,Point,Cnt)
fact(W, r_world(W,DX,DY)) :-
	world(W,DX,DY).
fact(W, r_content(W, P, K)) :-
	content(W, P, K).

%================================================
%   Alcuni mondi di caratteri
%===============================================

:- multifile(represents/3).
pred(represents(any, char, content)).
%   represents(W, Ch, Cnt) : nel mondo W il carattere Ch rappresenta
%   il contenuto Cnt
%   MODO  (?,?,?) nondet
%
represents(_,'O', ostacolo).
represents(cw(_),'A', agente).
represents(cw(_),'G', goal).

represents(delivery(_), *, waypoint).
represents(delivery(_), K, p(NK)):-
	member(K,['1','2','3','4','5','6','7','8']),
	atom_number(K,NK).
represents(delivery(_), a, c0).
represents(delivery(_), b, c1).

char_world(cw(1),[
'OOOOOOOOOOOOOOOOOOOO',
'O                  O',
'O   OOOOO    OO    O',
'O   OOOOO    OO    O',
'O   OOOOO    OO    O',
'O                  O',
'O     OOOOOOOO	    O',
'O     OOOOOOOO     O',
'O                  O',
'OOOOOOOOOOOOOOOOOOOO']).


char_world(cw(2),[
'OOOOOOOOOOOOOOOOOOOO',
'O                  O',
'O    OOOOOOOOOO    O',
'O    OOOO    OO    O',
'O      OO    OO    O',
'O      OO    OO    O',
'O                  O',
'O     OOOOOOOO	    O',
'O         OOOO     O',
'O                  O',
'OOOOOOOOOOOOOOOOOOOO']).


char_world(delivery(1), [
'OOOOOOOOOOOOOOOOOOOOOOOO',
'O        O        O    O',
'O        O        O    O',
'OOOOO8OOOOOOO6OOOOO    O',
'a*   *    *  *   *5    O',
'O    OOOOO7OOOO   O    O',
'O    O        O   O    O',
'O    O        O   OOOOOO',
'O    O        O   O    O',
'O    OOOOO2OOOO   O    O',
'b*   *	   *  *   *4    O',
'OOOOO1OOOOOOO3OOOOO    O',
'O        O        O    O',
'O        O        O    O',
'OOOOOOOOOOOOOOOOOOOOOOOO']).


char_world(delivery(2), [
'OOOOOOOOOOOOOOOOOOOOOOOO',
'O        O        O    O',
'O        O        O    O',
'OOOOO6OOOOOOO5OOOOO    O',
'O    *	      *    O    O',
'a*  *     *    * *4    O',
'O    OOOOO8OOOO   O    O',
'O    O        O   O    O',
'O    O        O   OOOOOO',
'O    O        O   O    O',
'O    OOOOO7OOOO   O    O',
'b*  *     *    * *3    O',
'O    *	      *    O    O',
'OOOOO1OOOOOOO2OOOOO    O',
'O        O        O    O',
'O        O        O    O',
'OOOOOOOOOOOOOOOOOOOOOOOO']).

char_world(diag, [
'O    ',
' O   ',
'  O  ',
'   O ',
'    O']).



%================================================================
%  PRIMITIVE GRAFICHE PER I MONDI
%  Disegnano il contenuto di un mondo usando il predicato
%  aperto fig_of
%===============================================================

open_pred(fig_of(any,content,fig, number)).
%  fig_of(W, Cnt, Fig, Size) :  nel mondo W il
%  contenuto Cnt è rappresentato da Fig, che viene iscritta in un
%  quadretto di dimensione Size (quella della quadrettatura usata in W)
%  MODO (++,++,--,--,--) semidet. Fallisce se non ci sono figure che
%  rappresentano Cnt
%  APERTO, in parte definito qui per i contenuti qui trattati
%  in parte lasciato all'utente in altri file
:- multifile(fig_of/4).

fig_of(_,ostacolo,box(Size,[col(black)]),Size).
fig_of(_,agente,circ(CircSize,[col(green)]), S) :-
	CircSize is truncate(3*S/4).
fig_of(_,goal,circ(CircSize,[col(yellow)]), S) :-
	CircSize is truncate(3*S/4).
fig_of(_, waypoint, circ(10,[]), _S).

%   le porte e le caselle dei mondi delivery
fig_of(delivery(_),p(K), box(Size,[text(TK), col(yellow)]),Size) :-
	atom_number(TK,K).
fig_of(delivery(_),c0,box(Size,[text(c0), col(yellow)]),Size).
fig_of(delivery(_),c1,box(Size,[text(c1), col(yellow)]),Size).


draw_loaded_world(W,Size) :-
	world(W, Width, Heigth),
	new_picture(W, Width, Heigth, Size),
	forall(between(1,Heigth,Row),
	       ( forall(between(1,Width,Col),draw_content(W, point(Row,Col), Size))
	           )).

draw_content(W, P, Size) :-
	% disegno tutti i contenuti di P
	forall((content(W,P,C), fig_of(W, C,Fig,Size)), draw_fig(W, Fig, P)).

draw_loaded_world(W,Size, DrawFig) :-
	world(W, Width, Heigth),
	new_picture(W, Width, Heigth, Size),
	forall(between(1,Heigth,Row),
	       (   forall(between(1,Width,Col),
		      call(DrawFig, point(Row,Col), Size))
	           )).
