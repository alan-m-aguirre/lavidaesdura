% No cambies esta declaración del módulo tp3, ya que podría traer
% problemas para correr las pruebas del proyecto.
:- module(tp3, [lecturaDensa/1, lectorIntenso/1]).

% ---------------------------------------------------------------------------- %

% --------------------------------
% Predicados a desarrollar
% --------------------------------

/*
Definir un predicado lecturaDensa/1 para saber si un material de lectura es denso:
- Un libro es denso si tiene al menos 200 páginas o si es de editorial Paidos.
- Por otro lado un paper es denso si la diferencia entre la cantidad de hojas y la cantidad de visitas es mayor que 100.
- Por último, una saga es densa si tiene más de cuatro libros.
*/

mayorIgualA(Valor1,Valor2):-
  Valor1 >= Valor2.

lecturaDensa(libro(_, _, CantidadDePaginas)):-
  mayorIgualA(CantidadDePaginas,200).

lecturaDensa(libro(_, paidos, _)).

lecturaDensa(paper(_, CantidadDeHojas, CantidadDeVisitas)):-
  Diferencia is CantidadDeHojas - CantidadDeVisitas,
  mayorIgualA(Diferencia,100).

lecturaDensa(saga(_, CantidadDeLibros)):-
  mayorIgualA(CantidadDeLibros,5).

/*
Usando el predicado lecturaDensa/1 definido para el ejercicio anterior, definir un predicado lectorIntenso/1 para saber si
una persona prefiere la lectura intensa. Esto sucede cuando leyó más de un material de lectura (que podrían ser dos libros
distintos, un paper y un libro, etc) y todo lo que leyó es denso.
*/

lectorVariado(Lector):-
  leyo(Lector, Material1),
  leyo(Lector, Material2),
  Material1 \= Material2.

lectorIntenso(Lector):-
  lectorVariado(Lector),
  forall(leyo(Lector,Material), lecturaDensa(Material)).

% --------------------------------
% Código inicial - NO TOCAR
% --------------------------------

leyo(nico, saga(dune,14)).
leyo(nico, libro(rebelionEnLaGranja,deBolsillo,144)).
leyo(nico, paper("No Silver Bullet: Essence and Accidents of Software Engineering", 230, 100)).
leyo(nico, paper("The relationship between design and verification", 250, 300)).

leyo(daiu, saga(fundacion,7)).
leyo(daiu, libro(elAleph,paidos,146)).

leyo(clara, paper("Evidence for a Distant Giant Planet in the Solar System", 170, 30)).
leyo(clara, paper("No Silver Bullet: Essence and Accidents of Software Engineering", 230, 100)).
leyo(clara, libro(rayuela,alfaguara,600)).
leyo(clara, saga(harryPotter,7)).

leyo(juan, libro(cosmos,planeta,362)).
leyo(juan, saga(elSeniorDeLosAnillos,3)).

leyo(flor, saga(harryPotter,7)).

% --------------------------------
% TESTS - NO TOCAR
% --------------------------------

:- begin_tests(tests_tp3_lecturaDensa).
test('un libro con al menos 200 paginas es denso', nondet):-
	lecturaDensa(libro(rayuela,alfaguara,600)).

test('un libro de editorial paidos es denso', nondet):-
  lecturaDensa(libro(elAleph,paidos,146)).

test('un libro corto de otra editorial no es denso', fail):-
  lecturaDensa(libro(rebelionEnLaGranja,deBolsillo,144)).

test('un paper es denso si la diferencia entre la cantidad de hojas y la cantidad de visitas es mayor que 100', nondet):-
  lecturaDensa(paper("Evidence for a Distant Giant Planet in the Solar System", 170, 30)).

test('un paper no es denso si la diferencia entre cantidad de hojas y visitas no llega a 100', fail):-
  lecturaDensa(paper("The relationship between design and verification", 250, 300)).

test('una saga es densa si tiene más de cuatro libros', nondet):-
  lecturaDensa(saga(dune,14)).

test('una saga corta no es densa', fail):-
  lecturaDensa(saga(elSeniorDeLosAnillos,3)).
:- end_tests(tests_tp3_lecturaDensa).

:- begin_tests(tests_tp3_lectorIntenso).
test('alguien que solo leyo un material de lectura no es lector intenso, incluso si ese material es denso', fail):-
	lectorIntenso(flor).

test('alguien que leyo algun material de lectura que no es denso no es lector intenso', fail):-
	lectorIntenso(nico).

test('alguien que leyo una saga densa y un libro denso es un lector inteso', nondet):-
  lectorIntenso(daiu).

test('alguien que leyo lecturas de todo tipo, y todas son densas, es lector intenso', nondet):-
  lectorIntenso(clara).

test('lectorIntenso/1 es inversible', set(Lector = [daiu, clara])):-
  lectorIntenso(Lector).
:- end_tests(tests_tp3_lectorIntenso).
