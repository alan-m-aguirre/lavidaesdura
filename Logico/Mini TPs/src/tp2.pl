% No cambies esta declaración del módulo tp2, ya que podría traer
% problemas para correr las pruebas del proyecto.
:- module(tp2, [incompatibles/2, ultimaEvolucion/1, predecible/1, parecidas/2]).

% ---------------------------------------------------------------------------- %

% --------------------------------
% Predicados a desarrollar
% --------------------------------

/* Necesitamos saber si dos tipos (ej. fuego, planta, volador...) son incompatibles.
Decimos que dos tipos son incompatibles si no existe ninguna especie que tenga ambos tipos a la vez.*/
incompatibles(Tipo1,Tipo2):-
        tipo(_,Tipo1),
        tipo(_,Tipo2),
        Tipo1 \= Tipo2,
        forall(tipo(Pokemon,Tipo1), not(tipo(Pokemon,Tipo2))).
        
/*Saber si una especie es ultimaEvolucion/1, que se cumple para aquellas especies
que son evolución de alguna otra especie pero no evolucionan en otra.*/
ultimaEvolucion(Pokemon):-
        evolucion(_,Pokemon),
        not(evolucion(Pokemon,_)).

/*Saber si una especie es predecible/1, que se cumple para una especie de un determinado
tipo si todas las especies a las que puede evolucionar (de haber alguna) también tienen ese tipo.*/
predecible(Pokemon):-
        tipo(Pokemon, Tipo),
        forall(puedeEvolucionar(Pokemon, Evolucion), tipo(Evolucion, Tipo)).
      
/*Saber si dos especies son parecidas/2 que se cumple si no pertenecen a una misma línea evolutiva
y además todos los tipos que tiene la primera también los tiene la segunda, y viceversa.
Esta relación debería ser simétrica e irreflexiva.*/
mismoTipo(Pokemon1,Pokemon2):-
        forall(tipo(Pokemon1,Tipo), tipo(Pokemon2,Tipo)),
        forall(tipo(Pokemon2,Tipo), tipo(Pokemon1,Tipo)).

mismaLinea(Pokemon1,Pokemon2):-
        puedeEvolucionar(Pokemon1,Pokemon2).
mismaLinea(Pokemon1,Pokemon2):-
        puedeEvolucionar(Pokemon2,Pokemon1).

parecidas(Pokemon1,Pokemon2):-
        tipo(Pokemon1,_),
        tipo(Pokemon2,_),
        Pokemon1 \= Pokemon2,
        not(mismaLinea(Pokemon1,Pokemon2)),
        mismoTipo(Pokemon1,Pokemon2).

% --------------------------------
% Código inicial - NO TOCAR
% --------------------------------

% tipo(Especie, Tipo).
tipo(charmander, fuego).
tipo(charmeleon, fuego).
tipo(charizard, fuego).
tipo(charizard, volador).

tipo(bulbasaur, planta).
tipo(bulbasaur, veneno).
tipo(ivysaur, planta).
tipo(ivysaur, veneno).
tipo(venusaur, planta).
tipo(venusaur, veneno).

tipo(squirtle, agua).
tipo(wartortle, agua).
tipo(blastoise, agua).

tipo(pikachu, electrico).
tipo(raichu, electrico).

tipo(farfetchd, normal).
tipo(farfetchd, volador).

tipo(eevee, normal).
tipo(flareon, fuego).
tipo(jolteon, electrico).
tipo(vaporeon, agua).
tipo(leafeon, planta).

tipo(moltres, fuego).
tipo(moltres, volador).

% evolucion(Especie, Evolucion).
evolucion(charmander, charmeleon).
evolucion(charmeleon, charizard).
evolucion(bulbasaur, ivysaur).
evolucion(ivysaur, venusaur).
evolucion(squirtle, wartortle).
evolucion(wartortle, blastoise).
evolucion(pikachu, raichu).
evolucion(eevee, flareon).
evolucion(eevee, jolteon).
evolucion(eevee, vaporeon).
evolucion(eevee, leafeon).

puedeEvolucionar(Especie, Evolucion):-
  evolucion(Especie, Evolucion).
puedeEvolucionar(Especie, Evolucion):-
  evolucion(Especie, OtraEspecie),
  puedeEvolucionar(OtraEspecie, Evolucion).

% --------------------------------
% TESTS - NO TOCAR
% --------------------------------

:- begin_tests(tests_tp2_tiposIncompatibles).

test(incompatiblesEsInversibleParaSuPrimerParametro, set(Tipo == [normal, planta, agua, electrico, veneno])):-
        incompatibles(fuego, Tipo).

test(incompatiblesEsInversibleParaSuSegundoParametro, set(Tipo == [agua,electrico,fuego,normal,volador])):-
        incompatibles(Tipo,veneno).

test(unTipoNoEsIncompatibleConsigoMismo, fail):-
        incompatibles(fuego, fuego).

test(siUnaMismaEspecieTieneDosTiposNoSonIncompatibles, fail):-
        incompatibles(planta,veneno).

:- end_tests(tests_tp2_tiposIncompatibles).

:- begin_tests(tests_tp2_ultimaEvolucion).

test(unaEvolucionIntermediaNoEsUltimaEvolucion, fail):-
        ultimaEvolucion(ivysaur).

test(unaEspecieBasicaQueNoEvolucionaNoEsUltimaEvolucion, fail):-
        ultimaEvolucion(farfetchd).

test(unaEspecieQueEsPrimerEvolucionEsUltimaSiNoHayUnaSegundaEvolucion, nondet):-
        ultimaEvolucion(flareon).

test(ultimaEvolucionEsInversible, set(Especie == [charizard, venusaur, blastoise, raichu, flareon, jolteon, vaporeon, leafeon])):-
        ultimaEvolucion(Especie).
:- end_tests(tests_tp2_ultimaEvolucion).

:- begin_tests(tests_tp2_predecible).

test(unaEspecieEsPredecibleSiTodasSusEvolucionesTienenUnMismoTipoQueElla, nondet):-
        predecible(charmander).

test(unaEspecieNoEsPredecibleSiNoTieneUnTipoQueTodasSusEvolucionesTambienTengan, fail):-
        predecible(eevee).

test(unaEspecieEsPredecibleSiNoPuedeEvolucionar, nondet):-
        predecible(flareon).

test(predecibeEsInversible, set(Especie == [charmander, charmeleon, charizard, bulbasaur, ivysaur, venusaur, squirtle, wartortle, blastoise, pikachu, raichu, farfetchd, flareon, jolteon, vaporeon, leafeon, moltres])):-
        predecible(Especie).
:- end_tests(tests_tp2_predecible).


:- begin_tests(tests_tp2_parecidas).

test(dosEspeciesSonParecidasSiTodosLosTiposCoincidenYNoPertenecenALaMismaLineaEvolutiva, nondet):-
        parecidas(moltres, charizard).

test(dosEspeciesNoSonParecidasSiPertenecenALaMismaLineaEvolutiva, fail):-
        parecidas(bulbasaur, ivysaur).

test(dosEspeciesNoSonParecidasSiAlgunTipoNoCoincide, fail):-
        parecidas(flareon, charizard).

test(parecidasEsInversibleParaSuPrimerParametro, set(Especie == [charmander, charmeleon])):-
        parecidas(Especie, flareon).

test(parecidasEsInversibleParaSuSegundoParametro, set(Especie == [pikachu, raichu])):-
        parecidas(jolteon, Especie).

:- end_tests(tests_tp2_parecidas).