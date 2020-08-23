personaje(pumkin,     ladron([licorerias, estacionesDeServicio])).
personaje(honeyBunny, ladron([licorerias, estacionesDeServicio])).
personaje(vincent,    mafioso(maton)).
personaje(jules,      mafioso(maton)).
personaje(marsellus,  mafioso(capo)).
personaje(winston,    mafioso(resuelveProblemas)).
personaje(mia,        actriz([foxForceFive])).
personaje(butch,      boxeador).

pareja(marsellus, mia).
pareja(pumkin,    honeyBunny).

%trabajaPara(Empleador, Empleado)
trabajaPara(marsellus, vincent).
trabajaPara(marsellus, jules).
trabajaPara(marsellus, winston).

%etc

/*
1. esPeligroso/1. Nos dice si un personaje es peligroso. Eso ocurre cuando:
- realiza alguna actividad peligrosa: ser matón, o robar licorerías. 
- tiene empleados peligrosos
*/
actividadPeligrosa(mafioso(maton)).
actividadPeligrosa(ladron(Tareas)):-
    member(licorerias, Tareas).

esPeligroso(Personaje):-
    personaje(Personaje, Actividad),
    actividadPeligrosa(Actividad).
esPeligroso(Personaje):-
    distinct(Personaje,
        (trabajaPara(Personaje,OtroPersonaje),
        esPeligroso(OtroPersonaje))
    ).

/*
2. duoTemible/2 que relaciona dos personajes cuando son peligrosos y además son pareja o amigos.
*/
amigo(vincent, jules).
amigo(jules, jimmie).
amigo(vincent, elVendedor).

esPersonaje(UnPersonaje):-
    personaje(UnPersonaje,_).

amigoReflexivo(UnPersonaje,OtroPersonaje):-
    amigo(UnPersonaje,OtroPersonaje).
amigoReflexivo(UnPersonaje,OtroPersonaje):-
    amigo(OtroPersonaje,UnPersonaje).

parejaReflexivo(UnPersonaje,OtroPersonaje):-
    pareja(UnPersonaje,OtroPersonaje).
parejaReflexivo(UnPersonaje,OtroPersonaje):-
    pareja(OtroPersonaje,UnPersonaje).

relacionados(UnPersonaje,OtroPersonaje):-
    amigoReflexivo(UnPersonaje,OtroPersonaje).
relacionados(UnPersonaje,OtroPersonaje):-
    parejaReflexivo(OtroPersonaje,UnPersonaje).

personasDistintas(UnPersonaje,OtroPersonaje):-
    esPersonaje(UnPersonaje),
    esPersonaje(OtroPersonaje),
    UnPersonaje \= OtroPersonaje.

duoPeligroso(UnPersonaje,OtroPersonaje):-
    esPeligroso(UnPersonaje),
    esPeligroso(OtroPersonaje).

duoTemible(UnPersonaje,OtroPersonaje):-
    personasDistintas(UnPersonaje,OtroPersonaje),
    duoPeligroso(UnPersonaje,OtroPersonaje),
    relacionados(UnPersonaje,OtroPersonaje).

/*
3.  estaEnProblemas/1: un personaje está en problemas cuando 
- el jefe es peligroso y le encarga que cuide a su pareja
- o bien, tiene que ir a buscar a un boxeador. 
Además butch siempre está en problemas.
*/
%encargo(Solicitante, Encargado, Tarea). 
%las tareas pueden ser cuidar(Protegido), ayudar(Ayudado), buscar(Buscado, Lugar)
encargo(marsellus, vincent,   cuidar(mia)).
encargo(vincent,  elVendedor, cuidar(mia)).
encargo(marsellus, winston, ayudar(jules)).
encargo(marsellus, winston, ayudar(vincent)).
encargo(marsellus, vincent, buscar(butch, losAngeles)).

estaEnProblemas(butch).
estaEnProblemas(Personaje):-
    trabajaPara(Jefe,Personaje),
    esPeligroso(Jefe),
    encargo(Jefe,Personaje,cuidar(Protegido)),
    parejaReflexivo(Jefe,Protegido).
estaEnProblemas(Personaje):-
    encargo(_,Personaje,buscar(Buscado,_)),
    personaje(Buscado,boxeador).

/*
4.  sanCayetano/1:  es quien a todos los que tiene cerca les da trabajo (algún encargo). 
Alguien tiene cerca a otro personaje si es su amigo o empleado. 
*/
cercano(UnPersonaje,OtroPersonaje):-
    personasDistintas(UnPersonaje,OtroPersonaje),
    amigoReflexivo(UnPersonaje,OtroPersonaje).
cercano(UnPersonaje,OtroPersonaje):-
    personasDistintas(UnPersonaje,OtroPersonaje),
    trabajaPara(UnPersonaje,OtroPersonaje).

sanCayetano(UnPersonaje):-
    esPersonaje(UnPersonaje),
    forall(cercano(UnPersonaje,OtroPersonaje), encargo(UnPersonaje,OtroPersonaje,_)).

/*
5. masAtareado/1. Es el más atareado aquel que tenga más encargos que cualquier otro personaje.
*/
listaTareasPersonaje(Personaje,Lista):-
    findall(Encargo, encargo(_,Personaje,Encargo), Lista).

cantidadTareas(Personaje,Cantidad):-
    esPersonaje(Personaje),
    listaTareasPersonaje(Personaje,Lista),
    length(Lista,Cantidad).

masAtareado(Personaje):-
    cantidadTareas(Personaje,Cantidad),
    not((cantidadTareas(OtroPersonaje,OtraCantidad),
    personasDistintas(Personaje,OtroPersonaje),
    Cantidad < OtraCantidad)).

/*
6. personajesRespetables/1: genera la lista de todos los personajes respetables. Es respetable cuando su actividad tiene un nivel de respeto mayor a 9. Se sabe que:
- Las actrices tienen un nivel de respeto de la décima parte de su cantidad de peliculas.
- Los mafiosos que resuelven problemas tienen un nivel de 10 de respeto, los matones 1 y los capos 20.
- Al resto no se les debe ningún nivel de respeto. 
*/

seRigePorOtrasReglas(mafioso(_)).
seRigePorOtrasReglas(actriz(_)).

respetoPorTrabajo(mafioso(capo),20).
respetoPorTrabajo(mafioso(resuelveProblemas),10).
respetoPorTrabajo(mafioso(maton),1).
respetoPorTrabajo(Trabajo,0):-
    distinct(Trabajo, (
    personaje(_,Trabajo),
    not(seRigePorOtrasReglas(Trabajo))
    )).

respeto(Personaje,Respeto):-
    personaje(Personaje,actriz(Peliculas)),
    length(Peliculas, Cantidad),
    Respeto is Cantidad / 10.
respeto(Personaje,Respeto):-
    personaje(Personaje,Trabajo),
    respetoPorTrabajo(Trabajo,Respeto).

respetable(Personaje):-
    respeto(Personaje,Respeto),
    Respeto > 9.

personajesRespetables(Lista):-
    findall(Personaje, respetable(Personaje), Lista).

/*
7. hartoDe/2: un personaje está harto de otro, cuando todas las tareas asignadas al primero requieren interactuar con el segundo (cuidar, buscar o ayudar) o un amigo del segundo.
*/
interactua(UnPersonaje,OtroPersonaje):-
    encargo(_,UnPersonaje,cuidar(OtroPersonaje)).
interactua(UnPersonaje,OtroPersonaje):-
    encargo(_,UnPersonaje,buscar(OtroPersonaje,_)).
interactua(UnPersonaje,OtroPersonaje):-
    encargo(_,UnPersonaje,ayudar(OtroPersonaje)).

personajeEsObjetivoTarea(cuidar(Personaje),Personaje).
personajeEsObjetivoTarea(buscar(Personaje,_),Personaje).
personajeEsObjetivoTarea(ayudar(Personaje),Personaje).

amigoOMismaPersona(UnPersonaje,OtroPersonaje):-
    amigoReflexivo(UnPersonaje,OtroPersonaje).
amigoOMismaPersona(UnPersonaje,OtroPersonaje):-
    esPersonaje(UnPersonaje),
    UnPersonaje == OtroPersonaje.

hartoDe(UnPersonaje,OtroPersonaje):-
    distinct(OtroPersonaje,
        (encargo(_,UnPersonaje,_),
        esPersonaje(OtroPersonaje),
        forall(encargo(_,UnPersonaje,Tarea), (personajeEsObjetivoTarea(Tarea,Personaje), amigoOMismaPersona(Personaje,OtroPersonaje))))
    ).

/*
8. Desarrollar duoDiferenciable/2, que relaciona a un dúo (dos amigos o una pareja) en el que uno tiene al menos una característica que el otro no. 
*/
caracteristicas(vincent,  [negro, muchoPelo, tieneCabeza]).
caracteristicas(jules,    [tieneCabeza, muchoPelo]).
caracteristicas(marvin,   [negro]).

duoDiferenciable(UnPersonaje,OtroPersonaje):-
    relacionados(UnPersonaje,OtroPersonaje),
    caracteristicas(UnPersonaje,Lista1),
    caracteristicas(OtroPersonaje,Lista2),
    member(Caracteristica,Lista1),
    not(member(Caracteristica,Lista2)).