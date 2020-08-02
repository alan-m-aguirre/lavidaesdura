vende(laGondoriana,trancosin,35).
vende(laGondoriana,sanaSam,35).
incluye(trancosin,athelas).
incluye(trancosin,cenizaBoromireana).
efecto(athelas,cura(desazon)).
efecto(athelas,cura(heridaDeOrco)).
efecto(cenizaBoromireana,cura(gripeA)).
efecto(cenizaBoromireana,potencia(deseoDePoder)).
estaEnfermo(eomer,heridaDeOrco). % eomer es varon
estaEnfermo(eomer,deseoDePoder).
estaEnfermo(eomund,desazon).
estaEnfermo(eowyn,heridaDeOrco). % eowyn es mujer
padre(eomund,eomer).
actividad(eomer,fecha(15,6,3014),compro(trancosin,laGondoriana)).
actividad(eomer,fecha(15,8,3014),preguntoPor(sanaSam,laGondoriana)).
actividad(eowyn,fecha(14,9,3014),preguntoPor(sanaSam,laGondoriana)).

% incluye(Producto,Droga).
% efecto(Droga,Efecto).
% vende(Farmacia,Medic,Precio).
% estaEnfermo(Persona,Enfer).
% padre(Padre,Hijo).
% actividad(Persona,Fecha,Activ).

% 1.
curaEnfermedad(Medicamento,Enfermedad):-
    incluye(Medicamento,Droga),
    efecto(Droga,cura(Enfermedad)).

esPersona(Persona):-
    distinct(Persona, estaEnfermo(Persona,_)).

esMedicamento(Medicamento):-
    distinct(Medicamento, incluye(Medicamento,_)).

potenciaEnfermedad(Medicamento,Enfermedad):-
    incluye(Medicamento,Droga),
    efecto(Droga,potencia(Enfermedad)).

medicamentoUtil(Persona,Medicamento):-
    distinct(Persona, (
        estaEnfermo(Persona,Enfermedad),
        curaEnfermedad(Medicamento,Enfermedad),
        forall(estaEnfermo(Persona,OtraEnfermedad), not(potenciaEnfermedad(Medicamento,OtraEnfermedad)))
    )).

% 2.
medicamentoMilagroso(Persona,Medicamento):-
    esPersona(Persona),
    esMedicamento(Medicamento),
    forall(estaEnfermo(Persona,Enfermedad), (
        curaEnfermedad(Medicamento,Enfermedad),
        not(potenciaEnfermedad(Medicamento,Enfermedad))
    )).

% 3.
esDroga(Droga):-
    distinct(Droga, incluye(_,Droga)).

listaEnfermedadesQueCura(Droga,Lista):-
    incluye(Medicamento,Droga),
    findall(cura(Enfermedad), curaEnfermedad(Medicamento,Enfermedad), Lista).

drogaSimpatica(Droga):-
    esDroga(Droga),
    listaEnfermedadesQueCura(Droga,Lista),
    length(Lista, Cantidad),
    Cantidad >= 4,
    not(efecto(Droga,potencia(_))).
drogaSimpatica(Droga):-
    estaEnfermo(eomer,UnaEnfermedad),
    curaEnfermedad(Droga,UnaEnfermedad),
    estaEnfermo(eowyn,OtraEnfermedad),
    curaEnfermedad(Droga,OtraEnfermedad),
    OtraEnfermedad \= UnaEnfermedad.
drogaSimpatica(Droga):-
    distinct(Droga, incluye(Medicamento,Droga)),
    vende(Farmacia,Medicamento,_),
    not(
        (vende(Farmacia,Medicamento,Precio),
        Precio > 10)
    ).

% 4.
comproMedicamento(Persona,Medicamento):-
    actividad(Persona,_,compro(Medicamento,_)).

tipoSuicida(Persona):-
    estaEnfermo(Persona,Enfermedad),
    comproMedicamento(Persona,Medicamento),
    potenciaEnfermedad(Medicamento,Enfermedad),
    not( 
        (estaEnfermo(Persona,OtraEnfermedad),
        curaEnfermedad(Persona,OtraEnfermedad))
    ).

% 5.
esComprador(Persona):-
    distinct(Persona, actividad(Persona,_,compro(_,_))).
preguntoPorPrecio(Persona,Medicamento):-
    actividad(Persona,FechaCompra,compro(Medicamento,FarmaciaCompra)),
    vende(FarmaciaCompra,Medicamento,PrecioCompra),
    actividad(Persona,FechaPregunta,preguntoPor(Medicamento,OtraFarmacia)),
    vende(OtraFarmacia,Medicamento,OtroPrecio),
    PrecioCompra < OtroPrecio,
    FechaCompra > FechaPregunta.

tipoAhorrativo(Persona):-
    esComprador(Persona),
    forall(comproMedicamento(Persona,Medicamento), preguntoPorPrecio(Persona,Medicamento)).

% 6.
tipoActivo(Persona,Mes,Anio):-
    actividad(Persona,fecha(_,Mes,Anio),_).

comproOPreguntoPorMedicamento(Persona,Fecha,Medicamento):-
    actividad(Persona,Fecha,compro(Medicamento,_)).
comproOPreguntoPorMedicamento(Persona,Fecha,Medicamento):-
    actividad(Persona,Fecha,preguntoPor(Medicamento,_)).

diaProductivo(Fecha):-
    actividad(_,Fecha,_),
    forall(actividad(Persona,Fecha,_),
        (comproOPreguntoPorMedicamento(Persona,Fecha,Medicamento),
        medicamentoUtil(Persona,Medicamento))
    ).

% 7.
pagoPorMedicamento(Persona,Precio):-
    actividad(Persona,_,compro(Medicamento,Farmacia)),
    vende(Farmacia,Medicamento,Precio).

gastoTotal(Persona,Plata):-
    esComprador(Persona),
    findall(Precio, pagoPorMedicamento(Persona,Precio), Ticket),
    sum_list(Ticket, Plata).
    
% 8.
ancestro(Antepasado,Descendiente):-
    padre(Antepasado,Descendiente).
ancestro(Antepasado,Descendiente):-
    padre(OtraPersona,Descendiente),
    ancestro(Antepasado,OtraPersona).

zafoDe(Persona,Enfermedad):-
    esPersona(Persona),
    ancestro(Antepasado,Persona),
    estaEnfermo(Antepasado,Enfermedad),
    not(estaEnfermo(Persona,Enfermedad)).