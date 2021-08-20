%Tp grupal Democracia

%1)
%candidato(Candidato, Partido)
candidato(frank, rojo).
candidato(claire,  rojo).
candidato(garrett, azul).
candidato(jackie, amarillo).
candidato(linda, azul ).
candidato(catherine, rojo).
candidato(heather, amarillo).

%persona(Candidato, Edad).
persona(frank, 50).
persona(claire, 52).
persona(garrett, 64).
persona(jackie, 38).
persona(linda, 30).
persona(catherine, 59).
persona(heather,52).

%postula(Partido, Provincias).
postula(azul,[buenosAires, chaco, tierraDelFuego, sanLuis, neuquen]).
postula(rojo, [buenosAires, santaFe, cordoba, chubut, tierraDelFuego, sanLuis]).
postula(amarillo, [chaco, formosa, tucuman, salta, santaCruz, laPampa, corrientes, misiones, buenosAires]).

%habitantes(Provincia, Cantidad)
habitantes(buenosAires, 15355000).
habitantes(chaco, 1143201).
habitantes(tierraDelFuego, 160720).
habitantes(sanLuis, 489255).
habitantes(neuquen, 637913).
habitantes(santaFe, 3397532).
habitantes(cordoba, 3567654).
habitantes(chubut, 577466).
habitantes(formosa, 527895).
habitantes(tucuman, 1687305).
habitantes(salta, 1333365).
habitantes(santaCruz, 273964).
habitantes(laPampa, 349299).
habitantes(corrientes, 992595).
habitantes(misiones, 1189446).

%2)
%   Provincia picante
esPicante(Provincia):-
    sePostulan(Provincia),
    habitantes(Provincia, Cantidad),
    cumpleHabitantes(Cantidad).

cumpleHabitantes(Cantidad):-
    Cantidad>1000000.

sePostulan(Provincia):-
    postula(UnPartido, Provincias),
    postula(OtroPartido, OtrasProvincias),
    UnPartido\=OtroPartido,
    member(Provincia, Provincias),
    member(Provincia, OtrasProvincias).

% intencionDeVotoEn(Provincia, Partido, Porcentaje)
intencionDeVotoEn(buenosAires, rojo, 40).
intencionDeVotoEn(buenosAires, azul, 30).
intencionDeVotoEn(buenosAires, amarillo, 30).
intencionDeVotoEn(chaco, rojo, 50).
intencionDeVotoEn(chaco, azul, 20).
intencionDeVotoEn(chaco, amarillo, 0).
intencionDeVotoEn(tierraDelFuego, rojo, 40).
intencionDeVotoEn(tierraDelFuego, azul, 20).
intencionDeVotoEn(tierraDelFuego, amarillo, 10).
intencionDeVotoEn(sanLuis, rojo, 50).
intencionDeVotoEn(sanLuis, azul, 20).
intencionDeVotoEn(sanLuis, amarillo, 0).
intencionDeVotoEn(neuquen, rojo, 80).
intencionDeVotoEn(neuquen, azul, 10).
intencionDeVotoEn(neuquen, amarillo, 0).
intencionDeVotoEn(santaFe, rojo, 20).
intencionDeVotoEn(santaFe, azul, 40).
intencionDeVotoEn(santaFe, amarillo, 40).
intencionDeVotoEn(cordoba, rojo, 10).
intencionDeVotoEn(cordoba, azul, 60).
intencionDeVotoEn(cordoba, amarillo, 20).
intencionDeVotoEn(chubut, rojo, 15).
intencionDeVotoEn(chubut, azul, 15).
intencionDeVotoEn(chubut, amarillo, 15).
intencionDeVotoEn(formosa, rojo, 0).
intencionDeVotoEn(formosa, azul, 0).
intencionDeVotoEn(formosa, amarillo, 0).
intencionDeVotoEn(tucuman, rojo, 40).
intencionDeVotoEn(tucuman, azul, 40).
intencionDeVotoEn(tucuman, amarillo, 20).
intencionDeVotoEn(salta, rojo, 30).
intencionDeVotoEn(salta, azul, 60).
intencionDeVotoEn(salta, amarillo, 10).
intencionDeVotoEn(santaCruz, rojo, 10).
intencionDeVotoEn(santaCruz, azul, 20).
intencionDeVotoEn(santaCruz, amarillo, 30).
intencionDeVotoEn(laPampa, rojo, 25).
intencionDeVotoEn(laPampa, azul, 25).
intencionDeVotoEn(laPampa, amarillo, 40).
intencionDeVotoEn(corrientes, rojo, 30).
intencionDeVotoEn(corrientes, azul, 30).
intencionDeVotoEn(corrientes, amarillo, 10).
intencionDeVotoEn(misiones, rojo, 90).
intencionDeVotoEn(misiones, azul, 0).
intencionDeVotoEn(misiones, amarillo, 0).


leGanaA(UnCandidato, OtroCandidato, Provincia):-
    candidato(UnCandidato, Partido),
    candidato(OtroCandidato, OtroPartido),
    sePostula(Partido, Provincia),
    leGana(Partido, OtroPartido, Provincia),
    UnCandidato\=OtroCandidato.

sePostula(Partido, Provincia):-
    postula(Partido, Provincias),
    member(Provincia, Provincias).

leGana(Partido, Partido, Provincia):-
    sePostula(Partido, Provincia).

leGana(Partido,OtroPartido, Provincia):-
    sePostula(Partido, Provincia),
    not(sePostula(OtroPartido, Provincia)).

leGana(Partido, OtroPartido, Provincia):-
    sePostula(Partido, Provincia),
    sePostula(OtroPartido, Provincia),
    intencionDeVotoEn(Provincia, Partido, Porcentaje),
    intencionDeVotoEn(Provincia, OtroPartido, OtroPorcentaje),
    Porcentaje > OtroPorcentaje,
    Partido\=OtroPartido.   



%4)el Gran Candidato HAY ALGUN ERROR QUE NO ESTARIA VIENDO
elGranCandidato(Candidato):-
    candidato(Candidato, Partido),
    persona(Candidato, Edad),
    elPartidoGana(Candidato,Partido),
    esElMasJoven(Candidato,Edad, Partido).

elPartidoGana(Candidato, Partido):-
    postula(Partido, Provincias),
     forall( (member(Provincia,Provincias),candidato(OtroCandidato, _)),
     leGanaA(Candidato, OtroCandidato, Provincia)).

esElMasJoven(Candidato,Edad, Partido):-
    forall((candidato(OtroCandidato,Partido),persona(OtroCandidato, OtraEdad),Candidato\=OtroCandidato),
     Edad<OtraEdad).
    
%5) Ajuste de consultoras
ajusteConsultora(Partido, Provincia, PorcentajeVerdadero):-
    intencionDeVotoEn(Provincia, Partido, PorcentajeVotos),
    modificaIntencion(Partido, Provincia, PorcentajeVotos, PorcentajeVerdadero).
    
modificaIntencion(Partido, Provincia, PorcentajeVotos, PorcentajeVerdadero):-
    ganaSegunIntencion(Partido, Provincia),
    PorcentajeVerdadero is PorcentajeVotos-20.
    
modificaIntencion(_, _, PorcentajeVotos, PorcentajeVerdadero):-
    not( ganaSegunIntencion(Partido, Provincia)),
    PorcentajeVerdadero is PorcentajeVotos+5.

ganaSegunIntencion(Partido,Provincia):-
    forall((intencionDeVotoEn(Provincia,Partido,Porcentaje),
        intencionDeVotoEn(Provincia,OtroPartido,OtroPorcentaje),
         Partido\=OtroPartido), 
    Porcentaje>OtroPorcentaje).

%6)Promesas de Campaña

% inflacion(cotaInferior, cotaSuperior)
% construir(listaDeObras)
% nuevosPuestosDeTrabajo(cantidad)

%listaDeObras= es una lista de functores -> edilicio(Tipo, Cantidad).
%promete(Partido, Promesa)
promete(azul,construir([edilicio(hospital,100),edilicio(jardin,100),edilicio(escuela,5)])).
promete(amarillo,construir([edilicio(hospital,100),edilicio(universidad,1),edilicio(comisaria,200)])).

promete(rojo,nuevosPuestosDeTrabajo(800000)).
promete(amarillo,nuevosPuestosDeTrabajo(10000)).

promete(azul,inflacion(2,4)).
promete(amarillo,inflacion(1,15)).
promete(rojo,inflacion(10,30)).

%7)Ajustes de boca de urna

influenciaDePromesas(inflacion(CotaInf,CotaSup), VarIntencionVoto):-
    VarIntencionVoto is (CotaInf+CotaSup)/(-2).

influenciaDePromesas(nuevosPuestosDeTrabajo(Cantidad), 3):-
    Cantidad > 50000. 

influenciaDePromesas(construir(Edilicios), VarIntencionVoto):-
    findall(Valor,(member(Edilicio,Edilicios),variacionEdilicio(Edilicio,Valor)),ListaValores),
    sumlist(ListaValores,VarIntencionVoto).


variacionEdilicio(edilicio(jardin,Cantidad),Valor):-
    Valor is Cantidad*1/10.
variacionEdilicio(edilicio(escuela,Cantidad),Valor):-
    Valor is Cantidad*1/10.

variacionEdilicio(edilicio(hospital,_),2).
variacionEdilicio(edilicio(comisaria,200),2).
variacionEdilicio(edilicio(universidad,_),0).
  
%variacionEdilicio(edilicio(_,_),-1).

%8) Nuevos Votos

promedioDeCrecimiento(Partido,Crecimiento):-
    findall(Valor,(promete(Partido,Promesa),influenciaDePromesas(Promesa,Valor)),ListaValores),
    sumlist(ListaValores,Crecimiento).



