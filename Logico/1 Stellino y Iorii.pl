% 1) 
serie(dexter,8,drama(800000)).
serie(scandal,2,drama(500000)).
serie(breakingBad,5,drama(10000000)).
serie(prisonBreak,4,accion(750000,fox)).
serie(theWalkingDead,7,accion(5000000,fox)).
serie(friends,10,comedia(2500000,1994,warner)).
serie(howIMetYourMother,9,comedia(100000,2005,sony)).
serie(theBigBangTheory,3,comedia(900000,2007,hbo)).

actor(aaronPaul,protagonista(breakingBad)).
actor(aaronPaul,protagonista(dexter)).
actor(michaelHall,secundario(dexter)).
actor(michaelHall,secundario(breakingBad)).
actor(jeffPerry,secundario(scandal)).
actor(jenniferAniston,protagonista(friends)).
actor(jenniferAniston,secundario(prisonBreak)).

% 2) 
trabajaEn(Actor,Serie):- actor(Actor,protagonista(Serie)).

trabajaEn(Actor,Serie):- actor(Actor,secundario(Serie)).

% 3)
esClasico(Serie):- serie(Serie,_,comedia(_,Anio,_)),Anio =< 2006.

esClasico(Serie):- emitidaPor(Serie,fox).

esClasico(Serie):- emitidaPor(Serie,warner).

esClasico(breakingBad).

emitidaPor(Serie,Emisora):- serie(Serie,_,accion(_,Emisora)).

emitidaPor(Serie,Emisora):- serie(Serie,_,comedia(_,_,Emisora)).

% 4)
tieneBuenElenco(Serie):- trabajaEn(_,Serie),forall(trabajaEn(Actor,Serie),esProtagonista(Actor,Serie)).
esProtagonista(Actor,Serie):- actor(Actor,protagonista(Serie)).

% 5) 
miran(Espectadores,Serie):- serie(Serie,_,drama(Espectadores)).

miran(Espectadores,Serie):- serie(Serie,_,accion(Espectadores,_)).

miran(Espectadores,Serie):- serie(Serie,_,comedia(Espectadores,_,_)).

% 6)
esFamoso(Actor):- esSiempreProtagonista(Actor), trabajaEn(Actor,Serie), laMiranMuchos(Serie).

esFamoso(Actor):- esSiempreProtagonista(Actor), forall((trabajaEn(Actor,Serie),serie(Serie,Temp,_)),Temp>4).

esFamoso(Actor):- esSiempreProtagonista(Actor), forall(trabajaEn(Actor,Serie),emitidaPor(Serie,fox)).

esFamoso(Actor):- esSiempreProtagonista(Actor), forall(trabajaEn(Actor,Serie),emitidaPor(Serie,warner)).

esSiempreProtagonista(Actor):- trabajaEn(Actor,_), forall(trabajaEn(Actor,Serie),esProtagonista(Actor,Serie)).

laMiranMuchos(Serie):- miran(_,Serie), not((forall(miran(Espectadores,Serie),Espectadores=<1000000))).

% 7)

seLlevanBien(UnActor,OtroActor):- actor(UnActor,_),actor(OtroActor,_),UnActor\=OtroActor,findall(_,(trabajaEn(UnActor,UnaSerie), trabajaEn(OtroActor,UnaSerie)), LsSerie), length(LsSerie,Cant), Cant > 1.


% 8)
ameritaNuevaTemporada(Serie):- trabajaEn(UnActor,Serie),esFamoso(UnActor),forall((trabajaEn(OtroActor,Serie),UnActor\=OtroActor),seLlevanBien(UnActor,OtroActor)).

% 9)
puedeContratarA(Actor):- trabajaSoloEnUnaSerie(Actor), esActorSecundario(Actor,Serie), tieneMenosDeTresTemp(Serie).

esActorSecundario(Actor,Serie):- actor(Actor,secundario(Serie)).

trabajaSoloEnUnaSerie(Actor):- trabajaEn(Actor,UnaSerie), forall(trabajaEn(Actor,OtraSerie),UnaSerie==OtraSerie).

tieneMenosDeTresTemp(Serie):- serie(Serie,Cantidad,_),Cantidad=<3.
