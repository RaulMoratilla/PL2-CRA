%Diccionario

determinante(det(X)) --> [X], {det(X)}.
det(el).
det(la).
det(un).
det(una).
det(mi).

nombre(n(X)) --> [X], {n(X)}.
%nombre(n(X)) --> nombre_propio(n(X)).
n(hombre).
n(mujer).
n(manzana).
n(gato).
n(raton).
n(alumno).
n(universidad).
n(juan).
n(maria).
n(jose).
n(hector).
n(irene).
n(filosofia).
n(derecho).
n(cafe).
n(mesa).
n(periodico).
n(patatas).
n(cerveza).
n(paella).
n(novela).
n(zumo).
n(procesador).
n(textos).
n(herramienta).
n(documentos).
n(raton).
n(gato).
n(hombre).
n(vecino).
n(escribir).

nombre_propio(np(X)) --> [X], {np(X)}.
np(juan).
np(maria).
np(jose).
np(hector).
np(irene).

verbo(v(X)) --> [X], {v(X)}.
v(ama).
v(come).
v(estudia).
v(bebe).
v(es).
v(toma).
v(recoge).
v(lee).
v(comen).
v(beben).
v(prefiere).
v(canta).
v(salta).
v(escala).
v(sirve).
v(cazo).
v(vimos).
v(era).

adjetivo(adj(X)) --> [X], {adj(X)}.
adj(roja).
adj(negro).
adj(grande).
adj(gris).
adj(pequeno).
adj(alta).
adj(moreno).
adj(fritas).
adj(potente).
adj(lento).

conjuncion(conj(X)) --> [X], {conj(X)}.
conj(y).
conj(e).
conj(ni).
conj(pero).
conj(aunque).

adverbio(adv(X)) --> [X], {adv(X)}.
adv(que).
adv(cuando).
adv(donde).
adv(mientras).
adv(muy).
adv(bastante).
adv(ayer).

preposicion(prep(X)) --> [X], {prep(X)}.
prep(a).
prep(de).
prep(para).

% Reglas gramaticales

compuesta(ocm(OCM)) --> oracion(OCM).
compuesta(ocm(OCM)) --> subordinada(OCM).
compuesta(ocm(OCM)) --> coordinada(OCM).

oracion(o(GV)) --> g_verbal(GV).
oracion(o(GN, GV)) --> g_nominal(GN), g_verbal(GV).

coordinada(oc(O,CONJ,O2)) --> oracion(O), conjuncion(CONJ), oracion(O2).
coordinada(oc(O,CONJ,O2)) --> oracion(O), conjuncion(CONJ), subordinada(O2).
coordinada(oc(O,CONJ,O2)) --> oracion(O), conjuncion(CONJ), coordinada(O2).

subordinada(or(O,ADV,O2)) --> oracion(O), adverbio(ADV), oracion(O2).
subordinada(or(O,ADV,O2)) --> oracion(O), adverbio(ADV), coordinada(O2).
subordinada(or(O,ADV,O2)) --> oracion(O), adverbio(ADV), subordinada(O2).
%subordinada(or(O,ADV,O2)) --> adverbio(ADV), oracion(O), oracion(O2).
%subordinada(or(O,ADV,O2)) --> adverbio(ADV), oracion(O), coordinada(O2).
%subordinada(or(O,ADV,O2)) --> adverbio(ADV), oracion(O), subordinada(O2).

g_nominal(gn(N)) --> nombre(N).
g_nominal(gn(D,N)) --> determinante(D), nombre(N).
g_nominal(gn(N,A)) --> nombre(N), g_adjetival(A).
g_nominal(gn(N,GP)) --> nombre(N), g_preposicional(GP).
g_nominal(gn(GN,CONJ,GN2)) --> nombre(GN), conjuncion(CONJ), nombre(GN2).

g_verbal(gv(V)) --> verbo(V).
g_verbal(gv(V,GN)) --> verbo(V), g_nominal(GN).
g_verbal(gv(V,A)) --> verbo(V), g_adjetival(A).

g_adjetival(gadj(A)) --> adjetivo(A).
g_adjetival(gadj(GADJ,GADJ2)) --> adjetivo(GADJ), adjetivo(GADJ2).
g_adjetival(gadj(GADJ,GN)) --> adjetivo(GADJ), g_nominal(GN).
g_adjetival(gadj(GADJ,GP)) --> adjetivo(GADJ), g_preposicional(GP).
g_adjetival(gadj(ADV,GADJ)) --> adverbio(ADV), adjetivo(GADJ).
g_adjetival(gadj(GADJ,CONJ,GADJ2)) --> adjetivo(GADJ), conjuncion(CONJ), adjetivo(GADJ2).

g_adverbial(gadv(ADV)) --> adverbio(ADV).
g_adverbial(gadv(GADV,GADV2)) --> adverbio(GADV), adverbio(GADV2).
g_adverbial(gadv(GADV,GN)) --> adverbio(GADV), g_preposicional(GN).

g_preposicional(gp(P,GN)) --> preposicion(P), g_nominal(GN).
g_preposicional(gp(P,GADJ)) --> preposicion(P), g_adjetival(GADJ).
g_preposicional(gp(P,GADV)) --> preposicion(P), g_adverbial(GADV).

o_prueba([jose,es,moreno,y,maria,es,alta]).
o_prueba([jose,estudia,filosofia,pero,maria,estudia,derecho]).
o_prueba([maria,toma,un,cafe,mientras,jose,recoge,la,mesa]).
o_prueba([jose,toma,cafe,y,lee,el,periodico]).
o_prueba([jose,y,hector,comen,patatas,fritas,y,beben,cerveza]).
o_prueba([jose,come,patatas,fritas,pero,maria,prefiere,paella,aunque,hector,toma,cafe,e,irene,lee,una,novela]).
o_prueba([irene,canta,y,salta,mientras,jose,estudia]).
o_prueba([hector,come,patatas,fritas,y,bebe,zumo,mientras,jose,canta,y,salta,aunque,maria,lee,una,novela]).
%o_prueba([jose,que,es,agil,escala,en,el,rocodromo,por,las,tardes]).
%o_prueba([jose,que,es,muy,delicado,come,solamente,manzanas,rojas]).
%o_prueba([el,procesador,de,textos,que,es,una,herramienta,bastante,potente,sirve,para,escribir,documentos]).
o_prueba([el,procesador,de,textos,es,una,herramienta,muy,potente,que,sirve,para,escribir,documentos,pero,es,bastante,lento]).
o_prueba([el,raton,que,cazo,el,gato,era,gris]).
o_prueba([el,hombre,que,vimos,ayer,era,mi,vecino]).












