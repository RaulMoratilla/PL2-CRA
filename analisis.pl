:-consult("draw.pl").

%Diccionario

determinante(det(X)) --> [X], {det(X)}.
det(el).
det(la).
det(las).
det(un).
det(una).
det(mi).

nombre(n(X)) --> [X], {n(X)}.
%nombre(n(X)) --> nombre_propio(n(X)).
n(hombre).
n(mujer).
n(manzana).
n(manzanas).
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
n(vecino).
n(escribir).
n(rocodromo).
n(tardes).

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
adj(rojas).
adj(negro).
adj(grande).
adj(gris).
adj(pequeno).
adj(alta).
adj(moreno).
adj(fritas).
adj(potente).
adj(lento).
adj(agil).
adj(delicado).

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
adv(solamente).

preposicion(prep(X)) --> [X], {prep(X)}.
prep(a).
prep(de).
prep(para).
prep(en).
prep(por).

% Reglas gramaticales

oracion(X, O, Y) :- compuesta(X, O, Y).
oracion(X, O, Y) :- simple(X, O, Y).

compuesta(ocm(OCM)) --> coordinada(OCM).

simple(os(GN, GV)) --> g_nominal(GN), g_verbal(GV).
simple(os(GV)) --> g_verbal(GV).

coordinada(oc(O,CONJ,O2)) --> simple(O), conjuncion(CONJ), simple(O2).
coordinada(oc(O,CONJ,O2)) --> simple(O), conjuncion(CONJ), compuesta(O2).

subordinada(or(ADV, O)) --> adverbio(ADV), oracion(O).

g_nominal(gn(N)) --> nombre(N).
g_nominal(gn(N, N1)) --> nombre(N), nombre(N1).
g_nominal(gn(D,N)) --> determinante(D), nombre(N).
g_nominal(gn(N,A)) --> nombre(N), g_adjetival(A).
g_nominal(gn(D,N,A)) --> determinante(D), nombre(N), g_adjetival(A).
g_nominal(gn(N,GP)) --> nombre(N), g_preposicional(GP).
g_nominal(gn(D,N,GP)) --> determinante(D), nombre(N), g_preposicional(GP).
g_nominal(gn(N, ADV, A)) --> nombre(N), adverbio(ADV), g_adjetival(A).
g_nominal(gn(ADV, N, A)) --> adverbio(ADV), nombre(N),  g_adjetival(A).
g_nominal(gn(N, ADV, GN)) --> nombre(N), adverbio(ADV), g_nominal(GN).
g_nominal(gn(N, OR)) --> nombre(N), subordinada(OR).
g_nominal(gn(DET, N, OR)) --> determinante(DET), nombre(N), subordinada(OR).
g_nominal(gn(DET, N, GA, OR)) --> determinante(DET), nombre(N), g_adjetival(GA), subordinada(OR).
g_nominal(gn(GN,CONJ,GN2)) --> nombre(GN), conjuncion(CONJ), nombre(GN2).

g_verbal(gv(V)) --> verbo(V).
g_verbal(gv(V,GN)) --> verbo(V), g_nominal(GN).
g_verbal(gv(V,A)) --> verbo(V), g_adjetival(A).
g_verbal(gv(V,OR)) --> verbo(V), subordinada(OR).
g_verbal(gv(V,GN,GP)) --> verbo(V), g_nominal(GN), g_preposicional(GP).
g_verbal(gv(V,GN,GP)) --> verbo(V), g_preposicional(GP), g_nominal(GN), g_preposicional(GP).
g_verbal(gv(V,GP,GP1)) --> verbo(V), g_preposicional(GP), g_preposicional(GP1).
g_verbal(gv(V,GP)) --> verbo(V), g_preposicional(GP).
g_verbal(gv(V,GADV)) --> verbo(V), g_adverbial(GADV).

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
o_prueba([jose,que,es,agil,escala,en,el,rocodromo,por,las,tardes]).
o_prueba([jose,que,es,muy,delicado,come,solamente,manzanas,rojas]).
o_prueba([el,procesador,de,textos,que,es,una,herramienta,bastante,potente,sirve,para,escribir,documentos]).
o_prueba([el,procesador,de,textos,es,una,herramienta,muy,potente,que,sirve,para,escribir,documentos,pero,es,bastante,lento]).
o_prueba([el,raton,que,cazo,el,gato,era,gris]).
o_prueba([el,hombre,que,vimos,ayer,era,mi,vecino]).