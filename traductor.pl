%:-consult("draw.pl").

%Diccionario

determinante(esp, det(X)) --> [X], {det(X,_)}.
determinante(eng, det(X)) --> [Y], {det(X,Y)}.
det(el,the).
det(la,the).
det(las,the).
det(un, a).
det(una, a).
det(mi, my).

nombre(esp, n(X)) --> [X], {n(X,_)}.
nombre(eng, n(X)) --> [Y], {n(X,Y)}.
n(maria, mary).
n(jose, joseph).
/*n(hombre).
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
n(tardes).*/

verbo(esp, v(X)) --> [X], {v(X,_)}.
verbo(eng, v(X)) --> [Y], {v(X,Y)}.
v(es, is).
/*v(ama).
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
v(era).*/

adjetivo(esp, adj(X)) --> [X], {adj(X,_)}.
adjetivo(eng, adj(X)) --> [Y], {adj(X,Y)}.
adj(moreno, brown).
adj(alta, tall).
/*adj(roja).
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
adj(delicado).*/

conjuncion(esp, conj(X)) --> [X], {conj(X,_)}.
conjuncion(eng, conj(X)) --> [Y], {conj(X,Y)}.
conj(y, and).
/*conj(e).
conj(ni).
conj(pero).
conj(aunque).*/

adverbio(esp, adv(X)) --> [X], {adv(X,_)}.
adverbio(eng, adv(X)) --> [Y], {adv(X,Y)}.
adv(cuando, when).
/*adv(que).
adv(donde).
adv(mientras).
adv(muy).
adv(bastante).
adv(ayer).
adv(solamente).*/

preposicion(esp, prep(X)) --> [X], {prep(X,_)}.
preposicion(eng, prep(X)) --> [Y], {prep(X,Y)}.
prep(para, from).
/*prep(a).
prep(de).
prep(para).
prep(en).
prep(por).*/

% Reglas gramaticales

oracion(esp, X, O, Y) :- compuesta(esp, X, O, Y).
oracion(esp, X, O, Y) :- simple(esp, X, O, Y).

oracion(eng, X, O, Y) :- compuesta(eng, X, O, Y).
oracion(eng, X, O, Y) :- simple(eng, X, O, Y).

%%%%

compuesta(esp, ocm(OCM)) --> coordinada(esp, OCM).

compuesta(eng, ocm(OCM)) --> coordinada(eng, OCM).

%%%%

simple(esp, os(GN, GV)) --> g_nominal(esp, GN), g_verbal(esp, GV).
simple(esp, os(GV)) --> g_verbal(esp, GV).

simple(eng, os(GN, GV)) --> g_nominal(eng, GN), g_verbal(eng, GV).
simple(eng, os(GV)) --> g_verbal(eng, GV).

%%%%

coordinada(esp, oc(O,CONJ,O2)) --> simple(esp, O), conjuncion(esp, CONJ), simple(esp, O2).
coordinada(esp, oc(O,CONJ,O2)) --> simple(esp, O), conjuncion(esp, CONJ), compuesta(esp, O2).

coordinada(eng, oc(O,CONJ,O2)) --> simple(eng, O), conjuncion(eng, CONJ), simple(eng, O2).
coordinada(eng, oc(O,CONJ,O2)) --> simple(eng, O), conjuncion(eng, CONJ), compuesta(eng, O2).

%%%%

subordinada(esp, or(ADV, O)) --> adverbio(esp, ADV), oracion(esp, O).

subordinada(eng, or(ADV, O)) --> adverbio(eng, ADV), oracion(eng, O).

%%%%

g_nominal(esp, gn(N)) --> nombre(esp, N).
g_nominal(esp, gn(N, N1)) --> nombre(esp, N), nombre(esp, N1).
g_nominal(esp, gn(D,N)) --> determinante(esp, D), nombre(esp, N).
g_nominal(esp, gn(N,A)) --> nombre(esp, N), g_adjetival(esp, A).
g_nominal(esp, gn(D,N,A)) --> determinante(esp, D), nombre(esp, N), g_adjetival(esp, A).
g_nominal(esp, gn(N,GP)) --> nombre(esp, N), g_preposicional(esp, GP).
g_nominal(esp, gn(D,N,GP)) --> determinante(esp, D), nombre(esp, N), g_preposicional(esp, GP).
g_nominal(esp, gn(N, ADV, A)) --> nombre(esp, N), adverbio(esp, ADV), g_adjetival(esp, A).
g_nominal(esp, gn(ADV, N, A)) --> adverbio(esp, ADV), nombre(esp, N),  g_adjetival(esp, A).
g_nominal(esp, gn(N, ADV, GN)) --> nombre(esp, N), adverbio(esp, ADV), g_nominal(esp, GN).
g_nominal(esp, gn(N, OR)) --> nombre(esp, N), subordinada(esp, OR).
g_nominal(esp, gn(DET, N, OR)) --> determinante(esp, DET), nombre(esp, N), subordinada(esp, OR).
g_nominal(esp, gn(DET, N, GA, OR)) --> determinante(esp, DET), nombre(esp, N), g_adjetival(esp, GA), subordinada(esp, OR).
g_nominal(esp, gn(GN,CONJ,GN2)) --> nombre(esp, GN), conjuncion(esp, CONJ), nombre(esp, GN2).

g_nominal(eng, gn(N)) --> nombre(eng, N).
g_nominal(eng, gn(N, N1)) --> nombre(eng, N), nombre(eng, N1).
g_nominal(eng, gn(D,N)) --> determinante(eng, D), nombre(eng, N).
g_nominal(eng, gn(N,A)) --> nombre(eng, N), g_adjetival(eng, A).
g_nominal(eng, gn(D,N,A)) --> determinante(eng, D), nombre(eng, N), g_adjetival(eng, A).
g_nominal(eng, gn(N,GP)) --> nombre(eng, N), g_preposicional(eng, GP).
g_nominal(eng, gn(D,N,GP)) --> determinante(eng, D), nombre(eng, N), g_preposicional(eng, GP).
g_nominal(eng, gn(N, ADV, A)) --> nombre(eng, N), adverbio(eng, ADV), g_adjetival(eng, A).
g_nominal(eng, gn(ADV, N, A)) --> adverbio(eng, ADV), nombre(eng, N),  g_adjetival(eng, A).
g_nominal(eng, gn(N, ADV, eng, GN)) --> nombre(eng, N), adverbio(eng, ADV), g_nominal(eng, GN).
g_nominal(eng, gn(N, OR)) --> nombre(eng, N), subordinada(eng, OR).
g_nominal(eng, gn(DET, N, OR)) --> determinante(eng, DET), nombre(eng, N), subordinada(eng, OR).
g_nominal(eng, gn(DET, N, GA, OR)) --> determinante(eng, DET), nombre(eng, N), g_adjetival(eng, GA), subordinada(eng, OR).
g_nominal(eng, gn(GN,CONJ,GN2)) --> nombre(eng, GN), conjuncion(eng, CONJ), nombre(eng, GN2).

%%%%

g_verbal(esp, gv(V)) --> verbo(esp, V).
g_verbal(esp, gv(V,GN)) --> verbo(esp, V), g_nominal(esp, GN).
g_verbal(esp, gv(V,A)) --> verbo(esp, V), g_adjetival(esp, A).
g_verbal(esp, gv(V,OR)) --> verbo(esp, V), subordinada(esp, OR).
g_verbal(esp, gv(V,GN,GP)) --> verbo(esp, V), g_nominal(esp, GN), g_preposicional(esp, GP).
g_verbal(esp, gv(V,GN,GP)) --> verbo(esp, V), g_preposicional(esp, GP), g_nominal(esp, GN), g_preposicional(esp, GP).
g_verbal(esp, gv(V,GP,GP1)) --> verbo(esp, V), g_preposicional(esp, GP), g_preposicional(esp, GP1).
g_verbal(esp, gv(V,GP)) --> verbo(esp, V), g_preposicional(esp, GP).
g_verbal(esp, gv(V,GADV)) --> verbo(esp, V), g_adverbial(esp, GADV).

g_verbal(eng, gv(V)) --> verbo(eng, V).
g_verbal(eng, gv(V,GN)) --> verbo(eng, V), g_nominal(eng, GN).
g_verbal(eng, gv(V,A)) --> verbo(eng, V), g_adjetival(eng, A).
g_verbal(eng, gv(V,OR)) --> verbo(eng, V), subordinada(eng, OR).
g_verbal(eng, gv(V,GN,GP)) --> verbo(eng, V), g_nominal(eng, GN), g_preposicional(eng, GP).
g_verbal(eng, gv(V,GN,GP)) --> verbo(eng, V), g_preposicional(eng, GP), g_nominal(eng, GN), g_preposicional(eng, GP).
g_verbal(eng, gv(V,GP,GP1)) --> verbo(eng, V), g_preposicional(eng, GP), g_preposicional(eng, GP1).
g_verbal(eng, gv(V,GP)) --> verbo(eng, V), g_preposicional(eng, GP).
g_verbal(eng, gv(V,GADV)) --> verbo(eng, V), g_adverbial(eng, GADV).

%%%%

g_adjetival(esp, gadj(A)) --> adjetivo(esp, A).
g_adjetival(esp, gadj(GADJ,GADJ2)) --> adjetivo(esp, GADJ), adjetivo(esp, GADJ2).
g_adjetival(esp, gadj(GADJ,GN)) --> adjetivo(esp, GADJ), g_nominal(esp, GN).
g_adjetival(esp, gadj(GADJ,GP)) --> adjetivo(esp, GADJ), g_preposicional(esp, GP).
g_adjetival(esp, gadj(ADV,GADJ)) --> adverbio(esp, ADV), adjetivo(esp, GADJ).
g_adjetival(esp, gadj(GADJ,CONJ,GADJ2)) --> adjetivo(esp, GADJ), conjuncion(esp, CONJ), adjetivo(esp, GADJ2).

g_adjetival(eng, gadj(A)) --> adjetivo(eng, A).
g_adjetival(eng, gadj(GADJ,GADJ2)) --> adjetivo(eng, GADJ), adjetivo(eng, GADJ2).
g_adjetival(eng, gadj(GADJ,GN)) --> adjetivo(eng, GADJ), g_nominal(eng, GN).
g_adjetival(eng, gadj(GADJ,GP)) --> adjetivo(eng, GADJ), g_preposicional(eng, GP).
g_adjetival(eng, gadj(ADV,GADJ)) --> adverbio(eng, ADV), adjetivo(eng, GADJ).
g_adjetival(eng, gadj(GADJ,CONJ,GADJ2)) --> adjetivo(eng, GADJ), conjuncion(eng, CONJ), adjetivo(eng, GADJ2).

%%%%

g_adverbial(esp, gadv(ADV)) --> adverbio(esp, ADV).
g_adverbial(esp, gadv(GADV,GADV2)) --> adverbio(esp, GADV), adverbio(esp, GADV2).
g_adverbial(esp, gadv(GADV,GN)) --> adverbio(esp, GADV), g_preposicional(esp, GN).

g_adverbial(eng, gadv(ADV)) --> adverbio(eng, ADV).
g_adverbial(eng, gadv(GADV,GADV2)) --> adverbio(eng, GADV), adverbio(eng, GADV2).
g_adverbial(eng, gadv(GADV,GN)) --> adverbio(eng, GADV), g_preposicional(eng, GN).

%%%%

g_preposicional(esp, gp(P,GN)) --> preposicion(esp, P), g_nominal(esp, GN).
g_preposicional(esp, gp(P,GADJ)) --> preposicion(esp, P), g_adjetival(esp, GADJ).
g_preposicional(esp, gp(P,GADV)) --> preposicion(esp, P), g_adverbial(esp, GADV).

g_preposicional(eng, gp(P,GN)) --> preposicion(eng, P), g_nominal(eng, GN).
g_preposicional(eng, gp(P,GADJ)) --> preposicion(eng, P), g_adjetival(eng, GADJ).
g_preposicional(eng, gp(P,GADV)) --> preposicion(eng, P), g_adverbial(eng, GADV).

%%%%

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