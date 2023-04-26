% Reglas Gramaticales

% Oraciones
oracion(X, O, Y) :- compuesta(X1, O, Y), once(aplanar_comp(X1, X2)), ajustar_compuestas(X2, X).
oracion(X, O, Y) :- simple(X1, O, Y), once(aplanar_comp(X1, X2)), ajustar_compuestas(X2, X).

% Compuesta
compuesta(ocm(OCM)) --> coordinada(OCM).

% Simple
simple(os(GN, GV)) --> g_nominal(GN), g_verbal(GV).
simple(os(GV)) --> g_verbal(GV).

% Coordinada
coordinada(oc(O,CONJ,O2)) --> simple(O), conjuncion(CONJ), simple(O2).
coordinada(oc(O,CONJ,O2)) --> simple(O), conjuncion(CONJ), compuesta(O2).

%Subordinada
subordinada(or(ADV, O)) --> adverbio(ADV), oracion(O).

% Complementos
complementos(comp(SUB)) --> subordinada(SUB).
complementos(comp(GADJ)) --> g_adjetival(GADJ).
complementos(comp(GADV)) --> g_adverbial(GADV).
complementos(comp(GPREP)) --> g_preposicional(GPREP).
complementos(comp(GN)) --> g_nominal(GN).
complementos(comp(CONJ, GN)) --> conjuncion(CONJ), g_nominal(GN).
complementos(comp(SUB, COMP)) --> subordinada(SUB), complementos(COMP).
complementos(comp(GADJ, COMP)) --> g_adjetival(GADJ), complementos(COMP).
complementos(comp(GADV, COMP)) --> g_adverbial(GADV), complementos(COMP).
complementos(comp(GPREP, COMP)) --> g_preposicional(GPREP), complementos(COMP).
complementos(comp(GN, COMP)) --> g_nominal(GN), complementos(COMP).
complementos(comp(CONJ, GN, COMP)) --> conjuncion(CONJ), g_nominal(GN), complementos(COMP).

% Grupos verbales
g_verbal(gv(V, COMP)) --> verbo(V), complementos(COMP).
g_verbal(gv(V)) --> verbo(V).

% Grupos nominales
g_nominal(gn(N, COMP)) --> nombre(N), complementos(COMP).
g_nominal(gn(N)) --> nombre(N).
g_nominal(gn(DET, N, COMP)) --> determinante(DET), nombre(N), complementos(COMP).
g_nominal(gn(DET, N)) --> determinante(DET), nombre(N).

% Grupos adjetivales
g_adjetival(gadj(A)) --> adjetivo(A).
g_adjetival(gadj(GADJ,GADJ2)) --> adjetivo(GADJ), g_adjetival(GADJ2).
g_adjetival(gadj(GADJ,GN)) --> adjetivo(GADJ), g_nominal(GN).
g_adjetival(gadj(GADJ,GP)) --> adjetivo(GADJ), g_preposicional(GP).
g_adjetival(gadj(ADV,GADJ)) --> adverbio(ADV), g_adjetival(GADJ).
g_adjetival(gadj(GADJ,CONJ,GADJ2)) --> adjetivo(GADJ), conjuncion(CONJ), g_adjetival(GADJ2).

% Grupos adverbiales
g_adverbial(gadv(ADV)) --> adverbio(ADV).
g_adverbial(gadv(GADV,GADV2)) --> adverbio(GADV), g_adverbial(GADV2).
g_adverbial(gadv(GADV,GN)) --> adverbio(GADV), g_preposicional(GN).

% Grupos preposicionales
g_preposicional(gp(P,GN)) --> preposicion(P), g_nominal(GN).
g_preposicional(gp(P,GADJ)) --> preposicion(P), g_adjetival(GADJ).
g_preposicional(gp(P,GADV)) --> preposicion(P), g_adverbial(GADV).

% Oraciones en espanol
o_prueba(1, [jose,es,moreno,y,maria,es,alta]).
o_prueba(2, [jose,estudia,filosofia,pero,maria,estudia,derecho]).
o_prueba(3, [maria,toma,un,cafe,mientras,jose,recoge,la,mesa]).
o_prueba(4, [jose,toma,cafe,y,lee,el,periodico]).
o_prueba(5, [jose,y,hector,comen,patatas,fritas,y,beben,cerveza]).
o_prueba(6, [jose,come,patatas,fritas,pero,maria,prefiere,paella,aunque,hector,toma,cafe,e,irene,lee,una,novela]).
o_prueba(7, [irene,canta,y,salta,mientras,jose,estudia]).
o_prueba(8, [hector,come,patatas,fritas,y,bebe,zumo,mientras,jose,canta,y,salta,aunque,maria,lee,una,novela]).
o_prueba(9, [jose,que,es,agil,escala,en,el,rocodromo,por,las,tardes]).
o_prueba(10, [jose,que,es,muy,delicado,come,solamente,manzanas,rojas]).
o_prueba(11, [el,procesador,de,textos,que,es,una,herramienta,bastante,potente,sirve,para,escribir,documentos]).
o_prueba(12, [el,procesador,de,textos,es,una,herramienta,muy,potente,que,sirve,para,escribir,documentos,pero,es,bastante,lento]).
o_prueba(13, [el,raton,que,cazo,el,gato,era,gris]).
o_prueba(14, [el,hombre,que,vimos,ayer,era,mi,vecino]).