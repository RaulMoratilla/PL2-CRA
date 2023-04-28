% Reglas Gramaticales

% Oraciones
oracion(X, O, Y) :- compuesta(X1, O, Y), once(aplanar_comp(X1, X2)), ajustar_compuestas(X2, X).
oracion(X, O, Y) :- simple(X1, O, Y), once(aplanar_comp(X1, X2)), ajustar_compuestas(X2, X).
oracion(X, O, Y) :- compuesta(X, O, Y), once(aplanar_comp(X1, X2)), ajustar_compuestas(X2, X).
oracion(X, O, Y) :- simple(X, O, Y), once(aplanar_comp(X1, X2)), ajustar_compuestas(X2, X).

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