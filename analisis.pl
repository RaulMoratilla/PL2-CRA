%Diccionario

determinante(det(X)) --> [X], {det(X)}.
det(el).
det(la).
det(un).
det(una).

nombre(n(X)) --> [X], {n(X)}.
n(hombre).
n(mujer).
n(juan).
n(maria).
n(manzana).
n(gato).
n(raton).
n(alumno).
n(universidad).

verbo(v(X)) --> [X], {v(X)}.
v(ama).
v(come).
v(estudia).
v(bebe).

adjetivo(adj(X)) --> [X], {adj(X)}.
adj(roja).
adj(negro).
adj(grande).
adj(gris).
adj(pequeno).

conjuncion(conj(X)) --> [X], {conj(X)}.
conj(y).
conj(e).
conj(ni).

adverbio(adv(X)) --> [X], {adv(X)}.
adv(que).
adv(cuando).
adv(donde).

% Reglas gramaticales
compuesta(ocm(OCM)) --> oracion(OCM).
compuesta(ocm(OCM)) --> coordinada(OCM).
compuesta(ocm(OCM)) --> subordinada(OCM).

oracion(o(GV)) --> g_verbal(GV).
oracion(o(GN, GV)) --> g_nominal(GN), g_verbal(GV).

coordinada(oc(O,CONJ,O2)) --> compuesta(O), conjuncion(CONJ), compuesta(O2).

subordinada(or(O,ADV,O2)) --> oracion(O), adverbio(ADV), oracion(O2).

g_nominal(gn(N)) --> nombre(N).
g_nominal(gn(D,N)) --> determinante(D), nombre(N).
g_nominal(gn(N,A)) --> nombre(N), adjetivo(A).
g_verbal(gv(V)) --> verbo(V).
g_verbal(gn(V,GN)) --> verbo(V), g_nominal(GN).
g_verbal(gv(V,A)) --> verbo(V), adjetivo(A).


