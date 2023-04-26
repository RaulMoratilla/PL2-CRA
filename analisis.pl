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
conj(mientras).

adverbio(adv(X)) --> [X], {adv(X)}.
adv(que).
adv(cuando).
adv(donde).
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

/**
 * Aplana cada uno de los N argumentos del compound X 
 * (todo lo que tenga comp(...)) y los mete en Y
 */
aplanar_args(_, _, 0).

aplanar_args(X, Y, N) :-
    compound(X),
    arg(N, X, ARGACT),
    arg(N, Y, ARGAPL),
    compound(ARGACT),
    aplanar_comp(ARGACT, ARGAPL),
    N1 is N-1,
    aplanar_args(X, Y, N1).
    
aplanar_args(X, Y, N) :-
    compound(X),
    arg(N, X, ARGACT),
    arg(N, Y, ARGACT),
    N1 is N-1,
    aplanar_args(X, Y, N1).

aplanar_args(X, X, _).

/**
 * Aplana el compound X (todo lo que tenga comp(...))
 * y lo devuelve en Y
 */
aplanar_comp(X, Y) :-
    aplanar_iter(X, Y1, NACT, NAPL),
    NACT =\= NAPL,
    aplanar_comp(Y1, Y).

aplanar_comp(X, Y) :-
    aplanar_iter(X, Y1, NACT, NAPL),
    NACT is NAPL,
    functor(X, F, N),
    functor(Y, F, N),
    aplanar_args(Y1, Y, N).

/**
 * Hace una iteracion de aplanar
 * el compound X (todo lo que tenga comp(...))
 * y lo devuelve en Y
 */
aplanar_iter(X, Y, NACT, NAPL) :-
    functor(X, F, NACT),
    args_aplanado(X, NACT, NAPL),
    functor(Y, F, NAPL),
    add_args(X, Y, 1, 1, NACT).


/**
 * Devuelve el numero de argumentos que tendra el compound aplanado
 */
args_aplanado(_, 0, 0).

args_aplanado(X, NACT, NAPL) :-
    NACT > 0,
    N1 is NACT - 1,
    compound(X),
    args_aplanado(X, N1, NAPL1),
    arg(NACT, X, ARG),
    compound(ARG),
    functor(ARG, comp, NSUM),
    NAPL is NAPL1 + NSUM.

args_aplanado(X, NACT, NAPL) :-
    NACT > 0,
    N1 is NACT - 1,
    compound(X),
    args_aplanado(X, N1, NAPL1),
    arg(NACT, X, ARG),
    \+ compound(ARG),
    NAPL is NAPL1 + 1.

args_aplanado(X, NACT, NAPL) :-
    NACT > 0,
    N1 is NACT - 1,
    \+ compound(X),
    args_aplanado(X, N1, NAPL1),
    NAPL is NAPL1 + 1.

args_aplanado(_, X, X) :- 
    X > 0.

/**
 * Anade a Y todos los compounds de X que no sean comp(...)
 * y los comp(...) los aplana y los anade a Y
 */
add_args(_, _, IX, _, NFIN) :- IX > NFIN.

add_args(X, Y, IX, IY, NFIN) :-
    IX =< NFIN,
    arg(IX, X, ARG),
    compound(ARG),
    functor(ARG, comp, NADD),
    add_args(ARG, Y, 1, IY, NADD),
    IYN is IY + NADD,
    IXN is IX + 1,
    add_args(X, Y, IXN, IYN, NFIN).

add_args(X, Y, IX, IY, NFIN) :-
    IX =< NFIN,
    arg(IX, X, ARG),
    arg(IY, Y, ARG),
    IXN is IX + 1,
    IYN is IY + 1,
    add_args(X, Y, IXN, IYN, NFIN).

buscar_subordinada(X, N, _) :-
    arg(N, X, ARG),
    compound(ARG),
    functor(ARG, or, _).

buscar_subordinada(X, N, N1) :-
    N =< N1,
    arg(N, X, ARG),
    ((compound(ARG),
    functor(ARG, F, M),
    F \= or,
    buscar_subordinada(ARG, 1, M));
    (N2 is N + 1,
    buscar_subordinada(X, N2, N1))).

ajustar_compuestas_args(_, _, N, N).

ajustar_compuestas_args(X, Y, N, M) :-
    arg(N, X, ARG),
    compound(ARG),
    ajustar_compuestas(ARG, Y1),
    arg(N, Y, Y1),
    N1 is N + 1,
    ajustar_compuestas_args(X, Y, N1, M).

ajustar_compuestas(X, X) :-
    \+ compound(X).

ajustar_compuestas(X, Y) :-
    compound(X),
    functor(X, os, N),
    buscar_subordinada(X, 1, N),
    functor(Y1, ocm, N),
    copy_compound(X, Y1, 1, 1, N),
    ajustar_compuestas_args(Y1, Y, 1, N).

ajustar_compuestas(X, Y) :-
    compound(X),
    functor(X, F, N),
    functor(Y, F, N),
    ajustar_compuestas_args(X, Y, 1, N).


% Reglas Gramaticales

oracion(X, O, Y) :- compuesta(X1, O, Y), once(aplanar_comp(X1, X)).%, ajustar_compuestas(X2, X).
oracion(X, O, Y) :- simple(X1, O, Y), once(aplanar_comp(X1, X)).%, ajustar_compuestas(X2, X).

compuesta(ocm(OCM)) --> coordinada(OCM).

simple(os(GN, GV)) --> g_nominal(GN), g_verbal(GV).
simple(os(GV)) --> g_verbal(GV).

coordinada(oc(O,CONJ,O2)) --> simple(O), conjuncion(CONJ), simple(O2).
coordinada(oc(O,CONJ,O2)) --> simple(O), conjuncion(CONJ), compuesta(O2).

subordinada(or(ADV, O)) --> adverbio(ADV), oracion(O).

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

g_verbal(gv(V, COMP)) --> verbo(V), complementos(COMP).
g_verbal(gv(V)) --> verbo(V).

g_nominal(gn(N, COMP)) --> nombre(N), complementos(COMP).
g_nominal(gn(N)) --> nombre(N).
g_nominal(gn(DET, N, COMP)) --> determinante(DET), nombre(N), complementos(COMP).
g_nominal(gn(DET, N)) --> determinante(DET), nombre(N).

g_adjetival(gadj(A)) --> adjetivo(A).
g_adjetival(gadj(GADJ,GADJ2)) --> adjetivo(GADJ), g_adjetival(GADJ2).
g_adjetival(gadj(GADJ,GN)) --> adjetivo(GADJ), g_nominal(GN).
g_adjetival(gadj(GADJ,GP)) --> adjetivo(GADJ), g_preposicional(GP).
g_adjetival(gadj(ADV,GADJ)) --> adverbio(ADV), g_adjetival(GADJ).
g_adjetival(gadj(GADJ,CONJ,GADJ2)) --> adjetivo(GADJ), conjuncion(CONJ), g_adjetival(GADJ2).

g_adverbial(gadv(ADV)) --> adverbio(ADV).
g_adverbial(gadv(GADV,GADV2)) --> adverbio(GADV), g_adverbial(GADV2).
g_adverbial(gadv(GADV,GN)) --> adverbio(GADV), g_preposicional(GN).

g_preposicional(gp(P,GN)) --> preposicion(P), g_nominal(GN).
g_preposicional(gp(P,GADJ)) --> preposicion(P), g_adjetival(GADJ).
g_preposicional(gp(P,GADV)) --> preposicion(P), g_adverbial(GADV).

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

ejecutar_pruebas(INI, INI).
ejecutar_pruebas(INI, FIN) :- 
    o_prueba(INI, O),
    oracion(X, O, []),
    draw(X),
    INI2 is INI+1,
    ejecutar_pruebas(INI2, FIN).


separar(X, oraciones(-)) :-
    \+ compound(X).

separar(X, C) :-
    compound(X),
    functor(X, F, N),
    F \= os,
    separar_args(X, C, N).

separar(X, C) :-
    compound(X),
    functor(X, os, M),
    separar_args(X, C1, M),
    functor(C1, F, N),
    N1 is N+1,
    functor(C, F, N1),
    copy_compound(C1, C, 1, 2, M),
    arg(1, C, X).

separar_args(_, oraciones(-), 0).

separar_args(X, C, N) :-
    arg(N, X, A),
    \+ compound(A),
    N1 is N-1,
    separar_args(X, C2, N1),
    concatenar_compound(oraciones(-), C2, C).

separar_args(X, C, N) :-
    arg(N, X, A),
    separar(A, C1),
    N1 is N-1,
    separar_args(X, C2, N1),
    concatenar_compound(C1, C2, C).


copy_compound(_, _, ACTX, _, MAX) :- ACTX > MAX.

copy_compound(X, Y, ACTX, ACTY, MAX) :-
    ACTX =< MAX,
    arg(ACTX, X, ARG),
    arg(ACTY, Y, ARG),
    ACTNX is ACTX + 1,
    ACTNY is ACTY + 1,
    copy_compound(X, Y, ACTNX, ACTNY, MAX).

concatenar_compound(oraciones(-), oraciones(-), oraciones(-)).

concatenar_compound(C, oraciones(-), C).

concatenar_compound(oraciones(-), C, C).

concatenar_compound(C, C1, C2) :-
    functor(C, F, N),
    functor(C1, F, N1),
    N2 is N + N1,
    functor(C2, F, N2),
    copy_compound(C1, C2, 1, 1, N1),
    N3 is N1 + 1,
    copy_compound(C, C2, 1, N3, N).