% APLANAR

/**
 * Aplana el argumento N del compound
 * de X (todo lo que tenga comp(...)) y los mete en Y
 */
aplanar_args(_, _, 0).

% Caso de que haya mas compounds dentro
aplanar_args(X, Y, N) :-
    compound(X),
    arg(N, X, ARGACT),
    arg(N, Y, ARGAPL),
    compound(ARGACT),
    aplanar_comp(ARGACT, ARGAPL),
    N1 is N-1,
    aplanar_args(X, Y, N1).
    
% Si no hay mas compounds dentro
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

% Si se puede seguir aplanando ese nivel continua (hasta que no cambie nada)
aplanar_comp(X, Y) :-
    aplanar_iter(X, Y1, NACT, NAPL),
    NACT =\= NAPL,
    aplanar_comp(Y1, Y).

% Si no se puede seguir aplanando ese nivel aplana los inferiores
aplanar_comp(X, Y) :-
    aplanar_iter(X, Y1, NACT, NAPL),
    NACT is NAPL,
    functor(X, F, N),
    functor(Y, F, N),
    aplanar_args(Y1, Y, N).

/**
 * Hace una iteracion de aplanar el compound X (todo lo que tenga comp(...))
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

% Caso de que haya un comp(...) dentro
args_aplanado(X, NACT, NAPL) :-
    NACT > 0,
    N1 is NACT - 1,
    compound(X),
    args_aplanado(X, N1, NAPL1),
    arg(NACT, X, ARG),
    compound(ARG),
    functor(ARG, comp, NSUM),
    NAPL is NAPL1 + NSUM.

% Caso de que no haya mas comp(...) dentro pero si otros compounds
args_aplanado(X, NACT, NAPL) :-
    NACT > 0,
    N1 is NACT - 1,
    compound(X),
    args_aplanado(X, N1, NAPL1),
    arg(NACT, X, ARG),
    \+ compound(ARG),
    NAPL is NAPL1 + 1.

% Caso de que no haya mas comp(...) ni otros compounds
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
 * Los comp(...) los aplana y los anade a Y
 */
add_args(_, _, IX, _, NFIN) :- IX > NFIN.

% Caso de IX sea un comp(...)
add_args(X, Y, IX, IY, NFIN) :-
    IX =< NFIN,
    arg(IX, X, ARG),
    compound(ARG),
    functor(ARG, comp, NADD),
    add_args(ARG, Y, 1, IY, NADD),
    IYN is IY + NADD,
    IXN is IX + 1,
    add_args(X, Y, IXN, IYN, NFIN).

% Caso de que IX no sea un comp(...)
add_args(X, Y, IX, IY, NFIN) :-
    IX =< NFIN,
    arg(IX, X, ARG),
    arg(IY, Y, ARG),
    IXN is IX + 1,
    IYN is IY + 1,
    add_args(X, Y, IXN, IYN, NFIN).

% AJUSTAR COMPUESTAS

/**
 * Devuelve True si encuentra una oracion subordinada en el arbol X
 * Accede al arbol de forma recursiva
 */

% Si encuentra la subordinada devuelve True
buscar_subordinada(X, N, _) :-
    arg(N, X, ARG),
    compound(ARG),
    functor(ARG, or, _).

% Si es coordinada no hay que buscar mas
buscar_subordinada(X, N, N1) :-
    N =< N1,
    arg(N, X, ARG),
    ((compound(ARG),
    functor(ARG, F, M),
    F \= or,
    F \= oc,
    buscar_subordinada(ARG, 1, M));
    (N2 is N + 1,
    buscar_subordinada(X, N2, N1))).

/**
 * Ayuda a iterar el arbol sintactico buscando si hay, y en 
 * su caso donde se encuentra una oracion subordinada
 */
ajustar_compuestas_args(_, _, N, M) :- N > M.

% Si N es compound itera sus hijos y sus hermanos
ajustar_compuestas_args(X, Y, N, M) :-
    N =< M,
    arg(N, X, ARG),
    compound(ARG),
    ajustar_compuestas(ARG, Y1),
    arg(N, Y, Y1),
    N1 is N + 1,
    ajustar_compuestas_args(X, Y, N1, M).

% Si N no es compound itera sus hermanos
ajustar_compuestas_args(X, Y, N, M) :-
    N =< M,
    arg(N, X, ARG),
    \+ compound(ARG),
    arg(N, Y, ARG),
    N1 is N + 1,
    ajustar_compuestas_args(X, Y, N1, M).

/**
 * Busca oraciones subordinadas dentro de oraciones simples,
 * y en caso de encontrarlas transforma las simples en compuestas
 */
ajustar_compuestas(X, X) :-
    \+ compound(X).

% Si es una oracion simple y tiene una subordinada la transforma en compuesta
ajustar_compuestas(X, Y) :-
    compound(X),
    functor(X, os, N),
    buscar_subordinada(X, 1, N),
    functor(Y1, ocm, N),
    copy_compound(X, Y1, 1, 1, N),
    functor(Y, ocm, N),
    ajustar_compuestas_args(Y1, Y, 1, N).

% Si no es una oracion simple sigue buscando
ajustar_compuestas(X, Y) :-
    compound(X),
    functor(X, F, N),
    functor(Y, F, N),
    ajustar_compuestas_args(X, Y, 1, N).

% SEPARAR ORACIONES EN COMPOUNDS

/**
 * Separa las oraciones que se encuentren en X en compounds. Es posible que se 
 * encuentren guiones (que no significan nada) por la forma en la que estan
 * construidos los compounds en prolog y la imposibilidad de crear uno vacio
 */
separar(X, oraciones(-)) :-
    \+ compound(X).

% Caso de que haya subordinada dentro
separar(X, C) :-
    compound(X),
    functor(X, ocm, M),
    buscar_subordinada(X, 1, M),
    separar_args(X, C1, M),
    quitar_subordinada(X, X1, 1, M),
    functor(C1, F, N),
    N1 is N+1,
    functor(C, F, N1),
    copy_compound(C1, C, 1, 2, N),
    arg(1, C, X1).

% Caso de que no haya subordinada dentro
separar(X, C) :-
    compound(X),
    functor(X, F, N),
    F \= os,
    separar_args(X, C, N).

% Caso de que sea una oracion simple
separar(X, C) :-
    compound(X),
    functor(X, os, M),
    separar_args(X, C1, M),
    functor(C1, F, N),
    N1 is N+1,
    functor(C, F, N1),
    copy_compound(C1, C, 1, 2, M),
    arg(1, C, X).

/**
 * Ayuda a iterar el arbol sintactico iterando los nodos hermanos
 * y los nodos hijos
 */
separar_args(_, oraciones(-), 0).

% Si N no es compound itera sus hermanos
separar_args(X, C, N) :-
    arg(N, X, A),
    \+ compound(A),
    N1 is N-1,
    separar_args(X, C2, N1),
    concatenar_compound(oraciones(-), C2, C).

% Si N es compound se separa y se itera sus hermanos
separar_args(X, C, N) :-
    arg(N, X, A),
    separar(A, C1),
    N1 is N-1,
    separar_args(X, C2, N1),
    concatenar_compound(C1, C2, C).

/**
 * Elimina las oraciones subordinadas de X
 * obteniendo en X1 el resto de la oracion
 */

% Si esa es la oracion subordinada se elimina y se devuelve su nivel consigo mismo
quitar_subordinada(X, X1, N, M) :-
    N =< M,
    functor(X, F, M),
    arg(N, X, A),
    compound(A),
    functor(A, or, _),
    TAM is M-1,
    functor(X1, F, TAM),
    copy_compound(X, X1, 1, 1, N-1),
    copy_compound(X, X1, N+1, N, M).

% Si no es la oracion subordinada se sigue buscando. Cuando se encuentra se anaden los compounds ncesarios
quitar_subordinada(X, X1, N, M) :-
    N =< M,
    functor(X, F, M),
    arg(N, X, A),
    compound(A),
    functor(A, F1, TAM),
    F1 \= os,
    N1 is N+1,
    ((quitar_subordinada(A, A1, 1, TAM),
    functor(X1, F, M),
    arg(N, X1, A1),
    copy_compound(X, X1, N1, N1, M),
    N2 is N-1,
    copy_compound(X, X1, 1, 1, N2));
    quitar_subordinada(X, X1, N1, M)).

% SUJETO

/**
 * Busca el sujeto de una oracion. Si lo encuentra devuelve true
 */

% Si encuentra un gn a la izquierda es el sujeto y devuelve true
buscar_sujeto(O, O) :-
    functor(O, gn, _).

% Si no encuentra un gn a la izquierda busca en el primer argumento
buscar_sujeto(O, SUJ) :-
    functor(O, F, _),
    F \= gv,
    arg(1, O, A),
    buscar_sujeto(A, SUJ).

/**
 * Llama a buscar sujeto para que se pueda iterar de forma recursiva
 */
get_sujeto(O, SUJ) :-
    oracion(X, O, []),
    buscar_sujeto(X, SUJ).

/**
 * En caso de que la oracion tenga sujeto, se anade al inicio de la oracion
 * siempre que no este anadido ya
 */
poner_sujeto([], [], _).

% Si el sujeto ya esta anadido no se anade
poner_sujeto([O|RO], [O|ROS], _) :-
    get_sujeto(O, SUJ1),
    get_oracion(SUJ1, SUJL),
    poner_sujeto(RO, ROS, SUJL).

% Si el sujeto no esta anadido se anade
poner_sujeto([O|RO], [OS|ROS], SUJ) :-
    append(SUJ, O, OS),
    poner_sujeto(RO, ROS, SUJ).

% DAR FORMATO DE LISTA A LAS ORACIONES Y PONER EL SUJETO

/**
 * Formatea las oraciones para que se puedan imprimir de forma correcta (en listas) y ponerles el sujeto
 */
formatear_oraciones(X, L) :-
    functor(X, _, N),
    get_oraciones(X, L1, N),
    poner_sujeto(L1, L, []).

/**
 * Obtiene del compound oraciones(...) cada una de las oraciones en listas
 */
get_oraciones(_, [], 0).

% Caso de que no sea un guion
get_oraciones(X, L, OACT) :-
    OACT > 0,
    arg(OACT, X, A),
    A \= -,
    OACT2 is OACT-1,
    get_oraciones(X, L1, OACT2),
    get_oracion(A, O),
    append(L1, [O], L).

% Caso de que sea un guion
get_oraciones(X, L, OACT) :-
    OACT > 0,
    arg(OACT, X, A),
    A = -,
    OACT2 is OACT-1,
    get_oraciones(X, L, OACT2).

/**
 * Obtiene del compound oracion(de cualquier tipo)(...) la oracion en una lista
 */
get_oracion(X, L) :-
    compound(X),
    functor(X, _, N),
    get_oracion(X, L, N).

get_oracion(_, [], 0).

% Caso de que N sea un compound
get_oracion(X, L, N) :-
    N > 0,
    arg(N, X, A),
    compound(A),
    N2 is N-1,
    get_oracion(X, L2, N2),
    get_oracion(A, L3),
    append(L2, L3, L).

% Caso de que N no sea un compound
get_oracion(X, L, N) :-
    N > 0,
    arg(N, X, A),
    \+ compound(A),
    N2 is N-1,
    get_oracion(X, L2, N2),
    append(L2, [A], L).

% AUXILIARES

/**
 * Copia los argumentos de las posiciones ACTX a MAX de X a las posiciones desde ACTY de Y
 */
copy_compound(_, _, ACTX, _, MAX) :- ACTX > MAX.

copy_compound(X, Y, ACTX, ACTY, MAX) :-
    ACTX =< MAX,
    arg(ACTX, X, ARG),
    arg(ACTY, Y, ARG),
    ACTNX is ACTX + 1,
    ACTNY is ACTY + 1,
    copy_compound(X, Y, ACTNX, ACTNY, MAX).

/**
 * Concatena dos compounds
 */
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