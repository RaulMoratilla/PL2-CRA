% Diccionario ESP - ENG

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