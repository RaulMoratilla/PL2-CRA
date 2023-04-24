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
n(hombre, man).
n(mujer, woman).
n(manzana, apple).
n(manzanas, apples).
n(gato, cat).
n(raton, mouse).
n(alumno, student).
n(universidad, university).
n(juan, john).
n(maria, mary).
n(jose, joseph).
n(hector, hector).
n(irene, irene).
n(filosofia, philosophy).
n(derecho, law).
n(cafe, coffee).
n(mesa, table).
n(periodico, newspaper).
n(patatas, potatoes).
n(cerveza, beer).
n(paella, paella).
n(novela, novel).
n(zumo, juice).
n(procesador, processor).
n(textos, texts).
n(herramienta, tool).
n(documentos, documents).
n(vecino, neighbor).
n(escribir, writing).
n(rocodromo, climbing-wall).
n(tardes, afternoon).

verbo(esp, v(X)) --> [X], {v(X,_)}.
verbo(eng, v(X)) --> [Y], {v(X,Y)}.
v(ama, love).
v(come, eats).
v(estudia, studies).
v(bebe, drinks).
v(es, is).
v(toma, takes).
v(recoge, collect).
v(lee, reeds).
v(comen, eat).
v(beben, drink).
v(prefiere, prefers).
v(canta, sings).
v(salta, jumps).
v(escala, climbs).
v(sirve, serves).
v(cazo, hunted).
v(vimos, saw).
v(era, was).

adjetivo(esp, adj(X)) --> [X], {adj(X,_)}.
adjetivo(eng, adj(X)) --> [Y], {adj(X,Y)}.
adj(roja, red).
adj(rojas, red).
adj(negro, black).
adj(grande, big).
adj(gris, gray).
adj(pequeno, little).
adj(alta, tall).
adj(moreno, brown).
adj(fritas, fried).
adj(potente, powerful).
adj(lento, slow).
adj(agil, agile).
adj(delicado, delicate).

conjuncion(esp, conj(X)) --> [X], {conj(X,_)}.
conjuncion(eng, conj(X)) --> [Y], {conj(X,Y)}.
conj(y, and).
conj(e, and).
conj(ni, neither).
conj(pero, but).
conj(aunque, although).

adverbio(esp, adv(X)) --> [X], {adv(X,_)}.
adverbio(eng, adv(X)) --> [Y], {adv(X,Y)}.
adv(cuando, when).
adv(que, that).
adv(donde, where).
adv(mientras, while).
adv(muy, very).
adv(bastante, quite).
adv(ayer, yesterday).
adv(solamente, only).

preposicion(esp, prep(X)) --> [X], {prep(X,_)}.
preposicion(eng, prep(X)) --> [Y], {prep(X,Y)}.
prep(a, to).
prep(de, of).
prep(para, to).
prep(en, in).
prep(por, by).