
:-consult("diccionarioTRA.pl").

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

%%%

complementos(esp, comp(SUB)) --> subordinada(esp, SUB).
complementos(esp, comp(GADJ)) --> g_adjetival(esp, GADJ).
complementos(esp, comp(GADV)) --> g_adverbial(esp, GADV).
complementos(esp, comp(GPREP)) --> g_preposicional(esp, GPREP).
complementos(esp, comp(GN)) --> g_nominal(esp, GN).
complementos(esp, comp(CONJ, GN)) --> conjuncion(esp, CONJ), g_nominal(esp, GN).
complementos(esp, comp(SUB, COMP)) --> subordinada(esp, SUB), complementos(esp, COMP).
complementos(esp, comp(GADJ, COMP)) --> g_adjetival(esp, GADJ), complementos(esp, COMP).
complementos(esp, comp(GADV, COMP)) --> g_adverbial(esp, GADV), complementos(esp, COMP).
complementos(esp, comp(GPREP, COMP)) --> g_preposicional(esp, GPREP), complementos(esp, COMP).
complementos(esp, comp(GN, COMP)) --> g_nominal(esp, GN), complementos(esp, COMP).
complementos(esp, comp(CONJ, GN, COMP)) --> conjuncion(esp, CONJ), g_nominal(esp, GN), complementos(esp, COMP).

complementos(eng, comp(SUB)) --> subordinada(eng, SUB).
complementos(eng, comp(GADJ)) --> g_adjetival(eng, GADJ).
complementos(eng, comp(GADV)) --> g_adverbial(eng, GADV).
complementos(eng, comp(GPREP)) --> g_preposicional(eng, GPREP).
complementos(eng, comp(GN)) --> g_nominal(eng, GN).
complementos(eng, comp(CONJ, GN)) --> conjuncion(eng, CONJ), g_nominal(eng, GN).
complementos(eng, comp(SUB, COMP)) --> subordinada(eng, SUB), complementos(eng, COMP).
complementos(eng, comp(GADJ, COMP)) --> g_adjetival(eng, GADJ), complementos(eng, COMP).
complementos(eng, comp(GADV, COMP)) --> g_adverbial(eng, GADV), complementos(eng, COMP).
complementos(eng, comp(GPREP, COMP)) --> g_preposicional(eng, GPREP), complementos(eng, COMP).
complementos(eng, comp(GN, COMP)) --> g_nominal(eng, GN), complementos(eng, COMP).
complementos(eng, comp(CONJ, GN, COMP)) --> conjuncion(eng, CONJ), g_nominal(eng, GN), complementos(eng, COMP).

%%%%

g_verbal(esp, gv(V, COMP)) --> verbo(esp, V), complementos(esp, COMP).
g_verbal(esp, gv(V)) --> verbo(esp, V).

g_verbal(eng, gv(V, COMP)) --> verbo(eng, V), complementos(eng, COMP).
g_verbal(eng, gv(V)) --> verbo(eng, V).

%%%%

g_nominal(esp, gn(N, COMP)) --> nombre(esp, N), complementos(esp, COMP).
g_nominal(esp, gn(N)) --> nombre(esp, N).
g_nominal(esp, gn(DET, N, COMP)) --> determinante(esp, DET), nombre(esp, N), complementos(esp, COMP).
g_nominal(esp, gn(DET, N)) --> determinante(esp, DET), nombre(esp, N).

g_nominal(eng, gn(N, COMP)) --> nombre(eng, N), complementos(eng, COMP).
g_nominal(eng, gn(N)) --> nombre(eng, N).
g_nominal(eng, gn(DET, N, COMP)) --> determinante(eng, DET), nombre(eng, N), complementos(eng, COMP).
g_nominal(eng, gn(DET, N)) --> determinante(eng, DET), nombre(eng, N).

%%%%

g_adjetival(esp, gadj(A)) --> adjetivo(esp, A).
g_adjetival(esp, gadj(GADJ,GADJ2)) --> adjetivo(esp, GADJ), g_adjetival(esp, GADJ2).
g_adjetival(esp, gadj(GADJ,GN)) --> adjetivo(esp, GADJ), g_nominal(esp, GN).
g_adjetival(esp, gadj(GADJ,GP)) --> adjetivo(esp, GADJ), g_preposicional(esp, GP).
g_adjetival(esp, gadj(ADV,GADJ)) --> adverbio(esp, ADV), g_adjetival(esp, GADJ).
g_adjetival(esp, gadj(GADJ,CONJ,GADJ2)) --> adjetivo(esp, GADJ), conjuncion(esp, CONJ), g_adjetival(esp, GADJ2).

g_adjetival(eng, gadj(A)) --> adjetivo(eng, A).
g_adjetival(eng, gadj(GADJ,GADJ2)) --> adjetivo(eng, GADJ), g_adjetival(eng, GADJ2).
g_adjetival(eng, gadj(GADJ,GN)) --> adjetivo(eng, GADJ), g_nominal(eng, GN).
g_adjetival(eng, gadj(GADJ,GP)) --> adjetivo(eng, GADJ), g_preposicional(eng, GP).
g_adjetival(eng, gadj(ADV,GADJ)) --> adverbio(eng, ADV), g_adjetival(eng, GADJ).
g_adjetival(eng, gadj(GADJ,CONJ,GADJ2)) --> adjetivo(eng, GADJ), conjuncion(eng, CONJ), g_adjetival(eng, GADJ2).

%%%%

g_adverbial(esp, gadv(ADV)) --> adverbio(esp, ADV).
g_adverbial(esp, gadv(GADV,GADV2)) --> adverbio(esp, GADV), g_adverbial(esp, GADV2).
g_adverbial(esp, gadv(GADV,GN)) --> adverbio(esp, GADV), g_preposicional(esp, GN).

g_adverbial(eng, gadv(ADV)) --> adverbio(eng, ADV).
g_adverbial(eng, gadv(GADV,GADV2)) --> adverbio(eng, GADV), g_adverbial(eng, GADV2).
g_adverbial(eng, gadv(GADV,GN)) --> adverbio(eng, GADV), g_preposicional(eng, GN).

%%%%

g_preposicional(esp, gp(P,GN)) --> preposicion(esp, P), g_nominal(esp, GN).
g_preposicional(esp, gp(P,GADJ)) --> preposicion(esp, P), g_adjetival(esp, GADJ).
g_preposicional(esp, gp(P,GADV)) --> preposicion(esp, P), g_adverbial(esp, GADV).

g_preposicional(eng, gp(P,GN)) --> preposicion(eng, P), g_nominal(eng, GN).
g_preposicional(eng, gp(P,GADJ)) --> preposicion(eng, P), g_adjetival(eng, GADJ).
g_preposicional(eng, gp(P,GADV)) --> preposicion(eng, P), g_adverbial(eng, GADV).

%%%%

o_pruebaESP(1, [jose,es,moreno,y,maria,es,alta]).
o_pruebaESP(2, [jose,estudia,filosofia,pero,maria,estudia,derecho]).
o_pruebaESP(3, [maria,toma,un,cafe,mientras,jose,recoge,la,mesa]).
o_pruebaESP(4, [jose,toma,cafe,y,lee,el,periodico]).
o_pruebaESP(5, [jose,y,hector,comen,patatas,fritas,y,beben,cerveza]).
o_pruebaESP(6, [jose,come,patatas,fritas,pero,maria,prefiere,paella,aunque,hector,toma,cafe,e,irene,lee,una,novela]).
o_pruebaESP(7, [irene,canta,y,salta,mientras,jose,estudia]).
o_pruebaESP(8, [hector,come,patatas,fritas,y,bebe,zumo,mientras,jose,canta,y,salta,aunque,maria,lee,una,novela]).
o_pruebaESP(9, [jose,que,es,agil,escala,en,el,rocodromo,por,las,tardes]).
o_pruebaESP(10, [jose,que,es,muy,delicado,come,solamente,manzanas,rojas]).
o_pruebaESP(11, [el,procesador,de,textos,que,es,una,herramienta,bastante,potente,sirve,para,escribir,documentos]).
o_pruebaESP(12, [el,procesador,de,textos,es,una,herramienta,muy,potente,que,sirve,para,escribir,documentos,pero,es,bastante,lento]).
o_pruebaESP(13, [el,raton,que,cazo,el,gato,era,gris]).
o_pruebaESP(14, [el,hombre,que,vimos,ayer,era,mi,vecino]).

o_pruebaENG(1, [joseph,is,brown,and,mary,is,tall]).
o_pruebaENG(2, [joseph,studies,philosophy,but,mary,studies,law]).
o_pruebaENG(3, [mary,takes,a,coffee,while,joseph,collect,the,table]).
o_pruebaENG(4, [joseph,takes,coffee,and,reeds,the,newspaper]).
o_pruebaENG(5, [joseph,and,hector,eat,potatoes,fried,and,drink,beer]).
o_pruebaENG(6, [joseph,eats,potatoes,fried,but,mary,prefers,paella,although,hector,takes,coffee,and,irene,reeds,a,novel]).
o_pruebaENG(7, [irene,sings,and,jumps,while,joseph,studies]).
o_pruebaENG(8, [hector,eats,potatoes,fried,and,drinks,juice,while,joseph,sings,and,jumps,although,mary,reeds,a,novel]).
o_pruebaENG(9, [joseph,that,is,agile,climbs,in,the,climbing-wall,by,the,afternoon]).
o_pruebaENG(10, [joseph,that,is,very,delicate,eats,only,apples,red]).
o_pruebaENG(11, [the,processor,of,texts,that,is,a,tool,quite,powerful,serves,to,writing,documents]).
o_pruebaENG(12, [the,processor,of,texts,is,a,tool,very,powerful,that,serves,to,writing,documents,but,is,quite,slow]).
o_pruebaENG(13, [the,mouse,that,hunted,the,cat,was,gray]).
o_pruebaENG(14, [the,man,that,saw,yesterday,was,my,neighbor]).

traducir(DESDE, HASTA, O) :-
	oracion(DESDE, X, O, []),
	oracion(HASTA, X, Y, []),
	write('Oracion en '), write(DESDE), write(': '), write(O), nl,
	write('Oracion en '), write(HASTA), write(': '), write(Y), nl, nl.

ejecutar_pruebasESP(INI, INI).
ejecutar_pruebasESP(INI, FIN) :- 
	o_pruebaESP(INI, O),
	traducir(esp, eng, O),
	INI2 is INI+1,
	ejecutar_pruebasESP(INI2, FIN).

ejecutar_pruebasENG(INI, INI).
ejecutar_pruebasENG(INI, FIN) :- 
	o_pruebaENG(INI, O),
	traducir(eng, esp, O),
	INI2 is INI+1,
	ejecutar_pruebasENG(INI2, FIN).