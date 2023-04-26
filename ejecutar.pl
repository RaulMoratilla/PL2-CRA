% Consults
:-consult("draw.pl").
:-consult("diccionarioESP.pl").
:-consult("procesamiento.pl").
:-consult("gramatica.pl")

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

% Ejecucion
ejecutar_pruebas(INI, INI).
ejecutar_pruebas(INI, FIN) :- 
    o_prueba(INI, O),
    oracion(X, O, []),
    draw(X),
    separar(X, C),
    formatear_oraciones(C, C1),
    write(C1), nl,
    INI2 is INI+1,
    ejecutar_pruebas(INI2, FIN).