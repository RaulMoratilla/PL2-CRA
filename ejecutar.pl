% Consults
:-consult("draw.pl").
:-consult("diccionarioESP.pl").
:-consult("procesamiento.pl").
:-consult("gramatica.pl")

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