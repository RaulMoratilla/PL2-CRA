import random


a = ["1","De","168","Ver","334","Claro","2","La","169","Veces","335","Iba","3","Que","170","Embargo","336","Éste","4","El","171","Partido","337","Pesetas","5","En","172","Personas","338","Orden","6","Y","173","Grupo","339","Español","7","A","174","Cuenta","340","Buena","8","Los","175","Pueden","341","Quiere","9","Se","176","Tienen","342","Aquella","10","Del","177","Misma","343","Programa","11","Las","178","Nueva","344","Palabras","12","Un","179","Cual","345","Internacional","13","Por","180","Fueron","346","Van","14","Con","181","Mujer","347","Esas","15","No","182","Frente","348","Segunda","16","Una","183","José","349","Empresa","17","Su","184","Tras","350","Puesto","18","Para","185","Cosas","351","Ahí","19","Es","186","Fin","352","Propia","20","Al","187","Ciudad","353","M","21","Lo","188","He","354","Libro","22","Como","189","Social","355","Igual","23","Más","190","Manera","356","Político","24","O","191","Tener","357","Persona","25","Pero","192","Sistema","358","Últimos","26","Sus","193","Será","359","Ellas","27","Le","194","Historia","360","Total","28","Ha","195","Muchos","361","Creo","29","Me","196","Juan","362","Tengo","30","Si","197","Tipo","363","Dios","31","Sin","198","Cuatro","364","C","32","Sobre","199","Dentro","365","Española","33","Este","200","Nuestro","366","Condiciones","34","Ya","201","Punto","367","México","35","Entre","202","Dice","368","Fuerza","36","Cuando","203","Ello","369","Solo","37","Todo","204","Cualquier","370","Único","38","Esta","205","Noche","371","Acción","39","Ser","206","Aún","372","Amor","40","Son","207","Agua","373","Policía","41","Dos","208","Parece","374","Puerta","42","También","209","Haber","375","Pesar","43","Fue","210","Situación","376","Zona","44","Había","211","Fuera","377","Sabe","45","Era","212","Bajo","378","Calle","46","Muy","213","Grandes","379","Interior","47","Años","214","Nuestra","380","Tampoco","48","Hasta","215","Ejemplo","381","Música","49","Desde","216","Acuerdo","382","Ningún","50","Está","217","Habían","383","Vista","51","Mi","218","Usted","384","Campo","52","Porque","219","Estados","385","Buen","53","Qué","220","Hizo","386","Hubiera","54","Sólo","221","Nadie","387","Saber","55","Han","222","Países","388","Obras","56","Yo","223","Horas","389","Razón","57","Hay","224","Posible","390","Ex","58","Vez","225","Tarde","391","Niños","59","Puede","226","Ley","392","Presencia","60","Todos","227","Importante","393","Tema","61","Así","228","Guerra","394","Dinero","62","Nos","229","Desarrollo","395","Comisión","63","Ni","230","Proceso","396","Antonio","64","Parte","231","Realidad","397","Servicio","65","Tiene","232","Sentido","398","Hijo","66","Él","233","Lado","399","Última","67","Uno","234","Mí","400","Ciento","68","Donde","235","Tu","401","Estoy","69","Bien","236","Cambio","402","Hablar","70","Tiempo","237","Allí","403","Dio","71","Mismo","238","Mano","404","Minutos","72","Ese","239","Eran","405","Producción","73","Ahora","240","Estar","406","Camino","74","Cada","241","San","407","Seis","75","E","242","Número","408","Quién","76","Vida","243","Sociedad","409","Fondo","77","Otro","244","Unas","410","Dirección","78","Después","245","Centro","411","Papel","79","Te","246","Padre","412","Demás","80","Otros","247","Gente","413","Barcelona","81","Aunque","248","Final","414","Idea","82","Esa","249","Relación","415","Especial","83","Eso","250","Cuerpo","416","Diferentes","84","Hace","251","Obra","417","Dado","85","Otra","252","Incluso","418","Base","86","Gobierno","253","Través","419","Capital","87","Tan","254","Último","420","Ambos","88","Durante","255","Madre","421","Europa","89","Siempre","256","Mis","422","Libertad","90","Día","257","Modo","423","Relaciones","91","Tanto","258","Problema","424","Espacio","92","Ella","259","Cinco","425","Medios","93","Tres","260","Carlos","426","Ir","94","Sí","261","Hombres","427","Actual","95","Dijo","262","Información","428","Población","96","Sido","263","Ojos","429","Empresas","97","Gran","264","Muerte","430","Estudio","98","País","265","Nombre","431","Salud","99","Según","266","Algunas","432","Servicios","100","Menos","267","Público","433","Haya","102","Año","268","Mujeres","434","Principio","103","Antes","269","Siglo","435","Siendo","104","Estado","270","Todavía","436","Cultura","105","Contra","271","Meses","437","Anterior","106","Sino","272","Mañana","438","Alto","107","Forma","273","Esos","439","Media","108","Caso","274","Nosotros","440","Mediante","109","Nada","275","Hora","441","Primeros","110","Hacer","276","Muchas","442","Arte","111","General","277","Pueblo","443","Paz","112","Estaba","278","Alguna","444","Sector","113","Poco","279","Dar","445","Imagen","114","Estos","280","Problema","446","Medida","115","Presidente","281","Don","447","Deben","116","Mayor","282","Da","448","Datos","117","Ante","283","Tú","449","Consejo","118","Unos","284","Derecho","450","Personal","119","Les","285","Verdad","451","Interés","120","Algo","286","María","452","Julio","121","Hacia","287","Unidos","453","Grupos","122","Casa","288","Podría","454","Miembros","123","Ellos","289","Sería","455","Ninguna","124","Ayer","290","Junto","456","Existe","125","Hecho","291","Cabeza","457","Cara","126","Primera","292","Aquel","458","Edad","127","Mucho","293","Luis","459","Etc.","128","Mientras","294","Cuanto","460","Movimiento","129","Además","295","Tierra","461","Visto","130","Quien","296","Equipo","462","Llegó","131","Momento","297","Segundo","463","Puntos","132","Millones","298","Director","464","Actividad","133","Esto","299","Dicho","465","Bueno","134","España","300","Cierto","466","Uso","135","Hombre","301","Casos","467","Niño","136","Están","302","Manos","468","Difícil","137","Pues","303","Nivel","469","Joven","138","Hoy","304","Podía","470","Futuro","139","Lugar","305","Familia","471","Aquellos","140","Madrid","306","Largo","472","Mes","141","Nacional","307","Partir","473","Pronto","142","Trabajo","308","Falta","474","Soy","143","Otras","309","Llegar","475","Hacía","144","Mejor","310","Propio","476","Nuevos","145","Nuevo","311","Ministro","477","Nuestros","146","Decir","312","Cosa","478","Estaban","147","Algunos","313","Primero","479","Posibilidad","148","Entonces","314","Seguridad","480","Sigue","149","Todas","315","Hemos","481","Cerca","150","Días","316","Mal","482","Resultados","151","Debe","317","Trata","483","Educación","152","Política","318","Algún","484","Atención","153","Cómo","319","Tuvo","485","González","154","Casi","320","Respecto","486","Capacidad","155","Toda","321","Semana","487","Efecto","156","Tal","322","Varios","488","Necesario","157","Luego","323","Real","489","Valor","158","Pasado","324","Sé","490","Aire","159","Primer","325","Voz","491","Investigación","160","Medio","326","Paso","492","Siguiente","161","Va","327","Señor","493","Figura","162","Estas","328","Mil","494","Central","163","Sea","329","Quienes","495","Comunidad","164","Tenía","330","Proyecto","496","Necesidad","165","Nunca","331","Mercado","497","Serie","166","Poder","332","Mayoría","498","Organización","167","Aquí","333","Luz","499","Nuevas","500","Calidad"]

print(a.index("Casi")/2)

def preguntar(i):
    c = input(i + " (v, n, d(det), b(adv), c(conj), j(adj), p(prep)): ")
    # c = random.choice(["v", "n", "d", "b", "c", "j", "p"])
    palabra = str(i).lower().replace("á", "a").replace("é", "e").replace("í", "i").replace("ó", "o").replace("ú", "u").replace("ñ", "n")
    if c == "v":
        return f"v({palabra}).\n", "", "", "", "", "", ""
    elif c == "n":
        return "", f"n({palabra}).\n", "", "", "", "", ""
    elif c == "d":
        return "", "", f"det({palabra}).\n", "", "", "", ""
    elif c == "b":
        return "", "", "", f"adv({palabra}).\n", "", "", ""
    elif c == "c":
        return "", "", "", "", f"conj({palabra}).\n", "", ""
    elif c == "j":
        return "", "", "", "", "", f"adj({palabra}).\n", ""
    elif c == "p":
        return "", "", "", "", "", "", f"prep({palabra}).\n"
    else:
        print("Error")
        return preguntar(i)

verbos = ""
nombres = ""
determinantes = ""
adverbios = ""
conjunciones = ""
adjetivos = ""
preposiciones = ""
for i in a:
    try:
        int(i)
    except Exception:
        v, n, d, b, c, j, p = preguntar(i)
        verbos += v
        nombres += n
        determinantes += d
        adverbios += b
        conjunciones += c
        adjetivos += j
        preposiciones += p

print("\n\n")
print(verbos)
print("\n\n")
print(nombres)
print("\n\n")
print(determinantes)
print("\n\n")
print(adverbios)
print("\n\n")
print(conjunciones)
print("\n\n")
print(adjetivos)
print("\n\n")
print(preposiciones)
print("\n\n")
