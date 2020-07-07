--------------------------------- Taller mecanico (TP)
-- Fernando Gabriel Camino
-- Legajo: 07-138102-7
-- Paradigmas de Programacion - K2051

-- Dominio
-- Todo auto tiene
-- ●	la patente, que puede tener formato viejo “RVM363” o el nuevo “AB808RD”
-- ●	el desgaste de cada una de las llantas, ej: [ 0.5, 0.1, 0, 0.2 ]
-- ●	las revoluciones por minuto a las que regula el motor, ej: 1500
-- ●	la temperatura del agua luego de 5 minutos de encendido el auto: 90
-- ●	la fecha del último arreglo

type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)

-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year

data Auto = Auto {
    patente :: Patente,
    desgasteLlantas :: [Desgaste],
    rpm :: Int,
    temperaturaAgua :: Int,
    ultimoArreglo :: Fecha
} deriving Show


--------------------------------- Punto 1 (común): Costo de reparación de un auto
-- Saber el costo de reparación de un auto
-- ●	si la patente tiene 7 dígitos, es $ 12.500
-- ●	si no, si la patente está entre las letras "DJ" y "NB", se aplica el calculoPatental
-- ○	que es $ 3.000 * la longitud para las patentes que terminen en 4
-- ○	o $ 20.000 para el resto de las patentes
-- ●	de lo contrario, se le cobra $ 15000
--------------------------------- Importante: tenés que usar composición en las funciones auxiliares

costoDeReparacion::Auto->Int
costoDeReparacion auto | tienePatenteNueva auto = 12500
    | patenteEntreDJyNB auto = calculoPatental auto
    | otherwise = 15000

patenteEntreDJyNB::Auto->Bool
patenteEntreDJyNB auto = ((>=).patente) auto "DJ" && ((<=).patente) auto "NB"

calculoPatental::Auto->Int
calculoPatental auto | ((=='4').(head.((drop 5).patente))) auto = ((*3000).(length.patente)) auto
    | otherwise = 20000

tienePatenteNueva::Auto->Bool
tienePatenteNueva = ((==7).(length.patente))


--------------------------------- Punto 2
--------------------------------- ATENCIÓN: Resolver únicamente con Composición y aplicación parcial
--------------------------------- No se puede utilizar recursividad en ningún paso de este punto.

-- Parte 1) Auto peligroso (integrante a)
-- Dado un auto, saber si es peligroso. Esta condición se cumple cuando el desgaste de la primera llanta es mayor a 0.5

esPeligroso::Auto->Bool
esPeligroso = ((>0.5).(head.desgasteLlantas))

-- Parte 2) Necesita revisión (integrante b)
-- Dado un auto, saber si necesita revisión. Esta condición se cumple cuando el último arreglo fue realizado en el año 2015 ó antes.

necesitaRevision::Auto->Bool
necesitaRevision = ((<=2015).(anio.ultimoArreglo))

--------------------------------- Punto 3: Personal técnico encargado de las reparaciones
-- Parte 1) Integrante a
-- Necesitamos definir a las siguientes personas que realizan actividades en el taller mecánico:
-- ●	Alfa: hace que el auto regule a 2.000 vueltas, salvo que esté a menos de 2.000 vueltas, en cuyo caso lo deja como está
-- ●	Bravo: cambia todas las cubiertas, dejándolas sin desgaste
-- ●	Charly:  realiza las mismas actividades que Alfa y Bravo

type Mecanico = Auto->Fecha->Auto

alfa::Mecanico
alfa auto fecha | rpm auto >= 2000 = auto {
    rpm = 2000,
    ultimoArreglo = fecha
 }
    | otherwise = auto

bravo::Mecanico
bravo auto fecha = auto {
    desgasteLlantas = [0.0, 0.0, 0.0, 0.0],
    ultimoArreglo = fecha
 }

charly::Mecanico
charly auto fecha = ((bravo.alfa auto) fecha) fecha


-- Parte 2) Integrante b
-- Necesitamos definir a las siguientes personas que realizan actividades en el taller mecánico
-- ●	Tango: le gusta decir que hizo muchas cosas pero en realidad no hace ningún arreglo
-- ●	Zulu: revisa la temperatura del agua, la deja a 90 y hace lo mismo que Lima (ver a continuación)
-- ●	Lima:  cambia las cubiertas delanteras (las dos primeras), dejándolas sin desgaste. Las posteriores quedan igual

tango::Mecanico
tango auto fecha = auto {
    ultimoArreglo = fecha }

zulu::Mecanico
zulu auto fecha = lima (auto {temperaturaAgua = 90}) fecha

lima::Mecanico
lima auto fecha = auto {
    desgasteLlantas = (cambiarDelanteras.desgasteLlantas) auto,
    ultimoArreglo = fecha }

cambiarDelanteras::[Float]->[Float]
cambiarDelanteras desgaste = [0.0, 0.0] ++ (drop 2 desgaste)


--------------------------------- Punto 4: Ordenamiento TOC de autos
--------------------------------- Solamente se puede utilizar recursividad en este punto.
--------------------------------- BONUS: Evitar repetición de código.

-- (Común para ambos integrantes) 
-- Dada una serie de autos, saber si están ordenados en base al siguiente criterio:
-- ●	los autos ubicados en la posición impar de la lista deben tener una cantidad de desgaste impar
-- ●	los autos ubicados en la posición par deben tener una cantidad de desgaste par
-- ●	asumimos que el primer elemento está en la posición 1, el segundo elemento en la posición 2, etc.

-- La cantidad de desgaste es la sumatoria de desgastes de las cubiertas de los autos multiplicada por 10. Ejemplo: 0.2 + 0.5 + 0.6 + 0.1 = 1.4 * 10 = 14. Para determinar si es par o no (y evitar errores de redondeo) es conveniente utilizar la función round.

estanBienOrdenados::[Auto]->Int->Bool
estanBienOrdenados [] _ = True
estanBienOrdenados autos n | odd (obtenerPosicion (head autos) autos n) && odd (sumatoriaDesgaste (head autos)) = estanBienOrdenados (tail autos) (incrementarNumero n)
    | even (obtenerPosicion (head autos) autos n) && even (sumatoriaDesgaste (head autos)) = estanBienOrdenados (tail autos) (incrementarNumero n)
    | otherwise = False
-- (UTILIZANDO n COMO INICIALIZADOR DE POSICIÓN, en este caso por consola habría que asignarle el número 1)

sumatoriaDesgaste::Auto->Int
sumatoriaDesgaste = (round.((*10).(sum.desgasteLlantas)))

incrementarNumero n = n +1

obtenerPosicion::Auto->[Auto]->Int->Int
obtenerPosicion _ [] _ = 0
obtenerPosicion y (x:xs) n | patente y == patente x = n
    | otherwise = obtenerPosicion y xs (incrementarNumero n)

--------------------------------- Punto 5: Orden de reparación
-- (Común para ambos integrantes) 
-- Aplicar una orden de reparación, que tiene
-- ●	una fecha
-- ●	una lista de técnicos
-- y consiste en que cada uno de los técnicos realice las reparaciones que sabe sobre el auto, al que además hay que actualizarle la última fecha de reparación.

aplicarOrdenDeReparacion::Fecha->[(Auto)->Fecha->Auto]->Auto->Auto
aplicarOrdenDeReparacion fecha [] auto = auto
aplicarOrdenDeReparacion fecha listaMecanicos auto = aplicarOrdenDeReparacion fecha (tail listaMecanicos) ((head listaMecanicos) auto fecha)

-- Casos de prueba a definir
-- Deben plantearlo los integrantes.

-- Un auto con desgaste de llantas [0.1, 0.4, 0.2, 0.1], 150 de temperatura y a 3000 revoluciones reparado por "Alfa" y "Lima" >>>>>> Que devuelva el auto regulado a 2000, con los ruedas delanteras sin desgaste y sin modificar la temperatura

auto1Punto5 = Auto  {
    patente = "ZZH032",
    desgasteLlantas = [0.1, 0.4, 0.2, 0.1],
    rpm = 3000,
    temperaturaAgua = 150,
    ultimoArreglo = (20,06,2014)
}

-- *Main> aplicarOrdenDeReparacion (8,5,2020) [alfa, lima] auto1Punto5
-- Auto {patente = "ZZH032", desgasteLlantas = [0.0,0.0,0.2,0.1], rpm = 2000, temperaturaAgua = 150, ultimoArreglo = (8,5,2020)}



-- Un auto con desgaste de llantas [0.1, 0.4, 0.2, 0.1], 150 de temperatura y a 3000 revoluciones reparado por "Charly" y "Tango" >>>>>> Que devuelva el auto regulado a 2000, con todas las llantas sin desgaste y sin modificar la temperatura

-- *Main> aplicarOrdenDeReparacion (8,5,2020) [charly, tango] auto1Punto5
-- Auto {patente = "ZZH032", desgasteLlantas = [0.0,0.0,0.0,0.0], rpm = 2000, temperaturaAgua = 150, ultimoArreglo = (8,5,2020)}




-- Un auto con desgaste de llantas [0.1, 0.4, 0.2, 0.1], 150 de temperatura y a 3000 revoluciones reparado por "Bravo" y "Zulu" >>>>>> Que devuelva con todas las llantas sin desgaste y con la temperatura a 90 (sin modificar regulacion)

-- *Main> aplicarOrdenDeReparacion (8,5,2020) [bravo, zulu] auto1Punto5
-- Auto {patente = "ZZH032", desgasteLlantas = [0.0,0.0,0.0,0.0], rpm = 3000, temperaturaAgua = 90, ultimoArreglo = (8,5,2020)}



-- Un auto con desgaste de llantas [0.1, 0.4, 0.2, 0.1], 150 de temperatura y a 3000 revoluciones reparado por todos los mecanicos >>>>>> Que devuelva con todas las llantas sin desgaste, la temperatura a 90 y regulado a 2000 revoluciones

-- *Main> aplicarOrdenDeReparacion (8,5,2020) [alfa, bravo, charly, tango, zulu, lima] auto1Punto5
-- Auto {patente = "ZZH032", desgasteLlantas = [0.0,0.0,0.0,0.0], rpm = 2000, temperaturaAgua = 90, ultimoArreglo = (8,5,2020)}


--------------------------------- Punto 6
--------------------------------- Solamente se puede utilizar funciones de orden superior en este punto.

-- Parte 1) Integrante a: Técnicos que dejan el auto en condiciones
-- Dada una lista de técnicos determinar aquellos técnicos que dejarían el auto en condiciones (que no sea peligroso andar, recordar el punto 2.1 del integrante a).

quienesLoDejanEnCondiciones::[Mecanico]->Auto->Fecha->Int->Int
quienesLoDejanEnCondiciones [] auto fecha n = n
quienesLoDejanEnCondiciones mecanicos auto fecha n | esPeligroso (aplicarOrdenDeReparacion fecha [head mecanicos] auto) = quienesLoDejanEnCondiciones (tail mecanicos) auto fecha n
    | otherwise = quienesLoDejanEnCondiciones (tail mecanicos) auto fecha (incrementarNumero n)


-- Parte 2) Integrante b: Costo de reparación de autos que necesitan revisión
-- Dada una lista de autos, saber cuál es el costo de reparación de los autos que necesitan revisión.

costoDeReparacionTotal::[Auto]->Int
costoDeReparacionTotal [] = 0
costoDeReparacionTotal autos | necesitaRevision (head autos) = costoDeReparacion (head autos) + costoDeReparacionTotal (tail autos)
    | otherwise = costoDeReparacionTotal (tail autos)

--------------------------------- Punto 7
-- Parte 1) Integrante a: Técnicos que dejan el auto en condiciones
-- En base al punto “dada una lista de técnicos determinar qué técnicos dejarían el auto en condiciones” y considerando una lista de técnicos  infinita, ¿podríamos obtener el primer técnico que deja el auto en condiciones? Muestre un ejemplo y justifique. 


--tecnicosInfinitos2 = zulu:tecnicosInfinitos2

--obtenerPrimerMecanico::[Mecanico]->Auto->Fecha->[Mecanico]
--obtenerPrimerMecanico [] auto fecha = []
--obtenerPrimerMecanico mecanicos auto fecha | (length (quienesLoDejanEnCondiciones [head mecanicos] auto fecha)) == 1 = [head mecanicos]
--    | otherwise = obtenerPrimerMecanico (tail mecanicos) auto fecha

-- RESPUESTA: No, no podemos obtener al primer técnico que deja el auto en condiciones ya que los hemos modelado (a los mecanicos) como funciones y en Haskell no tenemos forma de que el compilador nos devuelva (muestre) a la función propiamente dicha (la que en este caso deja el auto en condiciones).
-- En el ejemplo no se puede mostrar [head mecanicos], salida en la linea de código numero 228.


-----------------
-- Parte 2) Integrante b: Costo de reparación de autos que necesitan revisión
-- En base al punto “Dada una lista de autos, saber cuál es el costo de reparación de los autos que necesitan revisión.”,  ¿podríamos tener una lista infinita de autos? Muestre un ejemplo y justifique. 


-- RESPUESTA: No se puede utilizar una lista infinita en la función costoDeReparacionTotal ya que para mostrar el resultado primero tiene que recorrer toda la lista evaluando si cada elemento necesita una revisión, al nunca poder terminar de realizar dichas evaluaciones el compilador se quedará procesando información por tiempo indeterminado. Ejemplo:

autosInfinitos :: [Auto]
autosInfinitos = autosInfinitos' 0

autosInfinitos' :: Int -> [Auto]
autosInfinitos' n = Auto {
    patente = "AAA000",
    desgasteLlantas = [0.5, 0.1, 0.2, 0.3],
    rpm = 1500 + n,
    temperaturaAgua = 90,
    ultimoArreglo = (20, 1, 2013)
} : autosInfinitos' (n + 1)

-- *Main> costoDeReparacionTotal autosInfinitos
-- Interrupted.



-- Y si tomáramos en cuenta los tres primeros autos que necesitan revisión, ¿cómo debería cambiar la función? Por otra parte, ¿esta versión aceptaría una lista infinita de autos? Modifique la función 6.b con otro nombre y justifique sus respuestas.

-- RESPUESTA: Si, esta nueva versión aceptaría una lista infinita de autos ya que nosotros estamos acotando manualmente cuantos son los elementos que precisamos obtener, gracias a la evaluación perezosa (lazy evaluation) con la que trabaja Haskell se van evaluando los argumentos a medida que los va necesitando. Ejemplo:

disminuirNumero n = n -1

costoDeReparacionDeLosPrimeros::Int->[Auto]->Int
costoDeReparacionDeLosPrimeros n [] = 0
costoDeReparacionDeLosPrimeros n autos | n > 0 && necesitaRevision (head autos) = costoDeReparacion (head autos) + costoDeReparacionDeLosPrimeros (disminuirNumero n) (tail autos)
    | n == 0 = 0
    | otherwise = costoDeReparacionDeLosPrimeros n (tail autos)

-- *Main> costoDeReparacionDeLosPrimeros 3 autosInfinitos
-- 45000