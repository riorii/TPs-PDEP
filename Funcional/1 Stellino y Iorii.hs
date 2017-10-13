-- Punto 1
-- Modelado
import Text.Show.Functions
type Ejercicio = Gimnasta -> Gimnasta
type EjercicioRutina = (Int,Ejercicio)
type Rutina = (Int,[EjercicioRutina])
data Gimnasta = Gimnasta {
 nombre :: String,
 energia :: Int,
 equilibrio :: Int,
 flexibilidad :: Int,
 fuerza :: Int,
 ejercicios :: [Ejercicio]
} deriving (Show)

aumentarEnergiaEn :: Int -> Gimnasta -> Gimnasta
aumentarEnergiaEn cantidad gimnasta = gimnasta {energia = energia gimnasta + cantidad}

aumentarEquilibrioEn :: Int -> Gimnasta -> Gimnasta
aumentarEquilibrioEn cantidad gimnasta = gimnasta {equilibrio = equilibrio gimnasta + cantidad}

aumentarFlexibilidadEn :: Int -> Gimnasta -> Gimnasta
aumentarFlexibilidadEn cantidad gimnasta = gimnasta {flexibilidad = flexibilidad gimnasta + cantidad}

aumentarFuerzaEn :: Int -> Gimnasta -> Gimnasta
aumentarFuerzaEn cantidad gimnasta = gimnasta {fuerza = fuerza gimnasta + cantidad}

medialuna gimnasta = aumentarEquilibrioEn 5 gimnasta
rolAdelante velocidad gimnasta = aumentarEnergiaEn (div velocidad 2) gimnasta
vertical gimnasta = aumentarFuerzaEn 7 gimnasta
saltoConSoga cantSaltos gimnasta = (aumentarFuerzaEn cantSaltos.aumentarEnergiaEn (-div cantSaltos 2)) gimnasta
saltoMortal altura impulso gimnasta = (aumentarFuerzaEn altura.aumentarFlexibilidadEn (div impulso 2)) gimnasta


--2)
-- Aplicacion parcial
sonia = Gimnasta "sonia" 90 60 40 50 [medialuna,rolAdelante 20,saltoMortal 40 15]
pedro = Gimnasta "pedro" 70 50 50 60 [saltoConSoga 150,vertical,rolAdelante 30]
--3)
-- Recursividad
repetir :: Int -> (Gimnasta -> Gimnasta) -> Gimnasta -> Gimnasta
repetir 0 _ gimnasta = gimnasta
repetir repeticiones funcion gimnasta = repetir (repeticiones-1) funcion (funcion gimnasta)

aprender :: Ejercicio -> Gimnasta -> Gimnasta
aprender ejercicio gimnasta = gimnasta {ejercicios = (ejercicio:(ejercicios gimnasta))}

-- a)
ejercitar :: Int -> Ejercicio -> Gimnasta -> Gimnasta
ejercitar cantMinutos ejercicio gimnasta | (div cantMinutos 2) > 0 = ejercitar (cantMinutos -2) ejercicio (ejercicio gimnasta)
                                          | otherwise = aprender ejercicio gimnasta
-- b) - b)
entradaEnCalor :: Rutina
entradaEnCalor = (2, [(2 ,rolAdelante 10), (4, medialuna), (1,saltoConSoga 50), (1,saltoMortal 20 15)])

-- b) - c)
rutinaDiaria :: Rutina
rutinaDiaria = (3, [(1,rolAdelante 20),(1,saltoConSoga 30),(1,vertical),(1,medialuna),(1,saltoConSoga 10)])

-- c)
realizarEjercicioRutina :: Gimnasta -> EjercicioRutina -> Gimnasta
realizarEjercicioRutina gimnasta (repeticiones,ejercicio) = repetir repeticiones ejercicio gimnasta
-- Funcion de Orden Superior
realizarEjerciciosRutina :: [EjercicioRutina] -> Gimnasta -> Gimnasta
realizarEjerciciosRutina ejercicios gimnasta = foldl realizarEjercicioRutina gimnasta ejercicios

entrenar :: Rutina -> Gimnasta -> Gimnasta
entrenar (repeticiones,ejercicios) gimnasta = repetir repeticiones (realizarEjerciciosRutina ejercicios) gimnasta


-- d)
nivelDeFortaleza :: Gimnasta -> Int
nivelDeFortaleza (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = energia+fuerza

realizarEjerciciosPersonales :: Gimnasta -> Gimnasta
realizarEjerciciosPersonales gimnasta = foldr ($) gimnasta (ejercicios gimnasta) 

tienePotencial :: Int -> Gimnasta -> Bool
tienePotencial n gimnasta = (nivelDeFortaleza.realizarEjerciciosPersonales.entrenar rutinaDiaria) gimnasta > n

--4)
nombreMaximoGenericoConRutina :: (Gimnasta -> Int) -> Rutina -> [Gimnasta] -> String
nombreMaximoGenericoConRutina criterio_maximo rutina gimnastas = (nombre.maximoSegun criterio_maximo.entrenarGimnastasConRutina rutina) gimnastas

nombreMaximoGenericoConEjercicio :: (Gimnasta -> Int) -> Ejercicio -> Int -> [Gimnasta] -> String
nombreMaximoGenericoConEjercicio criterio_maximo ejercicio cantMinutos gimnastas = (nombre.maximoSegun criterio_maximo.entrenarConEjercicio ejercicio cantMinutos) gimnastas

--a)

entrenarGimnastasConRutina :: Rutina -> [Gimnasta] -> [Gimnasta]
entrenarGimnastasConRutina rutina gimnastas = map (entrenar rutina) gimnastas
--Funcion de Orden Superior
maximoSegun :: Ord b => (Gimnasta -> b) -> [Gimnasta] -> Gimnasta
maximoSegun _ [elemento] = elemento
maximoSegun funcion (x:y:xs) | funcion x > funcion y = maximoSegun funcion(x:xs)
                             | otherwise = maximoSegun funcion(y:xs)

maximoDespuesDeRutina :: [Gimnasta] -> String
maximoDespuesDeRutina gimnastas = nombreMaximoGenericoConRutina fuerza rutinaDiaria gimnastas


-- b)
fortaleza :: Gimnasta -> Int
fortaleza gimnasta = (energia gimnasta) + (fuerza gimnasta)

minimoEntreDos :: (Gimnasta->Int) -> (Gimnasta->Int) -> Gimnasta -> Int
minimoEntreDos funcion1 funcion2 gimnasta | (funcion1 gimnasta) > (funcion2 gimnasta) = funcion2 gimnasta
                                          | otherwise = funcion1 gimnasta

maximoConMinimo :: [Gimnasta] -> String
maximoConMinimo gimnastas = nombreMaximoGenericoConRutina (minimoEntreDos flexibilidad fortaleza) rutinaDiaria gimnastas


-- c)
entrenarConEjercicio :: Ejercicio -> Int -> [Gimnasta] -> [Gimnasta]
entrenarConEjercicio ejercicio cantMinutos gimnastas = map (ejercitar cantMinutos ejercicio) gimnastas
-- Composicion
cantidadDeEjercicios :: Gimnasta -> Int
cantidadDeEjercicios gimnasta = (length.ejercicios) gimnasta

maximoDespuesDeEjercicio :: [Gimnasta] -> Int -> Ejercicio -> String
maximoDespuesDeEjercicio gimnastas cantMinutos ejercicio = nombreMaximoGenericoConEjercicio (cantidadDeEjercicios) ejercicio cantMinutos gimnastas

--5)
--Para su resolucion se utiliza composición de funciones, expresiones lambda funcion de orden superior
-- El parametro 'e' sera comparable ya que se define como Eq, el parametro 'g' es una expresion lambda y una lista formada por elementos que sean iguales al parametro 'e'
-- Ejemplo de aplicaion de h:
-- h 4 (\n->n*2) [1,2]   => True
h :: Eq a1 => a1 -> (a -> a1) -> [a] -> Bool
h e g = any((\x->x). (== e)) . map g

--6)
--a)
-- Posible solucion pero no se puede implementar ya que el type Ejercicio no es de Eq
--interseccion:: Eq a => [a] -> [a] -> [a]
--interseccion ls xs = [l| l<-ls,x<-xs,l==x]
--aprenderConValidacion :: [Ejercicio] -> Gimnasta -> Gimnasta
--aprenderConValidacion ejercicio gimnasta = gimnasta {ejercicios = interseccion (ejericios gimnasta) ejercicio}
--ejercitarConValidacion :: Int -> [Ejercicio] -> Gimnasta -> Gimnasta
--ejercitarConValidacion cantMinutos ejercicios | (div cantMinutos 2) > 0 = ejercitar (cantMinutos -2) ejercicios
-- | otherwise = aprenderConValidacion ejercicios


--b)
-- Hay algun socio que tenga 10 de dinero en mi listaInfinita
-- h 10 (\n->n dinero) listaInfinita 
-- La funcion va a terminar solo si cumple con la condicion
data Socio = Socio {
 name :: String,
 dinero :: Int
} deriving (Show)
ignacio = Socio "ignacio" 10
nacho = Socio "nacho" 20
listaInfinita = cycle [ignacio,nacho]