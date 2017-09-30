-- Punto 1
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
medialuna gimnasta = gimnasta {equilibrio = equilibrio gimnasta+5}
rolAdelante velocidad gimnasta = gimnasta {energia = (energia gimnasta+(div velocidad 2))}
vertical gimnasta = gimnasta {fuerza = fuerza gimnasta + 7}
saltoConSoga cantSaltos gimnasta = gimnasta {energia = (energia gimnasta -(div cantSaltos 2)), fuerza = (fuerza gimnasta+cantSaltos)}
saltoMortal altura impulso gimnasta = gimnasta {flexibilidad = (flexibilidad gimnasta+(div impulso 2)), fuerza = (fuerza gimnasta+altura)}


--2)
sonia = Gimnasta "sonia" 90 60 40 50 [medialuna,rolAdelante 20,saltoMortal 40 15]
pedro = Gimnasta "pedro" 70 50 50 60 [saltoConSoga 150,vertical,rolAdelante 30]
--3)

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
realizarEjercicio :: Gimnasta -> Ejercicio -> Gimnasta
realizarEjercicio gimnasta ejercicio = ejercicio gimnasta

realizarEjercicioRutina :: Gimnasta -> EjercicioRutina -> Gimnasta
realizarEjercicioRutina gimnasta (repeticiones,ejercicio) = repetir repeticiones ejercicio gimnasta

realizarEjerciciosRutina :: [EjercicioRutina] -> Gimnasta -> Gimnasta
realizarEjerciciosRutina ejercicios gimnasta = foldl realizarEjercicioRutina gimnasta ejercicios

entrenar :: Rutina -> Gimnasta -> Gimnasta
entrenar (repeticiones,ejercicios) gimnasta = repetir repeticiones (realizarEjerciciosRutina ejercicios) gimnasta


-- d)
nivelDeFortaleza :: Gimnasta -> Int
nivelDeFortaleza (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = energia+fuerza

realizarEjerciciosPersonales :: Gimnasta -> Gimnasta
realizarEjerciciosPersonales gimnasta = foldl realizarEjercicio gimnasta (ejercicios gimnasta)

tienePotencial :: Int -> Gimnasta -> Bool
tienePotencial n gimnasta = nivelDeFortaleza (realizarEjerciciosPersonales (entrenar rutinaDiaria gimnasta)) > n

--4)
--a)

entrenarGimnastasConRutina :: [Gimnasta] -> Rutina -> [Gimnasta]
entrenarGimnastasConRutina gimnastas rutina = map (entrenar rutina) gimnastas

maximoSegun :: Ord b => (Gimnasta -> b) -> [Gimnasta] -> Gimnasta
maximoSegun _ [elemento] = elemento
maximoSegun funcion (x:y:xs) | funcion x > funcion y = maximoSegun funcion(x:xs)
                             | otherwise = maximoSegun funcion(y:xs)

maximoDespuesDeRutina :: [Gimnasta] -> String
maximoDespuesDeRutina gimnastas = nombre (maximoSegun fuerza (entrenarGimnastasConRutina gimnastas rutinaDiaria))

-- b)
fortaleza :: Gimnasta -> Int
fortaleza (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = energia + fuerza

minimoEntreDosCaracteristicas :: Gimnasta -> (Gimnasta->Int) -> (Gimnasta->Int) -> Int
minimoEntreDosCaracteristicas gimnasta caracteristica1 caracteristica2 | (caracteristica1 gimnasta) > (caracteristica2 gimnasta) = caracteristica2 gimnasta
                                                                       | otherwise = caracteristica1 gimnasta

maximoConMinimoSuperior :: (Gimnasta->Int) -> [Gimnasta] -> Gimnasta
maximoConMinimoSuperior _ [elemento] = elemento
maximoConMinimoSuperior minimoEntreDosCaracteristicas (x:y:xs) | minimoEntreDosCaracteristicas x > minimoEntreDosCaracteristicas y = maximoConMinimoSuperior minimoEntreDosCaracteristicas(x:xs)
                                                               | otherwise = maximoConMinimoSuperior minimoEntreDosCaracteristicas(y:xs)
-- c)
entrenarGimnastasConEjercicio :: [Gimnasta] -> Ejercicio -> Int -> [Gimnasta]
entrenarGimnastasConEjercicio gimnastas ejercicio cantMinutos = map (ejercitar cantMinutos ejercicio) gimnastas

maximoDespuesDeEjercicio :: [Gimnasta] -> Int -> Ejercicio -> String
maximoDespuesDeEjercicio gimnastas cantMinutos ejercicio = nombre (maximoSegun (length.ejercicios) (entrenarGimnastasConEjercicio gimnastas ejercicio cantMinutos))

--5)
--Para su resolucion se utiliza composición de funciones, expresiones lambda fucnion de orden superior
-- El parametro e sera comparable ya que se define como Eq, el parametro g es una expresion lambda y una lista formada por elementos que sean iguales al parametro e
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
--aprenderConValidacion ejercicio (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = Gimnasta nombre energia equilibrio flexibilidad fuerza (interseccion ejercicios ejercicio)
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