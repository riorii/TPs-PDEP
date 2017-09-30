-- Punto 1
import Text.Show.Functions
type Ejercicio = Gimnasta -> Gimnasta
type Rutina = (Int,[Ejercicio])
data Gimnasta = Gimnasta {
 nombre :: String,
 energia :: Int,
 equilibrio :: Int,
 flexibilidad :: Int,
 fuerza :: Int,
 ejercicios :: [Ejercicio]
} deriving (Show)
medialuna (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = Gimnasta nombre energia (equilibrio+5) flexibilidad fuerza ejercicios
rolAdelante velocidad (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = Gimnasta nombre (energia+(div velocidad 2)) equilibrio flexibilidad fuerza ejercicios
vertical (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = Gimnasta nombre energia equilibrio flexibilidad (fuerza+7) ejercicios
saltoConSoga cantSaltos (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = Gimnasta nombre (energia-(div cantSaltos 2)) equilibrio flexibilidad (fuerza+cantSaltos) ejercicios
saltoMortal altura  impulso (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = Gimnasta nombre energia equilibrio (flexibilidad+(div impulso 2)) (fuerza+altura) ejercicios

--2)
sonia = Gimnasta "sonia" 90 60 40 50 [medialuna,rolAdelante 20,saltoMortal 40 15]
pedro = Gimnasta "pedro" 70 50 50 60 [saltoConSoga 150,vertical,rolAdelante 30]
--3)
aprender :: [Ejercicio] -> Gimnasta -> Gimnasta
aprender ejercicio (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = Gimnasta nombre energia equilibrio flexibilidad fuerza (ejercicios++ejercicio)
--En consola seria (ejercitar 1 [medialuna]) sonia

ejercitar :: Int -> [Ejercicio] -> Gimnasta -> Gimnasta
ejercitar cantMinutos ejercicios | (div cantMinutos 2) > 0 = ejercitar (cantMinutos -2) ejercicios
                                | otherwise = aprender ejercicios

entradaEnCalor :: Rutina
entradaEnCalor = (2, [rolAdelante 10,rolAdelante 10,medialuna,medialuna,medialuna,medialuna,saltoConSoga 50,saltoMortal 20 15])

rutinaDiaria :: Rutina
rutinaDiaria = (3, [rolAdelante 20,saltoConSoga 30,vertical,medialuna,saltoConSoga 10])

entrenar :: Rutina  -> Gimnasta -> Gimnasta
entrenar (repeticiones ,[]) (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios)  = Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios
entrenar (repeticiones,(x:xs)) (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) | repeticiones == 1 = entrenar (repeticiones,xs) (x (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios))
                                                                                                   | repeticiones > 1 = entrenar (1,take ((length (x:xs))*repeticiones) (cycle (x:xs))) (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios)
                                                                                                   | otherwise = Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios

nivelDeFortaleza :: Gimnasta -> Int
nivelDeFortaleza (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = energia+fuerza

tienePotencial :: Int -> Gimnasta -> Bool
tienePotencial n (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = nivelDeFortaleza (entrenar (1,ejercicios) (entrenar rutinaDiaria (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios))) > n

--4)
--a)
buscarGimnastaPorCampo :: Eq b => [Gimnasta] -> (Gimnasta -> b) -> b -> Gimnasta
buscarGimnastaPorCampo [gimnasta] _ _ = gimnasta
buscarGimnastaPorCampo (x:xs) campo dato | campo x == dato = x
                                         | otherwise = buscarGimnastaPorCampo xs campo dato

maximoDespuesDeRutina :: [Gimnasta] -> String
maximoDespuesDeRutina gimnastas = nombre (buscarGimnastaPorCampo (map (entrenar rutinaDiaria) gimnastas) fuerza (maximum (map fuerza (map (entrenar rutinaDiaria) gimnastas))))

-- b)
fortaleza :: Gimnasta -> Int
fortaleza (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = energia + fuerza

minimoEntreDosCaracteristicas ::(Gimnasta->Int) -> (Gimnasta->Int) -> Gimnasta -> Int
minimoEntreDosCaracteristicas caracteristica1 caracteristica2 gimnasta | (caracteristica1 gimnasta) > (caracteristica2 gimnasta) = caracteristica2 gimnasta
                                                                       | otherwise = caracteristica1 gimnasta

maximoSegun :: [Gimnasta] -> String
maximoSegun [gimnasta] = nombre gimnasta
maximoSegun (x:y:xs) | minimoEntreDosCaracteristicas flexibilidad fortaleza x > minimoEntreDosCaracteristicas flexibilidad fortaleza y = maximoSegun (x:xs)
                     | otherwise = maximoSegun (y:xs)

-- c)
mayorCantidadDeHabilidades :: [Gimnasta] -> Int -> [Ejercicio] -> String
mayorCantidadDeHabilidades gimnastas cantMinutos ejercicio = nombre (buscarGimnastaPorCampo (map (ejercitar cantMinutos ejercicio) gimnastas) (length . ejercicios) (maximum (map (length . ejercicios) (map (ejercitar cantMinutos ejercicio) gimnastas))))

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
--											  | otherwise = aprenderConValidacion ejercicios


--b)
-- Para poder hacer un listado infinito de un data debe agregarse deriving Enum
-- Para aplicarle la funcion h a un listado de socios encontre la siguiente resolucion
data Socio = Ignacio | Robertino | Matias | Andrea deriving(Enum,Eq,Show)
socioInicial = Ignacio
-- h Ignacio (\n->n) [socioInicial..]