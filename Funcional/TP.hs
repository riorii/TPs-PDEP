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
--fortaleza :: Gimnasta -> Int
--fortaleza (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = energia + fuerza

minimoEntreDosCaracteristicas :: Gimnasta -> (Gimnasta->Int) -> (Gimnasta->Int) -> Int
minimoEntreDosCaracteristicas gimnasta caracteristica1 caracteristica2 | (caracteristica1 gimnasta) > (caracteristica2 gimnasta) = caracteristica2 gimnasta
                                                                       | otherwise = caracteristica1 gimnasta
-- maximoConMinimoSuperior :: [Gimnasta] -> String
-- maximoConMinimoSuperior gimnastas = (maximum  (map (minimoEntreDosCaracteristicas flexibilidad fortaleza) gimnastas))
maximoConMinimoSuperior :: (Gimnasta->Int) -> [Gimnasta] -> Gimnasta
maximoConMinimoSuperior _ [elemento] = elemento
maximoConMinimoSuperior minimoEntreDosCaracteristicas (x:y:xs) | minimoEntreDosCaracteristicas x > minimoEntreDosCaracteristicas y = maximoConMinimoSuperior minimoEntreDosCaracteristicas(x:xs)
												   			   | otherwise = maximoConMinimoSuperior minimoEntreDosCaracteristicas(y:xs)	
-- c)
mayorCantidadDeHabilidades :: [Gimnasta] -> Int -> [Ejercicio] -> String
mayorCantidadDeHabilidades gimnastas cantMinutos ejercicio = nombre (buscarGimnastaPorCampo (map (ejercitar cantMinutos ejercicio) gimnastas) (length . ejercicios) (maximum (map (length . ejercicios) (map (ejercitar cantMinutos ejercicio) gimnastas))))

--5)
h :: Eq a1 => a1 -> (a -> a1) -> [a] -> Bool
h e g = any((\x->x). (== e)) . map g