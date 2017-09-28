-- Punto 1
import Text.Show.Functions
type Ejercicios = Gimnasta -> Gimnasta
type Rutina = (Int,[Ejercicios])
data Gimnasta = Gimnasta {
 nombre :: String,
 energia :: Int,
 equilibrio :: Int,
 flexibilidad :: Int,
 fuerza :: Int,
 ejercicios :: [Ejercicios]
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
aprender :: [Ejercicios] -> Gimnasta -> Gimnasta
aprender ejercicio (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = Gimnasta nombre energia equilibrio flexibilidad fuerza (ejercicios++ejercicio)
--En consola seria (ejercitar 1 [medialuna]) sonia
ejercitar cantMinutos ejercicio | (div cantMinutos 2) > 0 = ejercitar (cantMinutos -2) ejercicio
                                | otherwise = aprender ejercicio

entradaEnCalor :: Rutina
entradaEnCalor = (2, [rolAdelante 10,rolAdelante 10,medialuna,medialuna,medialuna,medialuna,saltoConSoga 50,saltoMortal 20 15])

rutinaDiaria :: Rutina
rutinaDiaria = (3, [rolAdelante 20,saltoConSoga 30,vertical,medialuna,saltoConSoga 10])

entrenar :: Gimnasta -> Rutina -> Gimnasta
entrenar (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) (repeticiones ,[]) = Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios
entrenar (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) (repeticiones,(x:xs)) | repeticiones == 1 = entrenar (x (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios)) (repeticiones,xs)
                                                                                                   | repeticiones > 1 = entrenar (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) (1,take ((length (x:xs))*repeticiones) (cycle (x:xs)))
                                                                                                   | otherwise = Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios

nivelDeFortaleza :: Gimnasta -> Int
nivelDeFortaleza (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = energia+fuerza

tienePotencial :: Int -> Gimnasta -> Bool
tienePotencial n (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) = nivelDeFortaleza (entrenar (entrenar (Gimnasta nombre energia equilibrio flexibilidad fuerza ejercicios) rutinaDiaria) (1,ejercicios)) > n

--5)
h :: Eq a1 => a1 -> (a -> a1) -> [a] -> Bool
h e g = any((\x->x). (== e)) . map g
