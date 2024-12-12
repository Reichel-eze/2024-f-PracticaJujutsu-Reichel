module Library where
import PdePreludat

type Nombre = String
type Antiguedad = Number
type Grado = Number
type Clan = String

data Hechizero = UnHechizero {
    grado :: Grado,
    antiguedad :: Antiguedad,
    clan :: Clan
}deriving(Show, Eq)

-- data Clan = Kugisaki | Gojo | Zenin | Itadori | Kamo deriving(Show, Eq)

-- 1) Modelamos los siguientes hechizeros
-- siendo los grados menores los de mayor fuerza.
-- grados para medir su fuerza

nobara :: Hechizero
nobara = UnHechizero {
    grado = 3,
    antiguedad = 1,
    clan = "Kugisaki"
}

satoru :: Hechizero
satoru = UnHechizero {
    grado = 0,
    antiguedad = 15,
    clan = "Gojo"
}

maki :: Hechizero
maki = UnHechizero {
    grado = 4,
    antiguedad = 3,
    clan = "Zenin"
}

--yuji :: Hechizero
--yuji = UnHechizero {
--    antiguedad = 0,
--    clan = Itadori,
--    grado = 1
--}

yuji :: Hechizero
yuji = UnHechizero 1 0 "Itadori"

grupoA :: [Hechizero]
grupoA = [nobara, satoru, maki]

-- 2) Nos interesa saber cuando un equipo esta preparado (cuando el equipo tiene mas de 3 integrantes)

estaPreparado :: [Hechizero] -> Bool
estaPreparado equipo = length equipo > 3  

estaPreparado' :: [Hechizero] -> Bool
estaPreparado' = (>3) . length

-- 3) Cuando un grupo es invencible (cuando existe al menos un 
-- integrante que sea de grado especial, 
-- ya que son los hechiceros más poderosos y 
-- pueden con cualquier maldición.)

esInvencible' :: [Hechizero] -> Bool
esInvencible' = any ((==0) . grado) 

esInvencible :: [Hechizero] -> Bool
esInvencible hechizeros = any esEspecial hechizeros

esEspecial :: Hechizero -> Bool
esEspecial = (==0) . grado

-- 4) Cuando un hechizero es prestigioso, cuando pertenece a un clan prestigioso

--tienePrestigio :: Clan -> Bool
--tienePrestigio Zenin = True
--tienePrestigio Gojo = True
--tienePrestigio Kamo = True
--tienePrestigio _ = False

clanesPrestigiosos :: [Clan]
clanesPrestigiosos = ["Zenin", "Gojo", "Kamo"]

esPrestigioso :: Hechizero -> Bool
esPrestigioso hechizero = clan hechizero `elem` clanesPrestigiosos  

-- elem (clan hechizero) claesPrestigiosos
-- elem determina si un elemento se encuentra en una lista

-- 5) Queremos saber si un grupo es favorito de los altos mandos, 
-- esto se da cuando todos los integrantes del mismo son prestigiosos.

esFavorito :: [Hechizero] -> Bool
esFavorito hechizeros = all (esPrestigioso) hechizeros

-- all es si todos los elementos de la lista cumplen con la funcion

-- 6) Nos interesa saber de un grupo quienes son los expertos, 
-- que estarán al mando de las misiones. 
-- Decimos que los expertos serán aquellos que tengan más 
-- de un año de antigüedad.

sonExpertos :: [Hechizero] -> [Hechizero]
sonExpertos hechizeros = filter (esExperto) hechizeros

-- filter (funcion que debe cumplir elemento de la lista) (lista)

esExperto :: Hechizero -> Bool
esExperto = (>1) . antiguedad

-- primero saco la antiguedad del hechicero y luego la comparo, en composicion a la derecha va lo primero

-- 7) Queremos que sea posible subir de grado a nuestros
-- hechiceros a medida que se vuelven más fuertes. 
-- Para esto se le restará un punto a su grado, 
-- subiendo así a la siguiente categoría. 
-- En el caso de que sea un hechicero de grado especial, 
-- es decir que su grado es 0, como es el rango máximo, 
-- quedará en el mismo rango.

subirRango :: Hechizero -> Hechizero
subirRango hechizero 
    | esEspecial hechizero = hechizero
    | otherwise            = subirCategoria hechizero 

subirCategoria :: Hechizero -> Hechizero
subirCategoria hechizero = hechizero {grado = grado hechizero - 1} 

-- 8)
-- a) Queremos saber si un grupo es capaz de hacerle frente a cualquier maldición.
-- Esto puede suceder cuando el grupo es invencible, 
-- o bien si está preparado, de forma que pueden tener 
-- muchas habilidades para enfrentarla.

leHaceFrenteACualquierMaldicion :: [Hechizero] -> Bool
leHaceFrenteACualquierMaldicion hechiceros = 
    esInvencible hechiceros || estaPreparado' hechiceros
-- || disyuncion (o logico) que puede suceder uno o bien el otro


-- b) Luego de derrotar a una maldición poderosa, ¡la batalla 
-- les genera un power up! Queremos que sea posible que un 
-- grupo tenga un power up, es decir subirle el grado a cada 
-- integrante.

powerUp :: [Hechizero] -> [Hechizero]
powerUp hechizeros = map subirRango hechizeros

-- aplico map con la funcion subirGrado asi les subo el grado a todos los elementos de la lista de hechiceros

-- 9) Misiones en solitario, se tiene que elegir entre dos hechizeros
-- quien es el mas apto segun un criterio

type Criterio = (Hechizero -> Number)

-- esta generalizacion solo se puede hacer con los critieros que devuelven un numero (NO un char!!)
hechizeroMasAptoParaMision :: Hechizero -> Hechizero -> Criterio -> Hechizero
hechizeroMasAptoParaMision hechizero1 hechizero2 criterio
    | criterio hechizero1 > criterio hechizero2 = hechizero1
    | otherwise                                 = hechizero2

-- aca hice un caso de prueba
hechizeroMasApto :: Hechizero -> Hechizero -> (Hechizero -> Hechizero -> Hechizero) -> Hechizero
hechizeroMasApto hechizero1 hechizero2 criterio =
    criterio hechizero1 hechizero2

mayorNivelTryhard :: Hechizero -> Hechizero -> Hechizero
mayorNivelTryhard hechizero1 hechizero2 
    | nivelTryhard hechizero1 > nivelTryhard hechizero2 = hechizero1
    | otherwise                                         = hechizero2

mayorNivelBurocratico :: Hechizero -> Hechizero -> Hechizero
mayorNivelBurocratico hechizero1 hechizero2 
    | nivelBurocratrico hechizero1 > nivelBurocratrico hechizero2 = hechizero1
    | otherwise                                                   = hechizero2  

mayorNivelIntimidante :: Hechizero -> Hechizero -> Hechizero
mayorNivelIntimidante hechizero1 hechizero2 
    | nivelIntimidante hechizero1 > nivelIntimidante hechizero2 = hechizero1
    | otherwise                                                 = hechizero2 

mayorNivelDeSigilo :: Hechizero -> Hechizero -> Hechizero
mayorNivelDeSigilo hechizero1 hechizero2 
    | nivelDeSigilo hechizero1 > nivelDeSigilo hechizero2 = hechizero1
    | otherwise                                                 = hechizero2 

-- CRITERIOS
-- nivelTryhard
nivelTryhard :: Hechizero -> Number
nivelTryhard hechizero = 1 `div` (grado hechizero + 1)

nivelTryhard' :: Hechizero -> Number
nivelTryhard' = (1/) . ((+1) . grado) 

-- nivelBurocratrico
nivelBurocratrico :: Hechizero -> Number
nivelBurocratrico = length . clan

-- nivelIntimidante
nivelIntimidante :: Hechizero -> Char
nivelIntimidante = maximum . clan
-- maximum toma una lista de cosas que se pueden poner en algún tipo de orden y devuelve el elemento más grande.
-- (en este caso agarra el clan y me tira la letra mas grande)

--letraMayor :: String -> Char
--letraMayor palabra = maximum palabra

-- nivelDeSigilo 
nivelDeSigilo :: Hechizero -> Number
nivelDeSigilo hechicero = antiguedad hechicero * 6

nivelDeSigilo' :: Hechizero -> Number
nivelDeSigilo' = (*6) . antiguedad 