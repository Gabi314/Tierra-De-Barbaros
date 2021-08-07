import Text.Show.Functions
import Data.Char
import Data.List

someFunc :: IO ()
someFunc = putStrLn "someFunc"


{--
Punto 1

Se sabe que los bárbaros tienen nombre, fuerza, habilidades y objetos, que los ayudarán más adelante en su lucha contra el mal. 
Por ejemplo: 

dave = Barbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas]

--}

data Barbaro = Barbaro {
    nombreBarbaro :: String,
    fuerzaBarbaro :: Int,
    habilidadesBarbaro :: [Habilidad],
    objetosBarbaro :: [Objeto]
}deriving(Show)

type Habilidad = String
type Objeto = Barbaro->Barbaro

-- Se pide definir los siguientes objetos y definir algunos bárbaros de ejemplo

añadirHabilidad :: Habilidad -> Barbaro -> [Habilidad]
añadirHabilidad unaHabilidad  = (:) unaHabilidad.habilidadesBarbaro 

--Las espadas aumentan la fuerza de los bárbaros en 2 unidades por cada kilogramo de peso.

espadas :: Int->Objeto
espadas peso unBarbaro = unBarbaro {fuerzaBarbaro = (+)((*2) peso).fuerzaBarbaro $ unBarbaro } 

--Los amuletosMisticos puerco-marranos otorgan una habilidad dada a un bárbaro.

amuletosMisticos :: Habilidad -> Objeto
amuletosMisticos unaHabilidad unBarbaro = unBarbaro{habilidadesBarbaro = añadirHabilidad unaHabilidad unBarbaro }

-- Las varitasDefectuosas, añaden la habilidad de hacer magia, pero desaparecen todos los demás objetos del bárbaro.

varitasDefectuosas:: Habilidad -> Objeto
varitasDefectuosas unaHabilidad unBarbaro = unBarbaro {habilidadesBarbaro = añadirHabilidad "hacer magia" unBarbaro, objetosBarbaro=[] }

--Una ardilla, que no hace nada.
ardilla :: Objeto
ardilla  = id

-- Una cuerda, que combina dos objetos distintos,obteniendo uno que realiza las transformaciones de los otros dos. 
cuerda :: Objeto->Objeto->Objeto
cuerda  = (.)   

{--
Punto 2

El megafono es un objeto que potencia al bárbaro, concatenando sus habilidades y poniéndolas en mayúsculas. 

*Main> megafono dave
Barbaro "Dave" 100 ["TEJERESCRIBIRPOESIA"] [<function>,<function>]

Sabiendo esto, definir al megafono, y al objeto megafonoBarbarico, que está formado por una cuerda, una ardilla y un megáfono. 
--}



megafono :: Objeto
megafono unBarbaro = unBarbaro{habilidadesBarbaro = [map toUpper.concat.habilidadesBarbaro $ unBarbaro] } 

megafonoBarbarico :: Objeto
megafonoBarbarico  =  ardilla.megafono

{--
Punto 3 - Aventuras 

Los bárbaros suelen ir de aventuras por el reino luchando contra las fuerzas del mal, pero ahora que tienen nuestra ayuda, 
quieren que se les diga si un grupo de bárbaros puede sobrevivir a cierta aventura.  
Una aventura se compone de uno o más eventos, por ejemplo:
--}

type Evento =  (Barbaro->Bool)
type Aventura = [Evento]
--invasionDeSuciosDuendes: Un bárbaro sobrevive si sabe “Escribir Poesía Atroz”
invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes = elem "Escribir Poesía Atroz".habilidadesBarbaro 

--cremalleraDelTiempo: Un bárbaro sobrevive si no tiene pulgares. Los bárbaros llamados Faffy y Astro no tienen pulgares, los demás sí. 

esNombreDeBarbaroConPulgares :: String -> Bool
esNombreDeBarbaroConPulgares "Faffy" = False
esNombreDeBarbaroConPulgares "Astro" = False
esNombreDeBarbaroConPulgares _ = True

cremalleraDelTiempo :: Evento
cremalleraDelTiempo =  not.tienePulgares 

tienePulgares :: Barbaro->Bool
tienePulgares = esNombreDeBarbaroConPulgares.nombreBarbaro  
                       
{--
ritualDeFechorias: Un bárbaro puede sobrevivir si pasa una o más pruebas como las siguientes: 
saqueo: El bárbaro debe tener la habilidad de robar y tener más de 80 de fuerza.
gritoDeGuerra: El bárbaro debe tener un poder de grito de guerra igual a la cantidad de letras de sus habilidades. 
El poder necesario para aprobar es 4 veces la cantidad de objetos del bárbaro.
caligrafia: El bárbaro tiene caligrafía perfecta (para el estándar barbárico de la época) 
si sus habilidades contienen más de 3 vocales y comienzan con mayúscula.

--}

fuerzaNecesariaSaqueo :: Int
fuerzaNecesariaSaqueo = 80

cantMinimaVocales :: Int
cantMinimaVocales = 4

vocales :: String
vocales = "aeiouAEIOU"

poderNecesarioParaAprobar :: Barbaro->Int
poderNecesarioParaAprobar = (*4).length.objetosBarbaro

cantVocales :: String->Int
cantVocales = length.intersect  vocales 

comienzaConMayus :: String->Bool
comienzaConMayus unaHabilidad =  (== (id.head $ unaHabilidad)).head $ unaHabilidad -- en lugar de id iria toUpper

todasSusHabilidadesTienenVocales :: Evento
todasSusHabilidadesTienenVocales  = all ((>=cantMinimaVocales).cantVocales).habilidadesBarbaro 

todasSusHabilidadesComienzanConMayus :: Evento
todasSusHabilidadesComienzanConMayus  = all comienzaConMayus.habilidadesBarbaro

saqueo :: Prueba
saqueo unBarbaro = ((&&).any (=="robar").habilidadesBarbaro $ unBarbaro).(>fuerzaNecesariaSaqueo).fuerzaBarbaro $ unBarbaro


gritoDeGuerra :: Prueba
gritoDeGuerra unBarbaro = ((==).poderNecesarioParaAprobar $ unBarbaro).length.habilidadesBarbaro.megafono $ unBarbaro

caligrafia :: Prueba
caligrafia unBarbaro = ((&&).todasSusHabilidadesTienenVocales $ unBarbaro).todasSusHabilidadesComienzanConMayus $ unBarbaro

type Prueba = Barbaro->Bool

ritualDeFechorias :: [Prueba]->Evento
ritualDeFechorias listaPruebas unBarbaro = any ($ unBarbaro) listaPruebas

-- Sabiendo esto, se pide: Definir los eventos, modelar las aventuras y dar un ejemplo. 
-- QUEDA PENDIENTE DAR UN EJEMPLO



-- Definir la función sobrevivientes que tome una lista de bárbaros y una aventura, y diga cuáles bárbaros la sobreviven 
-- (es decir, pasan todas las pruebas)

sobrevivientes :: [Barbaro]->Aventura->[Barbaro]
sobrevivientes listaBarbaros unaAventura = filter (flip sobreviveAventura unaAventura) listaBarbaros

sobreviveAventura :: Barbaro->Aventura->Bool
sobreviveAventura unBarbaro = all ($ unBarbaro)   

{--
Punto 4 - Dinastía

A - Los bárbaros se marean cuando tienen varias habilidades iguales. 
Por todo esto, nos piden desarrollar una función que elimine los elementos repetidos de una lista (sin utilizar nub ni nubBy)

> sinRepetidos [1,2,3,4,4,5,5,6,7]
[1,2,3,4,5,6,7]

Nota: Puede usarse recursividad para este punto.

--}

sinRepetidos :: Eq a => [a]->[a]
sinRepetidos [] = []
sinRepetidos (x:xs) 
                    | elem x xs = sinRepetidos xs
                    | otherwise = x:sinRepetidos xs
                                    
{--
B - Los bárbaros son una raza muy orgullosa, tanto que quieren saber cómo van a ser sus descendientes y asegurarse 
de que los mismos reciban su legado.

El descendiente de un bárbaro comparte su nombre, y un asterisco por cada generación. Por ejemplo "Dave*", "Dave**" , "Dave***" , etc. 

Además, tienen en principio su mismo poder, habilidades sin repetidos, y los objetos de su padre, pero antes de pasar a la siguiente 
generación, utilizan (aplican sobre sí mismos) los objetos. Por ejemplo, el hijo de Dave será equivalente a:

(ardilla.varitasDefectuosas) (Barbaro "Dave*" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas])


Definir la función descendientes, que dado un bárbaro nos de sus infinitos descendientes. 
--}                                    
cambiarNombre :: Objeto
cambiarNombre unBarbaro = unBarbaro {nombreBarbaro = (++"*").nombreBarbaro $ unBarbaro } 

controlarHabilidades :: Objeto
controlarHabilidades unBarbaro = unBarbaro{habilidadesBarbaro = sinRepetidos.habilidadesBarbaro $ unBarbaro }


descendientes :: Barbaro -> [Barbaro]
descendientes unBarbaro = iterate (cambiarNombre.controlarHabilidades.flip (foldr ($)) (objetosBarbaro unBarbaro)) unBarbaro

-- C. Pregunta: ¿Se podría aplicar sinRepetidos sobre la lista de objetos? ¿Y sobre el nombre de un bárbaro? ¿Por qué?

-- No se podría aplicar sobre la lista de objetos porque los objetos (al ser funciones) no son de tipo Eq (no se pueden comparar) 
-- Sí, sobre el nombre de un bárbaro sí ya que los caracteres son comparables (de tipo Eq)