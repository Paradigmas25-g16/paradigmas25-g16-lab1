module Dibujo where

-- Definir el lenguaje via constructores de tipo
data Dibujo a = Basica a 
              | Rotar (Dibujo a)
              | Rotar45 (Dibujo a)
              | Espejar (Dibujo a)
              | Apilar Float Float (Dibujo a) (Dibujo a)
              | Juntar Float Float (Dibujo a) (Dibujo a)
              | Encimar (Dibujo a) (Dibujo a)
                     deriving (Eq, Show)

data FigBasicas = Rectangulo
                 | Triangulo
                 | Circulo
                 deriving (Eq, Show)




-- Composición n-veces de una función con sí misma.
comp :: (a -> a) -> Int -> a -> a
comp f 0 x = f x
comp f n x  = comp f (n-1) (f x)


-- Rotaciones de múltiplos de 90.
r180 :: Dibujo a -> Dibujo a
r180 = Rotar . Rotar

r270 :: Dibujo a -> Dibujo a
r270 = Rotar . r180


-- Pone una figura sobre la otra, ambas ocupan el mismo espacio.
(.-.) :: Dibujo a -> Dibujo a -> Dibujo a
(.-.) = Apilar 1 1


-- Pone una figura al lado de la otra, ambas ocupan el mismo espacio.
(///) :: Dibujo a -> Dibujo a -> Dibujo a
(///) = Juntar 1 1

-- Superpone una figura con otra.
(^^^) :: Dibujo a -> Dibujo a -> Dibujo a
(^^^) = Encimar
 
-- Dadas cuatro dibujos las ubica en los cuatro cuadrantes.
cuarteto :: Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a -> Dibujo a
cuarteto x y z w = Apilar 1 1 (Juntar 1 1 x y) (Juntar 1 1 z w)

-- Una dibujo repetido con las cuatro rotaciones, superpuestas.
encimar4 :: Dibujo a -> Dibujo a
encimar4 x = x ^^^ Rotar x ^^^ r180 x ^^^ r270 x


-- Cuadrado con la misma figura rotada i * 90, para i ∈ {0, ..., 3}.
-- No confundir con encimar4!
ciclar :: Dibujo a -> Dibujo a
ciclar x = cuarteto x (Rotar x) (r180 x) (r270 x)


-- Transfomar un valor de tipo a como una Basica.
pureDib :: a -> Dibujo a
pureDib = Basica

-- map para nuestro lenguaje.
mapDib :: (a -> b) -> Dibujo a -> Dibujo b
mapDib f (Basica x) = Basica (f x)
mapDib f (Rotar x) = Rotar (mapDib f x)
mapDib f (Rotar45 x) = Rotar45 (mapDib f x)
mapDib f (Espejar x) = Espejar (mapDib f x)
mapDib f (Apilar afl bfl x y) = Apilar afl bfl (mapDib f x) (mapDib f y)
mapDib f (Juntar afl bfl x y) = Juntar afl bfl (mapDib f x) (mapDib f y)
mapDib f (Encimar x y )= Encimar (mapDib f x) (mapDib f y)


-- Funcion de fold para Dibujos a
foldDib :: (a -> b) -> (b -> b) -> (b -> b) -> (b -> b) ->
       (Float -> Float -> b -> b -> b) -> 
       (Float -> Float -> b -> b -> b) -> 
       (b -> b -> b) ->
       Dibujo a -> b 
foldDib basic rot rot45 esp api junt enc (Basica d) = basic d
foldDib basic rot rot45 esp api junt enc (Rotar d) =
              rot (foldDib basic rot rot45 esp api junt enc d)
foldDib basic rot rot45 esp api junt enc (Rotar45 d) =
              rot45 (foldDib basic rot rot45 esp api junt enc d)
foldDib basic rot rot45 esp api junt enc (Espejar d) =
              esp (foldDib basic rot rot45 esp api junt enc d)
foldDib basic rot rot45 esp api junt enc (Apilar x y d1 d2) =
              api x y (foldDib basic rot rot45 esp api junt enc d1)
                      (foldDib basic rot rot45 esp api junt enc d2)
foldDib basic rot rot45 esp api junt enc (Juntar x y d1 d2) =
              junt x y (foldDib basic rot rot45 esp api junt enc d1)
                       (foldDib basic rot rot45 esp api junt enc d2)
foldDib basic rot rot45 esp api junt enc (Encimar d1 d2) =
              enc (foldDib basic rot rot45 esp api junt enc d1)
                  (foldDib basic rot rot45 esp api junt enc d2)



