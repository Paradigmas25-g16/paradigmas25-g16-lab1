import Dibujo
import Data.Foldable (Foldable(fold))

type Pred a = a -> Bool

--Para la definiciones de la funciones de este modulo, no pueden utilizar
--pattern-matching, sino alto orden a traves de la funcion foldDib, mapDib 

-- Dado un predicado sobre básicas, cambiar todas las que satisfacen
-- el predicado por el resultado de llamar a la función indicada por el
-- segundo argumento con dicha figura.
-- Por ejemplo, `cambiar (== Triangulo) (\x -> Rotar (Basica x))` rota
-- todos los triángulos.
cambiar :: Pred a -> a -> Dibujo a -> Dibujo a
cambiar pred chng = mapDib (\x -> if pred x then chng else x)

-- Alguna básica satisface el predicado.
anyDib :: Pred a -> Dibujo a -> Bool
anyDib p = foldDib
                p
                id
                id
                id
                (\_ _ d1 d2 -> d1 || d2)
                (\_ _ d1 d2 -> d1 || d2)
                (||)

-- Todas las básicas satisfacen el predicado.
allDib :: Pred a -> Dibujo a -> Bool
allDib p = foldDib  
                p
                id
                id
                id
                (\_ _ d1 d2 -> d1 && d2)
                (\_ _ d1 d2 -> d1 && d2)
                (&&)


-- Hay 4 rotaciones seguidas.
esRot360 :: Pred (Dibujo a)
esRot360 = fst . foldDib 
                    (const (False,0))
                    (\(b,n) -> (b || n+1==4 , n+1))
                    (const (False,0))
                    (const (False,0))
                    (\_ _ (b1,_) (b2,_) -> (b1 || b2,0))
                    (\_ _ (b1,_) (b2,_) -> (b1 || b2,0))
                    (\(b1,_) (b2,_) -> (b1||b2,0))

-- Hay 2 espejados seguidos.
esFlip2 :: Pred (Dibujo a)
esFlip2 = fst . foldDib 
                    (const (False,0))
                    (const (False,0))
                    (const (False,0))
                    (\(b,n) -> (b || n+1==2 , n+1))
                    (\_ _ (b1,_) (b2,_) -> (b1 || b2,0))
                    (\_ _ (b1,_) (b2,_) -> (b1 || b2,0))
                    (\(b1,_) (b2,_) -> (b1||b2,0))


data Superfluo = RotacionSuperflua | FlipSuperfluo

---- Chequea si el dibujo tiene una rotacion superflua
errorRotacion :: Dibujo a -> [Superfluo]
errorRotacion dib   | esRot360 dib  = [RotacionSuperflua]
                    | otherwise = []

---- Chequea si el dibujo tiene un flip superfluo
errorFlip :: Dibujo a -> [Superfluo]
errorFlip dib   | esFlip2 dib = [FlipSuperfluo]
                | otherwise = []

---- Aplica todos los chequeos y acumula todos los errores, y
---- sólo devuelve la figura si no hubo ningún error.
checkSuperfluo :: Dibujo a -> Either [Superfluo] (Dibujo a)
checkSuperfluo d = if null (errorRotacion d ++ errorFlip d) then Right d else Left (errorRotacion d ++ errorFlip d)