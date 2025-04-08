module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Display
import Graphics.UI.GLUT.Begin
import Dibujo
import Interp
import qualified Basica.Ejemplo as E
import qualified Basica.Escher as J
import qualified Graphics.Gloss.Data.Point.Arithmetic as V

--Funciones para rellenar el fondo de la imagen inicial

-- comprender esta función es un buen ejericio.
lineasH :: Vector -> Float -> Float -> [Picture]
lineasH origen@(x, y) longitud separacion = map (lineaH . (*separacion)) [0..]
  where lineaH h = line [(x, y + h), (x + longitud, y + h)]

-- Una grilla de n líneas, comenzando en origen con una separación de sep y
-- una longitud de l (usamos composición para no aplicar este
-- argumento)
grilla :: Int -> Vector -> Float -> Float -> Picture
grilla n origen sep l = pictures [ls, lsV]
  where ls = pictures $ take (n+1) $ lineasH origen sep l
        lsV = translate 0 (l*toEnum n) (rotate 90 ls)

-- Configuración para interpretar un dibujo
data Conf a = Conf {
    basic :: a -> ImagenFlotante
  , fig  :: Dibujo a
  , width :: Float
  , height :: Float
  , r :: Picture -> Picture  -- Reposicionar figura
  }

ejEscherTriangulo ancho alto = Conf {
                basic = J.interpBasEsch
              , fig = J.escher 10 J.Triangulo
              , width = ancho
              , height = alto
              , r = id
              }

ejEscherFish ancho alto = Conf {
                basic = J.interpBasEsch
              , fig = J.escher 10 J.Fish
              , width = ancho
              , height = alto
              , r = id
              }

-- Dada una computación que construye una configuración, mostramos por
-- pantalla la figura de la misma de acuerdo a la interpretación para
-- las figuras básicas. Permitimos una computación para poder leer
-- archivos, tomar argumentos, etc.
inicial :: IO (Conf J.Escher) -> IO ()
inicial cf = cf >>= \cfg ->
    let ancho  = (width cfg, 0)
        alto  = (0, height cfg)
        imagen = interp (basic cfg) (fig cfg) (0, 0) ancho alto
    in display win white . withGrid $ imagen
  where grillaGris = color grey $ grilla 10 (0, 0) 100 10
        withGrid p = pictures [p, grillaGris]
        grey = makeColorI 120 120 120 120

win = InWindow "Paradigmas 2025 - Lab1" (500, 500) (0, 0)
main = inicial $ return (ejEscherTriangulo 100 100)
