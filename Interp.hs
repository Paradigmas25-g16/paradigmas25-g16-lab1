-- Sacar del esqueleto final!
module Interp where
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as V


import Dibujo
import Data.Bits (Bits(rotate))

-- Gloss provee el tipo Vector y Picture.
type ImagenFlotante = Vector -> Vector -> Vector -> Picture
type Interpretacion a = a -> ImagenFlotante

mitad :: Vector -> Vector
mitad = (0.5 V.*)

-- Interpretaciones de los constructores de Dibujo

--interpreta el operador de rotacion
interp_rotar :: ImagenFlotante -> ImagenFlotante
interp_rotar img d w h = img (d V.+ w) h ((-1) V.* w)

--interpreta el operador de espejar
interp_espejar :: ImagenFlotante -> ImagenFlotante
interp_espejar img d w h = img (d V.+ w) ((-1) V.* w) h

--interpreta el operador de rotacion 45
interp_rotar45 :: ImagenFlotante -> ImagenFlotante
interp_rotar45 img d w h = img (d V.+ (mitad (w V.+ h))) (mitad (w V.+ h)) (mitad (h V.- w))

--interpreta el operador de apilar
interp_apilar :: Float -> Float ->
                 ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_apilar m n img1 img2 d w h =
    pictures[img1 (d V.+h') w (r V.* h) ,img2 d w h']
        where   r' = n/(m+n)
                r = m/(m+n)
                h'= r' V.* h

--interpreta el operador de juntar
interp_juntar :: Float -> Float ->
                 ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_juntar m n img1 img2 d w h =
    pictures[img1 d w' h , img2 (d V.+ w') (r' V.* w) h]
        where   r' = n/(m+n)
                r = m/(m+n)
                w' = r V.* w

--interpreta el operador de encimar
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_encimar img1 img2 d w h = pictures [img1 d w h, img2 d w h]

--interpreta cualquier expresion del tipo Dibujo a
--utilizar foldDib 
interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp f = foldDib f
                    interp_rotar
                    interp_rotar45
                    interp_espejar
                    interp_apilar
                    interp_juntar
                    interp_encimar
                    
