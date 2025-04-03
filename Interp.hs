-- Sacar del esqueleto final!
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}       --delete this
{-# HLINT ignore "Use camelCase" #-}                --delete this
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
interp_rotar img vi vj vk = Graphics.Gloss.rotate 90 (img vi vj vk)

--interpreta el operador de espejar
interp_espejar :: ImagenFlotante -> ImagenFlotante
interp_espejar img vi vj vk = scale (-1) 1 (img vi vj vk)

--interpreta el operador de rotacion 45
interp_rotar45 :: ImagenFlotante -> ImagenFlotante
interp_rotar45 img vi vj vk = Graphics.Gloss.rotate 45 (img vi vj vk)

--interpreta el operador de apilar
interp_apilar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_apilar n m img1 img2 vi vj vk = pictures [img1 vi (mitad vj) vk, img2 (vi V.+ mitad vj) vj vk]

--interpreta el operador de juntar
interp_juntar :: Float -> Float -> ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_juntar n m img1 img2 vi vj vk = pictures [img1 vi (mitad vj) vk, img2 (vi V.+ mitad vj) (mitad vj) vk]

--interpreta el operador de encimar
interp_encimar :: ImagenFlotante -> ImagenFlotante -> ImagenFlotante
interp_encimar img1 img2 vi vj vk = pictures [img1 vi vj vk, img2 vi vj vk]

--interpreta cualquier expresion del tipo Dibujo a
--utilizar foldDib 
interp :: Interpretacion a -> Dibujo a -> ImagenFlotante
interp f (Basica x) = f x
interp f (Rotar d) = interp_rotar (interp f d)
interp f (Rotar45 d) = interp_rotar45 (interp f d)
interp f (Espejar d) = interp_espejar (interp f d)
interp f (Apilar n m d1 d2) = interp_apilar n m (interp f d1) (interp f d2)
interp f (Juntar n m d1 d2) = interp_juntar n m (interp f d1) (interp f d2)
interp f (Encimar d1 d2) = interp_encimar (interp f d1) (interp f d2)

