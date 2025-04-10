# paradigmas25-g16-lab1

## Integrantes:
- Vazquez Joaquin
- Peralta Mariano Daniel
- Scavuzo Marco Andres
- Quevedo Tomás Luciano

## Explicaciones:
- <strong>Main.hs</strong>, es el archivo principal del proyecto, se compone de los demas y configura el entorno para dibujar un Dibujo Basico o Escher.

- <strong>Interp.hs</strong>, este utilizara la liberia Gloss y mediante algunas operaciones vectoriales, interpretara las funciones del tipo data Dibujo

- <strong>Pred.hs</strong>, este se encarga de de verificar un predicado y realizar la operacion designada, se maneja de alto orden utilizando las funciones foldDib y mapDib

- <strong>Dibujo.hs</strong>, este contiene y realiza las operaciones del tipo data Dibujo

- Basica/<strong>BasicaDoble.hs</strong>, este compone un Dibujo Basico de dos figuras Basicas

- Basica/<strong>Comun.hs</strong>, contiene las funciones para dibujar un triangulo, trianguloVioleta, trianguloRecto, trianguloBorde y la letra "F"

- Basica/<strong>Ejemplo.hs</strong>, este archivo es donde se colocan las funciones de <strong>Comun.hs</strong> y las operaciones sobre las mismas.

- Basica/<strong>Escher.hs</strong>, contiene las funciones que se utilizaran para realizar Dibujos como fractales mediante las figuras escher(fish, triagunlo, etc.)

## how to use:
```bash
$ cd paradigmas25-g16-lab1/
$ ghci
> :l Main.hs
> main
```

Por defecto dibujara un Dibujo Escher, con la figura "Vacio"