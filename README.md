# README — Punto 1 (Ordenamiento) y Punto 2 (Gestión dinámica en C)


---

## Descripción

* **Punto 1:** Ordenar una lista de estudiantes (nombre, nota) por nota en **orden descendente**, y en caso de empate, por **nombre ascendente**. Implementaciones incluidas: **Python (imperativo)** y **Haskell (funcional)**.

* **Punto 2:** Sistema en **C** que gestiona registros de estudiantes usando memoria dinámica (`malloc`/`free`), cadenas dinámicas, arrays de calificaciones ajustados al número de materias y un mecanismo de compactación simple.

---

## Estructura de archivos

* `imperativo.py`  — Implementación en Python (imperativo).
* `imperativo.py`  — Implementación en Haskell (funcional, compilable).
* `gestion_estudiantes.c` — Implementación en C (malloc/free, compactación).
* `ParcialPunto1.pdf`  — Documento LaTeX para Overleaf con enunciado, código y análisis.

---

## Instrucciones de uso

### Punto 1 — Python (imperativo)

1. Guarda el archivo `imperativo.py` en tu máquina.
2. Ejecuta:

```bash
 imperativo.py
```

### Punto 1 — Haskell (funcional)

Guarda `imperativo.py`. 

* Compilar y ejecutar:

```bash
ghc funcional.hs -o ordenar
./ordenar
```

### Punto 2 — C (gestión dinámica)

Guarda `GestionEstudiantes.c` y compila con `gcc`:


En Windows (PowerShell/CMD):

```powershell
gcc GestionEstudiantes.c -o gestion_estudiantes.exe
gestion_estudiantes.exe
```


**Comportamiento importante:**

* El programa utiliza `malloc` para asignar exactamente `strlen(nombre)+1` bytes para cadenas.
* Las calificaciones se almacenan en un array dinámico `float *` de tamaño `num_materias`.
* Para eliminar un estudiante se libera memoria de `nombre`, `apellido`, `calificaciones` y de la estructura.
* La compactación elimina huecos (`NULL`) en el array de punteros usando `realloc`.

### Punto 3 Implementación del Promedio en Cálculo Lambda

## Introducción

Este documento detalla la implementación del cálculo del promedio de una lista de números utilizando la notación de cálculo lambda. Se toma como referencia un ejemplo en Haskell para ilustrar la equivalencia y la transformación de un enfoque imperativo/funcional a uno puramente declarativo y abstracto, inherente al cálculo lambda.

El cálculo lambda, desarrollado por Alonzo Church, es un sistema formal en lógica matemática y teoría de la computación que se utiliza para expresar la computación basada en la abstracción de funciones y su aplicación. Es un modelo universal de computación que puede simular cualquier máquina de Turing, lo que lo convierte en una base teórica fundamental para los lenguajes de programación funcionales.

## Conceptos Fundamentales del Cálculo Lambda

Antes de sumergirnos en la implementación, es crucial entender algunos conceptos básicos del cálculo lambda:

*   **Variables:** Símbolos que representan valores.
*   **Abstracción (funciones):** La creación de una función. Se denota con la letra griega lambda (λ). Por ejemplo, `λx.M` define una función que toma un argumento `x` y produce el cuerpo `M`.
*   **Aplicación:** La aplicación de una función a un argumento. Por ejemplo, `(λx.M) N` significa aplicar la función `λx.M` al argumento `N`.

El cálculo lambda se basa en tres reglas principales:

1.  **α-conversión:** Renombrar variables ligadas. Por ejemplo, `λx.x` es equivalente a `λy.y`.
2.  **β-reducción:** La aplicación de una función. Es la regla más importante y define cómo se evalúan las expresiones lambda. `(λx.M) N` se reduce a `M[x:=N]`, donde `M[x:=N]` significa reemplazar todas las ocurrencias libres de `x` en `M` por `N`.
3.  **η-conversión:** Extensionalidad. `λx.(M x)` es equivalente a `M` si `x` no es una variable libre en `M`.

## Análisis del Código Haskell Original

El código Haskell proporcionado en el problema calcula el promedio de una lista de números. Desglosemos sus componentes clave:

```haskell
promedio xs = realToFrac (sum xs) / genericLength xs

main :: IO ()
main = do
  putStrLn "Ingrese una lista de números separados por espacios:"
  entrada <- getLine
  let numeros = map read (words entrada) :: [Double]
  if null numeros
  then putStrLn "La lista está vacía, no se puede calcular el promedio."
  else putStrLn $ "El promedio es: " ++ show (promedio numeros)
```

La función principal para el cálculo del promedio es `promedio xs = realToFrac (sum xs) / genericLength xs`. Aquí:

*   `sum xs`: Calcula la suma de todos los elementos en la lista `xs`.
*   `genericLength xs`: Calcula la longitud de la lista `xs`. Se usa `genericLength` en lugar de `length` para asegurar que el tipo de retorno sea numérico y compatible con la división de punto flotante.
*   `realToFrac`: Convierte el resultado de `sum xs` a un tipo de punto flotante (Fractional) para permitir una división precisa.
*   `/`: Operador de división.

El bloque `main` maneja la interacción con el usuario, solicitando una lista de números, procesándolos y mostrando el promedio o un mensaje de error si la lista está vacía. Para la implementación en cálculo lambda, nos centraremos en la lógica de la función `promedio`, ya que el cálculo lambda se ocupa de la computación de funciones puras, no de efectos secundarios como la entrada/salida.

## Implementación del Promedio en Cálculo Lambda

Para implementar el promedio en cálculo lambda, necesitamos representar la suma y la longitud de una lista, así como la división. El cálculo lambda no tiene operaciones aritméticas o estructuras de datos incorporadas directamente; estas deben ser codificadas utilizando solo abstracciones y aplicaciones de funciones. Esto se logra típicamente mediante codificaciones como los numerales de Church para números naturales y las tuplas de Church para estructuras de datos.

### Codificación de Números Naturales (Numerales de Church)

Un numeral de Church `n` es una función de orden superior que toma una función `f` y un argumento `x`, y aplica `f` a `x`, `n` veces. Es decir, `n = λf.λx.f^n x`.

*   `0 = λf.λx.x`
*   `1 = λf.λx.f x`
*   `2 = λf.λx.f (f x)`
*   `3 = λf.λx.f (f (f x))`

### Operaciones Aritméticas Básicas con Numerales de Church

*   **Sucesor (S):** `S = λn.λf.λx.f (n f x)`
*   **Suma (+):** `+ = λm.λn.λf.λx.m f (n f x)` o `+ = λm.λn.m S n`
*   **Multiplicación (*):** `* = λm.λn.λf.m (n f)`

### Codificación de Listas

Las listas pueden ser representadas de varias maneras en cálculo lambda. Una forma común es usar la codificación de Church para pares (tuplas) y la recursión. Sin embargo, para simplificar, podemos pensar en la suma y la longitud como funciones que operan sobre una secuencia de elementos.

### La Función `SUMA` en Cálculo Lambda

La suma de una lista de números `[n1, n2, ..., nk]` puede verse como una aplicación repetida de la operación de suma. En cálculo lambda, esto se puede lograr a través de recursión (usando el combinador de punto fijo Y) o, más simplemente para una lista finita, como una composición de sumas binarias.

Para una lista de `n` números, `xs = [x1, x2, ..., xn]`, la suma sería `x1 + x2 + ... + xn`.

Consideremos una función `SUMA` que toma una lista (representada de alguna manera) y devuelve su suma. Si la lista es `[x1, x2, x3]`, `SUMA` podría ser conceptualmente `λlist. (ADD (HEAD list) (SUMA (TAIL list)))`.

### La Función `LONGITUD` en Cálculo Lambda

Similar a la suma, la longitud de una lista `[x1, x2, ..., xn]` es `n`. Esto se puede conceptualizar como una función que cuenta los elementos.

Consideremos una función `LONGITUD` que toma una lista y devuelve su longitud. Si la lista es `[x1, x2, x3]`, `LONGITUD` podría ser conceptualmente `λlist. (SUCC (LONGITUD (TAIL list)))` hasta llegar a una lista vacía, cuya longitud es 0.

### La Función `DIVISION` en Cálculo Lambda

La división es más compleja de representar directamente con numerales de Church puros, ya que los numerales de Church solo representan números naturales y la división puede resultar en números racionales. Para manejar la división, a menudo se extiende el cálculo lambda con tipos de datos para números racionales o se asume un entorno donde la división ya está definida.

Sin embargo, si asumimos que tenemos funciones `SUMA_CHURCH` y `LONGITUD_CHURCH` que operan sobre numerales de Church y devuelven numerales de Church, y una función `DIVIDE_CHURCH` que realiza la división, la estructura del promedio sería:

`PROMEDIO = λlist. (DIVIDE_CHURCH (SUMA_CHURCH list) (LONGITUD_CHURCH list))`

### Ejemplo Simplificado: Promedio de dos números

Para ilustrar la idea sin la complejidad de la recursión y la representación de listas completas, consideremos el promedio de dos números `a` y `b`:

`PROMEDIO_DOS = λa.λb. (DIVIDE (ADD a b) TWO)`

Donde:

*   `ADD` es la función de suma para numerales de Church.
*   `TWO` es el numeral de Church para 2 (`λf.λx.f (f x)`).
*   `DIVIDE` es una función de división (asumiendo su existencia en un contexto extendido).

### Conclusión de la Implementación

La implementación completa del promedio en cálculo lambda, que maneje listas de tamaño `n` y números racionales, requeriría una construcción detallada de:

1.  **Numerales de Church** para representar los números.
2.  **Operaciones aritméticas** (suma, división) sobre estos numerales.
3.  **Codificación de listas** y funciones para acceder a sus elementos (`HEAD`, `TAIL`) y determinar si están vacías (`IS_EMPTY`).
4.  Un **combinador de punto fijo (Y)** para implementar la recursión necesaria para `SUMA` y `LONGITUD`.

Debido a la naturaleza fundamental del cálculo lambda, donde todo se construye a partir de funciones, la representación de conceptos como 


números y listas es un ejercicio complejo pero fundamental para entender los fundamentos de la computación.

La expresión general para el promedio de una lista `L` de `n` elementos `x_i` en cálculo lambda sería:

`PROMEDIO = λL. DIVIDE (SUMA L) (LONGITUD L)`

Donde:

*   `SUMA` es una función lambda que, dada una lista codificada, devuelve la suma de sus elementos (también codificada en lambda).
*   `LONGITUD` es una función lambda que, dada una lista codificada, devuelve su longitud (también codificada en lambda).
*   `DIVIDE` es una función lambda que realiza la división entre dos números codificados en lambda.

La construcción de `SUMA`, `LONGITUD` y `DIVIDE` a partir de los numerales de Church y las codificaciones de listas es un proceso extenso que involucra el uso de combinadores de punto fijo para la recursión y la definición de operaciones aritméticas sobre los numerales de Church. Dado que el cálculo lambda puro solo maneja funciones, todas las estructuras de datos y operaciones deben ser construidas a partir de estas funciones básicas.

Por ejemplo, la función `SUMA` para una lista `L` podría ser definida recursivamente (usando el combinador Y para la recursión) como:

`SUMA = Y (λs. λl. IF (IS_EMPTY l) ZERO (ADD (HEAD l) (s (TAIL l))))`

Donde:

*   `Y` es el combinador de punto fijo.
*   `IS_EMPTY`, `ZERO`, `ADD`, `HEAD`, `TAIL` son funciones y constantes que deben ser definidas en cálculo lambda puro.

De manera similar, la función `LONGITUD` podría ser:

`LONGITUD = Y (λlen. λl. IF (IS_EMPTY l) ZERO (SUCC (len (TAIL l))))`

Donde `SUCC` es la función sucesor para numerales de Church.

La `DIVIDE` función es la más compleja, ya que requiere la representación de números racionales o la aproximación de la división. En un contexto puramente lambda, esto se logra a menudo mediante la codificación de pares de números (numerador y denominador) para representar fracciones, y luego definiendo la aritmética sobre estas fracciones.

## Referencias

[1] Church, A. (1941). *The Calculi of Lambda-Conversion*. Princeton University Press.
[2] Barendregt, H. P. (1984). *The Lambda Calculus: Its Syntax and Semantics*. North-Holland.

---


