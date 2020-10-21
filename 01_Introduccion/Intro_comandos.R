
# 01 - Intro

3+3

sqrt(3)

# líneas que empiezan con una almohadilla son consideradas como comentarios.
# los comentarios son tan importantes como el código de un script.
# Comentad todo lo que hagáis, siempre!

# el logaritmo de un número se calcula con log()
log(5)

# EJERCICIO
# Crea un script para almacenar los comandos que usaremos esta tarde.

# la asignación se hace con "<-"
x <- 3+3

x

# "=" también funciona, pero no es recomendable
x = 3+3

# ¿por qué <- y no =?
# probad esto:
# mean(y = 1:10)
#vs.
# mean(y <- 1:10)

y <- x + 5

# suma, resta, multiplicación, división
x+y
x-y
x*y
x/y

# exponencial
x^y

# logaritmo, del cual podemos especificar la base
log(x, base = 10)

# raiz cuadrada
sqrt(x)

# etc. Podemos combinar operaciones siguiendo las reglas 
# matemáticas de precedencia
x + y*3
log(x*y)^2


# ¿es x igual a y?
x == y

# ¿es x diferente de y?
x != y

# ¿es x menor que y?
x < y

# ¿es x mayor o igual que y?
x >= y

# si queremos saber si una variable es mayor que 1 pero, 
# a la vez, menor que 10...
# usamos el operador lógico AND
(x > 1) & (x < 10)

# una variable menor de 1 o mayor de 10
# operador OR
(x < 1) | (x > 10)

# un signo de exclamación al principio de una condición 
# se interpreta como la negación de la misma
# por ejemplo, ¿es x menor que 1?
(x < 1)
# y en este caso, literalmente, ¿es x NO menor que 1?
!(x < 1)

# nuestra variable es un número entero, 
# que es una variante del modo "numeric"
str(x)

# los números reales son otra variante del modo "numeric"
str(3.5)

# para crear un número complejo, tenemos que especificar 
# su parte real y su parte imaginaria
z <- complex(real = 1, imaginary = 1)
str(z)

# y también podemos asignar variables a caracteres
a <- "mi variable"
str(a)

# los datos booleanos ya los hemos visto más arriba: 
# son tipos especiales que pueden tomar valores "TRUE" o "FALSE"
# EJERCICIO: crea una condición usando la variable "a" 
# y asígnala a una nueva variable

#4 formas de escribir el mismo vector
myvector <- c(1, 2, 3, 4, 5)
myvector <- c(1:5)
myvector <- 1:5
myvector #teclea my y tab para autocompletar

charvector <- c("caracter1","caracter2","caracter3")

# crear un vector con cien repeticiones del número 1
vec1 <- rep(1,100) # literalmente: REPite 1, 100 veces

# crear una secuencia 10, 20, 30,... hasta 100
vec2 <- seq(from = 10, to = 100, by = 10)
# literalmente: desde 10, hasta 100, de 10 en 10

vec2 + 1
vec2 * vec2
log(vec2) 

# podemos sumar todos los elementos de un vector
sum(vec2)

# también podemos crear nuevos vectores a partir de otros
vec3 <- c(vec2, vec2)

length(vec3)

# los dos vectores son del mismo tamaño, 
# así que la multiplicación es elemento a elemento
1:5 * 1:5
# ¿qué pasa si multiplicamos vectores de tamaños diferentes?
1:5 * 1:6

vec2

# quiero recuperar el quinto elemento de este vector
vec2[5]

# igual que lo puedo recuperar, puedo darle otro valor
vec2[5] <- 55
vec2

# entre los corchetes también pueden ir varios elementos, 
# por ejemplo si quiero poner el primer y el tercer elemento a cero
vec2[c(1,3)] <- 1
# ¿y por qué no esto? 
# vec2[1,3] <- 1 
# c(1,3) es un vector, mientras que 1,3 no lo es. 
# Probad a escribir ambos en la consola.


# si las posiciones que nos interesan están en otro vector, 
# también se puede utilizar
posiciones <- c(1,3)
vec2[posiciones]

# podemos "preguntar" qué elementos de un vector 
# cumplen una cierta condición, usando la función "which"
# esta función nos devuelve las posiciones que cumplen la condición
which(vec2 < 30)

# compara cómo se escribe una suma estándar
x + 1

# con cómo se escribe un logaritmo
log(x)

# raiz cuadrada, etc
sqrt(x)

# función para generar secuencias
seq(from = 1, to = 10, by = 0.1)

# suma todos los elementos de un vector numérico
sum(1:10)

# función para calcular la media de un vector
media <- mean(vec2)

# o su desviación típica
desv <- sd(vec2)

?seq
