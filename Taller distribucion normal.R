#Presentado por: Michael Rodriguez

# https://r-coder.com/distribucion-normal-r/

# funcion 'pnorm' util para calcular la probabilidad acumulada hasta ciertos puntos x<=n
# funcion 'qnorm' util para calcular a partir de la probabilidad el valor de x

# ------------------------------------------------------------------------------
# 1. La estatura de los habitantes de una ciudad tiene una distribución normal
# con media igual a 1.7 metros y desviación estándar de 0.2 metros. Hallar:

m <- 1.7  # media de alturas en metros
desv_std <- 0.2  # Desviacion estandar en metros

# grafica (preguntar)
x <- seq(m-(4*desv_std), m+(4*desv_std), by = 0.01)
y <- pnorm(x, mean = m, sd = desv_std)
plot(x, y)

# a. La probabilidad de que la estatura se encuentre entre 1.5 y 1.8 metros.
est_1 <- 1.5 # Estatura en metros
est_2 <- 1.8 # Estatura en metros
pnorm(est_2, mean = m, sd = desv_std) - pnorm(est_1, mean = media, sd = desv_std)

# b. La probabilidad de que la estatura sea mayor o igual a 1.8 metros.
1 - pnorm(est_2, mean = m, sd = desv_std)

# c. La probabilidad de que la estatura sea menor a 2 metros
est_3 <- 2 # Estatura en metros
pnorm(est_3, mean = m, sd = desv_std)

#   d. Suponga que la administración de la ciudad, decide convocar para las
# selecciones de baloncesto y boleyball, al 15% de las personas con la estura
# más alta. Cual es la estatura mínima que debe tener una persona para ser convocada?

qnorm(0.85, mean = m, sd = desv_std)

# ------------------------------------------------------------------------------
#2. Una fábrica produce pistones cuyos diámetros se encuentran distribuidos
# normalmente con una media de 5 cm y una desviación estándar igual a 0.2 cm.
# Para que un pistón pueda venderse su diámetro debe estar entre 4.8 y 5.2 cm.
# Si el diámetro del pistón es menor de 4.8 el pistón se desecha; si es mayor
# de 5.2 el pistón puede reprocesarse. Hallar la probabilidad de que: 

m <- 5.0  # Media de los diámetros de los pistones en cm
desv_std <- 0.2  # Desviación estándar de los diámetros en cm

# a. Un pistón fabricado pueda venderse 
pnorm(5.2, mean = m, sd = desv_std) - pnorm(4.8, mean = m, sd = desv_std)

# b. Un pistón fabricado sea desechado 
pnorm(4.8, mean = m, sd = desv_std)

# c. Que porcentaje de pistones es reprocesado?
1 - pnorm(5.2, mean = m, sd = desv_std)

# ------------------------------------------------------------------------------
# 3. Una universidad espera recibir una cantidad significativa de solicitudes de
# ingreso al primer año de de licenciatura. Se supone que las calificaciones
# obtenidas por los aspirantes en la prueba de admisión se encuentran normalmente
# distribuidas con una media de 950 puntos y una desviación estándar de 80 puntos.
# Si la universidad decide recibir al 25% de todos los aspirantes con la
# calificación más alta en la prueba de admisión, cual es la mínima calificación
# que es necesario obtener en esta prueba, para ser admitido por la universidad?

m <- 950  # Media de las calificaciones
desv_std <- 80  # Desviación estándar de las calificaciones

# Calcular la calificación mínima para estar en el 25% superior
qnorm(0.75, mean = m, sd = desv_std)

# ------------------------------------------------------------------------------
# 4. La vida útil de una marca de bombillas sigue una distribución normal de
# media 1.200 horas y desviación estándar de 250 horas. Si la empresa fabrica en
# una hora 1200 bombillas, responder:

m <- 1200  # Media de vida útil en horas
desv_std <- 250  # Desviación estándar
bombillas <- 1200  # Número total de bombillas fabricadas en 1 hora

#  a. Cuantas bombillas tienen un tiempo de vida inferior a 1050 horas?
prob_1050 <- pnorm(1050, mean = m, sd = desv_std)
prob_1050 * bombillas

#  b. Cuantas bombillas tienen un tiempo de vida superior a 1350 horas?
prob_1350 <- 1 - pnorm(1350, mean = m, sd = desv_std)
prob_1350 * bombillas

#  c. Cuantas bombillas tienen un tiempo de vida entre 1050 y 1350 horas?
prob_1050_1350 <- pnorm(1350, mean = m, sd = desv_std) - pnorm(1050, mean = m, sd = desv_std)
prob_1050_1350 * bombillas

# ------------------------------------------------------------------------------
# 5. La variable aleatoria Y tiene una distribución normal con media de 2.55 y
# desviación estándar 0.36. Halle los valores de la variable que cumplen las
# siguientes condiciones:

m <- 2.55
desv_std <- 0.36

# a. P(Y ≤ k) = 0.4801
qnorm(0.4801, mean = m, sd = desv_std)

# b. P(Y ≤ k) = 0.95
qnorm(0.95, mean = m, sd = desv_std)

# c. P(Y > k) = 0.54, lo que implica P(Y ≤ k) = 0.46
qnorm(0.46, mean = m, sd = desv_std)
