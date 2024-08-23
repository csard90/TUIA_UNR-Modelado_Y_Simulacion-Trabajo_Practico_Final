
### -------------------------------------- ###
### ---------- Evaluación FINAL ---------- ###
### -------------------------------------- ###


### -------------------------------------- ###
###     4.2 Rango de una Muestra NORMAL    ###
### -------------------------------------- ###


### -------------------------------------- ###
###    Introducción Teórica - Ejemplo 4.3
### -------------------------------------- ###


# Simulación de 100,000 muestras de tamaño 10, extraídas de una distribución normal 
# con una media de 100 y un desvío estándar de 10: NORM(mu=100, sg=10) 
# Luego, calcula el rango (diferencia entre el valor máximo y el mínimo) de cada muestra 
# y finalmente presenta un resumen estadístico y gráfico de estos rangos.

set.seed(1237)                  # Semilla para reproducibilidad.
m = 100000                      # Número de simulaciones a realizar.
n = 10                          # Tamaño de cada muestra.
mu = 100                        # Media de la distribución normal.
sg = 10                         # Desvío estándar de la distribución normal.
x = rnorm(m*n, mu, sg)          # Genera m*n valores aleatorios de una distr. normal NORM(mu, sg)
DTA = matrix(x, m)              # Convierte el vector x en una matriz con m filas.
x.mx = apply(DTA, 1, max)       # Máximo de cada fila de la matriz DTA.
x.mn = apply(DTA, 1, min)       # Mínimo de cada fila de la matriz DTA.
x.rg = x.mx - x.mn              # Rango (máximo - mínimo) para cada muestra.
rg_mean <- mean(x.rg)           # Media de los rangos.
rg_sd <- sd(x.rg)               # Desvío estándar de los rangos.
rg_quantiles <- quantile(x.rg, c(0.025,0.975))  # Percentiles 2.5% y 97.5% de los rangos.
hist(x.rg, prob=T)              # Dibuja un histograma de los rangos con densidades en lugar de frecuencias.

# Rediseño del histograma del apunte.
hist(x.rg, 
     prob = TRUE, 
     breaks = 26, 
     col = "aquamarine", 
     main = "Rangos Muestrales Simulados (n = 10)", 
     xlab = "Rango Muestral", 
     ylab = "Densidad")

# Agregado de Percentiles 2.5% y 97.5%
abline(v = rg_quantiles, col = "red", lty = 2)
text(rg_quantiles[1], 0.04, labels = round(rg_quantiles[1], 2), col = "red", pos = 2)
text(rg_quantiles[2], 0.04, labels = round(rg_quantiles[2], 2), col = "red", pos = 4)

# Valor de K para que R_unb = KR sea una estimación insesgada de sigma
k <- sg / rg_mean
k

# Intervalo de Confianza del 95% para sigma basado en el rango muestral
ic_r = rg_mean * 1/(rg_quantiles / sg)  

# Longitud promedio del IC calculado a partir de los extremos del IC
# (Resultado expresado como un factor de sigma)
E_LR <- (ic_r[1] - ic_r[2]) / sg 
E_LR

# Longitud promedio del IC calculada directamente con los percentiles de los rangos muestrales
# (Resultado expresado como un factor de sigma)
E_LR2 <- rg_mean * (1/rg_quantiles[1] - 1/rg_quantiles[2]) 
E_LR2


# Mostrar los valores en la consola
cat("Intervalo de confianza del 95% para sigma basado en R =", ic_r, "\n")
cat("Longitud del IC =", E_LR, "sigma\n\n")



### -------------------------------------- ###
###       Problemas de la sección 4.2
### -------------------------------------- ###


# Problema 4.13 
# -------------

# Variación del ejemplo 4.3 para muestras de tamaño 5.

set.seed(1237)                  # Semilla para reproducibilidad.
m = 100000                      # Número de simulaciones a realizar.
n = 5                           # Tamaño de cada muestra.
mu = 100                        # Media de la distribución normal.
sg = 10                         # Desvío estándar de la distribución normal.
x = rnorm(m*n, mu, sg)          # Genera m*n valores aleatorios de una distr. normal NORM(mu, sg)
DTA = matrix(x, m)              # Convierte el vector x en una matriz con m filas.
x.mx = apply(DTA, 1, max)       # Máximo de cada fila de la matriz DTA.
x.mn = apply(DTA, 1, min)       # Mínimo de cada fila de la matriz DTA.
x.rg = x.mx - x.mn              # Rango (máximo - mínimo) para cada muestra.
rg_mean <- mean(x.rg)           # Media de los rangos.
rg_sd <- sd(x.rg)               # Desvío estándar de los rangos.
rg_quantiles <- quantile(x.rg, c(0.025,0.975))  # Calcula los percentiles 2.5% y 97.5% de los rangos.

# Histograma de los rangos con densidades en lugar de frecuencias.
hist(x.rg, 
     prob = TRUE, 
     breaks = 30, 
     col = "aquamarine", 
     main = "Rangos Muestrales Simulados (n = 5)", 
     xlab = "Rango Muestral", 
     ylab = "Densidad")

# Agregado de Percentiles 2.5% y 97.5%
abline(v = rg_quantiles, col = "red", lty = 2)
text(rg_quantiles[1], 0.04, labels = round(rg_quantiles[1], 2), col = "red", pos = 2)
text(rg_quantiles[2], 0.04, labels = round(rg_quantiles[2], 2), col = "red", pos = 4)

# Valor de K para que R_unb = KR sea una estimación insesgada de sigma
k <- sg / rg_mean
k

# Intervalo de Confianza del 95% para sigma basado en el rango muestral
ic_r = rg_mean * 1/(rg_quantiles / sg)  

# Longitud promedio del IC calculado a partir de los extremos del IC
# (Resultado expresado como un factor de sigma)
E_LR <- (ic_r[1] - ic_r[2]) / sg 
E_LR

# Longitud promedio del IC calculada directamente con los percentiles de los rangos muestrales
# (Resultado expresado como un factor de sigma)
E_LR2 <- rg_mean * (1/rg_quantiles[1] - 1/rg_quantiles[2]) 
E_LR2


# Mostrar los valores en la consola
cat("Intervalo de confianza del 95% para sigma basado en R =", ic_r, "\n")
cat("Longitud del IC =", E_LR, "sigma\n\n")



# Problema 4.14 
# -------------

set.seed(1237)                  # Semilla para reproducibilidad.
m = 100000                      # Número de simulaciones a realizar para valores grandes de n.
n_values <- c(10, 30, 50, 100, 200, 500, 1000, 2000)  # Tamaños de muestra a probar.
mu = 100                        # Media de la distribución normal.
sg = 10                         # Desvío estándar de la distribución normal.

for (n in n_values) {
  x = rnorm(m*n, mu, sg)        # Genera m*n valores aleatorios de una distr. normal NORM(mu, sg)
  DTA = matrix(x, m)            # Convierte el vector x en una matriz con m filas.
  x.mx = apply(DTA, 1, max)     # Máximo de cada fila de la matriz DTA.
  x.mn = apply(DTA, 1, min)     # Mínimo de cada fila de la matriz DTA.
  x.rg = x.mx - x.mn            # Rango (máximo - mínimo) para cada muestra.
  rg_mean <- mean(x.rg)         # Media de los rangos.
  rg_sd <- sd(x.rg)             # Desvío estándar de los rangos.
  rg_quantiles <- quantile(x.rg, c(0.025,0.975))  # Calcula los percentiles 2.5% y 97.5% de los rangos.
  
  # Valor de K para que R_unb = KR sea una estimación insesgada de sigma
  K <- sg / rg_mean
  
  # Verificar cuál de los valores propuestos de K es más apropiado (Resultado debe ser próximo a 1)
  K_1_3 <- rg_mean / (3 * sg)
  K_1_4 <- rg_mean / (4 * sg)
  K_1_5 <- rg_mean / (5 * sg)
  K_1_6 <- rg_mean / (6 * sg)
  
  # Imprimir los resultados
  cat("n =", n, "\n")
  cat("Valor de K =", K, "\n")
  cat("K (1/3) =", K_1_3, "\n")
  cat("K (1/4) =", K_1_4, "\n")
  cat("K (1/5) =", K_1_5, "\n")
  cat("K (1/6) =", K_1_6, "\n")
  cat("Media de los rangos =", rg_mean, "\n")
  cat("Desvío estándar de los rangos =", rg_sd, "\n")
  cat("Cuantiles 0.025 y 0.975 =", rg_quantiles, "\n\n")
  
  # Dibuja un histograma de los rangos con densidades en lugar de frecuencias.
  hist(x.rg, 
       prob = TRUE, 
       #breaks = 20, 
       col = "aquamarine", 
       main = paste("Rangos Muestrales Simulados (n =", n, ")"), 
       xlab = "Rango Muestral", 
       ylab = "Densidad")
  
  # Agregado de Percentiles 2.5% y 97.5%
  abline(v = rg_quantiles, col = "red", lty = 2)
  text(rg_quantiles[1], 0.04, labels = round(rg_quantiles[1], 2), col = "red", pos = 2)
  text(rg_quantiles[2], 0.04, labels = round(rg_quantiles[2], 2), col = "red", pos = 4)
}



# Problema 4.15 
# -------------


# Ítem a)
# -------
set.seed(1237)                  # Semilla para reproducibilidad.
m = 100000                      # Número de simulaciones a realizar.
n = 10                          # Tamaño de cada muestra.
mu = 100                        # Media de la distribución normal.
sg = 10                         # Desvío estándar de la distribución normal.
x = rnorm(m*n, mu, sg)          # Genera m*n valores aleatorios de una distr. normal NORM(mu, sg)
DTA = matrix(x, m)              # Convierte el vector x en una matriz con m filas.
x.sd = apply(DTA, 1, sd)        # Desvío estándar de cada fila de la matriz DTA.
sd_mean <- mean(x.sd)           # Media de las desviaciones estándar muestrales.
sd_sd <- sd(x.sd)               # Desvío estándar de las desviaciones estándar muestrales.

# Valor de a para que S_unb = aS sea una estimación insesgada de sigma
a <- sg / sd_mean
a

# Valor de a teórico basado en la distribución chi-cuadrado 
c <- sqrt(2 / (n - 1)) * sqrt(qchisq(0.5, df = n - 1))
a_teorico <- 1 / c
a_teorico


# Ítem b)
# -------

# Intervalo de Confianza del 95% para sigma basado en el desvío estándar
sd_quantiles <- quantile(x.sd, c(0.025,0.975))
ic_s = sd_mean * 1/(sd_quantiles / sg)  

# Longitud promedio del IC calculado a partir de los extremos del IC
# (Resultado expresado como un factor de sigma)
E_LS <- (ic_s[1] - ic_s[2]) / sg 
E_LS

# Longitud promedio del IC calculada directamente con los percentiles de los desvíos
# (Resultado expresado como un factor de sigma)
E_LS2 <- sd_mean * (1/sd_quantiles[1] - 1/sd_quantiles[2]) 
E_LS2


# Límites de confianza del 95% de la distribución chi-cuadrado
chi_sq_limits <- qchisq(c(0.025, 0.975), df = n - 1)

# Intervalo de confianza del 95% para S usando la distribución chi-cuadrado
ic_s_chisq <- sqrt((n - 1) * sd_mean^2 / chi_sq_limits)

# Longitud promedio del IC a partir de la distribución chi-cuadrado
# (Resultado expresado como un factor de sigma)
E_LS_chisq <- (ic_s_chisq[1] - ic_s_chisq[2]) / sg
E_LS_chisq

# Mostrar los valores en la consola
cat("Media de los desvíos =", sd_mean, "\n\n")
cat("Intervalo de confianza del 95% para sigma basado en S =", ic_s, "\n")
cat("Longitud del IC =", E_LS, "sigma\n\n")
cat("Intervalo de confianza del 95% para sigma basado en chi-cuadrado =", ic_s_chisq, "\n")
cat("Longitud del IC =", E_LS_chisq, "sigma\n\n")


# Dibuja un histograma de las desviaciones estándar con densidades en lugar de frecuencias.
hist(x.sd, 
     prob = TRUE, 
     # breaks = 50, 
     col = "aquamarine", 
     main = "Desvíos Estándar Muestrales Simulados (n = 10)", 
     xlab = "Desvío Estándar Muestral", 
     ylab = "Densidad")

# Agregado del valor de la media
abline(v = sd_mean, col = "red", lty = 2)
text(sd_mean, 0.17, labels = round(sd_mean, 2), col = "red", pos = 4)

# Agregado de Percentiles 2.5% y 97.5%
abline(v = sd_quantiles, col = "red", lty = 2)
text(sd_quantiles[1], 0.10, labels = round(sd_quantiles[1], 2), col = "red", pos = 2)
text(sd_quantiles[2], 0.10, labels = round(sd_quantiles[2], 2), col = "red", pos = 4)



# Ítem c)
# -------
set.seed(1237)                  # Semilla para reproducibilidad.
m = 100000                      # Número de simulaciones a realizar.
n = 10                          # Tamaño de cada muestra.
mu = 100                        # Media de la distribución normal.
sg = 10                         # Desvío estándar de la distribución normal.
x = rnorm(m*n, mu, sg)          # Genera m*n valores aleatorios de una distr. normal NORM(mu, sg)
DTA = matrix(x, m)              # Convierte el vector x en una matriz con m filas.

# Calcular R y R_unb (Por si las variables del entorno fueron modificadas)
x.mx = apply(DTA, 1, max)       # Máximo de cada fila de la matriz DTA.
x.mn = apply(DTA, 1, min)       # Mínimo de cada fila de la matriz DTA.
x.rg = x.mx - x.mn              # Rango (máximo - mínimo) para cada muestra.
mean_rg = mean(x.rg)            # Media de los rangos.
K = sg / mean_rg                # Valor de K para que R_unb = KR sea una estimación insesgada de sigma.
R_unb = K * x.rg                # Estimador insesgado de sigma basado en el rango.
var_R_unb = var(R_unb)          # Varianza de R_unb.

# Calcular S y S_unb  (Por si las variables del entorno fueron modificadas)
x.sd = apply(DTA, 1, sd)        # Desvío estándar de cada fila de la matriz DTA.
mean_sd = mean(x.sd)            # Media de las desviaciones estándar muestrales.
a = sg / mean_sd                # Valor de a para que S_unb = aS sea una estimación insesgada de sigma.
S_unb = a * x.sd                # Estimador insesgado de sigma basado en S.
var_S_unb = var(S_unb)          # Varianza de S_unb.

# Comparar varianzas
cat("Varianza de S_unb:", var_S_unb, "\n")
cat("Varianza de R_unb:", var_R_unb, "\n")
cat("¿V(R_unb) >= V(S_unb)?", var_R_unb >= var_S_unb, "\n")



# Problema 4.16 
# -------------

set.seed(1237)                  # Semilla para reproducibilidad.
m = 100000                      # Número de simulaciones a realizar.
n = 2                           # Tamaño de cada muestra.
mu = 100                        # Media de la distribución normal.
sg = 10                         # Desvío estándar de la distribución normal.
x = rnorm(m*n, mu, sg)          # Genera m*n valores aleatorios de una distr. normal NORM(mu, sg)
DTA = matrix(x, m)              # Convierte el vector x en una matriz con m filas.
x.mx = apply(DTA, 1, max)       # Máximo de cada fila de la matriz DTA.
x.mn = apply(DTA, 1, min)       # Mínimo de cada fila de la matriz DTA.
x.rg = x.mx - x.mn              # Rango (máximo - mínimo) para cada muestra.
x.sd = apply(DTA, 1, sd)        # Desvío estándar de cada fila de la matriz DTA.

# Relación entre el rango y el desvío estándar
relation <- x.rg / x.sd         # Relación entre el rango y desvío estándar muestral
mean(relation)                  # Media de la relación. (Debería ser cercano a sqrt(2) )

# Estimador insesgado de sigma basado en rango muestral
K = sg / mean(x.rg)             # Valor de K para que R_unb = KR sea una estimación insesgada de sigma.
R_unb = K * x.rg                # Estimador insesgado de sigma basado en el rango.

# Estimador insesgado de sigma basado en desvío estándar muestral
a = sg / mean(x.sd)             # Valor de a para que S_unb = aS sea una estimación insesgada de sigma.
S_unb = a * x.sd                # Estimador insesgado de sigma basado en S.

# Relación entre los estimadores insesgados de sigma
relation_unb <- R_unb / S_unb
mean(relation_unb)

# Mostrar resultados
cat("Promedio de la relación rango / desvío estándar muestral: ", mean(relation), "\n")
cat("Promedio de la relación de los estimadores insesgados: ", mean(relation_unb), "\n")




# Problema 4.17 
# -------------


# Ítem a)
# -------
set.seed(1237)                  # Semilla para reproducibilidad.
m = 100000                      # Número de simulaciones a realizar.
n = 10                          # Tamaño de cada muestra.
mu = 100                        # Media de la distribución uniforme.
sg = 10                         # Desvío estándar de la distribución uniforme.
a = mu - sqrt(3) * sg           # Límite inferior de la distribución uniforme.
b = mu + sqrt(3) * sg           # Límite superior de la distribución uniforme.
x = runif(m*n, a, b)            # Genera m*n valores aleatorios de una distr. uniforme.
DTA = matrix(x, m)              # Convierte el vector x en una matriz con m filas.
x.mx = apply(DTA, 1, max)       # Máximo de cada fila de la matriz DTA.
x.mn = apply(DTA, 1, min)       # Mínimo de cada fila de la matriz DTA.
x.rg = x.mx - x.mn              # Rango (máximo - mínimo) para cada muestra.
x.sd = apply(DTA, 1, sd)        # Desvío estándar de cada fila de la matriz DTA.

# Estimador insesgado de sigma basado en rango muestral
K = sg / mean(x.rg)             # Valor de K para que R_unb = KR sea una estimación insesgada de sigma.
R_unb = K * x.rg                # Estimador insesgado de sigma basado en el rango.

# Estimador insesgado de sigma basado en desvío estándar muestral
a = sg / mean(x.sd)             # Valor de a para que S_unb = aS sea una estimación insesgada de sigma.
S_unb = a * x.sd                # Estimador insesgado de sigma basado en S.

# Mostrar los valores en la consola
cat("Constante K para el rango:", K, "\n")
cat("Constante K para el desvío estándar:", a, "\n")
cat("Media del estimador insesgado de sigma basado en rango muestral:", mean(R_unb), "\n")
cat("Media del estimador insesgado de sigma basado en desvío estándar muestral:", mean(S_unb), "\n")



# Ítem b)
# -------

# Calcular las varianzas
var_R_unb = var(R_unb)
var_S_unb = var(S_unb)

# Comparar varianzas
cat("Varianza de R_unb:", var_R_unb, "\n")
cat("Varianza de S_unb:", var_S_unb, "\n")
cat("¿V(R_unb) < V(S_unb)?", var_R_unb < var_S_unb, "\n")



# Ítem c)
# -------

# Calcular los cuantiles de R_unb y S_unb
quantiles_R_unb <- quantile(R_unb, c(0.025, 0.975))
quantiles_S_unb <- quantile(S_unb, c(0.025, 0.975))

# Intervalos de confianza del 95% para sigma basado en R_unb y S_unb
ic_runb = mean(R_unb) / (quantiles_R_unb / sg) # 14.760343  8.392721 
ic_sunb = mean(S_unb) / (quantiles_S_unb / sg) # 15.355552  7.666517

# En términos de sigma
ic_runb_sg = mean(R_unb) / (quantiles_R_unb ) # 1.4760343  0.8392721 
ic_sunb_sg = mean(S_unb) / (quantiles_S_unb ) # 1.5355552  0.7666517

# Longitud promedio del IC del 95% para sigma basado en R_unb y S_unb
# (Resultado expresado como un factor de sigma)
E_LRunb <- ic_runb_sg[1] - ic_runb_sg[2]
E_LSunb <- ic_sunb_sg[1] - ic_sunb_sg[2]


# Mostrar los valores en la consola
cat("Cuantiles de R_unb:", quantiles_R_unb, "\n")
cat("Cuantiles de S_unb:", quantiles_S_unb, "\n\n")
cat("IC del 95% para sigma basado en R_unb:", ic_runb, "\n")
cat("IC del 95% para sigma basado en R_unb en función de sigma:", ic_runb_sg, "(sigma)\n\n")
cat("IC del 95% para sigma basado en S_unb:", ic_sunb, "\n")
cat("IC del 95% para sigma basado en S_unb en función de sigma:", ic_sunb_sg, "(sigma)\n\n")
cat("Longitud del intervalo basado en R_unb:", E_LRunb, "sigma\n")
cat("Longitud del intervalo basado en S_unb:", E_LSunb, "sigma\n\n")


# Determinar cuál intervalo tiene la longitud esperada más corta
if (E_LRunb < E_LSunb) {
  cat("El intervalo de confianza basado en R_unb tiene la longitud esperada más corta.\n")
} else {
  cat("El intervalo de confianza basado en S_unb tiene la longitud esperada más corta.\n")
}


# Histograma de los rangos con densidades en lugar de frecuencias.
hist(R_unb, 
     prob = TRUE, 
     breaks = 26, 
     col = "aquamarine", 
     main = "Estimadores insesgados R_unb para sigma (Distr. Unif.)", 
     xlab = "R_unb", 
     ylab = "Densidad")

# Agregado de Percentiles 2.5% y 97.5%
abline(v = quantiles_R_unb, col = "red", lty = 2)
text(quantiles_R_unb[1], 0.20, labels = round(quantiles_R_unb[1], 2), col = "red", pos = 2)
text(quantiles_R_unb[2], 0.20, labels = round(quantiles_R_unb[2], 2), col = "red", pos = 4)


# Histograma de los desvíos estándar con densidades en lugar de frecuencias.
hist(S_unb, 
     prob = TRUE, 
     breaks = 26, 
     col = "aquamarine", 
     main = "Estimadores insesgados S_unb para sigma (Distr. Unif.)", 
     xlab = "S_unb", 
     ylab = "Densidad")

# Agregado de Percentiles 2.5% y 97.5%
abline(v = quantiles_S_unb, col = "red", lty = 2)
text(quantiles_S_unb[1], 0.20, labels = round(quantiles_S_unb[1], 2), col = "red", pos = 2)
text(quantiles_S_unb[2], 0.20, labels = round(quantiles_S_unb[2], 2), col = "red", pos = 4)
