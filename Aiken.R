
library(openxlsx)

calcular_indice_aiken <- function(puntuaciones, lo, c) {
  S <- sum(puntuaciones)
  n <- length(puntuaciones)
  V <- (S - n * lo) / (n * (c - 1))
  return(V)
}

intervalo_confianzacom <- function(V, k, n, alpha=0.05) {
  Z <- qnorm(1 - alpha / 2)  
  termino_raiz <- sqrt(4 * n * k * V * (1 - V) + Z^2)
  
  L <- (2 * n * k * V + Z^2 - Z * termino_raiz) / (2 * (n * k + Z^2))
  
  U <- (2 * n * k * V + Z^2 + Z * termino_raiz) / (2 * (n * k + Z^2))

  return(list(L = L, U = U))
}

ruta_archivo <- "data.xlsx"  
datos <- read.xlsx(ruta_archivo, sheet = 1)

head(datos)

lo <- 1
c <- 5

resultados <- lapply(2:ncol(datos), function(col) {
  puntuaciones <- datos[[col]]
  indice_v <- calcular_indice_aiken(puntuaciones, lo, c)
  n <- length(puntuaciones)
  k <- c - lo
  intervalo_confianza <- intervalo_confianzacom(indice_v, k, n)
  list(
    Pregunta = colnames(datos)[col],
    Indice_V = indice_v,
    Inferior = intervalo_confianza$L,
    Superior = intervalo_confianza$U
  )
})

for (res in resultados) {
  cat("Pregunta:", res$Pregunta, "\n")
  cat("El índice V de Aiken es:", res$Indice_V, "\n")
  cat("Intervalo de confianza del 95% para el índice V de Aiken:\n")
  cat("Inferior:", res$Inferior, "\n")
  cat("Superior:", res$Superior, "\n\n")
}
