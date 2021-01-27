# Repartiția normală (implicit standard); m = medie, s = deviație standard

den.normala <- function(m = 0, s = 1) {
  curve(expr = 1 / (s * sqrt(2 * pi)) * exp(1) ^ (-(x - m)^2 / (2 * s^2)),
        from = m - 3 * s,
        to   = m + 3 * s,
        ylab = "densitate",
        main = "Densitatea în repartiția normală")
  abline(v = 0, col = "gray")
}

rep.normala <- function(m = 0, s = 1) {
  curve(expr = pnorm(x, m, s),
        from = -9 * s,
        to   =  9 * s,
        ylab = "probabilitate",
        main = "Funcția repartiție normală")
}


# Repartiția exponențială

den.exponentiala <- function(l = 1) {
  curve(expr = l * exp(1) ^ (-l * x),
        from = 0,
        to   = 20,
        ylab = "densitate",
        main = "Densitatea în repartiția exponențială")
}

rep.exponentiala <- function(l = 1) {
  curve(expr = 1 - exp(1) ^ (-l * x),
        from = 0,
        to   = 10,
        ylab = "probabilitate",
        main = "Funcția repartiție exponențială")
}


# Repartiția beta

den.beta <- function(a, b) {
  curve(expr = x^(a - 1) * (1 - x) ^ (b - 1) /
          integrate(function(u) u^(a - 1) * (1 - u)^(b - 1),
                    0, 1, abs.tol = 0)$value,
        from = 0,
        to   = 1,
        ylab = "densitate",
        main = "Densitatea în repartiția beta")
}

# Teste: den.beta(0.5, 0.5)
#        den.beta(2, 2)
#        den.beta(2, 5)