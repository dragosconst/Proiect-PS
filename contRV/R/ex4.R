# Repartitia normala (implicit standard); m = medie, s = deviatie standard

den.normala <- function(m = 0, s = 1) {
  curve(expr = 1 / (s * sqrt(2 * pi)) * exp(1) ^ (-(x - m)^2 / (2 * s^2)),
        from = m - 3 * s,
        to   = m + 3 * s,
        ylab = "densitate",
        main = "Densitatea in repartitia normala")
  abline(v = 0, col = "gray")
}

rep.normala <- function(m = 0, s = 1) {
  curve(expr = pnorm(x, m, s),
        from = m - 3 * s,
        to   = m + 3 * s,
        ylab = "probabilitate",
        main = "Functia repartitie normala")
}


# Repartitia exponentiala

den.exponentiala <- function(l = 1) {
  curve(expr = l * exp(1) ^ (-l * x),
        from = 0,
        to   = 20,
        ylab = "densitate",
        main = "Densitatea in repartitia exponentiala")
}

rep.exponentiala <- function(l = 1) {
  curve(expr = 1 - exp(1) ^ (-l * x),
        from = 0,
        to   = 10,
        ylab = "probabilitate",
        main = "Functia repartitie exponentiala")
}


# Repartitia beta

den.beta <- function(a, b) {
  curve(expr = x^(a - 1) * (1 - x) ^ (b - 1) /
          integrate(function(u) u^(a - 1) * (1 - u)^(b - 1),
                    0, 1, abs.tol = 0)$value,
        from = 0,
        to   = 1,
        ylab = "densitate",
        main = "Densitatea in repartitia beta")
}

# Teste: den.beta(0.5, 0.5)
#        den.beta(2, 2)
#        den.beta(2, 5)

# la fel, dar animata; a - fixat, 0 < b <= bmax
den.beta_anim <- function(a = 2, bmax = 8) {

  par(bty = "o")

  for (b in seq(0.1, bmax, length.out = 150)) {

    d <- function(x) x^(a - 1) * (1 - x) ^ (b - 1) /
      integrate(function(u) u^(a - 1) * (1 - u)^(b - 1),
                0, 1, abs.tol = 0)$value

    x <- seq(0, 1, length.out = 1000)
    plot(x,
         d(x),
         type = "l",
         xlab = "x",
         ylab = "densitate",
         ylim = c(0, 7),
         main = "Densitatea in repartitia beta")

    legend(0.7, 6.5, legend = sprintf("b = %.2f", b), cex = 0.8)

    Sys.sleep(0.05)
  }

}

# Test: den.beta_anim()
#       den.beta_anim(0.8, 15)

rep.beta <- function(a, b) {
  curve(expr = pbeta(x, shape1 = a, shape2 = b),
        from = 0,
        to   = 1,
        ylab = "probabilitate",
        main = "Functia repartitie beta")
}

# Teste: rep.beta(5, 1)
#        rep.beta(2, 2)
#        rep.beta(2, 5)


# Repartitia gamma; k - „shape”, t - „scale”

den.gamma <- function(k, t) {
  curve(expr = dgamma(x, shape = k, scale = t),
        from = 0,
        to   = 20,
        ylab = "densitate",
        main = "Densitatea in repartitia gamma")
}

# Teste: den.gamma(1, 2)
#        den.gamma(2, 2)
#        den.gamma(7.5, 1)

rep.gamma <- function(k, t) {
  curve(expr = pgamma(x, shape = k, scale = t),
        from = 0,
        to   = 20,
        ylab = "probabilitate",
        main = "Functia repartitie gamma")
}

# Teste: rep.gamma(0.5, 1)
#        rep.gamma(7.5, 1)
