ex11 <- function(x){
  dens_marginala_x <- integrate(Vectorize(y), lower = -Inf, upper = Inf)$value #dy
  dens_marginala_y <- integrate(Vectorize(x), lower = -Inf, upper = Inf)$value #dx
  dens_condit_x_la_y = dens_comuna / dens_marginala_y
  dens_condit_y_la_x = dens_comuna / dens_marginala_x
}