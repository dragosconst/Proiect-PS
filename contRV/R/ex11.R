dens_condit_x_de_y <- function(Z)
{
  dens_marginala_y <- integrala(Z,1)
  f <- function(x,y) {if (dens_marginala_y(y) != 0) {Z@densitate(x,y) / dens_marginala_y(y)} else stop("Densitatea marginala este 0")}
  return (Vectorize(f))
}

dens_condit_y_de_x <- function(Z)
{
  dens_marginala_x <- integrala(Z,2)
  f <- function(x,y) {if (dens_marginala_y(y) != 0) {Z@densitate(x,y) / dens_marginala_x(x)} else stop("Densitatea marginala este 0")}
  return (Vectorize(f))
}
