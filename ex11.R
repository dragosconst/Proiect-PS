
dens_condit_x_la_y <- function(z)
{
  dens_marginala_y <- integrala(z,1)
  f <- function(x,y) {if (dens_marginala_y(y) != 0) {X@densitate(x,y) / dens_marginala_y(y)} else stop("Densitatea marginala este 0")}
  return (Vectorize(f))
}

dens_condit_y_la_x <- function(z)
{
  dens_marginala_x <- integrala(z,2)
  f <- function(x,y) {if (dens_marginala_y(y) != 0) {X@densitate(x,y) / dens_marginala_x(x)} else stop("Densitatea marginala este 0")}
  return (Vectorize(f))
}