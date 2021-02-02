ex11 <- function(X){
  dens_marginala_x <- integrala(X,2)
  dens_marginala_y <- integrala(X,1)
  dens_condit_x_la_y = X@densitate / dens_marginala_y
  dens_condit_y_la_x = X@densitate / dens_marginala_x
}