ex11 <- function(X){
  dens_marginala_x <- integrala(X,2)
  dens_marginala_y <- integrala(X,1)
  dens_condit_x_la_y = dens_comuna / dens_marginala_y
  dens_condit_y_la_x = dens_comuna / dens_marginala_x
}