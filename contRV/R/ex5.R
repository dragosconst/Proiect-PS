
# Un mic hack cu functia asta e ca daca vreti sa calculati integrala unei functii care nu e explicit densitate,
# puteti face un obiect contRv cu densitatea functia pe care vreti sa o integrati si val = f(...) = 1 (functia care duce tot in 1).
# Am introdus parametrul dt ca sa putem deriva o densitate comuna dx, respectiv dy, pentru marginale.
integrala <- function(X, dt = 0)
{
  dx <- 1
  dy <- 2

  if(X@bidimen)
  {
    if(dt ==0)
    {

      # -fac integrala pe fiecare interval cu fiecare
      sum <- 0
      for(i in X@suport[[2]])
      {
        for(j in X@suport[[1]])
        {
          tryCatch(sum <- sum +
                     integrate( function(y) {
                       sapply(y, function(y) {
                         integrate(function(x) X@densitate(x,y),j[1],j[2])$value
                       })
                     },i[1],i[2])$value,
                   error= function(err)
                    {
                     stop("Eroare la integrarea densitatii.")
                   })
        }
      }

      return(sum)
    }
    else if(dt == dx) # integrare doar pe x
    {
    #   Ok, deci ideea care mi a venit aici este sa construiesc un vector de functii asa:
    #   f_i+1 (y) = (integrala pe [a_i+1,b_i+1]dx) + f_i(y)

      funcs <- vector()
      factory <- function(i1, i2, pas)
      {
        i1; i2; pas; # pt closure
        if(pas == 1)
        {
          tmpF <-  Vectorize(function(y) {
                                sapply(y, function(y) {
                                  integrate(function(x) X@densitate(x,y),i1,i2)$value
                                })
          })
          funcs <<- c(funcs, tmpF)
        }
        else
        {
          tmpF <-  Vectorize(function(y) {
                                sapply(y, function(y) {
                                  integrate(function(x) X@densitate(x,y),i1,i2)$value
                                })
          })

          newF <- Vectorize(function(y){
            tmpF(y) + funcs[[pas - 1]](y)
          })
          funcs <<- c(funcs, newF)
        }
      }

      pas <- 0
      for(i in X@suport[[1]])
      {
        pas <- pas + 1
        factory(i[1], i[2], pas)
      }

      return(funcs[[length(funcs)]])
    }
    else if(dt == dy) # integrare doar pe y
    {
      funcs <- vector()
      factory <- function(i1, i2, pas)
      {
        i1; i2; pas; # pt closure
        if(pas == 1)
        {
          tmpF <-  Vectorize(function(x) {
            sapply(x, function(x) {
              integrate(function(y) X@densitate(x,y),i1,i2)$value
            })
          })
          funcs <<- c(funcs, tmpF)
        }
        else
        {
          tmpF <-  Vectorize(function(x) {
            sapply(x, function(x) {
              integrate(function(y) X@densitate(x,y),i1,i2)$value
            })
          })

          newF <- Vectorize(function(x){
            tmpF(x) + funcs[[pas - 1]](x)
          })
          funcs <<- c(funcs, newF)
        }
      }

      pas <- 0
      for(i in X@suport[[2]])
      {
        pas <- pas + 1
        factory(i[1], i[2], pas)
      }

      return(funcs[[length(funcs)]])
    }
  }
  else
  {
    sum <- 0
    for (i in X@suport[[1]]) {
      tryCatch(sum <- sum + integrate(Vectorize(X@densitate), i[1], i[2], abs.tol = 1.0e-13)$value,
               error= function(err)
               {
                 stop("Integrala a esuat.")
               })
    }
    return (sum)
  }
}


# -asa pot ridica o functie la puterea y si ca retval sa am tot o functie
# -f useful pt momente
powF <- function(f, y)
{
  ret <- function(...)
  {
    f(...) ^ y
  }
}

# returneaza valoarea mediei sau o eroare
media <- function(X)
{

  # -valoarea de sub integrala trebuie trimisa
  # ca o functie, cel mai lejer de scris e asa
  subIntegrala <- function(...)
  {
    X@val(...) * X@densitate(...)
  }

  subIntegrala <- Vectorize(subIntegrala)
  tmp <- contRV(densitate = subIntegrala, val = Vectorize(function(...) {retval <- 1}), bidimen = X@bidimen, suport = X@suport)

  tryCatch(retval <- integrala(tmp),
           error=function(err){
             stop("Media nu exista")
           }
  )
  return(retval)
}

dispersia <- function(X)
{
  # formula clasica cu integrala

  tryCatch(m <- media(X), warning=function(wr)
  {
    stop("Calcularea dispersiei a esuat, media nu exista")
  })

  xCoef <- function(...)
  {

    X@val(...) - m
  }

  coefRaised <- powF(xCoef, 2)
  coefRaised <- Vectorize(coefRaised)

  subIntegrala <- function(...)
  {
    coefRaised(...) * X@densitate(...)
  }
  subIntegrala <- Vectorize(subIntegrala)
  tmp <- contRV(densitate = subIntegrala, val = Vectorize(function(...) {retval <- 1}), bidimen = X@bidimen, suport = X@suport)

  tryCatch(retval <- integrala(tmp),
           error= function(err)
           {
             stop("Dispersia nu exista.")
           })
  return(retval)
}


moment_centrat <- function(X, ordin)
{
  # cazurile triviale
  if(ordin == 0)
    return(1)
  else if(ordin == 1)
    tryCatch({ # trebuie totusi verificat daca E(X) exista, ca altfel nu va da nici macar 0
      media(X)
      retval <- 0
    }, error= function(err)
    {
      stop(paste("Calcularea momentului centrat de ordin ", ordin, " a esuat, nu exista media."))
    })
  else if(ordin == 2)
    tryCatch({  # X dispersia nu exista, vrem un mesaj specific pt momente
      dispersia(X)
    }, error= function(err)
    {
      stop(paste("Calcularea momentului centrat de ordin ", ordin, " a esuat, nu exista dispersie."))
    })

  tryCatch(m <- media(X), warning=function(wr)
  {
    stop(paste("Calcularea momentului centrat de ordin ", ordin, " a esuat"))
  })

  xCoef <- function(...)
  {

    X@val(...) - m
  }

  coefRaised <- powF(xCoef, ordin)
  coefRaised <- Vectorize(coefRaised)

  subIntegrala <- function(...)
  {
    coefRaised(...) * X@densitate(...)
  }
  subIntegrala <- Vectorize(subIntegrala)
  tmp <- contRV(densitate = subIntegrala, val = Vectorize(function(...) {retval <- 1}), bidimen = X@bidimen, suport = X@suport)

  tryCatch(retval <- integrala(tmp),
           error= function(err)
           {
             stop(paste("Momentul centrat de ordin ", ordin, " nu exista."))
           })
  return(retval)
}

moment_initial <- function(X, ordin)
{
  # cazurile triviale
  if(ordin == 0)
    return(1)
  else if(ordin == 1)
    tryCatch({ # trebuie totusi verificat daca E(X) exista, ca altfel nu va da nici macar 0
      media(X)
    }, error= function(err)
    {
      stop(paste("Calcularea momentului initial de ordin ", ordin, " a esuat."))
    })


  xCoef <- function(...)
  {

    X@val(...)
  }

  coefRaised <- powF(xCoef, ordin)
  coefRaised <- Vectorize(coefRaised)

  subIntegrala <- function(...)
  {
    coefRaised(...) * X@densitate(...)
  }
  subIntegrala <- Vectorize(subIntegrala)
  tmp <- contRV(densitate = subIntegrala, val = Vectorize(function(...) {retval <- 1}), bidimen = X@bidimen, suport = X@suport)

  tryCatch(retval <- integrala(tmp),
           error= function(err)
           {
             stop(paste("Momentul initial de ordin ", ordin, " nu exista."))
           })
  return(retval)
}
