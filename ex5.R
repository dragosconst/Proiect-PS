
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
      
      # -nu am gasit o varianta mai desteapta (si ma tem ca nu exista, daca nu putem salva cumva integrala doar dx ca o functie)
      # -fac integrala pe fiecare interval cu fiecare
      # -problema sa fac mai intai toate integralele pe X, iar apoi pe Y, e ca nu prea am idei cum sa stochez functia rezultata dupa integrarea dupa X(care cred ca o sa arate destul de ciudat, tinand cont ca X poate fi spart pe intervale)
      # -scuze daca e vreo solutie evidenta sau ceva :)) dar eram obosit rau cand am scris asta
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
    else if(dt == dx) # derivare doar pe x
    {
    #   Ok, deci ideea care mi a venit aici este sa construiesc un vector de functii asa:
    #   f_i+1 (y) = (integrala pe [a_i+1,b_i+1]dx) + f_i(y)
    #   Nu stiu alta modalitate de a construi o functie care are si gauri pe intervale
     
      funcs <- vector()
      # Asta sigur o sa arate ciudat, nu prea am cum sa explic ce am facut aici fara sa ma intind pe o groaza de linii. Va recomand sa
      # cititi despre conceptul de closure https://en.wikipedia.org/wiki/Closure_(computer_programming) https://stackoverflow.com/questions/12481404/how-to-create-a-vector-of-functions
      #  ca sa va faceti o idee de ce fac aici.
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


# -hackish function
# -asa pot ridica o functie la puterea y si ca retval sa am tot o functie
# -f useful pt momente
# O mica problema cu approach-ul asta e ca se pare ca R trimite ca argument
# functiile ca referinte, nu cu shallow copy.
# Implicatia e ca daca schimbi f in afara lui powF, o sa fie schimbat si cand e
# aplicat de powF.
# Daca faci ceva de genu myFunc <- powF(myFunc,2), obtii o recursie infinita.
# Solutia este sa faci raisedFunc <- powF(myFunc, 2)
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
  
  # more evil R magic
  subIntegrala <- Vectorize(subIntegrala)
  tmp <- contRV(densitate = subIntegrala, val = Vectorize(function(...) {retval <- 1}), bidimen = X@bidimen, suport = X@suport,
                densitateX = X@densitateX, densitateY = X@densitateY)
  
  tryCatch(retval <- integrala(tmp),
           error=function(err){
             stop("Media nu exista")
           }
  )
  return(retval)
}

dispersia <- function(X)
{
  # -deoarece momentan lucrez doar cu functii de densitate, voi folosi
  # formula clasica cu integrala
  # -cand e gata clasa de variabile, putem incerca si formula aia mai rapida
  
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
  tmp <- contRV(densitate = subIntegrala, val = Vectorize(function(...) {retval <- 1}), bidimen = X@bidimen, suport = X@suport,
                densitateX = X@densitateX, densitateY = X@densitateY)
  
  tryCatch(retval <- integrala(tmp),
           error= function(err)
           {
             stop("Dispersia nu exista.")
           })
  return(retval)
}


# -referitor la momente, nu sunt sigur daca o strategie mai buna
# ar fi sa scriem o singura functie moment, in care sa se indice cu un
# parametru daca e centrat sau initial, ca sa mai economisim cod
# -dezavantajul e evident ca functia generala de moment ar arata mai urat
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
  tmp <- contRV(densitate = subIntegrala, val = Vectorize(function(...) {retval <- 1}), bidimen = X@bidimen, suport = X@suport,
                densitateX = X@densitateX, densitateY = X@densitateY)
  
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
  tmp <- contRV(densitate = subIntegrala, val = Vectorize(function(...) {retval <- 1}), bidimen = X@bidimen, suport = X@suport,
                densitateX = X@densitateX, densitateY = X@densitateY)
  
  tryCatch(retval <- integrala(tmp),
           error= function(err)
           {
             stop(paste("Momentul initial de ordin ", ordin, " nu exista."))
           })
  return(retval)
}