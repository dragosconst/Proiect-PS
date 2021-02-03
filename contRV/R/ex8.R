#fiecare repartitie este salvata intr-o lista de String, fiecare 2 elemente din lista fiind subtitlul(cu indentare) si informatia respectiva
rep_uniforma <- c("    Definitie:","O v.a. U este repartizata uniform pe intervalul (a,b) daca admite densitatea de repartitie f(x)=1/(b-1), x in (a,b).",
                  "    Utilizare:","Se foloseste atunci cand numarul cazurilor posibile tinde la infinit",
                  "    Notatie:","U ~ U((a,b)), unde (a,b) este intervalul pe care este repartizata variabila aleatoare",
                  "    Media:","E[U] = integrala de la -Inf la Inf din x*f(x) dx = (a+b)/2",
                  "    Variatia:","Var(U) = E[U^2]-E[U]^2 = ((b-a)^2)/12",
                  "    Sursa:","http://cs.unitbv.ro/~pascu/stat/Distributii%20continue%20clasice.pdf")

rep_exponentiala <- c("    Definitie:","O v.a. X este repartizata exponential de parametru lambda > 0 daca densitatea de repartitie a lui X este de forma f(x) = lambda * e^(-lambda*x), x > 0",
                      "    Utilizare:","Modeleaza timpul de asteptare pana la aparitia unui eveniment de interes.",
                      "    Notatie:","X ~ Exp(lambda)",
                      "    Media:","E[X] = integrala de la -Inf la Inf din x*f(x) dx = 1/lambda",
                      "    Variatia:","Var(X) = E[X^2] - (E[x])^2 = 1/(lambda^2)",
                      "    Sursa:","Curs 10, Probabilitati si Statistica, Prof. Alexandru Amarioarei")


rep_normala <- c("    Definitie:","Spunem ca o v.a. X este repartizata normal (sau Gaussian) de parametru miu si sigma^2 daca admite densitatea f(x) = (1/(sqrt(2pi)*sigma))*e^(-(x-miu)^2/2*sigma^2), unde x in R)",
                 "    Notatie:","X ~ N(miu,sigma^2)","In cazul in care miu = 0 si sigma = 1 spunem ca v.a. X este repartizata normal standard si notam X ~ N(0,1) si putem calcula:",
                 "    Media:","E[U] = integrala de la -Inf la Inf din x*f(x) dx = 0",
                 "    Variatia:","Var(U) = E[U^2]-E[U]^2 = 1 - 0 = 1",
                 "    Sursa:","Curs 10, Probabilitati si Statistica, Prof. Alexandru Amarioarei")


rep_gamma   <- c("    Definitie:","O v.a. X este repartizata Gamma cu parametrii lambda si p daca are densitatea de probabilitate de forma fX(x) = (lambda^p * x^(p-1) * e^(-lambda*x))/gamma(p), pentru x>0 ",
                 "    Notatie:","X ~ Gamma[p, lambda], unde integrala lambda(n) = (n-1)!",
                 "    Media:","E[U] = integrala de la -Inf la Inf din x*f(x) dx = p/lambda",
                 "    Variatia:","Var(U) = E[U^2]-E[U]^2 = p/lambda^2",
                 "    Sursa:","http://math.etc.tuiasi.ro/rstrugariu/cursuri/SPD2015/c7.pdf")


rep_beta    <- c("    Definitie:","O v.a. X are repartitie Beta daca densitatea sa de probabilitate este de forma: f(x,a,b) = (1/(beta(a,b))) * x^(a-1) * (1-x)^(b-1), pentru x in [0,1], unde a > 0 si b > 0 si beta(a,b) = integrala de la 0 la 1 din x^(a-1)*(1-x)^(b-1) dx ",
                 "    Notatie:","",
                 "    Media:","E[U] = integrala de la -Inf la Inf din x*f(x) dx = a/(a+b)",
                 "    Variatia:","Var(U) = E[U^2]-E[U]^2 = ab/((a+b)^2*(a+b+1)) ",
                 "    Sursa:","http://images.wikia.com/nccmn/ro/images/3/37/Capitolul_10_REPARTITII_CLASICE.pdf")


rep_X2     <- c("    Definitie:","O v.a. X este distribuita X-patrat cu n grade de libertate daca are densitatea de probabilitate de forma: f(x) = (1/(2^(n/2) * gamma(n/2))) * x^((n/2)-1) * e^(-x/2), pentru x > 0 ",
                 "    Notatie:","X ~ X^2(n), unde n reprezinta numarul de grade de libertate",
                 "    Media:","E[U] = integrala de la -Inf la Inf din x*f(x) dx = n",
                 "    Variatia:","Var(U) = E[U^2]-E[U]^2 = 2n",
                 "    Sursa:","http://math.etc.tuiasi.ro/rstrugariu/cursuri/SPD2015/c7.pdf")


rep_student <- c("    Definitie:","O v.a. X are o distributie T(sau distributie Student) cu n grade de libertate daca poate fi scrisa sub forma X=Y/sqrt(Z/n), unde Y ~ N(0,1) este o variabila normala standard iar Z ~ X^2(n) este o variabila aleatoare X^2 cu n grade de libertate independenta de Y. ",
                 "    Notatie:","X ~ T(n), unde n reprezinta numarul de grade de libertate",
                 "    Media:","E[U] = integrala de la -Inf la Inf din x*f(x) dx = 0",
                 "    Variatia:","Var(U) = E[U^2]-E[U]^2 = n/(n-2)",
                 "    Sursa:","http://cs.unitbv.ro/~pascu/stat/Distributii%20continue%20clasice.pdf")
#TODO
# notatie pentru rep_beta

# Functia Fisa_sinteza afiseaza informatiile despre o repartitie mai compact(fara randuri goale). De asemenea reprezinta o metoda mai intuitiva de a afisa aceste informatii.
Fisa_sinteza <- function(Rep){
  for (i in Rep)
  {
    print(i)
  }

}
