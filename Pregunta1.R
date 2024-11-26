dpois(30,25)
exp(-25)*(25^30)/factorial(30)

x<- 0:40
plot(x,dpois(x,25),type="h",col="red")

ppois(30,25)
plot(x,ppois(x,25),type="s", col="red")

ppois(25,25)-ppois(19,25)
########################
#Ejercicio como tal#
#    dexp= f(x)     pexp= F(x)    rexp= Simulacion
#    qexp= F^-1(0.5) -> Median    F^-1(0.25) -> 1er quartil

#el primero da 0 ya que seria la integral de 0.33 a 0.33 de f(x) y da 0#

t<- seq(0,2,0.01)

dexp(t,25)
plot(t,dexp(t,25),type="l", col="red" )
plot(t,pexp(t,25),type="l", col="red")

1-pexp(0.052,25)

rexp(5,25) #simulacion de 5 repeticiones

mean(rexp(500000,25))

