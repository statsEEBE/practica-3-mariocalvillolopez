#Ensayo de Bernoulli
x<- c(0,1)
f <- c(0.68,0.32)

plot(x, f, type ="h", ylim=c(0,1), col="red")
points(x,f,pch=16,col="red")


n<- 43
muestra<-sample(x,n,f,replace=TRUE)

pie(table(muestra))
mean(muestra)

table(muestra)/n
mean(muestra)

bar<-barplot(table(muestra)/n,ylim=c(0,1))
lines(bar,f,type="h",col="red")
points(bar,f,pch=16,col="red")

muestra <- sample(x,n,f,replace=TRUE)
muestra



Y<-function(i){sum(sample(x,n,f,replace=TRUE))}
Y(1)

set.seed(123)
m<- 400000
encuestas <- sapply(1:m,Y)
fr<-table(encuestas)/m
fr["13"]


dbinom(13,43,0.32)

xx<- names(fr)
xx
br <- barplot(table(encuestas)/m)
lines(br,dbinom(2:29,43,0.32),type="h",col="red")
points(br,dbinom(2:29,43,0.32),pch=16,col="red")

dbinom(17,44,0.32)
plot(0:43,dbinom(0:43,44,0.32),type="h",col="red")


pbinom(16,44,0.32)




n <- 24
x<-c(0,1)
f<- c(0.32,0.68)

Xstar<-function(i){sum(sample(x,n,f,replace=TRUE))}
set.seed(123)
m<- 400000
encuestas <- sapply(1:m,Xstar)
mean(encuestas)

n*0.68

var(encuestas)

n*0.68*0.32

qbinom(0.25,24,0.68)

plot(0:24,dbinom(0:24,24,0.68),type="h",col="red")

46*0.32

# SOlucion Ejercicio

x1 = 0:5   # Posibles resultados
f1 = c(1,2,2,3,4,3)/15 # Probabilidad de ocurrencia de cada resultado
# Creación de las líneas verticales
plot(x1, f1, type="h", col="red", lwd=3, main="Función de probabilidad", xlab="X", ylab="f(x)",
     xlim=c(-0.5,5.5), ylim=c(0,0.3)) 
# Se crean los puntos y se guarda la gráfica completa en un objeto para su uso posterior
points(x1, f1, col="red", lwd=10)




F1 = cumsum(f1) # Se genera un vector con la suma acumulada
plot(c(-1,x1,6), c(0,F1,1), type="s", col="red", lwd=3, main="Función de distribución", xlab="X",
     ylab="F(x)")
points(x1, F1, col="red", lwd=8)




#f(2)=2/15
#F(2)=1/3
#f(3.5)=0
#F(3.5)=8/15
#f(6)=0
#F(6)=1


miu.X <- sum(x1*f1); miu.X


Q2.X <- max(x1[F1<=0.5]); Q2.X


var.X <- sum((x1-miu.X)^2*f1); var.X


set.seed(12)
sim.venta <- sample(x1,30,replace=T,prob=f1)


fi <- table(sim.venta)/length(sim.venta) 
xb <- barplot(fi)
lines(xb, f1, type="h", col="red", lwd=3)  
points(xb, f1, col="red", lwd=10)

mean.sim <- mean(sim.venta); mean.sim

var.sim <- var(sim.venta); var.sim


set.seed(12)
sim.venta <- sample(x1,10000,replace=T,prob=f1)
fi <- table(sim.venta)/length(sim.venta) 
xb <- barplot(fi)
lines(xb, f1, type="h", col="red", lwd=3)  
points(xb, f1, col="red", lwd=10)


mean.sim <- mean(sim.venta); mean.sim

var.sim <- var(sim.venta); var.sim


miu.Y <- 0.75*miu.X-1.5; miu.Y
var.Y <- 0.75^2*var.X; var.Y

set.seed(12)
y1 <- 0.75*x1-1.5
sim.ben <- sample(y1,10000,replace=T,prob=f1)
fi <- table(sim.ben)/length(sim.ben) 
xb <- barplot(fi)
lines(xb, f1, type="h", col="red", lwd=3)  
points(xb, f1, col="red", lwd=10)


mean.sim.ben <- mean(sim.ben); mean.sim.ben

var.sim.ben <- var(sim.ben); var.sim.ben
