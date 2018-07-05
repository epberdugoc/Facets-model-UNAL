
### Poisson - Gamma ##################################################

## semilla de la simulación:


# 1000 valores Poisson con lambda=5
set.seed(12345)
y=rpois(n = 1000, lambda = 5)

# 1000 valores Gamma con alpha= 2 y beta=1/2

set.seed(12345)
x=rgamma(n = 1000, shape = 2, scale = 0.5)

## histograma Gamma:

## instalarlo:
library(Rcmdr)

par(mfrow=c(1,2))
Hist(x,scale = "density", xlab="severidad")

.x <- seq(0.016, 4.999, length.out=1000)  
plotDistr(.x, dgamma(.x, shape=2, scale=0.5), cdf=FALSE, xlab="x", ylab="Density", 
          main=paste("Gamma Distribution:  Shape=2, Scale (inverse rate)=0.5"))

## Función de probabilidad Poisson:

tabla.poisson=table(y)

par(mfrow=c(1,2))

plot(as.numeric(names(tabla.poisson)), as.numeric(tabla.poisson/1000), type = "h", xlab="número de siniestros",ylab="frecuencia relativa")
points(as.numeric(names(tabla.poisson)), as.numeric(tabla.poisson/1000))

.x <- 0:14
plotDistr(.x, dpois(.x, lambda=5), xlab="x", ylab="Probability Mass", 
          main="Poisson Distribution:  Mean=5", discrete=TRUE)

## Funciones de distribución acumulada:

#Poisson:
.x <- 0:max(as.numeric(names(tabla.poisson)))
plotDistr(.x,cumsum(as.numeric(tabla.poisson/1000)) , xlab="x",ylab="Cumulative Probability", 
          main="Poisson Distribution:  Mean=5", cdf=TRUE)

#Gamma:
tabla.gamma=table(x)

.x <- sort(unique(x)) 
plotDistr(.x, cumsum(as.numeric(tabla.gamma/1000)), cdf=TRUE, xlab="x", 
          ylab="Cumulative ", 
          main=paste("Gamma Distribution:  Shape=2, Scale (inverse rate)=0.5"), discrete=TRUE)



### Resummén básico de estadísticas:

datos=data.frame(y,x)

numSummary(datos[,c("x", "y")], statistics=c("mean", "sd", "quantiles", "skewness",
                                             "kurtosis"), quantiles=c(.5,.75,.9,.95,.99), type="3")


# Con las fórmulas:

# medias:

media=function(x){
  sum(x)/length(x) 
}

media(x);media(y)

# Desviación estándar:

desv.est=function(x){
 sqrt(sum( (x-sum(x)/length(x))^2)/(length(x)-1))
}

desv.est(x);desv.est(y)

## Asimetría

asimetria=function(z){
  sum( (z-media(z))^3)/((length(z)-1)*desv.est(z)^3)
  
}

asimetria(x);

## Curtosis:

curtosis=function(z){
  sum( (z-media(z))^4)/((length(z)-1)*desv.est(z)^4)-3

}

curtosis(x);curtosis(y)











