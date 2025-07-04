#Creado por: Aguas Montaño Sergio José - 217815601

#Entorno de trabajo
getwd()
setwd("E:/Personal/UNI/8vo Semestre/SSPIAII/Trabajos GitHub/SSPIAII.24A.AguasMontanoSergio/Rstudio")

options(scipen = 999)
set.seed(2000)


library(ggplot2)

#EJEMPLO 1 LDA
ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = dnorm, args = list(mean = -1.25, sd = 1),
                color = "firebrick") + 
  stat_function(fun = dnorm, args = list(mean = 1.25, sd = 1.5), color = "green3") +
  geom_vline(xintercept = 0, linetype = "longdash") +
  theme_bw()


#EJEMPLO 2 LDA
grupo_a <- rnorm(n = 30, mean = -1.25, sd = 1)
grupo_b <- rnorm(n = 30, mean = 1.25, sd = 1)
datos <- data.frame(valor = c(grupo_a, grupo_b),
                    grupo = rep(c("A","B"), each = 30))

ggplot(data = datos, aes(x = valor, fill = grupo)) +
  geom_histogram(alpha = 0.5, position = "identity") +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_vline(xintercept = (mean(grupo_a) + mean(grupo_b))/2)  +
  annotate(geom = "text", x = 1.5, y = 9, label = "Límite decisión LDA") +
  annotate(geom = "text", x = -1.5, y = 10, label = "Límite decisión Bayes") +
  theme_bw() + 
  theme(legend.position = "top")


#EJEMPLO 3 LDA
mu1 <- 0 # set mean x1
mu2 <- 0 # set mean x2
s11 <- 10 # set variance x1
s22 <- 10 # set variance x2
s12 <- 15 # set covariance x1 and x2
rho <- 0.5 # set correlation coefficient  x1 and x2
x1 <- seq(-10,10,length = 41) # generate vector  x1
x2 <- x1 # copy x1 to x2

f <- function(x1,x2) # multivariate function
{
  term1 <- 1/(2 * pi * sqrt(s11*s22*(1 - rho^2)))
  term2 <- -1/(2 * (1 - rho^2))
  term3 <- (x1 - mu1)^2/s11
  term4 <- (x2 - mu2)^2/s22
  term5 <- -2*rho*((x1 - mu1)*(x2 - mu2))/(sqrt(s11)*sqrt(s22))
  term1*exp(term2*(term3 + term4 - term5))
} 

z <- outer(x1,x2,f) # calculate density values

persp(x1, x2, z, # 3-D plot
      main = "Distribución multivariante con dos predictores",
      col = "lightblue",
      theta = 30, phi = 20,
      r = 50,
      d = 0.1,
      expand = 0.5,
      ltheta = 90, lphi = 180,
      shade = 0.75,
      ticktype = "simple",
      nticks = 5)


#EJEMPLO 4 LDA
install.packages("klaR")
library(klaR)
partimat(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width,
         data = iris, method = "lda", prec = 200,
         image.colors = c("darkgoldenrod1", "snow2", "skyblue2"),
         col.mean = "firebrick")