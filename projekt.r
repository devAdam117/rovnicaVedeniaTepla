############################################
# u_t - u_xx = 0,      x \in [0, 1], t > 0 #
# u(x,0) = x^3 +x^2,   x \in [0, 1]        #
# u(0,t) = t,          t > 0               #
# u(1,t) = 2 + t,      t > 0               #
############################################

# 0) Potrebna kniznica:
#install.packages("animation",dependencies=TRUE)
library(animation)

# 1) Definicia funkcie u:
u <- function(x,t,N){     # N clenov z nekonecnej sumy
  k <- 1:N
  # zaciatocna podmienka
  alfa0 <- (16*k*pi*(-1)^(k)-4*k*pi)/(pi*k)^4
  # partikularna cast
  alfaP <- 2*((-1)^k - 1)/(k*pi)^3 
  # koeficient v homogennej casti
  c <- alfa0 - alfaP 
  u <- rep(NA, times=length(x))
  v <- rep(NA, times=length(x))
  for (i in 1:length(x)){
    alfa <- c*exp(-k^2*pi^2*t)
    v[i] <- sum((alfa+alfaP)*sin(k*pi*x[i]))
    u[i] = v[i]+t+2*x[i]
  }
  return(u)
}

# 2) Priklad grafu, podla casu:
x <- seq(from=0, to=1, by=0.01)
plot(x, u(x,0,100), type="l", xlim=c(0,1), ylim=c(0,2), col="black")  # t = 0
lines(x, u(x,0.02,100), type="l") # t = 0,02
lines(x, u(x,0.04,100), type="l") # t = 0,04
lines(x, u(x,0.06,100), type="l") # t = 0,06
lines(x, u(x,0.08,100), type="l") # t = 0,08
lines(x, u(x,0.1,20), type="l")  # t = 0,1

# 3) Animacia:
timeInt <- seq(from=0, to=.5, by=0.01)
x <- seq(from=0, to=1, by=0.01)
ani.record(reset = TRUE)
for (i in 1:length(timeInt)) {
  plot(x, u(x,timeInt[i],100),col="blue",type="l",
       xlim=c(0,1), ylim=c(0,2),
       xlab="x", ylab="u(x,t)")
  ani.record()  
}
ani.options(interval = 0.25)
ani.replay()

# 4) Ulozenie animacie ako html:
saveHTML(ani.replay(),
         img.name = "priklad",       
         htmlfile = "./pdr.html") 