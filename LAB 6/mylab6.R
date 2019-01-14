# Task 1
getwd()

# Task 2
windows()
layout(matrix(1:4, nr=2,nc=2))
layout.show(4)

# normal distribution with mean = 10 and sd = 4
curve(dnorm(x, mean=10,sd=4),xlim=c(10-3*4,10+3*4))
# mu = 10, sd = 2
curve(dnorm(x, mean=10,sd=2),xlim=c(10-3*2,10+3*2))
# mu = 5, sd = 10
curve(dnorm(x, mean=5,sd=10),xlim=c(5-3*10,5+3*10))
# mu = 5, sd =0.5
curve(dnorm(x, mean=5,sd=0.5),xlim=c(5-3*0.5,5+3*0.5))

# Y~N(0,1),P(Y???2)
curve(dnorm(x,mean = 0, sd = 1), xlim = c(0-3*1,0+3*1))
xcurve = seq(2,3,length = 1000)
ycurve = dnorm(xcurve,mean = 0, sd = 1)
# fill polygon with the given vertices
polygon(c(2,xcurve,3), c(0,ycurve,0),col = "Red")
# Area of shaded region
prob = 1 - pnorm(2,mean =0, sd = 1)
prob = round(prob,4)
# Paste prob in shaded regions
text(locator(1), paste("Area = ", prob, sep=""))

# 	Y~N(??=4,??=2),P(1???Y<5)
windows()
curve(dnorm(x,mean =4, sd =2), xlim = c(4-3*2,4+3*2))
xcurve = seq(1,5, length =1000)
ycurve = dnorm(xcurve, mean =4, sd = 2)
polygon(c(1,xcurve,5),c(0,ycurve,0),col = "Blue")
# Area
prob = pnorm(5,mean =4, sd =2) - pnorm(1, mean =4, sd =2)
prob = round(prob,4)
text(locator(1), paste("Area =", prob, sep = ""))

# 	Y~N(??=10,??=4),P(Y<10)
curve(dnorm(x, mean = 10, sd = 4), xlim = c(10-3*4,10+3*4))
xcurve = seq(10-3*4,10, length =1000)
ycurve = dnorm(xcurve, mean =10, sd = 4)
polygon(c(10-3*4,xcurve,10),c(0,ycurve,0), col = "Yellow")
# Area
prob = pnorm(10,mean =10, sd =4)
text(locator(1), paste("Area = ", prob, sep =""))

# 	Y~N(??=-2,??=1/2),P(-3<Y??? -2)
mu = -2; std =1/2;
curve(dnorm(x, mean = mu, sd = std), xlim = c(mu - 3*std,mu+3*std))
xcurve = seq(-3,-2,length = 2000)
ycurve = dnorm(xcurve,mean = mu, sd = std)
polygon(c(-3,xcurve,-2), c(0,ycurve,0), col ="Green")
# Area 
prob = pnorm(-2,mean = mu, sd = std) - pnorm(-3, mean = mu, sd =std)
prob = round(prob,4)
text(locator(1), paste("Area = ", prob, sep =""))

# Task 3
#	Y~Gamma(shape=1,scale=1)
curve(dgamma(x,shape=1,scale=1),xlim=c(0,10),ylim=c(0,1),col="Red",lwd=2,
      ylab="Gamma density", main="Beta=1")
text(locator(1),paste("alpha=",1))
curve(dgamma(x,shape=3,scale=1),xlim=c(0,10),ylim=c(0,1),add = TRUE,lwd=2)
text(locator(1),paste("alpha=",3))
curve(dgamma(x,shape=5,scale=1),xlim=c(0,10),ylim=c(0,1),add = TRUE,col = "Blue",lwd=2)
text(locator(1),paste("alpha=",5))

# 	Y~Gamma(shape=3,scale=2),P(2<Y<5)
curve(dgamma(x,shape = 3, scale = 2), xlim = c(0,20), ylim = c(0,1))
xcurve = seq(2,5,length =1000)
ycurve = dgamma(xcurve, shape = 3, scale =2)
polygon(c(2,xcurve,5), c(0,ycurve,0),col = "Green")

# Area
prob = pgamma(5,shape =3, scale = 2) - pgamma(2, shape = 3, scale = 2 )
prob = round(prob,4)

text(locator(1), paste("Area = ", prob, sep =""))

# Y???Gamma(shape=6,scale=3),P(1???Y???4)
windows()
curve(dgamma(x,shape=6,scale=3),xlim=c(0,10),ylim=c(0,.1),col="Red",lwd=2,
      ylab="Gamma density", main="Beta=1")
xcurve = seq(1,4,length =1500)
ycurve = dgamma(xcurve,shape = 6, scale =3)
polygon(c(1,xcurve,4),c(0,ycurve,0), col ="Cyan")

# Area
prob = pgamma(4,shape =6, scale =3) - pgamma(1, shape = 6, scale = 3 )
prob = round(prob,4)

text(locator(1), paste("Area =", prob, sep = ""))

# Y~Gamma(shape=2,scale=4),P(3???Y<6)
windows()
curve(dgamma(x,shape =2, scale = 4), xlim = c(0,10))
xcurve = seq(3,6, length =1000)
ycurve = dgamma(xcurve,shape =2, scale = 4)
polygon(c(3,xcurve,6),c(0,ycurve,0), col = "Blue")

# Area
prob = pgamma (6,shape =2, scale = 4) - pgamma(3,shape =2, scale = 4)
prob = round(prob,4)
text(locator(1), paste("Area =", prob, sep =""))


# Task 4
windows()
layout(matrix(1:4,nr = 2, nc =2, byrow = TRUE))
layout.show(4)
# Y~chisq(df=1) 
curve(dchisq(x,df=1),xlim=c(0,10),ylim=c(0,1),col="Red",lwd=2,
      ylab="Chisq density", main="df=1")
# Y~chisq(df=2)
curve(dchisq(x,df=2),xlim=c(0,10),ylim=c(0,1),col="Red",lwd=2,
      ylab="Chisq density", main="df=2")
# Y~chisq(df=4)
curve(dchisq(x,df=4),xlim=c(0,10),ylim=c(0,1),col="Red",lwd=2,
      ylab="Chisq density", main="df=4")
# Y~chisq(df=20)
curve(dchisq(x,df=20),xlim=c(0,10),ylim=c(0,1),col="Red",lwd=2,
      ylab="Chisq density", main="df=20")

# probabilities 
# Y~chisq(df=2),P(2???Y???4)
windows()
curve(dchisq(x,df=2),xlim=c(0,10),ylim=c(0,1),col="Black",lwd=2,
      ylab="Chisq density", main="df=2")
xcurve = seq(2,4,length =1000)
ycurve = dchisq(xcurve, df = 2)
polygon(c(2,xcurve,4),c(0,ycurve,0), col = "Magenta")
# Area
prob = pchisq(4,df =2) - pchisq(2,df =2)
prob = round(prob,4)
text(locator(1), paste("Area =", prob, sep =""))

# Y~chisq(df=3),P(3???Y???5)
curve(dchisq(x,df=3),xlim=c(0,10),ylim=c(0,1),col="Black",lwd=2,ylab="Chisq density", main="df=3")
xcurve = seq(3,5,length =1000)
ycurve = dchisq(xcurve, df = 3)
polygon(c(3,xcurve,5),c(0,ycurve,0), col = "Red")
# Area
prob = pchisq(5,df =3) - pchisq(3,df =3)
prob = round(prob,4)
text(locator(1), paste("Area =", prob, sep =""))


# Y~chisq(df=20),P(10<Y???21) 
curve(dchisq(x,df=20),xlim=c(0,30),ylim=c(0,1),col="Black",lwd=2,
      ylab="Chisq density", main="df=3")
xcurve = seq(10,21,length =1000)
ycurve = dchisq(xcurve, df = 20)
polygon(c(10,xcurve,21),c(0,ycurve,0), col = "Blue")
# Area
prob = pchisq(21,df =20) - pchisq(10,df =20)
prob = round(prob,4)
text(locator(1), paste("Area =", prob, sep =""))

windows()
layout(matrix(1:4,nr =2,nc =2, byrow = TRUE))
layout.show(4)
curve(dweibull(x,shape = 1, scale = 3 ),xlim=c(0,10),ylim=c(0,1),lwd=2,
      ylab="Weibull density", main="Shape = 1, Scale =3")
curve(dweibull(x,shape = 2, scale = 3 ),xlim=c(0,10),ylim=c(0,1),lwd=2,
      ylab="Weibull density", main="Shape = 2, Scale =3")
curve(dweibull(x,shape = 2, scale = 1 ),xlim=c(0,10),ylim=c(0,1),lwd=2,
      ylab="Weibull density", main="Shape = 2, Scale =1")
curve(dweibull(x,shape = 4, scale = 2 ),xlim=c(0,10),ylim=c(0,1),lwd=2,
      ylab="Weibull density", main="Shape = 4, Scale =2")


# This is my function to generate Weibull probabilities
myweibull = function(shape = 1,scale = 3,col = "Red",lb = 2,ub = 4) {
  curve(dweibull(x,shape = shape, scale = scale ),xlim=c(0,10),ylim=c(0,1),lwd=2,
        ylab="Weibull density", main="Weibull")
  xcurve = seq(lb,ub,length =1000)
  ycurve = dweibull(xcurve, shape = shape, scale = scale)
  polygon(c(lb,xcurve,ub),c(0,ycurve,0), col = col)
  # Area
  prob = pweibull(ub,shape =shape, scale = scale) - pweibull(lb,shape =shape, scale = scale)
  prob = round(prob,4)
  text(locator(1), paste("Area =", prob, sep =""))
}
# P(2<=Y<=4) Y~Weibull(shape =1, scale =3)
windows()
myweibull(shape = 1,scale = 3, col = "Magenta", lb = 2, ub = 4)
#P(1<=Y<=5) Y~Weibull(shape =2, scale =3)
myweibull(shape =2, scale =3, col ="Red", lb = 1, ub = 5)
#P(2<=Y<=4) Y~Weibull(shape =2, scale =1)
myweibull(shape =2, scale =1, col ="Blue", lb = 2, ub = 4)
