# Task 1
getwd()

# Task 2
spruce.df = read.table(file.choose(), header = TRUE, sep =',')
head(spruce.df)

# Task 3
windows()
plot(Height~BHDiameter,pch = 21, bg= "Blue",ylim = c(0,1.1*max(Height)),xlim = c(0,1.1*max(BHDiameter)),  cex = 1.2, data = spruce.df)
title(main="Spruce Height Prediction")

library(s20x)
windows()
layout(matrix(1:3, nr = 3, nc = 1, byrow = TRUE))
layout.show(3)
trendscatter(Height~BHDiameter, f =0.5, data = spruce.df )
trendscatter(Height~BHDiameter, f =0.6, data = spruce.df )
trendscatter(Height~BHDiameter, f =0.7, data = spruce.df )

# make a linear model
spruce.lm = with(spruce.df, lm(Height~BHDiameter))
windows()
plot(Height~BHDiameter,pch = 21, bg= "Blue",ylim = c(0,1.1*max(Height)),xlim = c(0,1.1*max(BHDiameter)),  cex = 1.2, data = spruce.df)
title(main="Spruce Height Prediction")
abline(spruce.lm)

# Task 4
windows()
layout(matrix(1:4,nr = 2,nc =2, byrow = TRUE))
layout.show(4)
# Plot 1
plot(Height~BHDiameter,pch = 21, bg= "Blue",ylim = c(0,1.1*max(Height)),xlim = c(0,1.1*max(BHDiameter)),  cex = 1.2, data = spruce.df)
title(main="Spruce Height Prediction")
spruce.lm = with(spruce.df, lm(Height~BHDiameter))
abline(spruce.lm)
# Plot 2
plot(Height~BHDiameter,pch = 21, bg= "Blue",ylim = c(0,1.1*max(Height)),xlim = c(0,1.1*max(BHDiameter)),  cex = 1.2, data = spruce.df)
title(main="Spruce Height Prediction")
yhat=fitted(spruce.lm) # gives the ycoord of the fitted line
with(spruce.df,{segments(BHDiameter,Height,BHDiameter,yhat)})
abline(spruce.lm)
RSS = with(spruce.df, sum((Height-yhat)^2))
# Plot 3
plot(Height~BHDiameter,pch = 21, bg= "Blue",ylim = c(0,1.1*max(Height)),xlim = c(0,1.1*max(BHDiameter)),  cex = 1.2, data = spruce.df)
title(main="Spruce Height Prediction")
with(spruce.df, abline(h=mean(Height)))
abline(spruce.lm)
with(spruce.df, {segments(BHDiameter, mean(Height), BHDiameter, yhat, col = "Red")})
MSS = with(spruce.df,sum((yhat-mean(Height))^2))  
# Plot #4
plot(Height~BHDiameter,pch = 21, bg= "Blue",ylim = c(0,1.1*max(Height)),xlim = c(0,1.1*max(BHDiameter)),  cex = 1.2, data = spruce.df)
title(main="Spruce Height Prediction")
with(spruce.df,abline(h=mean(Height)))
with(spruce.df,{segments(BHDiameter, Height,BHDiameter,mean(Height), col = "Green")})
TSS = with(spruce.df, sum((Height-mean(Height))^2))  
TSS
RSS
MSS
MSS/TSS
MSS+RSS

# Task 5
summary(spruce.lm)
# slope = 0.48147
# Intercept = 9.14684
# Height = 9.14684 + 0.48147*BHDiameter

# Predict Height for BHDiameters = 15,18 & 20
predict(spruce.lm, data.frame(BHDiameter = c(15,18, 20)))

# Task #6
library(ggplot2)
g = ggplot(spruce.df, aes(x = BHDiameter, y = Height, colour = BHDiameter))
g = g+geom_point()+ geom_line()+geom_smooth(method="lm")
g +ggtitle("Height vs BHDiameter")
