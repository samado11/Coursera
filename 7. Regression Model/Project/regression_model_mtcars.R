#START : 2015.11.16
#END : 

data(mtcars)
head(mtcars)
?mtcars

#EDA for 'mtcars' dataset.
pairs(mtcars, panel = panel.smooth, main="Pair Graph of Motor Trend Car Road Tests")

par(mfrow = c(1, 2), mar=c(3.1, 3.1, 1.1, 2.1))
hist(mtcars$mpg[mtcars$am == 0], col = "pink", main = "MPG with Automatic(am=0)")
boxplot(mtcars$mpg[mtcars$am == 0], horizontal=T,  outline=T, frame=F, col = "royalblue", add = T)
hist(mtcars$mpg[mtcars$am == 1], col = "pink", main = "MPG with Manual(am=1)")
boxplot(mtcars$mpg[mtcars$am == 1], horizontal=T,  outline=T, frame=F, col = "royalblue", add = T)

#Comparing mean values by the am values
with(mtcars, tapply(mpg, am, mean))

#Inference(t.test)
#HO, H1
with(mtcars, t.test(mpg ~ am))

#Regression Model
mtcars$am <- factor(mtcars$am)
summary(step(lm(mpg ~ ., data = mtcars)))

#Residual diagnostics
plot(step(lm(mpg ~ ., data = mtcars)))
influence.measures(step(lm(mpg ~ ., data = mtcars)))
