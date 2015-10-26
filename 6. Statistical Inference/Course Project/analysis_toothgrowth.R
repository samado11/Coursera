#Now in the second portion of the class, we're going to analyze the ToothGrowth data in the R datasets package. 
#Load the ToothGrowth data and perform some basic exploratory data analyses 
#Provide a basic summary of the data.
#Use confidence intervals and/or hypothesis tests to compare tooth growth by supp and dose. (Only use the techniques from class, even if there's other approaches worth considering)
#State your conclusions and the assumptions needed for your conclusions. 
#Some criteria that you will be evaluated on
#Did you  perform an exploratory data analysis of at least a single plot or table highlighting basic features of the data?
#Did the student perform some relevant confidence intervals and/or tests?
#Were the results of the tests and/or intervals interpreted in the context of the problem correctly? 
#Did the student describe the assumptions needed for their conclusions?
#data("ToothGrowth")

# EDA of "ToothGrowth" dataset
dim(ToothGrowth)
str(ToothGrowth)
?ToothGrowth
#len : numeric	Tooth length
#supp : factor	Supplement type (VC or OJ).
#dose : numeric	Dose in milligrams.

#1. Basic EDA
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
par(mfrow = c(2, 1))
plot(len ~ supp, data = ToothGrowth, main = "Comparison between 'Orange Juice' and 'Vitamin C'")
plot(len ~ dose, data = ToothGrowth, main = "Comparison of amount of dose")

with(ToothGrowth, tapply(len, supp, mean))
with(ToothGrowth, tapply(len, dose, mean))

#2. t test for comparison between Supplement type
if (!require(tidyr)) { install.packages("tidyr") }
if (!require(dplyr)) { install.packages("dplyr") }

Supp.df <- ToothGrowth %>% select(supp, len) %>% spread(supp, len)
with(Supp.df, t.test(VC, OJ))

#3. t test for comparison among the amount of dosae
Dose.df <- ToothGrowth %>% select(dose, len) %>% spread(dose, len)
names(Dose.df) <- c("a_0.5", "a_1", "a_2")

Dose.t_1 <- with(Dose.df, t.test(a_0.5, a_1))
Dose.t_1$p.value
Dose.t_1$conf.int

Dose.t_2 <- with(Dose.df, t.test(a_0.5, a_2))
Dose.t_2$p.value
Dose.t_2$conf.int

Dose.t_3 <- with(Dose.df, t.test(a_1, a_2))
Dose.t_3$p.value
Dose.t_3$conf.int
