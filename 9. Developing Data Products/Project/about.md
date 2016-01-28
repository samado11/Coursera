### About this web app

#### How to use

1. Choose the Machine Learning Method : 7 methods available

2. Set the portion of the training data set :
	The 'iris' dataset has 150 observations. The count of observation of Training dataset is set by this portion you selected.

3. Select the seed number :
	The random choice of the training set is performed by this seed number.

4. Press 'Run the Machine Learning' button. :-)

#### Objects of the confusion matrix

* Sensitivity = A/(A+C)
* Specificity = D/(B+D)
* Prevalence = (A+C)/(A+B+C+D)
* PPV = (sensitivity * Prevalence)/((sensitivity*Prevalence) + ((1-specificity)*(1-Prevalence)))
* NPV = (specificity * (1-Prevalence))/(((1-sensitivity)*Prevalence) + ((specificity)*(1-Prevalence)))
* Detection Rate = A/(A+B+C+D)
* Detection Prevalence = (A+B)/(A+B+C+D)
* Balanced Accuracy = (Sensitivity+Specificity)/2


*quoted from the package 'caret' [home page](http://topepo.github.io/caret/other.html). And, the source codes are [here](https://github.com/tansansu/Coursera/tree/master/9.%20Developing%20Data%20Products/Project).*
