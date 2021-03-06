Words You May Type
========================================================
author: Julian Jang
date: 2016-04-19
autosize: true

What is it?
========================================================
This app guesses what is the word you may think  
and recommends the maximum three words that are highly probable.

- The design of the web is 'google.com' style. :-)
- The delay time showing the recommended words is less than 3 sec.
- I used 'n-gram' and the 'Back-Off model' for this work.
- Even if using it is very very easy, it has the instruction page, too.

__<small>The app link__ <https://tansansu.shinyapps.io/Next_Word_Prediction/>  
__All source codes and data of whole project are in [my Github Repository](https://github.com/tansansu/10.-Capstone-Project)</small>__


N-Gram
========================================================
*<small>`N-Gram` is a contiguous sequence of n items from a given sequence of text.([Wikipedia](https://en.wikipedia.org/wiki/N-gram))</small>*

- <small>I sampled the 20,000 sentences from each raw text files(twitter, news, blogs).</small>
- <small>I made the quad-gram, tri-gram, bi-gram and uni-gram from the above sampled data. And, I merged each n-grams into one data set.</small>
- <small>The data set has 1,655,508 rows and 3 columns. And, the shape of the table is below.</small>

| Lookup | Recommend | Freq |
|---|---|---|
|the end of|the|72|
|one of|the|347|
|of|the|4165|
|...|...|...|


Prediction Algorithm - Back-Off Model
========================================================
*<small>Katz back-off is a generative n-gram language model that estimates the conditional probability of a word given its history in the n-gram.([Wikipedia](https://en.wikipedia.org/wiki/Katz%27s_back-off_model))*

* If some words are inputted, the app searches matched n-gram in 'Lookup' column. After that, it is going to recommend a word in 'Recommend' column on the same row with Lookup' column of matched n-gram with inputted words.
* Next, those words are grouped by same words and computed the probability of the appearance. The formula I set is below.

```
[ { (0.5) x '4-gram' + (0.3) x '3-gram' + (0.2) x '2-gram' } x 0.95 ] + { '1-gram' x (0.05) }
```

* The 1-gram word in above formula is the recommended word by 4~2-gram through 'Back-Off model'. I added 0.05 weight to the 1-gram words to give a priority to words which people frequently used.
* If it fail to match with any n-grams, this app shows the most frequently used three words(1. the, 2. to, 3. and).</small>

The result of this model
========================================================

### <small>The accuracy of the train dataset : 50.6%
* train set: I used the sample of the data frame that was used making the prediction model.  

```r
index_train <- sample(1:nrow(grams_app), 1000)
trainset <- grams_app[index_train, ]
trainset$result <- sapply(trainset$Lookup, pred_word) #pred_word: The function of predicting words
sum(trainset$Recommend == trainset$result) / nrow(trainset)
```

### The accuracy of the test dataset : 21.5%
* I used the resampled data that was sampled from each raw text data(blogs, news, twitter).

```r
index_test <- sample(1:nrow(test_grams), 1000)
testset <- test_grams[index_test, ]
testset$result <- sapply(testset$Lookup, pred_word)
sum(testset$Recommend == testset$result) / nrow(testset)
```
</small>
