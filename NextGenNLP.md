

NextGen NLP App
========================================================
author: Hariharan D
date: November 01, 2017
transition: rotate
autosize: true

Power of NLP in OneTouch
========================================================

<b>Future is power of accurate prediction!</b>

NextGen NLP is state-of-art OneTouch app, which gives power of word prediction.

- Improves sentence completion by predicting successive words
- Improves spelling accuracy
- Improves speed of typing

This app is available [here]().
  
Application Interface
========================================================

This app helps in dynamic prediction of successive words when the user provides input in the text box. This is similar to most of the smart phone keyboards prediction. Below is the screenshot of the app.

![NextGenNLP](*.png)

Application Design
========================================================

* Inputs from three sources (Twitter, News, Blogs) were considered for sampling
* Data were cleansed and the words are Tokenized
* N-Grams were created and stored in descending order in frequency tables
* "Stupid Backoff" algorithm was selected for N-Grams design
* Output data from each of the above N-Grams were saved in ".RData" format

Summary
========================================================

* Application response time was around 2 to 4 secs
* Accuracy of the application is correlated to sample size
* Application size was around 40 MB
* GitHub link for app artefacts are [here]()
* Source data can be found [here](https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)
* Milestone report can be found [here](http://rpubs.com/coolfox0407/capstone)

