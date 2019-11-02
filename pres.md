Next-Word: Text Prediction Tool
====
author: Austin L. Bistline
date: November 2, 2019
autosize: true

The Next-Word Prediction App accepts user input and predicts what the 
following word should be based on statistics from text entered into twitter, blogs, and news.

URL to link:

https://abistline.shinyapps.io/Next_Word/

Data Structure
====

* The data.table structure is the key to this prediction 
algorithm.  

* It allows fast queries for minimal delay as the 
user types.  

* The head and tail of the data table are shown below


```
         frequency                          ngrams   nxtwrd
      1:      2549                              of      the
      2:      2404                              in      the
      3:      1278                              to      the
      4:      1192                             for      the
      5:      1102                              on      the
     ---                                                   
2221713:         1   elliot explains how wives can   revive
2221714:         1   explains how wives can revive    those
2221715:         1      how wives can revive those feelings
2221716:         1 wives can revive those feelings       of
2221717:         1    can revive those feelings of   esteem
```

Creating the Data Structure
====

* The structure begins as text from twitter, blogs, and news corpus

* These different sources are combined into one large corpus

* 25,000 separate entries (documents or posts) are randomly sampled 
from the large corpus 

* From this sampled corpus, text is converted to lower case, then 
numbers, punctuation, and special characters are removed

* Ngrams are created from this corpus, with n=2, to n=6

* A statistics table is created for each level of ngram to count the number of occurences

* Statistic tables are combined into a single data frame

* Another data frame is created wherein the last word of each ngram is separated into another column

* The data frame is converted to a data table for more efficient queries.  This data table is seen in the previous slide


Building the Algorithm 
====

* The Next-Word Prediction App accepts user input

* Input is converted to lower case, then numbers and special characters 
are removed

* The next word is then defined as the word in the third column of the row containing the ngram that matches the converted user input

* If there is no match, the first word of the user input is removed until a match is found

* If all words are removed and there is still no match, the nextword is simply defined as a random word from the word column in the data table


Final Product
====

* The App is created in the Shiny environment

* It can be used as an aid in writing, as well as simple typing practice

* The background image creates a calming effect while the app is being used

Enjoy!

The markdown file and Shiny code can be found at: 

https://github.com/abistline/Data-Science-Capstone


