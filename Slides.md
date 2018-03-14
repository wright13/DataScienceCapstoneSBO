FinishMyThought
========================================================
author: Sarah Wright
date: March 13, 2018
autosize: true

Overview
========================================================
Have you ever started typing, only to find yourself at a loss for words? Try FinishMyThought! FinishMyThought uses a streamlined prediction algorithm to suggest your next word every time you pause for thought.

App features include:
- Multiple predictions mean that FinishMyThought is more likely to think of the word that's on the tip of your tongue
- Predictions are generated every time you stop typing. No need to click Submit - just select the word you want, and then keep typing!

Technical Details
========================================================
FinishMyThought uses a simple but effective word prediction algorithm called Stupid Backoff. Stupid Backoff relies on a large collection of the most frequent short phrases (1-4 words) that appear online in blog posts, tweets, and news articles. Each phrase is linked to the word that most frequently appears after it. In the case of FinishMyThought, a phrase may also be linked to the second- and third-most frequent next words (this allows for multiple predictions).

The collection of phrases is searched for the last four words of your input into FinishMyThought, and the Stupid Backoff algorithm returns the word(s) most likely to follow. If those four words are not found, the last three words of input are used instead, and so on until a prediction is found. If even the last word of input doesn't appear in the collection of known phrases, the algorithm still does the best it can by predicting words that most frequently appear after uncommon words (these were consolidated under a single code in the phrase collection).


Performance by the numbers
========================================================
- **5000**:
    + The approximate number of phrases used to test the Stupid Backoff algorithm
- **17%**:
    + Single-word accuracy (the percentage of the time that the word predicted to be most likely was in fact correct)
- **27%**:
    + Three-word accuracy (the percentage of the time that one of the predicted words was correct)


Shiny App
========================================================
