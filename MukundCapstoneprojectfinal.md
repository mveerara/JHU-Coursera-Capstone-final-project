Predict Next Word Application - Coursera Datascience specialization Capstone project
========================================================
author: Mukund
date:  29-03-2018
autosize: true

Background
========================================================

- As part of Coursera Data Science specialization capstone, created a Shiny Web application for Next work Prediction

- This application is based on NLP algorthims, and can be used in various domain and fields.

- Simple UI and powerful Intutition, both for Most likely and less likely Word predictions 

- Also shows Confidence levels for most likely words thus giving instant validation.

Approach taken
========================================================
- large corpus of Twitter, Blog and News data was downloaded and cleaned ( removal of ) before turning it into training corpus

- Training corpus was sampled judiciously and multiple times to get better accuracy without overfitting.

- To increase speed, used parallelization and kept the Workspace clean not using instances of data unnecessarily.

- Started with a simple back off algorithim, going from 4- gram down to one word, if input is not found in higher gram respectively.

- Used Kneser-ney for smoothing and back off with kneser ney. Kneser is known to best for smoothing.

- Used Skip grams gram 5 and gram 6 for unknown words.

- Confidence interval has been chosen as stastical validation tool. 

```

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-2](MukundCapstoneprojectfinal-figure/unnamed-chunk-2-1.png)
