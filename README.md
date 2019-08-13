# caretStack
This package is primarily intended for internal use at the Laureate Institute for Brain Reasearch. The executable functions in this package are meant for implementation on LIBR datasets. The following is a step-by-step guide for how to create a stacked ML model on LIBR data using LIBR resources.

The plan is to create a caretStack2 for broader use shortly! For now, here is the tutorial for how to run this process in R and on the LIBR servers.

Be sure to check back regularly for updates!

## Step 1: Installation
To install in R:

```
install.packages("devtools")
devtools::install_github("kforthman/caretStack")
library(caretStack)
```

To install on the server:

```
# first, log onto the server through the terminal
ssh submit0
# (type your password and hit 'enter')
module load R
R
install.packages('devtools')
devtools::install_github("kforthman/caretStack")
library(caretStack)
```

## Step 2: Preparing your files.
You need at least 2 files to perform the ML analysis

1. A csv file listing the names of the predictors you would like to use in your analysis. The names should be the same as the column names in your dataset. An example is in the Examples folder and is named vars.csv.
2. Your dataset as a csv including the columns 'id' and 'LC_Category'. An example is in the Examples folder and is named data.csv. You are allowed to use a dataset that contains more variables than just the ones you wish to use in your analysis.

## Step 3: Writing the code to execute the rNCV.

## Step 4: Running rNCV on the server.

## Step 5: Summary and Visualization of the results.



# So, what does caretStack do?
**caretStack** uses repeated nested cross-validation (rNCV) to tune the parameters of a stacked machine learning model. We say the model is 'stacked' because the prediction is a weighted average of multiple machine learning methods. Each method is weighted by it's fit to the data. This fit is determined by the rNCV.

The rNCV is made up of two loops: The *outer* loop partitions the data in k<sub>1</sub> parts. One partition is used as a training set and the other parts are used for testing. The training set is used in the *inner* loop. The inner loop partitions the training set into k<sub>2</sub> parts. This inner loop is used to build the model parameters. See the figure below.
![RNCV Flowchart](/Images/RNCV flowchart.pdf)
The 'top' of the innerloop is the `train` function in caret. The 'lower' part of the inner loop is PredVal.


