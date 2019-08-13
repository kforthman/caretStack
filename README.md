# caretStack
This package is primarily intended for internal use at the Laureate Institute for Brain Reasearch. The executable functions in this package are meant for implementation on LIBR datasets. The following is a step-by-step guide for how to create a stacked ML model on LIBR data using LIBR resources.

The plan is to create a caretStack2 for broader use shortly! For now, here is the tutorial for how to run this process in R and on the LIBR servers.

Be sure to check back regularly for updates!

### Step 1: Installation
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

### Step 2: Preparing your files.
You need at least 2 files to perform the ML analysis

1. A csv file listing the names of the predictors you would like to use in your analysis. The names should be the same as the column names in your dataset. An example is in the Examples folder and is named vars.csv.
2. Your dataset as a csv including the columns 'id' and 'LC_Category'. An example is in the Examples folder and is named data.csv. You are allowed to use a dataset that contains more variables than just the ones you wish to use in your analysis.

### Step 3: Writing the code to execute the rNCV.
Most of the code you need to execute the rNCV is already written for you. All you need to do is call the function `predict_one` and input the files above as arguments. Do NOT try to run this function in your local instance of R/RStudio. It will take forever to run. Wait until step 4, you will run the function on the server. 

Argument definitions are in the documentation. Here is an example of how you might write out this function:

```
your_data <- read.csv('your_data.csv')
predict_one(your_data, 'your_target_variables_name', c('your_predictors.csv'), 'your_prefix')
```

### Step 4: Running rNCV on the server.
Save your code from step 3 as an R file. In this example, I'll name this file 'run_predict_one.R'. Now, create a seprate text file. In the text file, write:

```
#!/opt/apps/core/R/3.5.1/lib64/R/bin/Rscript

source('run_predict_one.R')
```

replacing 'run_predict_one.R' with whatever you named your file.

Now save the text file as 'run_R.Rscript'.

Create a second text file. Copy and paste into it the following text, replacing the text 'your_directory' with your desired path.

```
#!/bin/bash
#
#SBATCH --partition=c2_cpu
#SBATCH --ntasks=5
#SBATCH --mem=30000
#SBATCH --nodes=1
#SBATCH --output=/your_directory/out.log
#SBATCH --error=/your_directory/err.log
#SBATCH --job-name=ML-HB_slope-est
#SBATCH --mail-type=NONE
#SBATCH --workdir=/your_directory
#SBATCH --time=3-0
#
#################################################
#to submit this to the scheduler, run:
#sbatch < run_PredictionJob.ssub


#to run an interactive terminal
#srun -N 1 -c 4 --mem=24000 --pty --partition=c2_short /bin/bash

module load R/3.5.1
#this should go in your .bashrc file too, so R libraries installed for this version of R go in their own folder
R_LIBS_USER=~/R/3.5.1
export R_LIBS_USER



./run_R.Rscript 
```

Save this file as 'run_PredictionJob.ssub'.

Now, submit this job to the server by cd-ing to the directory where run_PredictionJob.ssub is located and enter the command

```
sbatch < run_PredictionJob.ssub
```

into the terminal.


### Step 5: Summary and Visualization of the results.
When the prediction job is finished, you should be ready to view your results! This is simple with the function `summarize_one`.

In a fresh R script, simply type

```
summarize_one('your_prefix', 'your_plot_title')
```

## So, what does caretStack *do*?
**caretStack** uses repeated nested cross-validation (rNCV) to tune the parameters of a stacked machine learning (ML) model. We say the model is 'stacked' because the prediction is a weighted average of multiple machine learning methods, called base learners. Each method is weighted by it's fit to the data. This fit is determined by the rNCV.

The rNCV is made up of two loops: 

The *outer* loop partitions the data in k<sub>1</sub> parts. In each fold, one of the partitions, τ, are removed. The removed partition is held out as the testing set, and the remaining partitions are used together as a training set. The training set is used to optimize model parameters in the *inner* loop. 

The *inner* loop partitions the training set into k<sub>2</sub> parts. This inner loop is used to build the model parameters. See the figure below.

![RNCV Flowchart](/Images/RNCVflowchart.png)

The top of the inner loop in the figure above is the `train` function in caret. The lower part of the inner loop in the figure is PredVal.

A more detailed description of the pipeline is below:
* Step 1. Building the base learners using `caretModels`. The base learners are built in the inner loop. k<sub>2</sub> folds are created. These folds enable the optimization of model parameters. The resulting optimized model is saved to a specified directory.
* Step 2. Extracting predicted values/probabilities, ŷ, from each of the optimized base learners. The models generated in step 1 are used to make a prediction on the testing set.
* Step 3. Model performance in the calibrating & hold-out sets of the outer loop.
* Step 4. Calculating variable importance.
