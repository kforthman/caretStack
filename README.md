# caretStack
The following is a step-by-step guide for how to create a stacked ML model on LIBR data using LIBR resources.

Be sure to check back regularly for updates!

List of base learners available in caret:
* https://rdrr.io/cran/caret/man/models.html
* https://topepo.github.io/caret/available-models.html

### Step 1: Installation
To install in R:

```
# Install devtools
install.packages("devtools")

# Install caretStack
devtools::install_github("kforthman/caretStack")

# Double-check that caretStack installed correctly
library(caretStack)
```

To install on the server:

```
# first, log onto the server through the terminal
ssh submit0
# (type your password and hit 'enter')

# Start an interactive job
srun -N 1 -c 1 --mem=20000 --pty --partition=c2_short /bin/bash

# If you have not yet loaded R, do so.
# You can specify the version of R you would like to use
# by instead typing `module load R/[version]`
module load R
R

# Install devtools
install.packages('devtools')
# You may need to select a mirror. I tend to use the one for Kansas. Type the number and press 'enter'.
# Once you select your mirror, it may take a couple of minutes to look like it's doing anything,
# so just be patient.

# Install caretStack
devtools::install_github("kforthman/caretStack")

# Double-check that caretStack installed correctly
library(caretStack)
```

### Step 2: Preparing your files.
You need at least 2 files to perform the ML analysis

1. A csv file listing the names of the predictors you would like to use in your analysis. The names should be the same as the column names in your dataset. An example is in the Examples folder and is named vars.csv.
2. Your dataset as a csv including the target and all predictors. You are allowed to use a dataset that contains more variables than just the ones you wish to use in your analysis. Any variables not in vars.csv will be removed automatically.

### Step 3: Writing the code to execute the rNCV.
Most of the code you need to execute the rNCV is already written for you. The example code is written in Examples/runcaretStack.R to get you started. There are three different examples for running `predict_two`, one for each target type. You may change any arguments you wish, the arguments in the example are only suggestions. Save your code from step 3 as an R file. In this example, I'll name this file 'runcaretStack.R'.

If you have a large number of observations, it is recommended that you do not run this function in your local instance of R. Wait until step 4, you will run the function on the server. 

Please find more information about the arguments are in the documentation. 

### Step 4: Running rNCV on the server.
There are two files you need in order to run your R script on the server. There are examples of these files in the Examples folder.

The first file is named 'run_R.Rscript'. If you named your R script something else, be sure to change the name of the sourced file.

The second file is named 'run_PredictionJob.ssub'. There are blanks where you need to fill in your project directory.

*Note: in the example files 'run_R.Rscript' and 'run_PredictionJob.ssub', R 4.0.0 is called. Please feel free to use a more updated version.

Now, submit this job to the server by cd-ing to the directory where run_PredictionJob.ssub is located and enter the command

```
sbatch < run_PredictionJob.ssub
```

into the terminal.

Now is a good time for a coffee break... this may take a few minutes! ☕️

Output files from `predict_two()` are listed out in the documentation.

### Step 5: Summary and Visualization of the results.
When the prediction job is finished, you should be ready to view your results! This is simple using the example file 'Examples/caretStackSummary.Rmd'.
Place a copy of this example file into your working directory. Now, fill in the blanks on lines 20-28 with the same information you input to `predict_two`. Then knit this file by clicking the icon that looks like a blue ball of yarn, located at the top of your screen.

![Knit Icon](/Images/knit.png)

A file will automatically be created in the working directory that contains the results called 'caretStackSummary.html'.

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
