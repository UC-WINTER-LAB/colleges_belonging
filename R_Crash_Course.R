#First thing's first; let's install some packages!
install.packages("tidyverse")
install.packages("lavaan")
install.packages("sjmisc")
install.packages("dplyr")
install.packages("Hmisc")
install.packages("ggpubr")
install.packages("ggplot2")
install.packages("stats")

#Then we run these lines of code, so R knows which packages to use for this script
library(tidyverse)
library(lavaan)
library(sjmisc)
library(dplyr)
library(Hmisc)
library(ggpubr)
library(ggplot2)
library(stats)


#Next, let's load in some publicly-available datasets
df_cars <- mtcars #a dataset containing info about car types
df_test <- HolzingerSwineford1939 #the mental ability scores of children


#If we're curious about a variable located in our dataframe of interest, we format 
#the code like this: dataframe$variable

#For example, running this line of code will tell R to list out all observations
#of this variable:
df_cars$gear
df_test$x1


###############Frequencies

#You can have a look at the frequencies of a particular variable by using the 
#"frq" function from the sjmisc package

#For example:
frq(df_cars$cyl) #car cylinders 
frq(df_cars$gear) #number of forward gears


############Averages

#This is how you might find the mean and standard deviation of a variable 
#you're interested in.

#For example, this code will give us the average number of forward gears in our
#dataset, and the standard deviation:
df_cars %>%
  summarise(MeanGear = mean(gear, na.rm = TRUE), SDGear = sd(gear, na.rm = TRUE))


############Correlations

#We can use the rcorr function from the Hmisc package to generate a complete
#correlation matrix of all the variables we're interested in.

#For example, this will create a correlation matrix between car cylinders, gears,
#and horsepower
df_Cor <- df_cars %>% #selecting our variables of interest
  select(cyl, gear, hp)

rcorr(as.matrix(df_Cor)) #generating the matrix


#You can also run and graph correlations with ggscatter, from the ggplot2 and ggpubr
#packages. It's always good to be able to visualise what's going on in your data.

#For example:
ggscatter(df_cars, x = "cyl", y = "hp", color = "green",
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Number of Cylinders", 
          ylab = "Horsepower")


############Regressions

#We run regressions using the stats function "lm."
#The general format is: lm(y_variable ~ x_variable) 


#For example, let's run a regression between horsepower and cylinders 
regression_model <- lm(hp ~ cyl, data = df_cars)
summary(regression_model)

#If you want to add more predictor variables, simply list each one, separated by
#a "+", like so:
regression2_model <- lm(hp ~ cyl + gear, data = df_cars)
summary(regression2_model)


###########Confirmatory Factor Analyses

#This will be more relevant to what you'll be doing for your project
#Here, we run CFAs or structual equation models (SEMs) with the lavaan package

#For this example, we'll be looking at the df_test dataset
#Recall that this dataset contains the mental ability scores of 7th-8th grade kids
#Here, we want to run a CFA on the visual, textual, and mental speed variables to
#determine whether these variables adequately capture a child's mental ability

#First, we specify the model and its latent variables. Note that for each latent
#variable, we include its name, followed by "=~", and the variables that comprise
#the latent variable

HS.model <- '
            visual =~ x1 + x2 + x3
            textual =~ x4 + x5 + x6
            speed =~ x7 + x8 + x9
            
            #the overarching mental score
            mental =~ 1*visual + 1*textual + 1*speed
'

#Then, all we need to do is run the model, and have a look at the results!
HS_fit <- sem(HS.model, data = df_test)
summary(HS_fit, fit.measures = TRUE)


###########Extracting Factor Estimates

#So, say you've run your CFAs, and now you want to extract those latent factor
#scores to your main working dataframe. This is something that you'll want to 
#do for your project, so you can run CFAs on the self-compassion items first,
#and then run mediations with them.

#So, this is how you do it!

#For this example, let's pretend that we're working in the df_test dataset, and 
#we want to add those "visual", "textual", and "speed" latent variables we 
#specified for our CFA into the dataframe.

idx <- lavInspect(HS_fit, "case.idx") #we look at the CFA fit we ran before
mental_scores <- lavPredict(HS_fit) #we extract the predicted estimates into this placeholder
for (fs in colnames(mental_scores)) { #we loop over each value
  df_test[idx, fs] <- mental_scores[ , fs] #and then add them to our dataset!
}

#So, if we take a look at our dataframe (df_test), the visual, textual, and speed
#scores should be added in the end, alongside the overarching mental ability score.


###########Mediation Analyses

#We run a mediation analysis in a very similar way, still using lavaan.
#The exception is that we specify some regressions in our model.

#Let's say, for this example, we're curious whether speed mediates the relationship
#between visual and textual scores (which I honestly doubt it does, but hey). 

#Key:
#c = the direct path from visual scores to textual scores
#a = the path from visual scores to speed scores
#b = the path from speed scores to textual scores

mediation.model <- '
                   #direct effect
                   textual ~ c * visual
                   
                   #mediator
                   speed ~ a * visual
                   textual ~ b * speed
                   
                   #indirect effect
                   indirect := a * b
                   
                   #total effect
                   total := c + indirect
'

#Same as always, we then run the model and look at the results!
mediation_fit <- sem(model = mediation.model, data = df_test)
summary(mediation_fit, fit.measures = TRUE)


#######################Writing to CSV

#Sometimes, after we've finished all of our analyses, we want to save them to a
#csv file. Here's how you would do that:

write_csv(df_test, "Test_Scores.csv")

#PLEASE NOTE: Do NOT do this with our RA data, as this is a security risk.


######################################

#Just to conclude, sometimes, when you've run all these analyses, your R Environment
#starts to become cluttered. If you want to clear it, and only save a dataset or 
#two, you can run the following code:

rm(list = setdiff(ls(), "df_test"))

#Just make sure that you only run something like this when you're sure that you're
#finished, otherwise all of your hard work will disappear, and you'll have to run
#all the code again.






