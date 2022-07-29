##################################
# Doing inferential statistics in SSR Quant Research Workshop Aug 2022

# R is a coding language -> You have to download R from CRAN https://www.r-project.org/ 

# R Studio is software that interfaces with R language. You have to also download R Studio.
  ## R Studio can be on cloud or you can download it as a software on your computer
  ## All of them are FREE!!!! 

# In this do-file (i.e., code file; i call it a do-file):
  ## the black/blue lines are the codes. You select these lines and you click on "run" button above
  ## the green lines are comments. They will not be executed when you click on "run" button
    # Anything after "#" is a comment. These comment(s) are not run by the software
    # Commenting your codes is a GOOD practice because they help you to remember what the codes were for. 

# Because R is open-sourced and is free, people like you and me develop R packages using R language 
# to do cool things such as statistical analysis, data wrangling, data vizualization, 
# create websites, creat resumes, create dashboards etc

##################################

# Let's start!
# To run analysis in this file, we have to:
  ## Load dataset
  ## load the R packages that we will need to use
  ## Clean the data (but we will not do this since we cleaned it in Excel)
  ## Run the different analyses to answer our research questions: 

# LOAD DATASET ####
# install.packages("readxl")
# library(readxl)
rm(list = ls())
getwd()

# Packages need to be dowloaded. You only need to install packages once. 
  ## .install.packages() function will install  
  ## After downloading, you need to load the package if you need to use it in each new session.
#install.packages("readxl")
library(readxl)

# read_excel below is the function from readxl package. It will read in the data from
# the path we specified, the excel sheet we want, and to skip the first row.

data <- read_excel(path = "parentingCB_syn.xlsx",
                   sheet = "inferential",
                   skip = 1)

# Let's do a quick look at our dataset using glimpse function from dplyr package
#install.packages("dplyr") 
library(dplyr)

# glimpse function will help see the dataset. You can also double-click on the data object in "environment"
  ## we have 313 respondents and 23 variables
glimpse(data)

# Let 's see the names of the variables in our dataset
names(data)

###############################################

# CLEANING DATASET ####
  ## Usually we clean the data next. But it has been cleaned in excel. 
  ## Let's go on to our research questions. We will answer 3 in total.

###############################################

# RESEARCH QUESTION Q1:
  ## 1) WOULD SEX OF PARENTS (MUMS VS DADS) BE RELATED TO THEIR ABILITY 
  ## TO BALANCE WORK & PARENTING AT HOME DURING CIRCUITBREAKER (AGREE VS. DISAGREE)?

  ### I suspect that mums are more likely to disagree that they can balance work 
  ### and parenting during CB while dads are more likely to agree.

  ### Since these are two nominal discrete vars, we use Chi-sq test for independence
  #### Both variables' names in the dataset are "wfh_1_r" and "sex_r"

# CHI-SQUARE TEST FOR INDEPENDENCE ####
  ## Use this to test for relationships between nominal variables

# install janitor package to do a cross-tabulation table between wfh_1_r & sex_r variable
#install.packages("janitor")
library(janitor) 

# %>% below is a forward pipe operator. It means "forward it on"
data %>% tabyl(wfh_1_r, sex_r, show_na = FALSE)

# %>% below is a forward pipe operator. It means "forward it on"
data %>% 
  tabyl(wfh_1_r, sex_r, show_na = FALSE) %>% 
  adorn_percentages("col") %>% 
  adorn_pct_formatting(digits = 0) %>%  
  adorn_ns()
  # You can see the results from the console below. 
  # Results show that 57% of fathers agree that they can balance wrk and parenting. 
  # But 63% of mothers disagree. It seems that there is relationship betwen sex and wfh_1_r
  # if there is no relationship, it should be closer to 50% for all

  # argument "show_na = FALSE" will exclude missing
  # adorn_percentages will show % for column; change to row for rows
  # adorn_ns will include frequencies beside the percentages


# To do chi-sq test, just use  chisq.test()
data %>% 
  tabyl(wfh_1_r, sex_r, show_na = FALSE) %>% 
  chisq.test()
  # p-value = 0.003747 which is <.05 => the relationship is significant 

# The below another alternative code for running chisq
# chisq.test(data$wfh_1_r, data$sex_r)

# CONCLUSION TO RESEARCH Q1:
# RESEARCH QUESTION Q1:
  ## 1) WOULD SEX OF PARENTS (MUMS VS DADS) BE RELATED TO THEIR ABILITY 
  ## TO BALANCE WORK & PARENTING AT HOME DURING CIRCUITBREAKER (AGREE VS. DISAGREE)?

  ## CONCLUSION: THERE IS A SIG RELATIONSHIP BTW SEX AND WORK-PARENTING BALANCE 
  ## WHERE MOTHERS AE MORE LIKELY THAN FATHERS TO DISAGREE THAT YTHEY CAN BALANCE

#######################################################################

# RESEARCH QUESTION Q2:
  ## 1) WOULD PARENTING STRESS SCORE BE DIFFERENT BETWEEN MUMS AND DADS? 
    ### I hypothesize that mums will likely report higher stress than dads

    ### The two variables in our dataset are "stress_mean" and "sex_r"

    ### Since SEX of parents is a categorical group variable and STRESS is continuous, 
    ### we use independent sample t-test to test for group differences

# T-TEST FOR DIFFERENCES IN GROUP MEANS ####

# Before doing t-test, let see the means and std deviation (descriptive statistics)
mean(data$stress_mean, na.rm = T) # na.rm = T will exclude the rows with missing stress_mean values
sd(data$stress_mean, na.rm = T)

# The below will look at "stress_mean" average scores for mothers and then for fathers
data %>% 
  group_by(sex_r) %>% 
  summarize(mean = mean(stress_mean, na.rm = T))

# Finally, let's do the t-test
t.test(data = data, stress_mean ~ sex_r, var.equal=FALSE)
  # Results show even though the mean for mum is higher than for dad,
  # this difference is not statistically significance because p = 0.314 (p > 0.05)


# CONCLUSION TO RESEARCH Q2:
  ## 1) WOULD PARENTING STRESS SCORE BE DIFFERENT BETWEEN MUMS AND DADS? 
    ### I hypothesized that mums will likely report higher stress than dads

  ## CONCLUSION: WHILE MOM'S STRESS IS HIGHER THAN DAD, THERE IS NO 
  ## SIG DIFFERENCE IN STRESS BETWEEN DADS AND MOMS

#######################################################################

# RESEARCH QUESTION Q3:
  ## 1) WOULD PARENTING STRESS SCORE OF PARENTS BE ASSOCIATED WITH IMPACT OF COVID? 
    ### I hypothesize that there will be a positive correlation 

    ### the two variable's names are "stress_mean" and "concern_mean"

    ### Since STRESS is continuous and COVID IMPACT is continuous
    ### we use correlation

# CORRELATION BETWEEN stress_post_mean AND  ####
cor.test(data$stress_mean, data$concern_mean)
  # Results show that correlation, r = 0.3726. Is this positive or neg relationship?
  # The r is statistically significant since p-value is 1.1e-10 or 0.00000000011 i.e. p < 0.05

# CONCLUSION TO RESEARCH Q3:
  ## 1) WOULD PARENTING STRESS SCORE OF PARENTS BE ASSOCIATED WITH IMPACT OF COVID? 
    ### I hypothesize that there will be a positive correlation 

  ## CONCLUSION: THERE IS A SIG POSITIVE RELATIONSHIP BTW STRESS AND IMPACT OF COVID 
  ## WHERE PARENTS WHO EXPERIENCED MORE IMPACT OF COVID, WILL ALSO REPORT HIGHER PARENTING STRESS

