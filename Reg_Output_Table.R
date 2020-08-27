#set WD
setwd("/Users/Anita/Documents/R Miscellaneous/Coding Club")

#### create pretend dataframe
dataframe <- data.frame(
  ID = c(1:200),
  Dependent_Variable = sample(c("Yes_PIU", "No_PIU"), size = 200, replace = TRUE),
  Predictor1 = sample(c(1:30), size = 200, replace = TRUE),
  Predictor2 = sample(c("Depression", "Anxiety", "ASD", "ADHD", "No_Dx")),
  covariate1 = sample(c("female", "male"), size = 200, replace = TRUE),
  covariate2 = sample(c("high_SES", "low_SES", "Middle_SES"), size = 200, replace = TRUE)
)
write.csv(dataframe, file="dataframe.csv", row.names=FALSE)

# run multiple regressions on various variables of interest within a dataframe and return a results table in csv format
## create a list that includes the column numbers for the relevant variables to subsequently put into regression loops
Variable_Column_numbers <- c(3, 4)

## input these variables into a loop that runs a regression of choice on each of the variables
# lapply applies my self-defined function onto each item in the list
# the function in this case is a glm (logistic regression)
# each item in my list of variables (defined as x) is being included as a predictor in the models by selecting the relevant column from my dataframe
my_regressions <- lapply(Variable_Column_numbers, function(x) glm(Dependent_Variable ~ dataframe[,x] + covariate1 + covariate2, family = binomial, data = dataframe))

## pull out the information that will go into the tables of results using multiple loops that iterate over the output of the previous regression loop
# lapply and sapply do the same thing (apply function to list) but sapply returns a vector while lapply returns a list
# outputs rounded to 2 digits
# outputs of regressions are being indexed according to value of interest (e.g. 2nd item in coefficients)
# here getting unstandardized regression coefficients, confidence intervals and p values
my_summaries <- lapply(my_regressions, summary)
my_coefs <- sapply(my_regressions, function(x) round(exp(x$coefficients[2]), digits = 2))
my_CI_1 <- sapply(my_regressions, function(x) round(exp(confint(x)[2, 1]), digits =2)) 
my_CI_2 <- sapply(my_regressions, function(x) round(exp(confint(x)[2, 2]), digits = 2))
my_ps <- sapply(my_summaries, function(x) round(x$coefficients[2, 4], digits=2))

## combine all relevant information from whole loop of regressions and save into one dataframe
# generates table with columns for relevant information (B, CIs, p values) and each row is for one variable of interest (here as predictor in regression models)
# saves table as csv file in a subfolder (Tables) within the current directory
Result_table_name <- data.frame(my_coefs, my_CI_1, my_CI_2, my_ps, row.names = NULL)
colnames(Result_table_name) <- c("B", "2.5% CI", "97.5% CI", "p value")
rownames(Result_table_name) <- c("Predictor 1", "Predictor 2")
write.csv(Result_table_name, file = "Result_table_name.csv")
