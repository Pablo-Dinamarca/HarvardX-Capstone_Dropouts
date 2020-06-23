#==============================================================================================
# School Dropouts Prediction
# Pablo Dinamarca
# 23/06/2020
#==============================================================================================
# NOTE: The following code is essentially the same as in the Rmarkdown document.
#==============================================================================================
#~~~~~~~~~~~~~~~~~~#
# Data Preparation #
#~~~~~~~~~~~~~~~~~~#

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(hrbrthemes)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(MLmetrics)) install.packages("plotly", repos = "http://cran.us.r-project.org")

# REG02_EPHC_ANUAL_2017.SAV dataset and 2018:
# https://www.dgeec.gov.py/microdatos/microdatos.php

dl <- tempfile()
download.file("https://www.dgeec.gov.py/microdatos/register/EPHC-ANUAL/REG02_EPHC_ANUAL_2017.SAV", dl)

dl <- tempfile()
download.file("https://www.dgeec.gov.py/microdatos/register/EPHC-ANUAL/REG02_EPHC_ANUAL_2018.SAV", dl)

EPHC_2017 <- read_sav("REG02_EPHC_ANUAL_2017.SAV")
EPHC_2018 <- read_sav("REG02_EPHC_ANUAL_2018.SAV")

#--------------------------------#
# Create Data and Validation set #
#--------------------------------#

Data <- EPHC_2017 %>% 
  select(c("DPTO","AREA","P02","P06","ED01","ED0504","ED10","e01aimde", "ipcm")) %>% 
  mutate(Drop_out = ifelse(ED0504 < 503 & ED0504 != 409 & !is.na(ED10) & !is.na(ED0504),
                           "Desertor", "No_Desertor")) %>% filter(!is.na(ipcm) & !is.na(ED01))

Validation <- EPHC_2018 %>% 
  select(c("DPTO","AREA","P02","P06","ED01","ED0504","ED10", "e01aimde", "ipcm")) %>%
  mutate(Drop_out = ifelse((ED0504 < 503 & ED0504 != 409 & !is.na(ED10)) & !is.na(ED0504),
                           "Desertor", "No_Desertor")) %>% filter(!is.na(ipcm) & !is.na(ED01)) 
#======================================================================================================
#-----------------#
# Modify the data #
#-----------------#

# As Factor some variables 
Data <- Data %>%
  mutate_at(vars(c("DPTO","P06","AREA", "ED01", "ED0504","ED10","Drop_out")),
            funs(as_factor(.)))

Validation <- Validation %>%
  mutate_at(vars(c("DPTO","P06","AREA", "ED01", "ED0504","ED10","Drop_out")),
            funs(as_factor(.)))


# Relevel the possitive variable
Data$Drop_out <- relevel(Data$Drop_out, "Desertor")
Validation$Drop_out <- relevel(Validation$Drop_out, "Desertor")


# Rename the Variables 
names(Data) <- c("Department","Area", "Age", "Sex", "Language",
                 "Grade","Cause","Income", "Income_pc","Drop_out")

names(Validation) <- c("Department","Area", "Age", "Sex", "Language",
                       "Grade","Cause","Income", "Income_pc","Drop_out")


# Create the test set and train set
set.seed(1, sample.kind = "Rounding")
index <- createDataPartition(Data$Drop_out, times = 1, p = 0.1, list = FALSE)
test_set <- Data[index,] 
train_set <- Data[-index,]


# Remove Extra Datasets
rm(EPHC_2017,EPHC_2018, index)
#======================================================================================================
#~~~~~~~~~~~~~~~~~~#
# Data Exploration #
#~~~~~~~~~~~~~~~~~~#

# Pre-visualize the Data
names(Data)
head(Data, 5)
dim(Data)
class(Data)
glimpse(Data)
summary(Data)

# View some tables
table(Data$Sex)
table(Data$Area)
Datades <- table(Data$Drop_out)
Valdes <- table(Validation$Drop_out)

Desertor_Data <- Datades[1]/Datades[2]
Desertor_Val <- Valdes[1]/Valdes[2]
Desertor_Data
Desertor_Val


rm(Datades, Valdes)

# When we look at the Data tables and dropout validation, 
# we notice that the dataset is clearly unbalanced.
#======================================================================================================
#-----------------------------#
# Exploration by each feature #
#-----------------------------#
#======================================================================================================

# Desertion by Department

Desertion <- as.data.frame(table(select(Data, Department, Drop_out))) %>%
  ggplot(aes(Freq, Department, color=Drop_out)) +
  geom_point(alpha=0.9) +
  ggtitle("School Dropout") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))
ggplotly(Desertion)

# Department by population
table(as.data.frame(Data$Department))


# If we look at the graph and table, 
# we will see that the departments with the largest population 
# also tend to have the highest number of deserters.
#======================================================================================================

# Desertion by Area

Desertion <- as.data.frame(table(select(Data, Area, Drop_out))) %>%
  ggplot(aes(Freq, Area, color=Drop_out)) +
  geom_point(alpha=0.9) +
  ggtitle("School Dropout") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))
ggplotly(Desertion)

Data %>% ggplot(aes(Area, fill=Drop_out)) + geom_bar(alpha=0.9) +
  ggtitle("School Dropout") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15)) + 
  xlab("Area") +
  ylab("Data")

Des <- as.data.frame(table(select(Data, Area, Drop_out)))
Des

# Desertor in percentage by Area
data.frame(Urbana = (Des$Freq[1]/(Des$Freq[1]+Des$Freq[3]))*100, 
           Rural = (Des$Freq[2]/(Des$Freq[2]+Des$Freq[4]))*100)


# When looking at the graphs we clearly notice that the rural area tends to have 
# more school dropouts and with the tables we affirm our observations.
#======================================================================================================

# Desertion by Age

Data %>% 
  ggplot(aes(Age, fill=Drop_out)) + geom_histogram(color="#e9ecef", alpha=0.9) +
  ggtitle("School Dropout") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))

only_Des <- Data %>% filter(Drop_out=="Desertor")

Desertion <- as.data.frame(table(only_Des$Age)) %>% 
  filter(Freq > 100) %>% 
  ggplot(aes(Var1,Freq)) + geom_point(fill="#69b3a2", alpha=0.9) +
  ggtitle("School Dropout") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15)) + 
  xlab("Age") +
  ylab("Data")
ggplotly(Desertion)

as.data.frame(table(only_Des$Age)) %>% filter(Freq > 100) %>% arrange(-Freq) %>% head(5)


# This graph is interesting since we visualize a timeline created with the age of the people. 
# We can calculate the number of deserters by age and, as expected, 
# the generation that currently has 20 years deserted less than the generation that currently has 40.
#======================================================================================================

# Desertion by Sex

Data %>% ggplot(aes(Sex, fill=Drop_out)) + geom_bar(alpha=0.9) +
  ggtitle("School Dropout") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15)) + 
  xlab("Sex") +
  ylab("Data")

Des <- as.data.frame(table(select(Data, Sex, Drop_out)))
Des

# Desertor in percentage
data.frame(Man = (Des$Freq[1]/(Des$Freq[1]+Des$Freq[3]))*100, 
           Woman = (Des$Freq[2]/(Des$Freq[2]+Des$Freq[4]))*100)


# We can note that school dropouts are not due to gender, 
# although we also note that men dropped out a little more than women.
#======================================================================================================

# Desertion by Language

Data %>% ggplot(aes(Language, fill=Drop_out)) + geom_bar(alpha=0.9) +
  ggtitle("School Dropout") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15)) + 
  xlab("Language") +
  ylab("Data") + coord_flip()

Data %>% ggplot(aes(Area, fill=Language)) + geom_bar() +
  ggtitle("School Dropout") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15)) + 
  xlab("Area") +
  ylab("Data")


# The Language by Area chart demonstrates an interesting reality in Paraguay. 
# Most of the population living in rural areas tend to speak in more closed Guarani 
# and those in urban areas tend to speak jopara or Spanish.
#======================================================================================================

# Desertion by Grade

Grade_Des <- as.data.frame(table(Data$Grade, Data$Drop_out)) %>% 
  filter(Freq > 2000)

names(Grade_Des) <- c("Grade","Drop_out", "Count")

Grade_Des %>% 
  ggplot(aes(Count, Grade, color=Drop_out)) +
  geom_point(alpha=0.9) +
  ggtitle("School Dropout") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))

Grade_Des %>% filter(Grade=="Educ. Escolar Básica 6°")


# We note that for some reason people tend to drop out more in 6th grade than in others, in fact, 
# there are more 6th grade dropouts(Desertores) than No_Dropouts(No_Desertores).
#======================================================================================================

# Desertion by Cause

Causa <- Data %>% select(Cause,Sex) %>% table()  %>% as.data.frame()

Causa %>% 
  ggplot(aes(Freq, Cause, color=Sex)) +
  geom_point(alpha=0.9) +
  ggtitle("School Dropout") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))

as.data.frame(table(Data$Cause, Data$Sex)) %>% filter(Freq>1000) %>% arrange(Var2, -Freq)


# Here we list the main reasons why people drop out of school and note some differences by gender.
#======================================================================================================

# Desertion by Income

Data %>% 
  ggplot(aes(Age, Income, color=Drop_out)) +
  geom_point(alpha=0.9) +
  ggtitle("School Dropout") +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))

Data %>% 
  ggplot(aes(x=Sex, y=Income, color=Drop_out)) +
  geom_boxplot(alpha=0.9) +
  ggtitle("School Dropout") +
  scale_y_log10() +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))

# In the first graph we can see the distribution of income by age and classified by dropout. 
# We note that dropouts tend to have less income than non-dropouts.

# In the second graph, we notice a big difference between the wages of Men 
# compared to that of women. 
# We can see that deserters men or do not tend to earn more than women.
#======================================================================================================

# Desertion by Income per capita

Data %>% 
  ggplot(aes(x=Area, y=Income_pc, color=Drop_out)) +
  geom_boxplot(alpha=0.9) +
  ggtitle("School Dropout") +
  scale_y_log10() +
  theme_ipsum() +
  theme(plot.title = element_text(size=15))

# In this comparison, in the urban area, school dropouts tend to have less income 
# than in the rural area.
#======================================================================================================
#~~~~~~~~~~~~~~~#
# Data Cleaning #
#~~~~~~~~~~~~~~~#

# Remove extra variable
rm(Causa, Desertion, only_Des, Grade_Des, Des)

#======================================================================================================
#~~~~~~~~~~~~~~~~~~~~#
# Evaluation Results #
#~~~~~~~~~~~~~~~~~~~~#

# Here we define the loss functions:

# F1-Score
f1 <- function(data, lev = NULL, model = NULL) {
  f1_val <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = lev[1])
  c(F1 = f1_val)
}


# Model - Class Weights
Model_W <- function(data) {
  ifelse(data$Drop_out == "Desertor",
         (1/table(data$Drop_out)[1]) * 0.5,
         (1/table(data$Drop_out)[2]) * 0.5)
}
#==============================================================================================
#~~~~~~~~~~~~~~~~~~~#
# Create the Models #
#~~~~~~~~~~~~~~~~~~~#

# We will train the models using the caret package and the training function. 
# We will follow the following modeling order:
  
# 1. First we will create a model with simple logistic regression and 
# another adjusted by weights to compare the improvement of the algorithm.

# 2. We will apply a formula to train 4 of the 5 available models. 
# Due to limitations in computing capacity, we calculate the fda without 
# taking into account the weights and separately. Then we compare it with the other models.

# 3. Once you have selected the best prediction model, 
# we train the final algorithm with the Validation set. 
# To finish, we compared the model with random sampling to compare it.

# Keep in mind that while training the model we will also compare other metric 
# such as Accuracy with F1_Score.

#==============================================================================================

## Logistic Regression

# Create the fit model with glm.
fit_glm <- train(Drop_out ~ Age + Department + Sex + Area + Income + Income_pc + Language, 
                 data = train_set, 
                 method = "glm",
                 metric = "F1",
                 trControl = trainControl(method = "cv",
                                          number = 3,
                                          classProbs = TRUE,
                                          summaryFunction = f1))


fit_glm$result

# Test the model with Test_data.
Matrix_glm <- confusionMatrix(predict(fit_glm, test_set), test_set$Drop_out, mode = "prec_recall")

Model_Results <- tibble(Method="Logistic_Regression", 
                        F1_Score = Matrix_glm$byClass["F1"],
                        Accuracy = Matrix_glm$byClass["Balanced Accuracy"])

# Saw the Results
data.frame(Model_Results)
#==============================================================================================

## Logistic Regression by Weights

# Create the fit model with glm.
fit_glm <- train(Drop_out ~ Age + Department + Sex + Area + Income + Income_pc + Language, 
                 data = train_set, 
                 method = "glm",
                 metric = "F1",
                 weights = Model_W(train_set),
                 trControl = trainControl(method = "cv",
                                          number = 3,
                                          classProbs = TRUE,
                                          summaryFunction = f1))


fit_glm$result

# Test the model with Test_data.
Matrix_glm <- confusionMatrix(predict(fit_glm, test_set), test_set$Drop_out, mode = "prec_recall")

Model_Results <- bind_rows(Model_Results,
                           tibble(Method="LR_Weights", 
                                  F1_Score = Matrix_glm$byClass["F1"],
                                  Accuracy = Matrix_glm$byClass["Balanced Accuracy"]))

# Saw the Results
data.frame(Model_Results)
#==============================================================================================

## All Models

# Note: this process could take a couple of minutes

# Select the model to predict.
models <- c("glm", "lda", "qda", "rf")


# Create the fit model with different models.
fits <- lapply(models, function(model){ 
  print(model)
  train(Drop_out ~ Age + Department + Sex + Area + Income + Income_pc, 
        data = train_set, 
        method = model,
        metric = "F1",
        weights = Model_W(train_set),
        trControl = trainControl(method = "cv",
                                 number = 3,
                                 classProbs = TRUE,
                                 summaryFunction = f1))
})

names(fits) <- models

# Test the model with Validation set.
Matrix_Models <- sapply(fits, function(fits){ 
  confusionMatrix(predict(fits, newdata = test_set), 
                  test_set$Drop_out, mode = "prec_recall")$byClass["F1"]
})

Matrix_Acc <- sapply(fits, function(fits){ 
  confusionMatrix(predict(fits, newdata = test_set), 
                  test_set$Drop_out, mode = "prec_recall")$byClass["Balanced Accuracy"]
})


Model_Results <- data.frame(Model=models,
                               F1_Score=as.factor(Matrix_Models),
                               Accuracy=as.factor(Matrix_Acc)) %>% arrange(desc(F1_Score))

# Saw the Results
Model_Results
#==============================================================================================

## Flexible Discriminant Analysis

# Note: For computing problems we need to do the fda modeling without the weights.

# Create the fit model with fda.
fit_fda <- train(Drop_out ~ Age + Department + Sex + Area + Income + Income_pc + Language, 
                 data = train_set, 
                 method = "fda",
                 metric = "F1",
                 trControl = trainControl(method = "cv",
                                          number = 3,
                                          classProbs = TRUE,
                                          summaryFunction = f1))


fit_fda$result

# Test the model with Test_data.
Matrix_fda <- confusionMatrix(predict(fit_fda, test_set), test_set$Drop_out, mode = "prec_recall")

Model_Results <- bind_rows(Model_Results,
                           data.frame(Model="fda", 
                                  F1_Score = as.factor(Matrix_fda$byClass["F1"]),
                                  Accuracy = as.factor(Matrix_fda$byClass["Balanced Accuracy"]))) %>% 
  arrange(desc(F1_Score))

# Saw the Results
Model_Results
#==============================================================================================
#  Model          F1_Score            Accuracy
#   fda      0.626605884790717    0.757305771399003
#   qda      0.468683651804671    0.691455183149355
#   glm      0.414422638064811    0.646067845534319
#   lda      0.001536098310292    0.500384319754035
#==============================================================================================
## Remove extra variables

rm(fit_glm, fit_s, fits, F1_s, fit_fda,
   Matrix_glm, Matrix_s, Matrix_Models, Matrix_fda,
   train_set, test_set, models,
   Model_Results)
#==============================================================================================

# Final Validation

# Create the fit model with fda.
fit <- train(Drop_out ~ Age + Department + Sex + Area + Income + Income_pc + Language, 
             data = Data, 
             method = "fda",
             metric = "F1",
             trControl = trainControl(method = "cv",
                                      number = 3,
                                      classProbs = TRUE,
                                      summaryFunction = f1))


fit$result

# Test the model with Validation set.
Matrix <- confusionMatrix(predict(fit, Validation), Validation$Drop_out, mode = "prec_recall")

Model_Result <- tibble(Method="FDA", 
                       F1_Score = Matrix$byClass["F1"])

# Saw the Results
data.frame(Model_Result)
#==============================================================================================

# Compare the model with a sample.

# By Sample
set.seed(1, sample.kind = "Rounding")

# Create the fit model with a sample.
fit_s <- sample(c("Desertor", "No_Desertor"), replace = TRUE, nrow(Validation))

F1_s <- F1_Score(Validation$Drop_out, fit_s)

Model_Result <- bind_rows(Model_Result,
                           tibble(Method="By Sample", 
                        F1_Score = F1_s))

# Saw the Results
data.frame(Model_Result)
#==============================================================================================
#~~~~~~~~~~~~~~#
# Final Report #
#~~~~~~~~~~~~~~#

# We can notice is that the data is clearly unbalanced with 0.2258787 of deserters 
# in the Validation data, so if we make a prediction based on random samples 
# we only get a precision of 0.2703410 so our model clearly improves the prediction of 
# this dataset with 0.6274714 of precision.
#======================================================================================================