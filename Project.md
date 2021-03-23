Project
================
Ananda Poudel
3/16/2021

# Libraries and setting directory.

``` r
setwd("/Users/apoudel53/Desktop/Job Search/project")
#The data came in partitioned.
hrAnalytics <- read.csv("aug_train.csv")
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
#library(corrplot)
library(caret)
library(vip)
```

    ## 
    ## Attaching package: 'vip'

    ## The following object is masked from 'package:utils':
    ## 
    ##     vi

``` r
library(ROCR)
```

# Exploratory data analysis.

#### Dealing with missing values.

``` r
#Checking for na and blank values in each column for our data.

emptyValues <- sapply(colnames(hrAnalytics), function(x){
  sum(is.na(hrAnalytics[,x]) | hrAnalytics[,x] == '') 
})


#Set the threshold of missing values by averaging the number of mising values from the columns that contain those missing values.
emptyValues <- as.data.frame(emptyValues)
names(emptyValues)[1] <- "values"
emptyValues <- emptyValues %>%
  filter(values > 0)
avg <- mean(emptyValues$values)
removeEmptyValues <- emptyValues %>%
  filter(values < avg)
removeEmptyValues
```

    ##                     values
    ## enrolled_university    386
    ## education_level        460
    ## experience              65
    ## last_new_job           423

``` r
# These variables will not consider the blank values since the number of blank values are below the mean so we'll remove the rows from the original dataset where the values were blank.

emptyValues <- emptyValues %>%
  filter(values > avg)
emptyValues
```

    ##                  values
    ## gender             4508
    ## major_discipline   2813
    ## company_size       5938
    ## company_type       6140

``` r
#These variables will consider the blank values as a factor. 


hrAnalytics <- hrAnalytics %>%
  filter(enrolled_university != '', education_level != '', experience != '', last_new_job != '')

#Checking to make sure there are no empty values for the specified columns
sapply(c("enrolled_university", "education_level", "experience", "last_new_job"), function(x){
  sum(is.na(hrAnalytics[,x]) | hrAnalytics[,x] == '')
})
```

    ## enrolled_university     education_level          experience        last_new_job 
    ##                   0                   0                   0                   0

#### Renaming variables and converting data types.

``` r
hrAnalytics$gender[which(hrAnalytics$gender == '')] <- 'Not specified'
hrAnalytics$enrolled_university[which(hrAnalytics$enrolled_university == 'no_enrollment')] <- "Not enrolled"
hrAnalytics$major_discipline[which(hrAnalytics$major_discipline == '')] <- "Not specified"
hrAnalytics$experience[which(hrAnalytics$experience == '>20')] <- "More than 20"
hrAnalytics$experience[which(hrAnalytics$experience == '<1')] <- "Less than 1"
hrAnalytics$company_size[which(hrAnalytics$company_size == '')] <- "Not specified"
hrAnalytics$company_size[which(hrAnalytics$company_size == '<10')] <- "Less than 10"
hrAnalytics$company_size[which(hrAnalytics$company_size == '10/49')] <- "10-49"
hrAnalytics$company_size[which(hrAnalytics$company_size == '10000+')] <- "More than 10000"
hrAnalytics$company_type[which(hrAnalytics$company_type == '')] <- "Not specified"
hrAnalytics$company_type[which(hrAnalytics$company_type == 'Pvt Ltd')] <- "Private Limited"
hrAnalytics$last_new_job[which(hrAnalytics$last_new_job == '>4')] <- "Greater than 4"


str(hrAnalytics)
```

    ## 'data.frame':    18014 obs. of  14 variables:
    ##  $ enrollee_id           : int  8949 29725 11561 666 21651 28806 402 27107 699 29452 ...
    ##  $ city                  : chr  "city_103" "city_40" "city_21" "city_162" ...
    ##  $ city_development_index: num  0.92 0.776 0.624 0.767 0.764 0.92 0.762 0.92 0.92 0.624 ...
    ##  $ gender                : chr  "Male" "Male" "Not specified" "Male" ...
    ##  $ relevent_experience   : chr  "Has relevent experience" "No relevent experience" "No relevent experience" "Has relevent experience" ...
    ##  $ enrolled_university   : chr  "Not enrolled" "Not enrolled" "Full time course" "Not enrolled" ...
    ##  $ education_level       : chr  "Graduate" "Graduate" "Graduate" "Masters" ...
    ##  $ major_discipline      : chr  "STEM" "STEM" "STEM" "STEM" ...
    ##  $ experience            : chr  "More than 20" "15" "5" "More than 20" ...
    ##  $ company_size          : chr  "Not specified" "50-99" "Not specified" "50-99" ...
    ##  $ company_type          : chr  "Not specified" "Private Limited" "Not specified" "Funded Startup" ...
    ##  $ last_new_job          : chr  "1" "Greater than 4" "never" "4" ...
    ##  $ training_hours        : int  36 47 83 8 24 24 18 46 123 32 ...
    ##  $ target                : num  1 0 0 0 1 0 1 1 0 1 ...

``` r
#Characters need to be turned into factor for it to be considered in the model.
hrAnalytics <- hrAnalytics %>% 
  mutate_if(sapply(hrAnalytics, is.character), as.factor)
str(hrAnalytics)
```

    ## 'data.frame':    18014 obs. of  14 variables:
    ##  $ enrollee_id           : int  8949 29725 11561 666 21651 28806 402 27107 699 29452 ...
    ##  $ city                  : Factor w/ 123 levels "city_1","city_10",..: 6 78 65 51 58 50 84 6 6 65 ...
    ##  $ city_development_index: num  0.92 0.776 0.624 0.767 0.764 0.92 0.762 0.92 0.92 0.624 ...
    ##  $ gender                : Factor w/ 4 levels "Female","Male",..: 2 2 3 2 3 2 2 2 3 3 ...
    ##  $ relevent_experience   : Factor w/ 2 levels "Has relevent experience",..: 1 2 2 1 1 1 1 1 1 2 ...
    ##  $ enrolled_university   : Factor w/ 3 levels "Full time course",..: 2 2 1 2 3 2 2 2 2 1 ...
    ##  $ education_level       : Factor w/ 5 levels "Graduate","High School",..: 1 1 1 3 1 2 1 1 1 2 ...
    ##  $ major_discipline      : Factor w/ 7 levels "Arts","Business Degree",..: 7 7 7 7 7 5 7 7 7 5 ...
    ##  $ experience            : Factor w/ 22 levels "1","10","11",..: 22 7 16 22 3 16 5 18 9 12 ...
    ##  $ company_size          : Factor w/ 9 levels "10-49","100-500",..: 9 4 9 4 9 4 7 4 8 9 ...
    ##  $ company_type          : Factor w/ 7 levels "Early Stage Startup",..: 4 6 4 2 4 2 6 6 6 4 ...
    ##  $ last_new_job          : Factor w/ 6 levels "1","2","3","4",..: 1 5 6 4 1 1 5 1 5 6 ...
    ##  $ training_hours        : int  36 47 83 8 24 24 18 46 123 32 ...
    ##  $ target                : num  1 0 0 0 1 0 1 1 0 1 ...

#### Data visualization and transformation.

``` r
#Visualizing gender and job change.
hrAnalytics$jobChange <- "Looking for a job change"
hrAnalytics$jobChange[which(hrAnalytics$target == 0)] <- "Not looking for a job change"

ggplot(data = hrAnalytics, aes(x = gender, fill = jobChange)) + geom_bar(position = 'dodge') + xlab("Gender") +
  ylab("Job Changes Count") + ggtitle("Number of Job Changes Based on Gender") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -1, position = position_dodge(0.9)) +
  labs(fill = "Job Change") +
  theme(plot.title = element_text(color="red", size=14, face="bold", hjust = 0.5))
```

![](Project_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
#From this graph, in terms of proportion, we see that more males are not looking for a job change than any other genders. Also, there are more males looking for a job change as well but in terms of sample size compared against each gender, that still seems relatively low.


#We are going to visualize to see whether students currently enrolled in universities are looking for a job change based on education level.
ggplot(data = hrAnalytics, aes(x = enrolled_university, fill = education_level)) + geom_bar(position = 'dodge') + facet_wrap( ~ jobChange) + xlab("Enrolled or Not Enrolled") +
  ylab("Job Changes Count") + ggtitle("Number of Job Changes Based on University Enrollment and Education Level") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -1, position = position_dodge(0.9)) +
  labs(fill = "Education Level") +
  theme(plot.title = element_text(color="blue", size=14, face="bold", hjust = 0.5))
```

![](Project_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
#From this graph, we see that for each category of current school enrollment based on the education level, more candidates are not looking for a job change. We can also look at the proportion table below for better understanding of the data.
table(hrAnalytics[,c('enrolled_university', 'education_level', 'jobChange')]) %>% prop.table
```

    ## , , jobChange = Looking for a job change
    ## 
    ##                    education_level
    ## enrolled_university     Graduate  High School      Masters          Phd
    ##    Full time course 0.0497946042 0.0123237482 0.0111024759 0.0002220495
    ##    Not enrolled     0.1101365605 0.0065504608 0.0350283113 0.0027201066
    ##    Part time course 0.0106583768 0.0017763961 0.0029421561 0.0001110248
    ##                    education_level
    ## enrolled_university Primary School
    ##    Full time course   0.0005551238
    ##    Not enrolled       0.0013322971
    ##    Part time course   0.0001665371
    ## 
    ## , , jobChange = Not looking for a job change
    ## 
    ##                    education_level
    ## enrolled_university     Graduate  High School      Masters          Phd
    ##    Full time course 0.0666148551 0.0339180637 0.0185411347 0.0010547352
    ##    Not enrolled     0.3520039969 0.0442433663 0.1588209171 0.0175419118
    ##    Part time course 0.0318641057 0.0071055845 0.0082713445 0.0004996114
    ##                    education_level
    ## enrolled_university Primary School
    ##    Full time course   0.0011102476
    ##    Not enrolled       0.0126013101
    ##    Part time course   0.0003885867

``` r
#Based on the proportions, it looks like majority of the candidates are not looking for a job change.

#Next we'll look at the training hours for each major and see whether they are looking for a job change.
ggplot(data = hrAnalytics, aes(x = major_discipline, y = training_hours)) + geom_boxplot() + facet_wrap( ~ jobChange) + xlab("Major") +
  ylab("Training Hours") + ggtitle("Number of Job Changes Based on Training Hours and Majors") +
  theme(plot.title = element_text(color="purple", size=14, face="bold", hjust = 0.5))
```

![](Project_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

``` r
#From the box plot, we can see that the average number of training hours are about the same for all the majors.

#Last visualization with city development index.
#Dividing the city development index based on quantiles for better visualization.
hrAnalytics$groupedIndex <- NA
summary(hrAnalytics$city_development_index)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.4480  0.7450  0.9100  0.8317  0.9200  0.9490

``` r
hrAnalytics$groupedIndex[which(hrAnalytics$city_development_index > 0.4470 & hrAnalytics$city_development_index < 0.7450)] <- "0.4470-0.7449"
hrAnalytics$groupedIndex[which(hrAnalytics$city_development_index > 0.7449 & hrAnalytics$city_development_index < 0.8317)] <- "0.7450-0.8316"
hrAnalytics$groupedIndex[which(hrAnalytics$city_development_index > 0.8316 & hrAnalytics$city_development_index < 0.9200)] <- "0.8317-0.9199"
hrAnalytics$groupedIndex[which(hrAnalytics$city_development_index > 0.9199 & hrAnalytics$city_development_index < 0.9500)] <- "0.9200-0.9499"

ggplot(data = hrAnalytics, aes(x = groupedIndex, fill = jobChange)) + geom_bar(position = 'dodge') + xlab("Group City Development Indices") +
  ylab("Job Changes Count") + ggtitle("Number of Job Changes Based on City Development Index") +
  theme(plot.title = element_text(color="Green", size=14, face="bold", hjust = 0.5)) + 
  labs(fill = "Job Change Status")
```

![](Project_files/figure-gfm/unnamed-chunk-4-4.png)<!-- -->

``` r
#This is very interesting because we see that as the index increases, more candidates are lenient to not looking for a new job.
```

#### Creating a logistic regression model by partitioning.

``` r
#Need to set target as a factor
hrAnalytics$target <- as.factor(hrAnalytics$target)
a <- createDataPartition(hrAnalytics$target, p = 0.8, list = FALSE) #Partioning the data by splitting 80-20.
train <- hrAnalytics[a,-c(1,2,15,16)] #Deselecting enrolee_id, city, jobChange and groupedIndex
test <- hrAnalytics[-a,-c(1,2,15,16)]
mod_fit <- train(target ~., data = train, method = "glm", family = "binomial")
vip(mod_fit)
```

![](Project_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
#From the variable importance model, we see that city development index variable is significantly improtant than other variables.

#Confussion matrix to determine accuracy
pred <- predict(mod_fit, newdata = test)
confusionMatrix(data = pred, test$target)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 2532  634
    ##          1  186  250
    ##                                          
    ##                Accuracy : 0.7723         
    ##                  95% CI : (0.7583, 0.786)
    ##     No Information Rate : 0.7546         
    ##     P-Value [Acc > NIR] : 0.006645       
    ##                                          
    ##                   Kappa : 0.2586         
    ##                                          
    ##  Mcnemar's Test P-Value : < 2.2e-16      
    ##                                          
    ##             Sensitivity : 0.9316         
    ##             Specificity : 0.2828         
    ##          Pos Pred Value : 0.7997         
    ##          Neg Pred Value : 0.5734         
    ##              Prevalence : 0.7546         
    ##          Detection Rate : 0.7029         
    ##    Detection Prevalence : 0.8790         
    ##       Balanced Accuracy : 0.6072         
    ##                                          
    ##        'Positive' Class : 0              
    ## 

``` r
#We can see that we achieved 77.01% accuracy from this model.
```

#### Creating a logistic regression model by cross-validation.

``` r
fold <- trainControl(method = "repeatedcv", number = 10) #10 fold cross validation
mod_fit2 <- train(target ~., data = hrAnalytics[,-c(1,2,15,16)], method = "glm", family = "binomial", trControl = fold)
vip(mod_fit2)
```

![](Project_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
pred2 <- predict(mod_fit2, newdata = test)
confusionMatrix(data = pred2, test$target)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction    0    1
    ##          0 2535  637
    ##          1  183  247
    ##                                          
    ##                Accuracy : 0.7723         
    ##                  95% CI : (0.7583, 0.786)
    ##     No Information Rate : 0.7546         
    ##     P-Value [Acc > NIR] : 0.006645       
    ##                                          
    ##                   Kappa : 0.2565         
    ##                                          
    ##  Mcnemar's Test P-Value : < 2.2e-16      
    ##                                          
    ##             Sensitivity : 0.9327         
    ##             Specificity : 0.2794         
    ##          Pos Pred Value : 0.7992         
    ##          Neg Pred Value : 0.5744         
    ##              Prevalence : 0.7546         
    ##          Detection Rate : 0.7038         
    ##    Detection Prevalence : 0.8806         
    ##       Balanced Accuracy : 0.6060         
    ##                                          
    ##        'Positive' Class : 0              
    ## 

``` r
#Although we see a slight difference in the importance of variables between the two models, the accuracy remains the same using cross validation.
```
