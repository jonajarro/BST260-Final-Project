NSDUH Final Project
================
Jesse Osmar Najarro
2022-12-06

\#Load in data and packages

``` r
load("/Users/jesse/Downloads/ICPSR_36361/DS0001/36361-0001-Data.rda")
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dslabs)
library(caret)
```

    ## Loading required package: lattice
    ## 
    ## Attaching package: 'caret'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     lift

\#Exploring data

``` r
data<- da36361.0001

#Exploring outcome var 
unique(data$AMDEYR)
```

    ## [1] (2) AMDELT=1 & ADPB2WK=2       (1) Yes (AMDELT=1 & ADPB2WK=1)
    ## [3] <NA>                          
    ## Levels: (1) Yes (AMDELT=1 & ADPB2WK=1) (2) AMDELT=1 & ADPB2WK=2

``` r
#Need to make sure its a factor for regression and classification
levels(data$AMDEYR) 
```

    ## [1] "(1) Yes (AMDELT=1 & ADPB2WK=1)" "(2) AMDELT=1 & ADPB2WK=2"

``` r
summary(data$AMDEYR)
```

    ## (1) Yes (AMDELT=1 & ADPB2WK=1)       (2) AMDELT=1 & ADPB2WK=2 
    ##                           3186                          38019 
    ##                           NA's 
    ##                          14066

``` r
#Renaming outcome var 
levels(data$AMDEYR) <- c("Yes", "No")

#Checking it went through
summary(data$AMDEYR)
```

    ##   Yes    No  NA's 
    ##  3186 38019 14066

``` r
#Checking our independent variable
class(data$NEWRACE2)
```

    ## [1] "factor"

``` r
levels(data$NEWRACE2)
```

    ## [1] "(1) NonHisp White"                   "(2) NonHisp Black/Afr Am"           
    ## [3] "(3) NonHisp Native Am/AK Native"     "(4) NonHisp Native HI/Other Pac Isl"
    ## [5] "(5) NonHisp Asian"                   "(6) NonHisp more than one race"     
    ## [7] "(7) Hispanic"

``` r
summary(data$NEWRACE2)
```

    ##                   (1) NonHisp White            (2) NonHisp Black/Afr Am 
    ##                               33534                                6693 
    ##     (3) NonHisp Native Am/AK Native (4) NonHisp Native HI/Other Pac Isl 
    ##                                 907                                 299 
    ##                   (5) NonHisp Asian      (6) NonHisp more than one race 
    ##                                2355                                1959 
    ##                        (7) Hispanic 
    ##                                9524

``` r
#Preprocessing 

#Drop AGE<18 categories
`%notin%` <- Negate(`%in%`)
data_prac <- data[data$AGE2 %notin% c("(01) Respondent is 12 years old", "(02) Respondent is 13 years old", "(03) Respondent is 14 years old", "(04) Respondent is 15 years old", "(05) Respondent is 16 years old", "(06) Respondent is 17 years old"), ]

data_prac <- data[data$EDUCCAT2 %notin% c("(5) 12 to 17 year olds (AGE2<=6)"), ]

#Checking it all went through
data_prac$AGE2 <- factor(data_prac$AGE2)
levels(data_prac$AGE2)
```

    ##  [1] "(07) Respondent is 18 years old"               
    ##  [2] "(08) Respondent is 19 years old"               
    ##  [3] "(09) Respondent is 20 years old"               
    ##  [4] "(10) Respondent is 21 years old"               
    ##  [5] "(11) Respondent is 22 or 23 years old"         
    ##  [6] "(12) Respondent is 24 or 25 years old"         
    ##  [7] "(13) Respondent is between 26 and 29 years old"
    ##  [8] "(14) Respondent is between 30 and 34 years old"
    ##  [9] "(15) Respondent is between 35 and 49 years old"
    ## [10] "(16) Respondent is between 50 and 64 years old"
    ## [11] "(17) Respondent is 65 years old or older"

``` r
data_prac$EDUCCAT2 <- factor(data_prac$EDUCCAT2)
levels(data_prac$EDUCCAT2)
```

    ## [1] "(1) Less than high school (IREDUC2<=7 and AGE2>=7)"
    ## [2] "(2) High school graduate (IREDUC2=8 and AGE2>=7)"  
    ## [3] "(3) Some college (IREDUC2=9-10 and AGE2>=7)"       
    ## [4] "(4) College graduate (IREDUC2=11 and AGE2>=7)"

``` r
#Refactoring levels of AGE
library(plyr)
```

    ## ------------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## ------------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

    ## The following object is masked from 'package:purrr':
    ## 
    ##     compact

``` r
data_prac$AGE2 <- revalue(data_prac$AGE2, c("(07) Respondent is 18 years old" = "(1) Respondent is 18 years old", '(08) Respondent is 19 years old' = '(2) Respondent is 19 years old', '(09) Respondent is 20 years old' = '(3) Respondent is 20 years old', '(10) Respondent is 21 years old'='(4) Respondent is 21 years old', '(11) Respondent is 22 or 23 years old'='(5) Respondent is 22 or 23 years old', '(12) Respondent is 24 or 25 years old'='(6) Respondent is 24 or 25 years old', '(13) Respondent is between 26 and 29 years old'='(7) Respondent is between 26 and 29 years old', '(14) Respondent is between 30 and 34 years old' = '(8) Respondent is between 30 and 34 years old', '(15) Respondent is between 35 and 49 years old'= '(9) Respondent is between 35 and 49 years old', '(16) Respondent is between 50 and 64 years old'='(10) Respondent is between 50 and 64 years old', '(17) Respondent is 65 years old or older'= '(11) Respondent is 65 years old or older'))

#Checking it went through
levels(data_prac$AGE2)
```

    ##  [1] "(1) Respondent is 18 years old"                
    ##  [2] "(2) Respondent is 19 years old"                
    ##  [3] "(3) Respondent is 20 years old"                
    ##  [4] "(4) Respondent is 21 years old"                
    ##  [5] "(5) Respondent is 22 or 23 years old"          
    ##  [6] "(6) Respondent is 24 or 25 years old"          
    ##  [7] "(7) Respondent is between 26 and 29 years old" 
    ##  [8] "(8) Respondent is between 30 and 34 years old" 
    ##  [9] "(9) Respondent is between 35 and 49 years old" 
    ## [10] "(10) Respondent is between 50 and 64 years old"
    ## [11] "(11) Respondent is 65 years old or older"

``` r
#Making dummy variables out of the predictors 
predictors <- data_prac|> select(AGE2, IRSEX, INCOME, EDUCCAT2, WRKEDYR, ALCDAYS, CIG30USE, LIFANXD, LIFDIAB, LIFHBP)

#Find class of all predictors
sapply(predictors, class)
```

    ##      AGE2     IRSEX    INCOME  EDUCCAT2   WRKEDYR   ALCDAYS  CIG30USE   LIFANXD 
    ##  "factor"  "factor"  "factor"  "factor"  "factor" "numeric" "numeric"  "factor" 
    ##   LIFDIAB    LIFHBP 
    ##  "factor"  "factor"

``` r
#Data frame of factor predictors 
predictors_factors<- data_prac|> select(NEWRACE2, AGE2, IRSEX, INCOME, EDUCCAT2, WRKEDYR, LIFANXD, LIFDIAB, LIFHBP)

#Levels of factor predictors 
sapply(predictors_factors, levels)
```

    ## $NEWRACE2
    ## [1] "(1) NonHisp White"                   "(2) NonHisp Black/Afr Am"           
    ## [3] "(3) NonHisp Native Am/AK Native"     "(4) NonHisp Native HI/Other Pac Isl"
    ## [5] "(5) NonHisp Asian"                   "(6) NonHisp more than one race"     
    ## [7] "(7) Hispanic"                       
    ## 
    ## $AGE2
    ##  [1] "(1) Respondent is 18 years old"                
    ##  [2] "(2) Respondent is 19 years old"                
    ##  [3] "(3) Respondent is 20 years old"                
    ##  [4] "(4) Respondent is 21 years old"                
    ##  [5] "(5) Respondent is 22 or 23 years old"          
    ##  [6] "(6) Respondent is 24 or 25 years old"          
    ##  [7] "(7) Respondent is between 26 and 29 years old" 
    ##  [8] "(8) Respondent is between 30 and 34 years old" 
    ##  [9] "(9) Respondent is between 35 and 49 years old" 
    ## [10] "(10) Respondent is between 50 and 64 years old"
    ## [11] "(11) Respondent is 65 years old or older"      
    ## 
    ## $IRSEX
    ## [1] "(1) Male"   "(2) Female"
    ## 
    ## $INCOME
    ## [1] "(1) Less than $20,000" "(2) $20,000 - $49,999" "(3) $50,000 - $74,999"
    ## [4] "(4) $75,000 or More"  
    ## 
    ## $EDUCCAT2
    ## [1] "(1) Less than high school (IREDUC2<=7 and AGE2>=7)"
    ## [2] "(2) High school graduate (IREDUC2=8 and AGE2>=7)"  
    ## [3] "(3) Some college (IREDUC2=9-10 and AGE2>=7)"       
    ## [4] "(4) College graduate (IREDUC2=11 and AGE2>=7)"     
    ## 
    ## $WRKEDYR
    ## [1] "(1) Yes" "(2) No" 
    ## 
    ## $LIFANXD
    ## [1] "(1) Response entered"     "(6) Response not entered"
    ## 
    ## $LIFDIAB
    ## [1] "(1) Response entered"     "(6) Response not entered"
    ## 
    ## $LIFHBP
    ## [1] "(1) Response entered"     "(6) Response not entered"

``` r
#Transform factor predictors to categorical 
library(fastDummies)
data_dummy<- data_prac

data_dummy<- data_dummy[!is.na(data_dummy$AMDEYR),]
data_dummy<- data_dummy[!is.na(data_dummy$NEWRACE2),]

data_dummy<- data_dummy[!is.na(data_dummy$AGE2),]

data_dummy<- data_dummy[!is.na(data_dummy$IRSEX),]
data_dummy<- data_dummy[!is.na(data_dummy$INCOME),]
data_dummy<- data_dummy[!is.na(data_dummy$EDUCCAT2),]
data_dummy<- data_dummy[!is.na(data_dummy$WRKEDYR),]
data_dummy<- data_dummy[!is.na(data_dummy$LIFANXD),]
data_dummy<- data_dummy[!is.na(data_dummy$LIFHBP),]

data_dummy<- dummy_cols(data_dummy, select_columns= c("NEWRACE2", "AGE2", "IRSEX", "INCOME", "EDUCCAT2", "WRKEDYR", "LIFANXD", "LIFDIAB", "LIFHBP"),ignore_na=TRUE)

#Barplot 
library(ggplot2)
ggplot(data_dummy, aes(x=NEWRACE2, y=AMDEYR)) +geom_bar(stat="identity", fill="red") + ggtitle("Bar plot of depression by race/ethnicity")+ theme(axis.text.x = element_text(angle = 25, vjust = 0.8, hjust=1))+ylab("Depression Count") + theme(axis.title.x = element_text(hjust=1)) +  theme(axis.text.y=element_blank()) 
```

![](NSDUH-Final-Project-Github-Doc-2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#Table 1 
library(tableone)
vars<- c("NEWRACE2", "AGE2", "IRSEX", "INCOME", "EDUCCAT2", "WRKEDYR", "LIFANXD", "LIFDIAB", "LIFHBP")
tableOne <- CreateTableOne(vars = vars, strata = c("NEWRACE2"), data = data_dummy)
```

``` r
#Create data partition 
set.seed(1990)
y<- data_dummy$AMDEYR
test_index <- createDataPartition(y, times = 1, p = 0.8, list = FALSE)
train_set <- data_dummy |> slice(test_index)
test_set <- data_dummy |> slice(-test_index)
```

``` r
#Training simple logistic regression model based on race/ethnicity alone 
simple_model<- train_set|>glm(AMDEYR~NEWRACE2, data=_, family="binomial")

#Training simple logisic regression model based on multiple predictors
fit_glm_mp <- train_set |> glm(AMDEYR ~ NEWRACE2 + AGE2 + IRSEX + INCOME + EDUCCAT2 + CIG30USE + ALCDAYS + LIFANXD + LIFDEPRS + LIFDIAB + LIFHBP, data=_, family = "binomial")

#Getting OR 
simple_model$coefficients<-exp(simple_model$coefficients)

#Getting OR 
fit_glm_mp2<- fit_glm_mp
fit_glm_mp2$coefficients<- exp((fit_glm_mp$coefficients[2:7]))
```

``` r
#Training simple logistic regression model based on dummy race variables 
simple_model2<- train_set|>glm(AMDEYR~`NEWRACE2_(1) NonHisp White`+`NEWRACE2_(2) NonHisp Black/Afr Am`+`NEWRACE2_(3) NonHisp Native Am/AK Native`+`NEWRACE2_(4) NonHisp Native HI/Other Pac Isl`+`NEWRACE2_(5) NonHisp Asian`+`NEWRACE2_(6) NonHisp more than one race`+`NEWRACE2_(7) Hispanic`, data=_, family="binomial")

#Getting OR 
exp(coef(simple_model2))


fit_glm_mpd <- train_set |> glm(AMDEYR ~ `NEWRACE2_(1) NonHisp White` + `NEWRACE2_(2) NonHisp Black/Afr Am`+ `NEWRACE2_(3) NonHisp Native Am/AK Native`+`NEWRACE2_(4) NonHisp Native HI/Other Pac Isl`+`NEWRACE2_(5) NonHisp Asian`+`NEWRACE2_(6) NonHisp more than one race`+`NEWRACE2_(7) Hispanic`+ `AGE2_(1) Respondent is 18 years old`+ `AGE2_(2) Respondent is 19 years old`+`AGE2_(3) Respondent is 20 years old`+`AGE2_(4) Respondent is 21 years old`+`AGE2_(5) Respondent is 22 or 23 years old`+`AGE2_(6) Respondent is 24 or 25 years old`+`AGE2_(7) Respondent is between 26 and 29 years old`+ `AGE2_(8) Respondent is between 30 and 34 years old`+`AGE2_(9) Respondent is between 35 and 49 years old`+ `AGE2_(10) Respondent is between 50 and 64 years old`+ `AGE2_(11) Respondent is 65 years old or older`+ IRSEX+ `INCOME_(1) Less than $20,000`+`INCOME_(2) $20,000 - $49,999`+`INCOME_(3) $50,000 - $74,999`+`INCOME_(4) $75,000 or More`+`EDUCCAT2_(1) Less than high school (IREDUC2<=7 and AGE2>=7)`+`EDUCCAT2_(2) High school graduate (IREDUC2=8 and AGE2>=7)`+`EDUCCAT2_(3) Some college (IREDUC2=9-10 and AGE2>=7)`+`EDUCCAT2_(4) College graduate (IREDUC2=11 and AGE2>=7)`+ CIG30USE+ALCDAYS+LIFANXD+ LIFDEPRS+LIFHBP+WRKEDYR, data=_, family="binomial")

#Getting OR 
exp(coef(fit_glm_mpd))
```

``` r
library(sjPlot)
```

    ## Install package "strengejacke" from GitHub (`devtools::install_github("strengejacke/strengejacke")`) to load all sj-packages at once!

``` r
plot_model(simple_model, vline.color = "Blue", title="Forest plot of depression according to race/ethnicity (unadjusted model)")
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

![](NSDUH-Final-Project-Github-Doc-2_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
plot_model(fit_glm_mp2, vline.color = "Blue", title="Forest plot of depression according to race/ethnicity (fully-adjusted model)")
```

![](NSDUH-Final-Project-Github-Doc-2_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
#Predicting simple model 
p_hat_logit <- predict(simple_model, newdata = test_set, type = "response")
head(p_hat_logit)
```

    ##         1         2         3         4         5         6 
    ## 0.9962608 0.9962608 0.9962608 0.9962608 0.9962608 0.9962608

``` r
pre1<-ifelse(p_hat_logit > 0.9152, "Yes", "No") |> factor()
confusionMatrix(pre1, test_set$AMDEYR)$overall[["Accuracy"]]
```

    ## Warning in confusionMatrix.default(pre1, test_set$AMDEYR): Levels are not in the
    ## same order for reference and data. Refactoring data to match.

    ## [1] 0.1462926

``` r
confusionMatrix(pre1, test_set$AMDEYR)
```

    ## Warning in confusionMatrix.default(pre1, test_set$AMDEYR): Levels are not in the
    ## same order for reference and data. Refactoring data to match.

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  Yes   No
    ##        Yes  219 1278
    ##        No     0    0
    ##                                           
    ##                Accuracy : 0.1463          
    ##                  95% CI : (0.1288, 0.1652)
    ##     No Information Rate : 0.8537          
    ##     P-Value [Acc > NIR] : 1               
    ##                                           
    ##                   Kappa : 0               
    ##                                           
    ##  Mcnemar's Test P-Value : <2e-16          
    ##                                           
    ##             Sensitivity : 1.0000          
    ##             Specificity : 0.0000          
    ##          Pos Pred Value : 0.1463          
    ##          Neg Pred Value :    NaN          
    ##              Prevalence : 0.1463          
    ##          Detection Rate : 0.1463          
    ##    Detection Prevalence : 1.0000          
    ##       Balanced Accuracy : 0.5000          
    ##                                           
    ##        'Positive' Class : Yes             
    ## 

``` r
#Predicting multiple predictor model 
p_hat_glm_mp <- predict(fit_glm_mp, newdata= test_set, type="response")
head(p_hat_glm_mp)
```

    ##         1         2         3         4         5         6 
    ## 0.5824702        NA        NA        NA        NA        NA

``` r
pre2 <- ifelse(p_hat_glm_mp > 0.9152, "Yes", "No")|> factor()
confusionMatrix(pre2, test_set$AMDEYR)$overall[["Accuracy"]]
```

    ## Warning in confusionMatrix.default(pre2, test_set$AMDEYR): Levels are not in the
    ## same order for reference and data. Refactoring data to match.

    ## [1] 0.4592275

``` r
confusionMatrix(pre2, test_set$AMDEYR)
```

    ## Warning in confusionMatrix.default(pre2, test_set$AMDEYR): Levels are not in the
    ## same order for reference and data. Refactoring data to match.

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction Yes  No
    ##        Yes   7  82
    ##        No   44 100
    ##                                          
    ##                Accuracy : 0.4592         
    ##                  95% CI : (0.394, 0.5255)
    ##     No Information Rate : 0.7811         
    ##     P-Value [Acc > NIR] : 1.0000000      
    ##                                          
    ##                   Kappa : -0.247         
    ##                                          
    ##  Mcnemar's Test P-Value : 0.0009799      
    ##                                          
    ##             Sensitivity : 0.13725        
    ##             Specificity : 0.54945        
    ##          Pos Pred Value : 0.07865        
    ##          Neg Pred Value : 0.69444        
    ##              Prevalence : 0.21888        
    ##          Detection Rate : 0.03004        
    ##    Detection Prevalence : 0.38197        
    ##       Balanced Accuracy : 0.34335        
    ##                                          
    ##        'Positive' Class : Yes            
    ## 

``` r
#Creating a ROC and precision recall curve to assess different cut-offs 
library(pROC)
pred <- prediction(p_hat_logit, test_set)
test_roc = roc(test_set$AMDEYR ~ pred, plot = TRUE, print.auc = TRUE)

install.packages("PRROC")
library(PRROC)
score1= p_hat_logit[train_set$AMDEYR=="Yes"]
score0= p_hat_logit[train_set$AMDEYR=="No"]
roc= roc.curve(score1, score0, curve = T)
roc$auc

pr= pr.curve(score1, score0, curve = T)
pr
plot(pr, main="In-sample PR curve")
pr.test= pr.curve(score1.test, score0.test, curve = T)
pr.test
plot(pr.test, main="Out-of-sample PR curve")
```

\#KNN (Predicting depression)

``` r
#Pre-processing for knn 

#Removing predictors with 0 variance 
library(caret)
nzv <- nearZeroVar(train_set)
col_index <- setdiff(1:ncol(train_set), nzv)
length(col_index)
```

    ## [1] 1795

``` r
col_index2<- col_index[c(1769:1795)]

image(matrix(1:3136 %in% nzv, 56, 56))+title(main="Zero Variance Predictors")
```

![](NSDUH-Final-Project-Github-Doc-2_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

    ## integer(0)

``` r
#Performing KNN 
n <- 1873
b <- 5
index <- sample(nrow(train_set), n)
control <- trainControl(method = "cv", number = b, p = .9)
train_knn <- train(train_set[index, col_index2], train_set$AMDEYR[index], 
                   method = "knn", 
                   tuneGrid = data.frame(k = c(2:15)),
                   trControl = control)

train_knn
```

    ## k-Nearest Neighbors 
    ## 
    ## 1873 samples
    ##   27 predictor
    ##    2 classes: 'Yes', 'No' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 1499, 1498, 1499, 1498, 1498 
    ## Resampling results across tuning parameters:
    ## 
    ##   k   Accuracy   Kappa     
    ##    2  0.8275494  0.14887958
    ##    3  0.8398360  0.12812151
    ##    4  0.8430446  0.11666800
    ##    5  0.8483822  0.11548988
    ##    6  0.8489141  0.11384477
    ##    7  0.8483807  0.08017309
    ##    8  0.8473098  0.07341370
    ##    9  0.8478460  0.06190008
    ##   10  0.8467779  0.03731664
    ##   11  0.8494446  0.05205489
    ##   12  0.8489098  0.04662730
    ##   13  0.8483750  0.03608944
    ##   14  0.8489070  0.03677612
    ##   15  0.8521141  0.04816991
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was k = 15.

``` r
saveRDS(train_knn, file="train_knn_fp.RDS")
plot(train_knn, main="Accuracy for number of neighbors tested")
```

![](NSDUH-Final-Project-Github-Doc-2_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
fit_knn <- knn3(train_set[, col_index2], train_set$AMDEYR,  k = 15)
y_hat_knn <- predict(fit_knn, train_set[, col_index2], type="class")
cm <- confusionMatrix(y_hat_knn, test_set$AMDEYR[test_index]) 
cm$overall["Accuracy"]
```

    ##  Accuracy 
    ## 0.8341709

``` r
cm$byClass
```

    ##          Sensitivity          Specificity       Pos Pred Value 
    ##          0.017142857          0.974484789          0.103448276 
    ##       Neg Pred Value            Precision               Recall 
    ##          0.852360515          0.103448276          0.017142857 
    ##                   F1           Prevalence       Detection Rate 
    ##          0.029411765          0.146566164          0.002512563 
    ## Detection Prevalence    Balanced Accuracy 
    ##          0.024288107          0.495813823

``` r
cm$table
```

    ##           Reference
    ## Prediction Yes  No
    ##        Yes   3  26
    ##        No  172 993

``` r
#Perform RF 
control <- trainControl(method="cv", number = 5)
grid <- data.frame(mtry = c(1:10))

train_rf <-  train(train_set[, col_index2], train_set$AMDEYR, 
                   method = "rf", 
                   ntree = 100,
                   trControl = control,
                   tuneGrid = grid,
                   nSamp = 1000)
train_rf
```

    ## Random Forest 
    ## 
    ## 5992 samples
    ##   27 predictor
    ##    2 classes: 'Yes', 'No' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (5 fold) 
    ## Summary of sample sizes: 4793, 4794, 4794, 4793, 4794 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa     
    ##    1    0.8536383  0.00000000
    ##    2    0.8536383  0.00000000
    ##    3    0.8539733  0.06378176
    ##    4    0.8544744  0.12197838
    ##    5    0.8519717  0.16607011
    ##    6    0.8471319  0.16245371
    ##    7    0.8444608  0.18450397
    ##    8    0.8421250  0.18351116
    ##    9    0.8357836  0.17744615
    ##   10    0.8339468  0.18001807
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 4.

``` r
plot(train_rf, main="Accuracy for number of predictors tested ")
```

![](NSDUH-Final-Project-Github-Doc-2_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
library(randomForest)
```

    ## randomForest 4.7-1.1

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
fit_rf <- randomForest(train_set[, col_index2], train_set$AMDEYR, 
                       mtry = train_rf$bestTune$mtry)

y_hat_rf <- predict(fit_rf, train_set[ ,col_index2])
cm2 <- confusionMatrix(y_hat_rf, test_set$AMDEYR[test_index])
cm2$overall["Accuracy"]
```

    ##  Accuracy 
    ## 0.8433836

``` r
cm2$byClass
```

    ##          Sensitivity          Specificity       Pos Pred Value 
    ##          0.051428571          0.979391560          0.300000000 
    ##       Neg Pred Value            Precision               Recall 
    ##          0.857388316          0.300000000          0.051428571 
    ##                   F1           Prevalence       Detection Rate 
    ##          0.087804878          0.146566164          0.007537688 
    ## Detection Prevalence    Balanced Accuracy 
    ##          0.025125628          0.515410066

``` r
cm2$table
```

    ##           Reference
    ## Prediction Yes  No
    ##        Yes   9  21
    ##        No  166 998

``` r
imp <- importance(fit_rf)
mat <- rep(0, ncol(train_set))
mat[col_index] <- imp
```

    ## Warning in mat[col_index] <- imp: number of items to replace is not a multiple
    ## of replacement length

``` r
image(matrix(mat, 50, 50))+title(main="Importance Graph for Random Forest")
```

    ## Warning in matrix(mat, 50, 50): data length [3184] is not a sub-multiple or
    ## multiple of the number of rows [50]

![](NSDUH-Final-Project-Github-Doc-2_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

    ## integer(0)
