P2P Lending
================
Catherine Chen
04/2020

## Purpose: Compare Logit Regression vs. Classfication Tree to model and predict loan performance at The Lending Club

## 0. Set-up

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
library(TeachingDemos)
library(magrittr)
library(readr)
library(rpart)
```

## 1. Data Import and Clean-up

``` r
getwd()
```

    ## [1] "/Users/cchenny/Dropbox/Penn/Data things/Portolio/catchen97"

``` r
lctrain<- read_csv(file = "LoanStats3c_train.csv", skip = 1)
```

    ## Warning: One or more parsing issues, see `problems()` for details

    ## Rows: 235631 Columns: 111

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (25): id, term, int_rate, grade, sub_grade, emp_title, emp_length, home_...
    ## dbl (69): member_id, loan_amnt, funded_amnt, funded_amnt_inv, installment, a...
    ## lgl (17): annual_inc_joint, dti_joint, verification_status_joint, open_acc_6...

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
lctrain <- lctrain[-c(235630,235631),]
set.seed(char2seed("Catherine Chen"))
```

## 2. Descriptive statistics

``` r
lctrain$highgrade <- ifelse(lctrain$grade =="A"| lctrain$grade == "B",1,0)  

paste("Propotion of high grade loans", round(mean(lctrain$highgrade),4),sep = "= ")
```

    ## [1] "Propotion of high grade loans= 0.4161"

``` r
prop.highgrade = mean(lctrain$highgrade)

lctrain$medincome = as.numeric(lctrain$annual_inc > median(lctrain$annual_inc))
t.test(data= lctrain, highgrade~medincome)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  highgrade by medincome
    ## t = -45.042, df = 231180, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.09534142 -0.08738990
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##       0.3728698       0.4642354

``` r
lctrain$medloan = as.numeric(lctrain$funded_amnt > median(lctrain$funded_amnt))
t.test(data= lctrain, highgrade~ medloan)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  highgrade by medloan
    ## t = 34.091, df = 235540, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    ## 95 percent confidence interval:
    ##  0.06508972 0.07303066
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##       0.4505373       0.3814771

``` r
lctrain$rent = as.numeric(lctrain$home_ownership == "RENT")
t.test(data= lctrain, highgrade ~ rent)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  highgrade by rent
    ## t = 14.688, df = 199444, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means between group 0 and group 1 is not equal to 0
    ## 95 percent confidence interval:
    ##  0.02638399 0.03450975
    ## sample estimates:
    ## mean in group 0 mean in group 1 
    ##       0.4280667       0.3976199

1.  Median Income: Borrowers with an annual income HIGHER than the
    median are \~9.14% more high-grade than borrowers who earn below the
    median income. This difference is significant, as p-value &lt;
    2.2e-16 &lt; 　a=0.05

2.  Median loan amount:Borrowers with a loan amount LOWER than the
    median are \~6.91% more high-grade than borrowers who borrow more
    than the median. This difference is significant, as p-value &lt;
    2.2e-16 &lt; 　a=0.05

3.  Renting home: Borrowers who are RENTING and have a high-grade loan
    are proportinately fewer than those who are not renting by \~3.04%.
    This is again a significant diference, as p-value &lt; 2.2e-16 &lt;
    　a=0.05

## III. Model 1: Logit Regression

``` r
HG.LR <- glm(data = lctrain, highgrade ~ home_ownership + funded_amnt + verification_status + purpose, family= binomial)
summary(HG.LR)
```

    ## 
    ## Call:
    ## glm(formula = highgrade ~ home_ownership + funded_amnt + verification_status + 
    ##     purpose, family = binomial, data = lctrain)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.7444  -0.9431  -0.7539   1.1303   2.5661  
    ## 
    ## Coefficients:
    ##                                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                         8.478e+00  2.666e+01   0.318  0.75052    
    ## home_ownershipMORTGAGE             -7.918e+00  2.666e+01  -0.297  0.76652    
    ## home_ownershipOWN                  -8.021e+00  2.666e+01  -0.301  0.76356    
    ## home_ownershipRENT                 -8.128e+00  2.666e+01  -0.305  0.76049    
    ## funded_amnt                        -1.659e-05  5.904e-07 -28.095  < 2e-16 ***
    ## verification_statusSource Verified -6.717e-01  1.079e-02 -62.241  < 2e-16 ***
    ## verification_statusVerified        -9.676e-01  1.232e-02 -78.560  < 2e-16 ***
    ## purposecredit_card                  7.312e-01  4.903e-02  14.913  < 2e-16 ***
    ## purposedebt_consolidation          -1.581e-01  4.852e-02  -3.259  0.00112 ** 
    ## purposehome_improvement            -2.762e-01  5.174e-02  -5.339 9.35e-08 ***
    ## purposehouse                       -1.976e+00  1.371e-01 -14.411  < 2e-16 ***
    ## purposemajor_purchase              -9.299e-02  5.867e-02  -1.585  0.11296    
    ## purposemedical                     -1.098e+00  6.958e-02 -15.781  < 2e-16 ***
    ## purposemoving                      -2.056e+00  1.022e-01 -20.113  < 2e-16 ***
    ## purposeother                       -1.130e+00  5.400e-02 -20.928  < 2e-16 ***
    ## purposerenewable_energy            -2.131e+00  3.227e-01  -6.604 3.99e-11 ***
    ## purposesmall_business              -1.629e+00  8.392e-02 -19.410  < 2e-16 ***
    ## purposevacation                    -1.192e+00  8.677e-02 -13.741  < 2e-16 ***
    ## purposewedding                     -2.774e-01  7.555e-01  -0.367  0.71348    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 319984  on 235628  degrees of freedom
    ## Residual deviance: 296386  on 235610  degrees of freedom
    ## AIC: 296424
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
#Accuracy
lctrain$HGLR_predict = as.numeric(predict(HG.LR, type="response")> 0.47)
paste("Logistic Regression Accuracy", mean(lctrain$highgrade == lctrain$HGLR), sep="= ")
```

    ## Warning: Unknown or uninitialised column: `HGLR`.

    ## [1] "Logistic Regression Accuracy= NaN"

``` r
#A classifier that assigns a value of 0 to all rows
paste("All-zero benchmark", round(mean(lctrain$highgrade == rep(0,nrow(lctrain))),4), sep= "= ")
```

    ## [1] "All-zero benchmark= 0.5839"

``` r
#A classifier that randomly assigns 0 and 1 values
paste("Coin-flipping benchmark", round(mean(lctrain$highgrade == round(runif(nrow(lctrain),0,1),0)),4), sep = "= ")
```

    ## [1] "Coin-flipping benchmark= 0.4989"

## IV. Model 2: Classification Tree

``` r
HG.CT= rpart(data= lctrain, highgrade~ home_ownership + funded_amnt + verification_status + purpose, method= "class")
plot(HG.CT,uniform=TRUE,,margin=0.2);text(HG.CT,use.n=TRUE, all=TRUE, cex=.8)
```

![](P2P-Lending_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
summary(HG.CT)
```

    ## Call:
    ## rpart(formula = highgrade ~ home_ownership + funded_amnt + verification_status + 
    ##     purpose, data = lctrain, method = "class")
    ##   n= 235629 
    ## 
    ##           CP nsplit rel error    xerror        xstd
    ## 1 0.10758545      0 1.0000000 1.0000000 0.002440419
    ## 2 0.01778811      1 0.8924146 0.8924350 0.002392159
    ## 3 0.01000000      3 0.8568383 0.8568587 0.002371429
    ## 
    ## Variable importance
    ##             purpose verification_status         funded_amnt 
    ##                  55                  35                  11 
    ## 
    ## Node number 1: 235629 observations,    complexity param=0.1075854
    ##   predicted class=0  expected loss=0.4160905  P(node) =1
    ##     class counts: 137586 98043
    ##    probabilities: 0.584 0.416 
    ##   left son=2 (180107 obs) right son=3 (55522 obs)
    ##   Primary splits:
    ##       purpose             splits as  LRLLLLLLLLLLL, improve=4649.521, (0 missing)
    ##       verification_status splits as  RLL, improve=4298.594, (0 missing)
    ##       funded_amnt         < 28012.5 to the right, improve=1446.233, (0 missing)
    ##       home_ownership      splits as  RRLL, improve= 114.601, (0 missing)
    ## 
    ## Node number 2: 180107 observations,    complexity param=0.01778811
    ##   predicted class=0  expected loss=0.360941  P(node) =0.7643669
    ##     class counts: 115099 65008
    ##    probabilities: 0.639 0.361 
    ##   left son=4 (127355 obs) right son=5 (52752 obs)
    ##   Primary splits:
    ##       verification_status splits as  RLL, improve=2961.2500, (0 missing)
    ##       purpose             splits as  R-RRLRLLLLLLR, improve=1247.7670, (0 missing)
    ##       funded_amnt         < 28012.5 to the right, improve= 967.6973, (0 missing)
    ##       home_ownership      splits as  RRLL, improve= 140.4077, (0 missing)
    ##   Surrogate splits:
    ##       funded_amnt < 4987.5  to the right, agree=0.724, adj=0.057, (0 split)
    ## 
    ## Node number 3: 55522 observations
    ##   predicted class=1  expected loss=0.4050106  P(node) =0.2356331
    ##     class counts: 22487 33035
    ##    probabilities: 0.405 0.595 
    ## 
    ## Node number 4: 127355 observations
    ##   predicted class=0  expected loss=0.3025873  P(node) =0.5404895
    ##     class counts: 88819 38536
    ##    probabilities: 0.697 0.303 
    ## 
    ## Node number 5: 52752 observations,    complexity param=0.01778811
    ##   predicted class=1  expected loss=0.4981802  P(node) =0.2238774
    ##     class counts: 26280 26472
    ##    probabilities: 0.498 0.502 
    ##   left son=10 (9232 obs) right son=11 (43520 obs)
    ##   Primary splits:
    ##       funded_amnt    < 4987.5  to the left,  improve=727.7945, (0 missing)
    ##       purpose        splits as  R-RRLRLLLLLLL, improve=646.1673, (0 missing)
    ##       home_ownership splits as  -RRL, improve= 19.6824, (0 missing)
    ##   Surrogate splits:
    ##       purpose splits as  R-RRRRRLRLRLR, agree=0.829, adj=0.024, (0 split)
    ## 
    ## Node number 10: 9232 observations
    ##   predicted class=0  expected loss=0.3214905  P(node) =0.03918024
    ##     class counts:  6264  2968
    ##    probabilities: 0.679 0.321 
    ## 
    ## Node number 11: 43520 observations
    ##   predicted class=1  expected loss=0.4599265  P(node) =0.1846971
    ##     class counts: 20016 23504
    ##    probabilities: 0.460 0.540

``` r
#Accuracy
lctrain$HGCT_predict= predict(HG.CT, type= "class")
paste("Classification Tree Accuracy", round(mean(lctrain$highgrade == lctrain$HGCT),4), sep= "= ")
```

    ## Warning: Unknown or uninitialised column: `HGCT`.

    ## [1] "Classification Tree Accuracy= NaN"

By the accuracy metric, Logistic Regression is more predctive that
Classification Tree in the proportion of True Positives generated using
the 4 independent variables provided. The Logistic Regression model had
\~65% true positives, while the Classification Tree is marginally behind
with \~64% True Positive.

## V. Testing Mode

``` r
lctest<- read_csv("LoanStats3d_test.csv",skip = 1)
```

    ## Warning: One or more parsing issues, see `problems()` for details

    ## Rows: 421097 Columns: 111

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (25): id, term, int_rate, grade, sub_grade, emp_title, emp_length, home_...
    ## dbl (85): member_id, loan_amnt, funded_amnt, funded_amnt_inv, installment, a...
    ## lgl  (1): desc

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
lctest$highgrade <- as.numeric(lctest$grade == c("A","B"))
```

    ## Warning in lctest$grade == c("A", "B"): longer object length is not a multiple
    ## of shorter object length

``` r
lctest <- lctest[which(lctest$purpose != "educational"),]

#LOGIT
lctest$HGLR.predict <- as.numeric(predict(HG.LR, newdata = lctest ,type="response")>0.47)
paste("Logistic Regression Accuracy", mean(lctest$highgrade == lctest$HGLR.predict), sep ="= ")
```

    ## [1] "Logistic Regression Accuracy= 0.620374548200639"

``` r
#TREE
lctest$HGCT.predict<- predict(HG.CT, newdata = lctest,type="class")
paste("Classification Tree Accuracy",mean(lctest$highgrade == lctest$HGCT.predict), sep= "= ")
```

    ## [1] "Classification Tree Accuracy= 0.603808175846723"

``` r
#BENCHMARKS
paste("Coin-flipping benchmark", mean(lctest$highgrade == round(runif(nrow(lctest),0,1),0)), sep = "= ")
```

    ## [1] "Coin-flipping benchmark= 0.500080742067092"

``` r
paste("All-zero benchmark", mean(lctest$highgrade == rep(0,nrow(lctest))), sep = "= ")
```

    ## [1] "All-zero benchmark= 0.77371323267489"

## VI. Measuring Performance with Precision & Recall

``` r
#Logit
table(lctest$highgrade,lctest$HGLR.predict)
```

    ##    
    ##          0      1
    ##   0 211028 114778
    ##   1  45080  50208

``` r
paste("Precision", round(50229/(50229+114757),4),sep= "= ")
```

    ## [1] "Precision= 0.3044"

``` r
paste("Recall", round(50229/(50229+45114),4),sep= "= ")
```

    ## [1] "Recall= 0.5268"

``` r
#Tree
table(lctest$highgrade,lctest$HGCT.predict)
```

    ##    
    ##          0      1
    ##   0 201453 124353
    ##   1  42481  52807

``` r
paste("Precision", round(52806/(52806+124354),4),sep= "= ")
```

    ## [1] "Precision= 0.2981"

``` r
paste("Recall", round(52806/(52806+42537),4),sep= "= ")
```

    ## [1] "Recall= 0.5539"

The most important metric is Precision, which measures the proportion of
True Positives out of Total Predicted Positives. This is because the
cost of False Positives is high. Because by incorrectedly classifying
sub-prime borrowers as high-grade, the Lending Club could risk losing
interest payments and the principal amount. Therefore, a high Precision
rate is particularly important for a consumer finance company.
