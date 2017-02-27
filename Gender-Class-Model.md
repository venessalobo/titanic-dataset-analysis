    #Set Working Directory
    setwd("C:/Users/vanlo/Google Drive/Documents/Projects/Kaggle/Titanic DataSet")

    #Import DataSets
    train <- read.csv("train.csv")
    test <- read.csv("test.csv")

    #Analysis of dataframe structure
    str(train)

    ## 'data.frame':    891 obs. of  12 variables:
    ##  $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
    ##  $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
    ##  $ Name       : Factor w/ 891 levels "Abbing, Mr. Anthony",..: 109 191 358 277 16 559 520 629 417 581 ...
    ##  $ Sex        : Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...
    ##  $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
    ##  $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
    ##  $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
    ##  $ Ticket     : Factor w/ 681 levels "110152","110413",..: 524 597 670 50 473 276 86 396 345 133 ...
    ##  $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
    ##  $ Cabin      : Factor w/ 148 levels "","A10","A14",..: 1 83 1 57 1 1 131 1 1 1 ...
    ##  $ Embarked   : Factor w/ 4 levels "","C","Q","S": 4 2 4 4 4 3 4 4 4 2 ...

    #Convert Sex to factor
    train$Sex <- as.factor(train$Sex)
    str(train$Sex)

    ##  Factor w/ 2 levels "female","male": 2 1 1 1 2 2 2 2 1 1 ...

    #Understand gender distribution
    summary(train$Sex)

    ## female   male 
    ##    314    577

    prop.table(table(train$Sex, train$Survived))

    ##         
    ##                   0          1
    ##   female 0.09090909 0.26150393
    ##   male   0.52525253 0.12233446

    # Calculate proportions row-wise (Sex)
    prop.table(table(train$Sex, train$Survived),1)

    ##         
    ##                  0         1
    ##   female 0.2579618 0.7420382
    ##   male   0.8110919 0.1889081

    #Set new column in test dataset to predict survival
    test$Survived <- 0 

    #If female passenger, set to 1 i.e. Survived as females where allowed on life boats first
    test$Survived [test$Sex == 'female'] <- 1

    #Consider Age variable to identify children
    summary(train$Age)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    0.42   20.12   28.00   29.70   38.00   80.00     177

    train$Child <- 0
    train$Child[train$Age < 18 ] <- 1

    # Identify passengers with Age as NA. 
    which (is.na(train$Age))

    ##   [1]   6  18  20  27  29  30  32  33  37  43  46  47  48  49  56  65  66
    ##  [18]  77  78  83  88  96 102 108 110 122 127 129 141 155 159 160 167 169
    ##  [35] 177 181 182 186 187 197 199 202 215 224 230 236 241 242 251 257 261
    ##  [52] 265 271 275 278 285 296 299 301 302 304 305 307 325 331 335 336 348
    ##  [69] 352 355 359 360 365 368 369 376 385 389 410 411 412 414 416 421 426
    ##  [86] 429 432 445 452 455 458 460 465 467 469 471 476 482 486 491 496 498
    ## [103] 503 508 512 518 523 525 528 532 534 539 548 553 558 561 564 565 569
    ## [120] 574 579 585 590 594 597 599 602 603 612 613 614 630 634 640 644 649
    ## [137] 651 654 657 668 670 675 681 693 698 710 712 719 728 733 739 740 741
    ## [154] 761 767 769 774 777 779 784 791 793 794 816 826 827 829 833 838 840
    ## [171] 847 850 860 864 869 879 889

Assume the passengers with NA are adults in further analysis.

    # Analyze the survival rate based on gender of children
    aggregate(Survived ~ Child + Sex, data = train, FUN = sum)

    ##   Child    Sex Survived
    ## 1     0 female      195
    ## 2     1 female       38
    ## 3     0   male       86
    ## 4     1   male       23

    aggregate(Survived ~ Child + Sex, data = train, FUN = length)

    ##   Child    Sex Survived
    ## 1     0 female      259
    ## 2     1 female       55
    ## 3     0   male      519
    ## 4     1   male       58

    aggregate(Survived ~ Child + Sex, data = train, FUN = function(x){sum(x)/length(x)})

    ##   Child    Sex  Survived
    ## 1     0 female 0.7528958
    ## 2     1 female 0.6909091
    ## 3     0   male 0.1657033
    ## 4     1   male 0.3965517

Age analysis again indicates a higher survival rate for females.

    # Group Fares into categories
    train$Fare2 <- '30+'
    train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
    train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'

    #Analyze survival rate based on Fare, Class and Sex
    aggregate(Survived ~ Fare2 + Pclass + Sex, data = train, FUN=function(x) sum(x)/length(x))

    ##    Fare2 Pclass    Sex  Survived
    ## 1  20-30      1 female 0.8333333
    ## 2    30+      1 female 0.9772727
    ## 3  10-20      2 female 0.9142857
    ## 4  20-30      2 female 0.9000000
    ## 5    30+      2 female 1.0000000
    ## 6  10-20      3 female 0.5813953
    ## 7  20-30      3 female 0.3333333
    ## 8    30+      3 female 0.5000000
    ## 9  20-30      1   male 0.4000000
    ## 10   30+      1   male 0.3586957
    ## 11 10-20      2   male 0.1587302
    ## 12 20-30      2   male 0.1600000
    ## 13   30+      2   male 0.1500000
    ## 14 10-20      3   male 0.2368421
    ## 15 20-30      3   male 0.1250000
    ## 16   30+      3   male 0.1228070

Anomaly observed with lower survival rate among females in Pclass = 3 &
Fare &gt; 20

    # Update prediction on test data
    test$Survived <- 0
    test$Survived[test$Sex == 'female'] <- 1
    test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >=20] <- 0

    # Submission file
    submit <- data.frame(PassengerID = test$PassengerId, Survived = test$Survived)
    write.csv(submit, file = "Gender-Class-Model.csv", row.names = FALSE)
