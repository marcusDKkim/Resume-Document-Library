#Appendix A: 
length(which(catData$CatOwned==0)) 
# == 2410 households without cats
length(which(catData$CatOwned==1)) 
# == 775 households with cat(s)
table(catData$CatOwned)

#Data Preparation
#--Appendix F: Exclude observations with null values
catData <- na.exclude(catData)
#--Appendix G: Transform into dummy variable
catData$CatOwned <- ifelse(catData$Cats>=1,1,0)
#--Appendix H: Transform numerical value FEdu and MEdu
catData$AvgEdu <- round((catData$MEdu + catData$FEdu)/2)
#--Appendix I: Subset data
catDataSub<- catData[,c("CatOwned","AvgEdu","HHInc")]

#Data Visualization
#Visualize the Number of Households at a given Number of Pets
Cat_counts <- table(catData$Cats) 
barplot(Cat_counts, main= "Number of Households with Number of Pets?",
        xlab="Number of Pets",
        ylab="Number of Households",
        names.org=c("No","Yes"))
#Visualize the Number of Households with and Without a Cat
ownedCount <- table(catData$CatOwned)
barplot(ownedCount, main="Number of Households
that do not (0) or do (1) own a Cat",
        xlab="Ownership",
        ylab="Number of Households",
        names.org=c("Yes","No"))
#Visualize the Number of Households at each Income Level
incomeCount <- table(catData$HHInc)
barplot(incomeCount, main="Number of Households at an Income Level",
        xlab="Income",
        ylab="Frequency",
        names.org=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14"))
table(catData$HHInc)

#Visualize the Number of Households at the Average Education Level 
summary(catData$AvgEdu)
eduCount <- table(catData$AvgEdu)
barplot(eduCount, main="Number of Households at an Average Education Level",
        xlab="Education Level",
        ylab="Frequency",
        names.org=c("1","2","3","4","5","6","7","8","9","10","11"))
table(catData$AvgEdu)
#Visualize the Number of Households vs. Income Level with/without a Cat
catsIncome <- table(catData$CatOwned, catData$HHInc)
barplot(catsIncome, beside=TRUE, main="Number of Households 
vs. 
        Income Level with/without Cat", 
        xlab="Income Level + with/without Cat",
        ylab="Frequency",
        legend.text=c("0","1"),
        names.org=c("1","2","3","4","5","6","7","8","9","10","11"))
table(catData$CatOwned, catData$HHInc)
inc1perc <- length(which(catData$CatOwned==1 & catData$HHInc==1))/length(which(catData$HHInc==1))
inc2perc <- length(which(catData$CatOwned==1 & catData$HHInc==2))/length(which(catData$HHInc==2))
inc3perc <- length(which(catData$CatOwned==1 & catData$HHInc==3))/length(which(catData$HHInc==3))
inc4perc <- length(which(catData$CatOwned==1 & catData$HHInc==4))/length(which(catData$HHInc==4))
inc5perc <- length(which(catData$CatOwned==1 & catData$HHInc==5))/length(which(catData$HHInc==5))
inc6perc <- length(which(catData$CatOwned==1 & catData$HHInc==6))/length(which(catData$HHInc==6))
inc7perc <- length(which(catData$CatOwned==1 & catData$HHInc==7))/length(which(catData$HHInc==7))
inc8perc <- length(which(catData$CatOwned==1 & catData$HHInc==8))/length(which(catData$HHInc==8))
inc9perc <- length(which(catData$CatOwned==1 & catData$HHInc==9))/length(which(catData$HHInc==9))
inc10perc <- length(which(catData$CatOwned==1 & catData$HHInc==10))/length(which(catData$HHInc==10))
inc11perc <- length(which(catData$CatOwned==1 & catData$HHInc==11))/length(which(catData$HHInc==11))
inc12perc <- length(which(catData$CatOwned==1 & catData$HHInc==12))/length(which(catData$HHInc==12))
inc13perc <- length(which(catData$CatOwned==1 & catData$HHInc==13))/length(which(catData$HHInc==13))
inc14perc <- length(which(catData$CatOwned==1 & catData$HHInc==14))/length(which(catData$HHInc==14))
inc1perc #12.87%
inc2perc #17.69%
inc3perc #22.06%
inc4perc #22.75%
inc5perc #26.06%
inc6perc #22.06%
inc7perc #30.18%
inc8perc #26.38%
inc9perc #27.27%
inc10perc #30.46%
inc11perc #27.92%
inc12perc #17.33%
inc13perc #29.03%
inc14perc #27.27%
#Visualize the Number of Households with/without a Cat at a given Average Education Level
catsEdu <- table(catData$CatOwned, catData$AvgEdu)
barplot(catsEdu, beside=TRUE, main="Number of Households
vs. 
        Average Education Level with/without Cat", 
        xlab="Average Education Level with/without Cat", 
        ylab="Frequency",
        legend.text=c("0","1"),
        names.org=c("1","2","3","4","5","6","7","8","9","10","11"))
table(catData$CatOwned, catData$AvgEdu)
edu1perc <- length(which(catData$CatOwned==1 & catData$AvgEdu==1))/length(which(catData$AvgEdu==1))
edu2perc <- length(which(catData$CatOwned==1 & catData$AvgEdu==2))/length(which(catData$AvgEdu==2))
edu3perc <- length(which(catData$CatOwned==1 & catData$AvgEdu==3))/length(which(catData$AvgEdu==3))
edu4perc <- length(which(catData$CatOwned==1 & catData$AvgEdu==4))/length(which(catData$AvgEdu==4))
edu5perc <- length(which(catData$CatOwned==1 & catData$AvgEdu==5))/length(which(catData$AvgEdu==5))
edu6perc <- length(which(catData$CatOwned==1 & catData$AvgEdu==6))/length(which(catData$AvgEdu==6))
edu7perc <- length(which(catData$CatOwned==1 & catData$AvgEdu==7))/length(which(catData$AvgEdu==7))
edu8perc <- length(which(catData$CatOwned==1 & catData$AvgEdu==8))/length(which(catData$AvgEdu==8))
edu9perc <- length(which(catData$CatOwned==1 & catData$AvgEdu==9))/length(which(catData$AvgEdu==9))
edu10perc <- length(which(catData$CatOwned==1 & catData$AvgEdu==10))/length(which(catData$AvgEdu==10))
edu11perc <- length(which(catData$CatOwned==1 & catData$AvgEdu==11))/length(which(catData$AvgEdu==11))
edu1perc #14.29%
edu2perc #15.63%
edu3perc #21.93%
edu4perc #21.89%
edu5perc #25%
edu6perc #25.84%
edu7perc #29.23%
edu8perc #28.90%
edu9perc #22.04%
edu10perc #23.18%
edu11perc #36.96%

#Modeling
#--Appendix J: Logistic Regression
set.seed(1) # Set the random seed so that everybody gets the same data partitioning results
myIndex<- createDataPartition(catDataSub$CatOwned, p=0.6, list=FALSE) # Randomly partition the data into training and validation sets
trainSet <- catDataSub[myIndex,] 
validationSet <- catDataSub[-myIndex,]

trainSet$CatOwned <- as.factor(trainSet$CatOwned)  # This is to make sure that R sees the response variable as binary
 
#--Appendix K
logisticModel <- glm(CatOwned~AvgEdu+HHInc,
                     family=binomial,data=trainSet)
summary(logisticModel)
#--Appendix L
trainSet$pHat <- logisticModel$fitted.values

#--Appendix M
averageP <- mean(trainSet$pHat[trainSet$CatOwned==1])
averageP # ==0.24 or 24%

trainSet$PotCustomer <- ifelse(trainSet$pHat >= 0.24, 1,0)

#Cutoff value = 24.72% or 0.2472 --calculated by finding the model's 
#mean predicted probability for those who already own a cat 
#PotCustomer is the equivalent to y-hat

#--Appendix N: Confusion matrix at 25% cutoff value 
trainSet$TP <- ifelse(trainSet$CatOwned == 1 & trainSet$PotCustomer == 1, 1, 0)
trainSet$TN <- ifelse(trainSet$CatOwned == 0 & trainSet$PotCustomer == 0, 1, 0)
trainSet$FP <- ifelse(trainSet$CatOwned == 0 & trainSet$PotCustomer == 1, 1, 0)
trainSet$FN <- ifelse(trainSet$CatOwned == 1 & trainSet$PotCustomer == 0, 1, 0)
sum(trainSet$TP) # ==241
sum(trainSet$TN) # ==745
sum(trainSet$FP) # ==705
sum(trainSet$FN) # ==220

Accuracy <- (sum(trainSet$TP)+sum(trainSet$TN))/(sum(trainSet$TP)+sum(trainSet$TN)+sum(trainSet$FP)+sum(trainSet$FN))
Accuracy # == 51.59%
Sensitivity <- sum(trainSet$TP) / (sum(trainSet$TP) + sum(trainSet$FN))
Sensitivity # == 52.28%
Specificity <- sum(trainSet$TN) / (sum(trainSet$TN) + sum(trainSet$FP))
Specificity # == 51.38%

#--Appendix O: Confusion Matrix and Measures at 15% cutoff value 
trainSet$PotCustomer15 <- ifelse(trainSet$pHat>=0.15,1,0)

trainSet$TP15 <- ifelse(trainSet$CatOwned == 1 & trainSet$PotCustomer15 == 1, 1, 0)
trainSet$TN15 <- ifelse(trainSet$CatOwned == 0 & trainSet$PotCustomer15 == 0, 1, 0)
trainSet$FP15 <- ifelse(trainSet$CatOwned == 0 & trainSet$PotCustomer15 == 1, 1, 0)
trainSet$FN15 <- ifelse(trainSet$CatOwned == 1 & trainSet$PotCustomer15 == 0, 1, 0)
sum(trainSet$TP15) # ==465
sum(trainSet$TN15) # ==0
sum(trainSet$FP15) # ==1446
sum(trainSet$FN15) # ==0

Accuracy15 <- (sum(trainSet$TP15)+sum(trainSet$TN15))/(sum(trainSet$TP15)+sum(trainSet$TN15)+sum(trainSet$FP15)+sum(trainSet$FN15))
Accuracy15 # == 24.33%
Sensitivity15 <- sum(trainSet$TP15) / (sum(trainSet$TP15) + sum(trainSet$FN15))
Sensitivity15 # == 100%
Specificity15 <- sum(trainSet$TN15) / (sum(trainSet$TN15) + sum(trainSet$FP15))
Specificity15 # == 0%

#--Appendix P: Confusion Matrix and Measures at 20% cutoff value 
trainSet$PotCustomer20 <- ifelse(trainSet$pHat>=0.20,1,0)

trainSet$TP20 <- ifelse(trainSet$CatOwned == 1 & trainSet$PotCustomer20 == 1, 1, 0)
trainSet$TN20 <- ifelse(trainSet$CatOwned == 0 & trainSet$PotCustomer20 == 0, 1, 0)
trainSet$FP20 <- ifelse(trainSet$CatOwned == 0 & trainSet$PotCustomer20 == 1, 1, 0)
trainSet$FN20 <- ifelse(trainSet$CatOwned == 1 & trainSet$PotCustomer20 == 0, 1, 0)
sum(trainSet$TP20) # ==443
sum(trainSet$TN20) # ==148
sum(trainSet$FP20) # ==1298 
sum(trainSet$FN20) # ==22

Accuracy20 <- (sum(trainSet$TP20)+sum(trainSet$TN20))/(sum(trainSet$TP20)+sum(trainSet$TN20)+sum(trainSet$FP20)+sum(trainSet$FN20))
Accuracy20 # == 30.93%
Sensitivity20 <- sum(trainSet$TP20) / (sum(trainSet$TP20) + sum(trainSet$FN20))
Sensitivity20 # == 95.27%
Specificity20 <- sum(trainSet$TN20) / (sum(trainSet$TN20) + sum(trainSet$FP20))
Specificity20 # == 10.24%

#--Appendix N: Confusion Matrix and Measures at 30% cutoff value 
trainSet$PotCustomer30 <- ifelse(trainSet$pHat>=0.30,1,0)

trainSet$TP30 <- ifelse(trainSet$CatOwned == 1 & trainSet$PotCustomer30 == 1, 1, 0)
trainSet$TN30 <- ifelse(trainSet$CatOwned == 0 & trainSet$PotCustomer30 == 0, 1, 0)
trainSet$FP30 <- ifelse(trainSet$CatOwned == 0 & trainSet$PotCustomer30 == 1, 1, 0)
trainSet$FN30 <- ifelse(trainSet$CatOwned == 1 & trainSet$PotCustomer30 == 0, 1, 0)
sum(trainSet$TP30) # ==26.   --Predicted to own cat, do have cart
sum(trainSet$TN30) # ==1382  --Predicted to not have cat, do not have cat
sum(trainSet$FP30) # ==64    --Predicted to get cat, do not have cat 
sum(trainSet$FN30) # ==439.  --Predicted to not have cat, do have cat 


Accuracy30 <- (sum(trainSet$TP30)+sum(trainSet$TN30))/(sum(trainSet$TP30)+sum(trainSet$TN30)+sum(trainSet$FP30)+sum(trainSet$FN30))
Accuracy30 # == 73.68%
Sensitivity30 <- sum(trainSet$TP30) / (sum(trainSet$TP30) + sum(trainSet$FN30))
Sensitivity30 # == 5.59% --Indicates that high percentage of people that should not own a cat, but do, which is bad 
Specificity30 <- sum(trainSet$TN30) / (sum(trainSet$TN30) + sum(trainSet$FP30))
Specificity30 # == 95.57% --Indicates that high percentage of people correctly identified as not owning cat, smallp % of people that would not be marketed to 

validationSet$CatOwned <- as.factor(validationSet$CatOwned)
validationSet$pHat <- predict(logisticModel, validationSet, type = "response")
validationSet$yHat <- ifelse(validationSet$pHat >= 0.30, 1,0)   # Note that the cutoff value could be different from 0.5

validationSet$TP <- ifelse(validationSet$CatOwned == 1 & validationSet$yHat == 1, 1, 0)
validationSet$TN <- ifelse(validationSet$CatOwned == 0 & validationSet$yHat == 0, 1, 0)
validationSet$FP <- ifelse(validationSet$CatOwned == 0 & validationSet$yHat == 1, 1, 0)
validationSet$FN <- ifelse(validationSet$CatOwned == 1 & validationSet$yHat == 0, 1, 0)
sum(validationSet$TP) 
sum(validationSet$TN)
sum(validationSet$FP)
sum(validationSet$FN)

AccuracyV <- (sum(validationSet$TP)+sum(validationSet$TN))/(sum(validationSet$TP)+sum(validationSet$TN)+sum(validationSet$FP)+sum(validationSet$FN))
AccuracyV # == 72.76%
SensitivityV <- sum(validationSet$TP) / (sum(validationSet$TP) + sum(validationSet$FN))
SensitivityV # == 4.84% --Indicates that high percentage of people that should not own a cat, but do, which is bad 
SpecificityV <- sum(validationSet$TN) / (sum(validationSet$TN) + sum(validationSet$FP))
SpecificityV # ==94.61% 

