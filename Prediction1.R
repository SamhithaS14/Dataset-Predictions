train <- HireTrainApr10 #put data into variable train

summary(train)

hired <- train[train$Hired == 'Yes',] #all those who were hired
notHired <- train[train$Hired == 'No',]

#queries as in Section 14 of Active Textbook

#1. 
table(train$College, train$Coding) 


#2.
table(train[(train$Coding == 'Weak') & (train$Major == 'CS'),]$Impression)


#3.
nrow(train[train$Major == 'IT' & train$Coding == 'Weak' & train$Hired == 'Yes',])/nrow(hired[hired$Hired == 'Yes',])
nrow(train[train$Major == 'DataScience' & train$Coding == 'Weak' & train$Hired == 'Yes',])/nrow(hired[hired$Hired == 'Yes',])


#4.
Prior<-nrow(train[train$Impression =='Confident',])/nrow(train)
Prior
PriorOdds<-round(Prior/(1-Prior),2)
PriorOdds
TruePositive<-round(nrow(train[train$Impression =='Confident'& train$Coding == 'OK',])/nrow(train[train$Impression =='Confident',]),2)
TruePositive
FalsePositive<-round(nrow(vote[train$Impression !='Confident'& train$Coding == 'OK',])/nrow(train[train$Impression !='Confident',]),2)
FalsePositive
LikelihoodRatio<-round(TruePositive/FalsePositive,2)
LikelihoodRatio
PosteriorOdds <-LikelihoodRatio * PriorOdds
PosteriorOdds
Posterior <-PosteriorOdds/(1+PosteriorOdds)
Posterior

#5.
Prior<-nrow(train[train$Hired =='Yes',])/nrow(train)
Prior
PriorOdds<-round(Prior/(1-Prior),2)
PriorOdds
TruePositive<-round(nrow(train[train$Hired =='Yes' & train$College == 'Redbrick',])/nrow(train[train$Hired =='Yes',]),2)
TruePositive
FalsePositive<-round(nrow(train[train$Hired !='Yes' & train$College == 'Redbrick',])/nrow(train[train$Hired !='Yes',]),2)
FalsePositive
LikelihoodRatio<-round(TruePositive/FalsePositive,2)
LikelihoodRatio
PosteriorOdds <-LikelihoodRatio * PriorOdds
PosteriorOdds
Posterior <-PosteriorOdds/(1+PosteriorOdds)
Posterior

#6.
Prior<-nrow(train[train$Hired =='No',])/nrow(vote)
Prior
PriorOdds<-round(Prior/(1-Prior),2)
PriorOdds
TruePositive<-round(nrow(train[train$Hired =='No'& train$Coding == 'Excellent',])/nrow(train[train$Hired =='No',]),2)
TruePositive
FalsePositive<-round(nrow(train[train$Hired !='No'& train$Coding == 'Excellent',])/nrow(train[train$Hired !='No',]),2)
FalsePositive
LikelihoodRatio<-round(TruePositive/FalsePositive,2)
LikelihoodRatio
PosteriorOdds <-LikelihoodRatio * PriorOdds
PosteriorOdds
Posterior <-PosteriorOdds/(1+PosteriorOdds)
Posterior


#tables
table(hired$Coding)
table(hired$Impression)
table(hired$Major)
table(hired$College)
table(notHired$Coding)
table(notHired$Impression)
table(notHired$Major)
table(notHired$College)

#create plots
colors<- c('red','blue','cyan','yellow','green')
barplot(table(hired$Coding),xlab="Hired",ylab="Coding",col=colors, 
        main="Barplot for Hires based on Coding",border="black")

barplot(table(train$Coding),xlab="Hired",ylab="Coding",col=colors, 
        main="Barplot for Hires based on Coding in total dataset",border="black")

barplot(table(hired$Impression),xlab="Hired",ylab="Impression",col=colors, 
        main="Barplot for Hires based on Impression",border="black")

barplot(table(train$Impression),xlab="Hired",ylab="Impression",col=colors, 
        main="Barplot for Hires based on Impression in total dataset",border="black")

barplot(table(hired$Major),xlab="Hired",ylab="Major",col=colors, 
        main="Barplot for Hires based on Major",border="black")

barplot(table(train$Major),xlab="Hired",ylab="Major",col=colors, 
        main="Barplot for Hires based on Major in total dataset",border="black")

barplot(table(hired$College),xlab="College",ylab="Hired",col=colors, 
        main="Barplot for Hires based on College",border="black")

barplot(table(train$College),xlab="College",ylab="Hired",col=colors, 
        main="Barplot for Hires based on College in total dataset",border="black")

#based on plots of whole data versus those hired only...impression and major make less of a 
#significant difference in hiring processes bc amount hired doesn't change by that much compared to
#coding and college for HIRED

#chi square to see relationship between 2 attributes 

data<-table(hired$College, hired$Hired)
chisq.test(data)

data1<-table(hired$Coding, hired$Hired)
chisq.test(data1)

#as bar plots also expected... in these 2 cases we fail to reject independence hypothesis since p < 0.05

data2<-table(hired$Major, hired$Hired)
chisq.test(data2)

data3<-table(hired$Impression, hired$Hired)
chisq.test(data3)

#as bar plots also expected... in these 2 cases we reject independence hypothesis since p > 0.05

#let's take a look at NOT HIRED

data4 <- table(notHired$Coding, notHired$Hired)
chisq.test(data4)

data5 <- table(notHired$College, notHired$Hired)
chisq.test(data5)

data7 <- table(notHired$Impression, notHired$Hired)
chisq.test(data7)

#as bar plots also expected... in these 3 cases we fail to reject independence hypothesis since p < 0.05

data6 <- table(notHired$Major, notHired$Hired)
chisq.test(data6)

#as bar plots also expected... in these 2 cases we reject independence hypothesis since p > 0.05



#take closer look at coding and college because it seems to affect hiring the most

#further tabling
table(hired[hired$Coding == 'Excellent',]$Impression)
table(hired[hired$Coding == 'Excellent',]$Major)
table(hired[hired$Coding == 'Excellent',]$College)

table(hired[hired$Coding == 'OK',]$Impression)
table(hired[hired$Coding == 'OK',]$Major)
table(hired[hired$Coding == 'OK',]$College)

table(hired[hired$Coding == 'Weak',]$Impression)
table(hired[hired$Coding == 'Weak',]$Major)
table(hired[hired$Coding == 'Weak',]$College)

table(hired[hired$College == 'Redbrick',]$Impression)
table(hired[hired$College == 'Redbrick',]$Major)
table(hired[hired$College == 'Redbrick',]$Coding) #excellent or ok redbricks mostly hired

table(hired[hired$College == 'BestCollege',]$Impression)
table(hired[hired$College == 'BestCollege',]$Major)
table(hired[hired$College == 'BestCollege',]$Coding) #excellent or ok bestcollegers mostly hired

table(hired[hired$College == 'Peters',]$Impression)
table(hired[hired$College == 'Peters',]$Major)
table(hired[hired$College == 'Peters',]$Coding) #excellent or ok peters mostly hired

table(hired[hired$College == 'BYU',]$Impression)
table(hired[hired$College == 'BYU',]$Major)
table(hired[hired$College == 'BYU',]$Coding) #excellent or ok byus mostly hired

table(hired[hired$College == 'PJIT',]$Impression)
table(hired[hired$College == 'PJIT',]$Major)
table(hired[hired$College == 'PJIT',]$Coding) #excellent or ok pjits mostly hired

#supports barplot that mostly its people with excellent or ok coding skills are hired

#more specific queries with colleges and coding --> looking for NONUNIFORM distributions...
#tried different queries and these gave me some anomalies

#looking for NONUNIFORM distributions
#just ran a bunch of subsets to see which would give anomalies, here are some I thought were significant
#did this with a lot of copy and pasting and replacing attributes to see more significant anomalies

table(hired[hired$College == 'Redbrick' & hired$Coding == 'Weak',]$Impression) #nerdy hired 
table(hired[hired$College == 'BestCollege' & hired$Coding == 'OK',]$Impression) #confident and outgoing hired

table(hired[hired$Major == 'DataScience' & hired$Coding == 'Weak',]$Impression) #nerdy hired more often
table(hired[hired$Major == 'IT' & hired$Coding == 'Weak',]$Impression) #nerdy hired more often
table(hired[hired$Major == 'Stats' & hired$Coding == 'Weak',]$Impression) #shy and nerdy the most
table(hired[hired$Major == 'CS' & hired$Coding == 'Weak',]$Impression) #nerdy hired more often
table(hired[hired$Major == 'CS' & hired$Coding == 'OK',]$Impression) #confident or nerdy hired more often

table(hired[hired$Impression == 'Nerdy' & hired$Coding == 'Weak',]$College) #redbrick anomaly ... weak coders

#let's look into it ...weak coding redbricks usually hired compared to others... here are some examples

table(hired[hired$Coding == 'Weak',]$College) #mainly redbricks hired w weak coding
  table(hired[hired$Major == 'DataScience' & hired$Coding == 'Weak',]$College) #not many hired
  table(hired[hired$Major == 'IT' & hired$Coding == 'Weak',]$College) #not many hired

table(hired[hired$Impression == 'Shy' & hired$Coding == 'OK',]$College) #bestcollege not hired, mostly redbricks hired

table(hired[hired$Impression == 'Confident' & hired$Coding == 'Weak',]$Major) #not many hired
table(hired[hired$Impression == 'Shy' & hired$Coding == 'Weak',]$Major) #not many hired

table(hired[hired$College == 'BestCollege' & hired$Impression == 'Shy',]) #shows for EVERY major!!!
table(hired[hired$Impression == 'Shy' & hired$Coding == 'Excellent' & hired$College == 'BestCollege'])

#overarching features...
table(hired[hired$Impression == 'Shy' & hired$Coding == 'OK' & hired$College == 'BestCollege',])
table(hired[hired$Coding == 'Weak',])
table(hired[hired$Impression == 'Nerdy' & hired$Coding == 'Weak' & hired$College != 'Redbrick',])


#extra pattern I found
#noticed even if i change the major, mainly (sometimes ONLY) those with excellent or ok coding skills hired from any college
#examples... (i changed around the majors for every college and ran these)

#better supports that weak coders were not usually hired !!!!

table(hired[hired$College == 'PJIT' & hired$Major == 'DataScience',]$Coding)
table(hired[hired$College == 'BYU' & hired$Major == 'CS',]$Coding)
table(hired[hired$College == 'Peters' & hired$Major == 'Stats',]$Coding)
table(hired[hired$College == 'Redbrick' & hired$Major == 'IT',]$Coding)
table(hired[hired$College == 'BestCollege' & hired$Major == 'DataScience',]$Coding)


#now confirm with with NOT HIRED

table(notHired[notHired$Coding == 'Excellent',]$Impression) #nerdy and outgoing not hired
table(notHired[notHired$Coding == 'Excellent',]$Major)
table(notHired[notHired$Coding == 'Excellent',]$College) #bestcollege not hired

table(notHired[notHired$Coding == 'OK',]$Impression) #shy not hired
table(notHired[notHired$Coding == 'OK',]$Major) 
table(notHired[notHired$Coding == 'OK',]$College) #bestcollege not hired

table(notHired[notHired$Coding == 'Weak',]$Impression) #big numbers for all of these categories where coding is weak
table(notHired[notHired$Coding == 'Weak',]$Major)
table(notHired[notHired$Coding == 'Weak',]$College)


#1 step cross validation

v<- sample(1:nrow(train))
v[1:5]
trainScrambled<-train[v, ]
trainSample <- trainScrambled[nrow(trainScrambled)-10:nrow(trainScrambled), ]
myprediction <- trainSample

decision <- rep('Yes', nrow(myprediction)) #initializing to yes, since there were many instances of hire...
#now will work with anomalies i found


#MY DECISION VECTORS!!!
#took my smaller tables and combined them to make broader categories based on shared attributes such as shy and ok coders

decision[myprediction$Impression == 'Shy' & myprediction$Coding == 'OK' & myprediction$College == 'BestCollege'] <- 'No'
#overarching point...can be divided into smaller sections
decision[myprediction$Coding == 'Weak'] <- 'No'
decision[myprediction$Impression == 'Nerdy' & myprediction$Coding == 'Weak' & myprediction$College != 'Redbrick'] <- 'No'

myprediction$Hired <- decision
error <- mean(trainSample$Hired != myprediction$Hired)
error

#MAKING MY SUBMISSION FILE

myprediction$Hired <- decision
error <- mean(trainSample$Hired != myprediction$Hired)
error

test <- test_challenge1
submission <- sample_submission_challenge1

myprediction <- test
decision <- rep('Yes', nrow(myprediction))

#MY DECISION VECTORS from BEFORE
decision[myprediction$Impression == 'Shy' & myprediction$Coding == 'OK' & myprediction$College == 'BestCollege'] <- 'No'
decision[myprediction$Coding == 'Weak'] <- 'No'
decision[myprediction$Impression == 'Nerdy' & myprediction$Coding == 'Weak' & myprediction$College != 'Redbrick'] <- 'No'

#into submission file
submission$Prediction <- decision
write.csv(submission, 'submission.csv', row.names = FALSE)
submission
