install.packages("rpart")

library(rpart)

hire_df <- HireRTrain1.1

library(rpart.plot)

install.packages("devtools") 
devtools::install_github("devanshagr/CrossValidation")


table(hire_df[hire_df$Hired == 'Yes',]$Coding)
table(hire_df[hire_df$Hired == 'Yes',]$Impression)
table(hire_df[hire_df$Hired == 'Yes',]$Major)
table(hire_df[hire_df$Hired == 'Yes',]$College)

#make into histograms for easier visualization of breaks and frequency of hires for "lower" vs "higher" number of followers/following
hist(table(hire_df[hire_df$Hired == 'Yes',]$TwitterFOLLOWERS), breaks = 2)
hist(table(hire_df[hire_df$Hired == 'Yes',]$TwitterFOLLOWING), breaks = 2)
hist(table(hire_df[hire_df$Hired == 'Yes',]$TikTokFOLLOWERS), breaks = 2)
hist(table(hire_df[hire_df$Hired == 'Yes',]$TikTokTFOLLOWING), breaks = 2)

tree<- rpart(Hired ~ Coding+Impression+Major+College+TwitterFOLLOWERS+TwitterFOLLOWING+TikTokFOLLOWERS+TikTokTFOLLOWING, 
             data = hire_df,method = "class")

rpart.plot(tree)

pred <- predict(tree, hire_df, type="class")
head(pred)

#training accuracy
mean(hire_df$Hired==pred)

CrossValidation::cross_validate(hire_df,tree,5,0.7)

#noticed tiktok followers seems to impact the hiring decisions

#### looking at split now


tree <- rpart(Hired ~ Coding+Impression+Major+College+TwitterFOLLOWERS+TwitterFOLLOWING+TikTokFOLLOWERS+TikTokTFOLLOWING, 
             data = hire_df,method = "class", control=rpart.control(minsplit = 200))

rpart.plot(tree, extra = 2)

pred <- predict(tree, hire_df, type="class")
head(pred)

#training accuracy
mean(hire_df$Hired==pred)

CrossValidation::cross_validate(hire_df,tree,5,0.7)

#noticed not as many tiktok followers and coding is not weak are more often hired

#variance lower than for first tree, but values in accuracy_subset not as close to training accuracy

####

tree <- rpart(Hired ~ Coding+Impression+Major+College+TwitterFOLLOWERS+TwitterFOLLOWING+TikTokFOLLOWERS+TikTokTFOLLOWING, 
               data = hire_df,method = "class", control=rpart.control(minsplit = 100))

rpart.plot(tree, extra = 2)

pred <- predict(tree, hire_df, type="class")
head(pred)

#training accuracy
mean(hire_df$Hired==pred)

CrossValidation::cross_validate(hire_df,tree,5,0.7)


#noticed twitter followers this time, the less twitter followers the more people seem to be hired
#training accuracy is higher this time at 86, but accuracy subset not that close to training

####

tree <- rpart(Hired ~ Coding+Impression+Major+College+TwitterFOLLOWERS+TwitterFOLLOWING+TikTokFOLLOWERS+TikTokTFOLLOWING, 
               data = hire_df,method = "class", control=rpart.control(minsplit = 50))

rpart.plot(tree, extra = 2)

pred <- predict(tree, hire_df, type="class")
head(pred)

#training accuracy
mean(hire_df$Hired==pred)

CrossValidation::cross_validate(hire_df,tree,5,0.7)


#noticed with "low" followers not many are hired, and with too many followings not many are hired
#noticed with weak coding skills and less followers not hired, but with weak coding skills and more followers hired
#noticed less followers and great coding skills you're hired (intuition... and verified by tree)


#looking pretty good, as cross-validation accuracies for the tree that was passed 
  #(accuracy_subset) are fairly high and close to our training accuracy
#values in accuracy_subset are reasonably close to each other


#### looking at buckets now

tree <- rpart(Hired ~ Coding+Impression+Major+College+TwitterFOLLOWERS+TwitterFOLLOWING+TikTokFOLLOWERS+TikTokTFOLLOWING, 
               data = hire_df,method = "class", control=rpart.control(minbucket = 50))

rpart.plot(tree, extra = 2)

pred <- predict(tree, hire_df, type="class")
head(pred)

#training accuracy
mean(hire_df$Hired==pred)

CrossValidation::cross_validate(hire_df,tree,5,0.7)

#definetely a higher variance here, and training accuracy is lower than it was for some other trees I tested

####

tree <- rpart(Hired ~ Coding+Impression+Major+College+TwitterFOLLOWERS+TwitterFOLLOWING+TikTokFOLLOWERS+TikTokTFOLLOWING, 
              data = hire_df,method = "class", control=rpart.control(minbucket = 100))

rpart.plot(tree, extra = 2)

pred <- predict(tree, hire_df, type="class")
head(pred)

#training accuracy
mean(hire_df$Hired==pred)

CrossValidation::cross_validate(hire_df,tree,5,0.7)

#very low training accuracy compared to past trees -- SIZE of tree shrunk, there's only one split now

####
tree <- rpart(Hired ~ Coding+Impression+Major+College+TwitterFOLLOWERS+TwitterFOLLOWING+TikTokFOLLOWERS+TikTokTFOLLOWING, 
              data = hire_df,method = "class", control=rpart.control(minbucket = 50))

rpart.plot(tree, extra = 2)

pred <- predict(tree, hire_df, type="class")
head(pred)

#training accuracy
mean(hire_df$Hired==pred)

CrossValidation::cross_validate(hire_df,tree,5,0.7)

#prediction accuracy is at its average...it seems to be a lot of the trees yielding training accuracy of about 84%
#but subset accuracy isn't really that great, and subset accuracy does seem to vary from training by a bit
#size of the tree grew since the minimum decreased

####
tree <- rpart(Hired ~ Coding+Impression+Major+College+TwitterFOLLOWERS+TwitterFOLLOWING+TikTokFOLLOWERS+TikTokTFOLLOWING, 
              data = hire_df,method = "class", control=rpart.control(minbucket = 1))

rpart.plot(tree, extra = 2)

pred <- predict(tree, hire_df, type="class")
head(pred)

#training accuracy
mean(hire_df$Hired==pred)

CrossValidation::cross_validate(hire_df,tree,5,0.7)

#seems to be showing the highest training accuracy so far...along with very low variances
#the accuracy_subset is pretty close to the training accuracy as well

#### now try with cp control

tree <- rpart(Hired ~ Coding+Impression+Major+College+TwitterFOLLOWERS+TwitterFOLLOWING+TikTokFOLLOWERS+TikTokTFOLLOWING, 
              data = hire_df,method = "class", control=rpart.control(cp = 0.05))

rpart.plot(tree)

pred <- predict(tree, hire_df, type="class")
head(pred)

#training accuracy
mean(hire_df$Hired==pred)

CrossValidation::cross_validate(hire_df,tree,5,0.7)

#lower accuracy compared to others
#telling us that tiktokfollower amount is a DEFINING SPLIT

####
tree <- rpart(Hired ~ Coding+Impression+Major+College+TwitterFOLLOWERS+TwitterFOLLOWING+TikTokFOLLOWERS+TikTokTFOLLOWING, 
              data = hire_df,method = "class", control=rpart.control(cp = 0.005))

rpart.plot(tree)

pred <- predict(tree, hire_df, type="class")
head(pred)

#training accuracy
mean(hire_df$Hired==pred)

CrossValidation::cross_validate(hire_df,tree,5,0.7)

#looks like training is high but average falls still at low eighties
#training consistently at .899



#### ADDING ANOTHER VECTOR...

#i commented this out because i decided not to use this tree as my final, but
#if i run this, it adds a vector called combined to the dataset...but it seemed the error would be the same no matter 
#what I changed the other attribute (besides TTFollowers) to be

#hire_dfTest <- HireRTrain1.1

#colnames(hire_dfTest)
#hire_df$combined<-hire_df$TwitterFOLLOWERS  + 50*hire_df$TikTokFOLLOWERS
#hire_dfTest$combined<-hire_df$TwitterFOLLOWERS  + 50*hire_df$TikTokFOLLOWERS

# adding weight to twitter followers because from exploration it seems to have more importance in hiring decisions

#colnames(hire_df)
#tree<-rpart(Hired~., data = hire_df,method = "class")
#tree
#rpart.plot(tree)
#pred <- predict(tree, newdata=hire_dfTest, type="class") 
#error <- mean(hire_dfTest$Hired!= pred) 
#error

#error is not improving with this new decision vector, and is usually around 13%, in this case the combined decision
#vector is helpful to visualize the tree, but the error rate is still pretty high


#### COMBINING MODELS separated by tiktok followers which seems to be main distinction based on previous exploration
#so used that as the splitting vector in my matrix

hire_dfTest <- HireRTrain1.1

#making separate
model1<-rpart(Hired~., data=hire_df[hire_df$TikTokFOLLOWERS<4710,], control=rpart.control(cp = 0.005)); 
model2<-rpart(Hired~., data=hire_df[hire_df$TikTokFOLLOWERS>=4710,], control=rpart.control(cp = 0.005)); 

#i tried this with 2 of the controls that yielded best accuracy in my above explorations...minsplit = 50 and cp = 0.005...
#in the end the cp control gave me a smaller error
model1 
rpart.plot(model1)
model2 
rpart.plot(model2)

pred1 <- predict(model1, newdata=hire_dfTest[hire_dfTest$TikTokFOLLOWERS<4710,], type="class") 
pred2 <- predict(model2, newdata=hire_dfTest[hire_dfTest$TikTokFOLLOWERS>=4710,], type="class") 

myprediction<-hire_dfTest #my prediction is a copy of my dataset
#subset(myprediction, select =- c(1:4, 6:9))

decision <- rep('Yes',nrow(myprediction)) #decision vector
decision[myprediction$TikTokFOLLOWERS<4710] <- as.character(pred1) #fill in vector based on rpart model
decision[myprediction$TikTokFOLLOWERS>=4710] <-as.character(pred2)  #fill in vector based on rpart model

myprediction$Hired <-decision  #making changes based on decision vector
#subset(myprediction, select =- c(1:4, 6:9))

error <- mean(hire_df$Hired!= myprediction$Hired) 
error

#error coming out to 0.095 consistently meaning this decision vector is a decent indicator of what allows someone to be hired
#tiktok followers are an anomaly and should be considered

#### now apply to actual testing data set and making submission file
#making separate trees

test<- test_challenge2
submission <- sample_submission2

#using control as well because with my cross validation this yielded a
model1<-rpart(Hired~., data=hire_df[hire_df$TikTokFOLLOWERS<4710,], control=rpart.control(cp = 0.005)); 
model2<-rpart(Hired~., data=hire_df[hire_df$TikTokFOLLOWERS>=4710,], control=rpart.control(cp = 0.005));
model1 
rpart.plot(model1)
model2 
rpart.plot(model2)

pred1 <- predict(model1, newdata=test[test$TikTokFOLLOWERS<4710,], type="class") 
pred2 <- predict(model2, newdata=test[test$TikTokFOLLOWERS>=4710,], type="class") 

myprediction<-test #my prediction is a copy of my dataset
#subset(myprediction, select =- c(1:4, 6:9))

decision <- rep('Yes',nrow(myprediction)) #decision vector
decision[myprediction$TikTokFOLLOWERS<4710] <- as.character(pred1) #fill in vector based on rpart model
decision[myprediction$TikTokFOLLOWERS>=4710] <-as.character(pred2)  #fill in vector based on rpart model

submission$Prediction<- decision #instead of making changes to existing vector we add changes to submission file now
submission

write.csv(submission, 'submission2.csv', row.names = FALSE)
submission