library(sjPlot)
library(sjmisc)
library(ggplot2)
a<-read.csv("~/psych_data/final_variables/L7/L7post_18-19_ACC.csv")
b<-read.csv("~/psych_data/final_variables/L7/L7pre_18-19_ACC.csv")
c<-read.csv("~/psych_data/subjectID_gender.csv")
t <- matrix(data = NA, ncol = 5, nrow = 0)
colnames(t) <- c("ID", "postEasy","postHard","preEasy","preHard")
for (i in 1:nrow(a)){
  for (j in 1:nrow(b)){
    if (a[i,2] == b[j,2]){
      temp<- matrix(data = NA, ncol = 5, nrow =1)
      temp[1,1]<-a[i,2]
      temp[1,2]<-a[i,24]
      temp[1,3]<-a[i,25]
      temp[1,4]<-b[j,24]
      temp[1,5]<-b[j,25]
      t <- rbind(t,temp)
    }
  }
}
postEasy<-t[,2]
postHard<-t[,3]
preEasy<-t[,4]
preHard<-t[,5]
# summary(t)
q <- matrix(data = NA, ncol = 1, nrow = 0)
colnames(q) <- c("gender")
for (i in 1:nrow(t)){
  for (j in 1:nrow(c)){
    if (t[i,1] == c[j,1]){
      temp<- matrix(data = NA, ncol = 1, nrow =1)
      temp[1,1]<-c[j,2]
      q <- rbind(q,temp)
    }
  }
}
t <- cbind(t,q)
t<- na.omit(t)
new_format <- matrix(data=NA, ncol=5, nrow = 0)
colnames(new_format) <- c("ID", "time", "difficulty", "gender", "ACC")

for (x in 1:nrow(t)){
  temp<- matrix(data = NA, ncol = 5, nrow = 4)
  temp[1,1]<-t[x,1] #subject ID
  temp[1,2]<-"Post"
  temp[1,3]<-"Easy"
  temp[1,4]<-t[x,6] #gender
  temp[1,5]<-t[x,2] #post Easy ACC
  temp[2,1]<-t[x,1] #subject ID
  temp[2,2]<-"Post"
  temp[2,3]<-"Hard"
  temp[2,4]<-t[x,6] #gender
  temp[2,5]<-t[x,3] #postHard ACC
  temp[3,1]<-t[x,1]#subject ID
  temp[3,2]<-"Pre"
  temp[3,3]<-"Easy"
  temp[3,4]<-t[x,6] #gender
  temp[3,5]<-t[x,4] #preE ACC
  temp[4,1]<-t[x,1]#subject ID
  temp[4,2]<-"Pre"
  temp[4,3]<-"Hard"
  temp[4,4]<-t[x,6] #gender
  temp[4,5]<-t[x,5] #preHard ACC
  new_format <- rbind(new_format,temp)
}

new_format<-as.data.frame(new_format)
new_format$ID <- as.factor(new_format$ID)
new_format$difficulty <- as.factor(new_format$difficulty)
new_format$time <- as.factor(new_format$time)
new_format$ACC <- as.character(new_format$ACC)
new_format$ACC <- as.numeric(new_format$ACC)
new_format$time <- relevel(new_format$time, "Pre")
new_format$gender <- as.factor(new_format$gender)
#new_format$time <- factor(new_format,levels(new_format$time)[c(2,1)])
gender <- new_format$gender
levels(gender)[1] <- "female"
levels(gender)[2] <- "male"
time <- new_format$time
difficulty <-new_format$difficulty
ACC <- new_format$ACC


myfit1 <- aov(ACC~time*difficulty*gender)
myfit2 <- aov(ACC~time*difficulty)
myfit3 <- aov(ACC~time*gender)
myfit4 <- aov(ACC~difficulty*gender)
summary(myfit1)
#plot_model(myfit1, type = "pred")
interaction.plot(time, difficulty, ACC, xlab = "time", ylab = "ACC", type="b",pch=c(21,19), las=1)
plot_model(myfit1, type = "int", line.size = 1, show.values = TRUE) #3-way interaction
plot_model(myfit2, type = "int")
plot_model(myfit3, type = "int")
plot_model(myfit4, type="int")
#Dummy Variable Regression
#new_format$category <- factor(new_format$category, levels = c(1,2,3,4), labels = c("postEasy", "postHard", "preEasy", "preHard"))
levels(new_format$category)[1] <- "postEasy"
levels(new_format$category)[2] <- "postHard"
levels(new_format$category)[3] <- "preEasy"
levels(new_format$category)[4] <- "preHard"
contrasts(new_format$category) <- contr.treatment(4, 4) #if you change the basis here different results for the regression
myfit <- lm(new_format$ACC ~ new_format$category)
summary(myfit)

#ttestModel = t.test(post ~ treat, data=df, var.equal=T)
subject <- new_format$ID
newfit <- aov(ACC ~ (time*difficulty*gender) + Error(subject/(time*difficulty)), data=new_format)
summary(newfit)




