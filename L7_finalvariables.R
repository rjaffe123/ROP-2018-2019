data_acc <- read.csv("/Users/rachaeljaffe/psych_data/clean_files/L7/L7pre_18-19_ACC.csv")
#data_acc <- data_acc[,-1]
colnames(data_acc)[1] <- c("SUBJECT_ID")
data_ct1 <- read.csv("/Users/rachaeljaffe/psych_data/clean_files/L7post_15-16_CT.csv")
data_ct <- head(data_ct1, -6)
data_ct <- data_ct[,-1]
data_ct1 <- data_ct1[,-1]
colnames(data_ct1)[1] <- c("SUBJECT_ID")
#pre easy Q = 6, 9, 10, 11, 15
#pre hard Q = 7, 8, 12, 13, 14
#post easy Q = 7, 8, 10, 13, 14
#post hard Q = 6, 9, 11, 12, 15
# change questions depending on pre or posta
additional_columns <- matrix(data = NA, ncol=2, nrow = 1)
colnames(additional_columns) <- c("EASY_ACC(%)", 'HARD_ACC(%)')
for (x in 1:nrow(data_acc)){
  easy_total <- 0 # number of easy questions correctly answered 
  hard_total <- 0 # number of hard questions correctly answered 
  for (y in c(7,10,11,12,16)){ #pre easy Q = 6, 9, 10, 11, 15 (7,10,11,12,16) or 
                              # post easy Q = 7, 8, 10, 13, 14 (8,9,11,14,15)
    easy_total <- easy_total+data_acc[x,y]
  }
  for (y in c(8,9,13,14,15)){ #pre hard Q = 7, 8, 12, 13, 14 (8,9,13,14,15) or 
                                # post hard Q = 6, 9, 11, 12, 15 (7,10,12,13,16)
    hard_total  <- hard_total + data_acc[x,y]
  }
  temp <- data.frame((easy_total/500)*100, (hard_total/500)*100)
  colnames(temp) <- colnames(additional_columns)
  additional_columns <- rbind(additional_columns, temp)
}

additional_columns<-additional_columns[-1,]
data_acc <- cbind(data_acc, additional_columns)

new_data_frame <- matrix(data = NA, ncol = 8, nrow = 1)
colnames(new_data_frame) <- c("EASY_CT_AVE", "HARD_CT_AVE", "EASY_MED", "HARD_MED", 
                              "MOTION1_CT_AVE", 'MOTION2_CT_AVE', "M1_MED", "M2_MED")

for (x in 1:nrow(data_ct)){
  easy_ct <- c() # crtical time for easy questions
  hard_ct <- c() # ct for hard questions
  motion1_ct <- c() #ct for motion 1
  motion2_ct <- c() #ct for motion 2
  for (y in c(7,10,11,12,16)){#pre easy Q = 6, 9, 10, 11, 15 (7,10,11,12,16) or 
    # post easy Q = 7, 8, 10, 13, 14 (8,9,11,14,15)
    easy_ct  <- c(easy_ct, data_ct[x,y])
  }
  for (y in c(8,9,13,14,15)){#pre hard Q = 7, 8, 12, 13, 14 (8,9,13,14,15) or 
    # post hard Q = 6, 9, 11, 12, 15 (7,10,12,13,16)
    hard_ct  <- c(hard_ct, data_ct[x,y])
  }
  for (y in 2:6){
    motion1_ct  <- c(motion1_ct, data_ct[x,y])
  }
  for (y in 17:21){
    motion2_ct <- c(motion2_ct, data_ct[x,y])
  }
  average_easy <- mean(easy_ct)
  average_hard <- mean(hard_ct)
  easy_med <- median(easy_ct)
  hard_med <- median(hard_ct)
  average_motion1 <- mean(motion1_ct)
  average_motion2 <- mean(motion2_ct)
  m1_med <- median(motion1_ct)
  m2_med <- median(motion2_ct)
  temp <- data.frame(average_easy, average_hard, easy_med, hard_med, 
                     average_motion1, average_motion2, m1_med, m2_med)
  colnames(temp) <- colnames(new_data_frame)
  new_data_frame <- rbind(new_data_frame, temp)
}

new_data_frame<-new_data_frame[-1,]
new_data_frame[nrow(new_data_frame)+6,] <- NA
data_ct1 <- cbind(data_ct1, new_data_frame)

write.csv(data_ct1, file = "/Users/rachaeljaffe/psych_data/final_variables/L7pre_18-16_CT.csv")
write.csv(data_acc, file = "/Users/rachaeljaffe/psych_data/final_variables/L7pre_18-19_ACC.csv")









