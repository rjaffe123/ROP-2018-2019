data_acc <- read.csv("/Users/rachaeljaffe/psych_data/clean_files/L10A_15-16_ACC.csv")
data_acc <- data_acc[,-1]
colnames(data_acc)[1] <- c("SUBJECT_ID")
# data_ss <- read.csv("/Users/rachaeljaffe/psych_data/clean_files/L10B_17-18_SS.csv")
# data_ss <- data_ss[,-1]
# colnames(data_ss)[1] <- c("SUBJECT_ID")
data_ct1 <- read.csv("/Users/rachaeljaffe/psych_data/clean_files/L10A_15-16_CT.csv")
data_ct <- head(data_ct1, -6)
data_ct <- data_ct[,-1]
data_ct1 <- data_ct1[,-1]
colnames(data_ct1)[1] <- c("SUBJECT_ID")
# pretest: 2,3,4,5,6,7,8,9
# medium neg: 11,12,13,14, 27,28,29,30, 43,44,45,46
# large neg: 15,16,17,18, 31,32,33,34, 47,48,49,50
# medium pos: 19,20,21,22, 35,36,37,38, 51,52,53,54
# large pos: 23,24,25,26,39,40,41,42, 55, 56, 57, 58
# posttest: 60,61,62,63,64,65,66,67
# CORREL_lrg_accy	Average accuracy across large-positive and large-negative correlations.
# CORREL_sml_accy	Average accuracy across medium-positive, medium-negative, and near-zero correlations.
# CORREL_total_accy	Sum of accuracy marks across all five types of correlations.
additional_columns <- matrix(data = NA, ncol=9, nrow = 1)
colnames(additional_columns) <- c("pretest_acc", "posttest_acc", 'lrgNEG_acc','lrgPOS_acc','medNEG_acc','medPOS_acc',
                                  'lrg_total_acc', 'sml_total_acc', 'overal_training_total_acc')
for (x in 1:nrow(data_acc)){
  pretest <- 0
  pretest_len <- 0
  large_pos <- 0 
  large_neg_len <- 0
  large_neg <- 0 
  large_pos_len <- 0
  med_pos <- 0
  med_pos_len <- 0
  med_neg <- 0 
  med_neg_len <- 0
  posttest <- 0
  posttest_len <- 0 
  
  for (y in c(2,3,4,5,6,7,8,9)){
    pretest <- pretest+data_acc[x,y+1]
    pretest_len <- pretest_len + 1
  }
  for (y in c(15,16,17,18, 31,32,33,34, 47,48,49,50)){
    large_neg <- large_neg+data_acc[x,y+1]
    large_neg_len <- large_neg_len + 1
  }
  for (y in c(23,24,25,26,39,40,41,42, 55, 56, 57, 58)){ 
    large_pos <- large_pos + data_acc[x,y+1]
    large_pos_len <- large_pos_len + 1
  }
  for (y in c(11,12,13,14, 27,28,29,30, 43,44,45,46)){ 
    med_neg <- med_neg + data_acc[x,y+1]
    med_neg_len <- med_neg_len + 1
  }
  for (y in c(19,20,21,22, 35,36,37,38, 51,52,53,54)){ 
    med_pos <- med_pos + data_acc[x,y+1]
    med_pos_len <- med_pos_len + 1
  }
  for (y in c(60,61,62,63,64,65,66,67)){
    posttest <- posttest+data_acc[x,y+1]
    posttest_len <- posttest_len + 1
  }
  
  temp <- data.frame((pretest/pretest_len)*1, (posttest/posttest_len)*1, (large_neg/large_neg_len)*1, (large_pos/large_pos_len)*1, 
                     (med_neg/med_neg_len)*1, (med_pos/med_pos_len)*1,
                     ((large_neg+large_pos)/(large_pos_len+large_neg_len))*1, 
                     ((med_neg+med_pos)/(med_neg_len+med_pos_len))*1,
                     ((large_neg+large_pos+med_neg+med_pos)/(large_pos_len+large_neg_len+med_neg_len+med_pos_len))*1)
  colnames(temp) <- colnames(additional_columns)
  additional_columns <- rbind(additional_columns, temp)
}

additional_columns<-additional_columns[-1,]
data_acc <- cbind(data_acc, additional_columns)

new_data_frame <- matrix(data = NA, ncol=9, nrow = 1)
colnames(new_data_frame) <- c("pretest_ave", "posttest_ave", 'lrgNEG_ave','lrgPOS_ave','medNEG_ave','medPOS_ave',
                                  'lrg_total_ave', 'sml_total_ave', 'overall_training_total_ave')

for (x in 1:nrow(data_ct)){
  pretest <- 0
  large_pos <- 0 
  large_neg <- 0 
  med_pos <- 0
  med_neg <- 0
  posttest <- 0
  for (y in c(2,3,4,5,6,7,8,9)){
    pretest <- pretest+data_acc[x,y+1]
  }
  for (y in c(15,16,17,18, 31,32,33,34, 47,48,49,50)){
    large_neg <- large_neg+data_acc[x,y+1]
  }
  for (y in c(23,24,25,26,39,40,41,42, 55, 56, 57, 58)){ 
    large_pos <- large_pos + data_acc[x,y+1]
  }
  for (y in c(11,12,13,14, 27,28,29,30, 43,44,45,46)){ 
    med_neg <- med_neg + data_acc[x,y+1]
  }
  for (y in c(19,20,21,22, 35,36,37,38, 51,52,53,54)){ 
    med_pos <- med_pos + data_acc[x,y+1]
  }
  for (y in c(60,61,62,63,64,65,66,67)){
    posttest <- posttest+data_acc[x,y+1]
  }
  pretest <- pretest/8
  large_neg <- large_neg/12
  large_pos <- large_pos/12
  med_neg <- med_neg/12
  med_pos <- med_pos/12
  posttest <- posttest/8
  temp <- data.frame(pretest, posttest, large_neg, large_pos, med_neg, med_pos, 
                     (large_pos+large_neg), (med_pos+med_neg), (large_pos+large_neg+med_pos+med_neg))
  colnames(temp) <- colnames(new_data_frame)
  new_data_frame <- rbind(new_data_frame, temp)
}

new_data_frame<-new_data_frame[-1,]
new_data_frame[nrow(new_data_frame)+6,] <- NA
data_ct1 <- cbind(data_ct1, new_data_frame)

new_data_frame <- matrix(data = NA, ncol=9, nrow = 1)
colnames(new_data_frame) <- c("pretest_ave", "posttest_ave", 'lrgNEG_ave','lrgPOS_ave','medNEG_ave','medPOS_ave',
                              'lrg_total_ave', 'sml_total_ave', 'overall_training_total_ave')

# for (x in 1:nrow(data_ss)){
#   pretest <- 0
#   large_pos <- 0 
#   large_neg <- 0 
#   med_pos <- 0
#   med_neg <- 0
#   posttest <- 0
#   for (y in c(2,3,4,5,6,7,8,9)){
#     pretest <- pretest+data_acc[x,y+1]
#   }
#   for (y in c(15,16,17,18, 31,32,33,34, 47,48,49,50)){
#     large_neg <- large_neg+data_acc[x,y+1]
#   }
#   for (y in c(23,24,25,26,39,40,41,42, 55, 56, 57, 58)){ 
#     large_pos <- large_pos + data_acc[x,y+1]
#   }
#   for (y in c(11,12,13,14, 27,28,29,30, 43,44,45,46)){ 
#     med_neg <- med_neg + data_acc[x,y+1]
#   }
#   for (y in c(19,20,21,22, 35,36,37,38, 51,52,53,54)){ 
#     med_pos <- med_pos + data_acc[x,y+1]
#   }
#   for (y in c(60,61,62,63,64,65,66,67)){
#     posttest <- posttest+data_acc[x,y+1]
#   }
#   pretest <- pretest/8
#   large_neg <- large_neg/12
#   large_pos <- large_pos/12
#   med_neg <- med_neg/12
#   med_pos <- med_pos/12
#   posttest <- posttest/8
#   temp <- data.frame(pretest, posttest, large_neg, large_pos, med_neg, med_pos, 
#                      (large_pos+large_neg), (med_pos+med_neg), (large_pos+large_neg+med_pos+med_neg))
#   colnames(temp) <- colnames(new_data_frame)
#   new_data_frame <- rbind(new_data_frame, temp)
# }
# 
# new_data_frame<-new_data_frame[-1,]
# data_ss <- cbind(data_ss, new_data_frame)

write.csv(data_ct1, file = "/Users/rachaeljaffe/psych_data/final_variables/L10A_15-16_CT.csv")
write.csv(data_acc, file = "/Users/rachaeljaffe/psych_data/final_variables/L10A_15-16_ACC.csv")
#write.csv(data_ss, file = "/Users/rachaeljaffe/psych_data/final_variables/L10A_14-15_SS.csv")








