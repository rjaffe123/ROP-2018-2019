# MASTER CLEANING CODE
# Only use what functions you need for the data. 
# First, make sure your data file is a csv version and the data structure is PivotTable.
# data_ct <-read.csv('/Users/rachaeljaffe/psych_data/dirty_files/L7pre_18-19_AA.csv') #upload file of critical time
# data_ct <-data_ct[-nrow(data_ct),] #removes last row because the last row is a total which messes up the averages
data_acc <- read.csv('/Users/rachaeljaffe/psych_data/dirty_files/L7post_15-16_ACC.csv') #upload file of accuracy
data_acc <-data_acc[,-1]
# data_ss <- read.csv('/Users/rachaeljaffe/psych_data/dirty_files/L10/L10A_14-15_SS.csv') #upload file of accuracy
# data_ss <-data_ss[-nrow(data_ss),]
# This is the consent file
file<-read.csv('/Users/rachaeljaffe/psych_data/email_ID_18-19.csv')
file <- file[-nrow(file),] #only use this line if you have used a pivot table and the last row is a row of totals. 

# The following function matches the emails in the data to the emails that have given consent. 
# Those that have not given consent will be deleted. 
# matrix = the file that needs to be cleaned
# matrix1 = the consent file
match_consent <- function(matrix, matrix1){
  t <- matrix(data = NA, ncol = ncol(matrix), nrow = 1)
  colnames(t) <- colnames(matrix)
  for (i in 1:nrow(matrix)){
    for (j in 1:nrow(matrix1)){
      if (as.character(matrix[i,1]) == as.character(matrix1[j,1])){
        temp <- data.frame(matrix1[j,2], matrix[i,-1])
        colnames(temp) <- colnames(matrix)
        t <- rbind(t, temp) 
      }
    }
  }
  t <- t[-1,]
  return(t)
}

# The follwoing function removes the incompleted data. 
# Those who haven't responded to more than 80% have been deleted. 
# matrix = the file that needs to be cleaned 
greater_than_80 <- function(matrix){
  n <- c()
  for (i in 1:nrow(matrix)){
    a <- 0
    for (j in 1:ncol(matrix)){
      if (is.na(matrix[i,j])){
        a <- a + 1
      }
    }
    if (a / (ncol(matrix)-2) > 0.2){
      n <- c(i,n)
    }
  }
  if (length(n) > 0){
    for (i in 1:length(n)){
      #print(n[i])
      matrix <- matrix[-(n[i]),]
    }
  }
  return (matrix)
}

# This function replaces accuracy score of the outliers to 0. "matrix" should be the critical time data 
# and "matrix1" should be the accuracy data.
change_acc <- function(matrix, matrix1){ 
  for (i in 2:ncol(matrix)){
    data <- matrix[,i]
    #print(data)
    q1<-quantile(data,0.25,type =1, na.rm=TRUE)
    q3<-quantile(data,0.75,type =1, na.rm=TRUE)
    a<- q1-3*(q3-q1)
    b<- q3+3*(q3-q1)
    for (j in 1:length(data)){
      if (data[j] < a || is.na(data[j]==FALSE)) {
        #matrix[j,i]<- mean(data, na.rm=TRUE)
        matrix1[j,i]<- 0
        
      }
      if (data[j] > b || is.na(data[j]==FALSE)){
        #matrix[j,i]<- mean(data, na.rm=TRUE)
        matrix1[j,i]<- 0
      }
    }
  }
  return (matrix1)
}

# This function replaces the outliers within each group (the specific question, or column) 
# with the mean of the column. 
# matrix = the critical time data
remove_outliers <- function(matrix){ #function to replace outliers with mean of data
  percentage_of_outliers <- 'percentage of outliers'
  percentage_of_outliers_left <- 'percentage of outliers leftside'
  percentage_of_outliers_right <- 'percentage of outliers rightside'
  upper_bound <- "upper bound"
  lower_bound <- "lower bound"
  v <- c(as.character(percentage_of_outliers))
  h <- c(as.character(percentage_of_outliers_left))
  w <- c(as.character(percentage_of_outliers_right))
  upper_row <- c(as.character(upper_bound))
  lower_row <- c(as.character(lower_bound))
  total <- nrow(matrix)
  for (i in 2:ncol(matrix)){
    c <- 0
    d <- 0
    data <- matrix[,i]
    x <- 0
    q1<-quantile(data,0.25,type =1, na.rm=TRUE)
    q3<-quantile(data,0.75,type =1, na.rm=TRUE)
    a<- q1-3*(q3-q1)
    b<- q3+3*(q3-q1)
    print(a)
    for (j in 1:length(data)){
      if (data[j] < a && is.na(data[j])==FALSE) {
        #print(j)
       # print(i)
       # print(matrix[j,i])
        matrix[j,i]<- mean(data, na.rm=TRUE)
        x <- x+1
        c <- c+ 1
      }
      if (data[j] > b && is.na(data[j])==FALSE){
        matrix[j,i]<- mean(data, na.rm=TRUE)
        x <- x + 1
        d <- d +1
        
      }
    }
    h <- c(h,c/total*100)
    w <- c(w,d/total*100)
    v <- c(v,x/total*100)
    lower_row <- c(lower_row, a)
    upper_row <- c(upper_row, b)
  }
  na_row <- c(NA*length(ncol(matrix)))
  matrix <- rbind(matrix, na_row)
  matrix <- rbind(matrix,h)
  matrix <- rbind(matrix,w)
  matrix <- rbind(matrix,v)
  matrix <- rbind(matrix, lower_row)
  matrix <- rbind(matrix, upper_row)
  return (matrix)
}


#matching consent
# acc <- match_consent(data_acc, file)
ct <- match_consent(data_ct, file)
#ss <- match_consent(data_ss, file)

#removing greater than 80
# acc <- greater_than_80(acc)
ct <- greater_than_80(ct)
#ss <- greater_than_80(ss)

#changing the accuracies of the outliers
# acc <- change_acc(ct, acc)
#removing said outliers
ct <- remove_outliers(ct)

#writing the new cleaned csv
write.csv(ct, file = '/Users/rachaeljaffe/psych_data/clean_files/L7post_18-19_TT.csv') #output file for clean CT
# write.csv(acc, file = '/Users/rachaeljaffe/psych_data/clean_files/L7post_15-16_ACC.csv') #output file for acc
#write.csv(ss, file = '/Users/rachaeljaffe/psych_data/clean_files/L10A_14-15_SS.csv')
