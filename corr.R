# Course 2 Week 2 Polutant program
# Write a function that takes a directory of data files and a threshold for complete cases 
# and calculates the correlation between sulfate and nitrate for monitor locations where 
# the number of completely observed cases (on all variables) is greater than the threshold. 
# The function should return a vector of correlations for the monitors that meet the 
# threshold requirement. If no monitors meet the threshold requirement, then the function
# should return a numeric vector of length 0.

source("complete.R")

corr <- function(directory, threshold = 0) {
  # directory is a character vector of length 1 which contains the directory string
  # threshold is min number of complet cases
  fidx = 1:332
  df = complete(directory)
  id = fidx[df["nobs"] > threshold]
  #sprint(id)
  cors = numeric(length(id))

  i<- 0
  for (monitor in id) {
    id3 <- sprintf("%03d", monitor)
    fn <- paste("./",directory,"/",id3,".csv", sep = "")
    mon <- read.csv(fn)
    i <- i + 1
    cors[i] <-  cor(mon$sulfate, mon$nitrate, "pairwise.complete.obs")
    }
  return(cors)
}