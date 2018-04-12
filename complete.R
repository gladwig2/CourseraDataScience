# Course 2 Week 2 Polutant program
# Write a function that reads a directory full of files and reports the number of 
# completely observed cases in each data file. The function should return a data frame 
# where the first column is the name of the file and the second column is the 
# number of complete cases. A prototype of this function follows

complete <- function(directory, id = 1:332) {
  # directory is a character vector of length 1 which contains the directory string
  # id, monitor id to read. Files are of form "id"+ ".csv"
  df <- data.frame("id" = numeric(), "nobs" = numeric(),  stringsAsFactors = FALSE)
  i <- 0
  for (monitor in id) {
    id3 <- sprintf("%03d", monitor)
    fn <- paste("./",directory,"/",id3,".csv", sep = "")
    mon <- read.csv(fn)
    i <- i + 1
    df[i,"nobs"] <-  sum(complete.cases(mon))
    df[i, "id"] <- monitor
    }
  return(df)
}