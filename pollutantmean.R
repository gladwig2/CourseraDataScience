# Course 2 Week 2 Polutant program
#Write a function named 'pollutantmean' that calculates the mean of a pollutant 
#(sulfate or nitrate) across a specified list of monitors. The function 
#'pollutantmean' takes three arguments: 'directory', 'pollutant', and 'id'. 
#'Given a vector monitor ID numbers, 'pollutantmean' reads that monitors' particulate 
#'matter data from the directory specified in the 'directory' argument and 
#'returns the mean of the pollutant across all of the monitors, 
#'ignoring any missing values coded as NA. A prototype of the function is as follows
#'

pollutantmean <- function(directory,polutant,id = 1:332) {
  # directory is a character vector of length 1 which contains the directory string
  # polutant - either "sulfate" or "nitrate"
  # id, monitor id to read. Files are of form "id"+ ".csv"
  
  accum <- 0
  cnt <- 0
  for (monitor in id) {
    id3 <- sprintf("%03d", monitor)
    fn <- paste("./",directory,"/",id3,".csv", sep = "")
    mon <- read.csv(fn)
    accum <- accum + sum(mon[,polutant],na.rm = TRUE)
    cnt <- cnt + sum(!is.na(mon[,polutant]))
  }
  #print(cum_mean)
  return(accum/cnt)
}