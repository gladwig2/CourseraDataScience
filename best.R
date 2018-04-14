best <- function(state, ailment) {
  ## Read outcome data
  oc <- read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  suppressWarnings(oc[, 11] <- as.numeric(oc[, 11]))
  suppressWarnings(oc[, 17] <- as.numeric(oc[, 17]))
  suppressWarnings(oc[, 23] <- as.numeric(oc[, 23]))
  
  ## Check that state and outcome are valid
  if (!(state %in% oc$State)) { stop("invalid state")}
  ailment = tolower(sub(" ","",ailment))
  if (ailment == "heartfailure") {col = 17}
  else if (ailment == "heartattack") {col = 11}
  else if (ailment == "pneumonia") { col = 23 }
  else {stop("invalid outcome")}
  
  ## Return hospital name in that state with lowest 30-day death
  mval = min(oc[oc$State == state,col], na.rm = TRUE)
  return(sort(oc[oc$State == state & oc[,col] == mval, "Hospital.Name"])[1])
  
  ## rate
}