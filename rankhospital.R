rankhospital <- function(state, ailment, rank) {
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
  se <- oc[oc$State == state & !is.na(oc[,col]),]
  seord = order(se[, col], se$Hospital.Name )
  if ((is.numeric(rank) & (rank > length(seord)))) { return(NA) }

  if (tolower(rank) == "worst" ) { r = length(seord)}
  else if (tolower(rank) == "best") {r = 1 }
  else { r = rank}
  return( se$Hospital.Name[seord[r]])
  ## rate
}