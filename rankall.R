rankall <- function(ailment, rank=1) {
  ## Read outcome data
  oc <- read.csv("./rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", colClasses = "character")
  suppressWarnings(oc[, 11] <- as.numeric(oc[, 11]))
  suppressWarnings(oc[, 17] <- as.numeric(oc[, 17]))
  suppressWarnings(oc[, 23] <- as.numeric(oc[, 23]))
  
  ## Check that outcome is valid
  ailment = tolower(sub(" ","",ailment))
  if (ailment == "heartfailure") {col = 17}
  else if (ailment == "heartattack") {col = 11}
  else if (ailment == "pneumonia") { col = 23 }
  else {stop("invalid outcome")}
  
  states = sort(unique(oc$State))
  df = data.frame(hospital=numeric(),state=character(),stringsAsFactors=FALSE)
  
  i <- 1
  for (state in states){
    se <- oc[oc$State == state & !is.na(oc[,col]),]
    seord = order(se[, col], se$Hospital.Name )

    if (tolower(rank) == "worst" ) { r = length(seord)}
    else if (tolower(rank) == "best") { r = 1 }
    else { r = rank }

    df[i,"state"] <- state
    if ((is.numeric(rank) & (rank > length(seord)))) {
      df[i,"hospital"] <- NA
    } else { 
      df[i,"hospital"] <- se$Hospital.Name[seord[r]]
    }
    i = i + 1
  }
  return(df)
  ## rate
}