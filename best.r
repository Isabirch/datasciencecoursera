best <- function(state, outcome) {
  ## Read outcome data
  rawdata <- read.csv(
    "rprog_data_ProgAssignment3-data/outcome-of-care-measures.csv", 
    colClasses = "character")
  
  ## Check that state and outcome are valid
  ### valid values for state are listed in the state data set $abb
  if(state %in% state.abb) {
    #print(paste("valid state ", state))
  } else {
    stop("invalid state")
  }
  
  ### valid values for outcome include: "heart attack", "heart failure", "pneumonia"
  if(outcome == "heart attack") {
    colname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  } else if (outcome == "heart failure") {
    colname <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  } else if (outcome == "pneumonia") {
    colname <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  } else {
    stop("invalid outcome")
  }
  
  
  ## Return hospital name in that state with lowest 30-day death rate
  ### subset data to pick only state, hosp name, and selected outcome
  select_data <- rawdata[rawdata[,"State"] == state, c("Hospital.Name", colname)]
  ### coerce selected outcome to numerical
  select_data[,colname] <- as.numeric(select_data[,colname])
  ### find best outcome
  min_rate <- min(select_data[,colname], na.rm = T)
  #print(paste("min rate ", min_rate))
  ### find rows that match best outcome
  best_hosps <- select_data[!is.na(select_data[,2]) & select_data[,2] == min_rate,]
  ### select row with lowest alphabetical hosp name
  hosp_names <- best_hosps[,1]
  min_name <- min(hosp_names)
  min_name
}
