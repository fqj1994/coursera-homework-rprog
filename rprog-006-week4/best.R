best <- function(state, outcome) {
    data <- read.csv('outcome-of-care-measures.csv', colClasses='character')
    states <- unique(data$State)
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(state %in% states))
        stop("Invalid state")
    if (!(outcome %in% outcomes))
        stop("Invalid outcome")
    dataInState <- data[data$State == state, ]
    dataInState[, c(11, 17, 23)] = sapply(dataInState[, c(11, 17, 23)], as.numeric)
    if (outcome == "heart attack")
        best <- dataInState[which.min(dataInState[[11]]), "Hospital.Name"]
    else if (outcome == "heart failure")
        best <- dataInState[which.min(dataInState[[17]]), "Hospital.Name"]
    else 
        best <- dataInState[which.min(dataInState[[23]]), "Hospital.Name"]
    best
}
