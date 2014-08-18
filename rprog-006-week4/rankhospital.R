rankhospital <- function(state, outcome, num="best") {
    data <- read.csv('outcome-of-care-measures.csv', colClasses='character')
    states <- unique(data$State)
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(state %in% states))
        stop("Invalid state")
    if (!(outcome %in% outcomes))
        stop("Invalid outcome")
    dt <- data[data$State == state, ]
    dt[, c(11, 17, 23)] <- sapply(dt[, c(11, 17, 23)], as.numeric)
    if (outcome == "heart attack") {
        dt <- dt[order(dt[, 11], dt[, 2]), ]
        dt <- dt[!is.na(dt[, 11]), ]
    } else if (outcome == "heart failure") {
        dt <- dt[order(dt[, 17], dt[, 2]), ]
        dt <- dt[!is.na(dt[, 17]), ]
    } else {
        dt <- dt[order(dt[, 23], dt[, 2]), ]
        dt <- dt[!is.na(dt[, 23]), ]
    }
    if (num == "best")
        num <- 1;
    if (num == "worst")
        num <- nrow(dt)
    dt[num, "Hospital.Name"]
}
