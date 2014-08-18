rankall <- function(outcome, num="best") {
    data <- read.csv('outcome-of-care-measures.csv', colClasses='character')
    outcomes <- c("heart attack", "heart failure", "pneumonia")
    if (!(outcome %in% outcomes))
        stop("Invalid outcome")
    data[, c(11, 17, 23)] <- sapply(data[, c(11, 17, 23)], as.numeric)
    res <- lapply(split(data, data$State), function(dt) {
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
                  xx <- data.frame(hospital=NA, state=NA)
                  hos = dt[num, "Hospital.Name"]
                  hos
            })
    res2 <- data.frame(hospital=NA, state=NA)
    for (i in names(res)) {
        if (is.na(i)) next
        res2[i, ] <- c(res[i], i)
    }
    res2
}
