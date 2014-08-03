pollutantmean <- function(directory, pollutant, id = 1:332) {
    nums = c()
    for (i in id) {
        data <- read.csv(paste(directory, "/", sprintf("%03d", i), ".csv", sep=""))
        col_data <- data[, pollutant]
        nums <- append(nums, col_data[!is.na(col_data)])
    }
    mean(nums)
}
