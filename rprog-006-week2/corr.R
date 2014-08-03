corr <- function(directory, threshold = 0) {
    ans = c()
    for (i in seq_len(332)) {
        data <- read.csv(paste(directory, "/", sprintf("%03d", i), ".csv", sep=""))
        hasna <- apply(is.na(data), 1, max)
        cnt <- length(hasna[hasna < 1])
        if (cnt <= threshold) next
        selected_data <- data[hasna < 1, ]
        col1 <- selected_data["sulfate"]
        col2 <- selected_data["nitrate"]
        ans <- append(ans, (cor(col1, col2)))
    }
    ans
}
