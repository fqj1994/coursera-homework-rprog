complete <- function(directory, id = 1:332) {
    ans <- id
    for (i in id) {
        data <- read.csv(paste(directory, "/", sprintf("%03d", i), ".csv", sep=""))
        hasna <- apply(is.na(data), 1, max)
        cnt <- length(hasna[hasna < 1])
        ans <- append(ans, cnt)
    }
    dim(ans) <- c(length(id), 2)
    ans <- data.frame(ans)
    names(ans) <- c("id", "nobs")
    ans
}
