##Part 1

pollutantmean <- function(directory, pollutant, id = 1:332) {
        file_name <- list.files(directory, full.names = TRUE)
        new_table <- data.frame()
        for(i in id) {
                new_data <- read.csv(file_name[i])   ##read data from certain files
                new_table <- rbind(new_table, new_data)
        }
        mean(new_table[,pollutant], na.rm = TRUE)
}



##Part 2

complete <- function(directory, id = 1:322) {
        fn <- list.files(directory, full.names = TRUE) ##list all files in the directory
        table <- data.frame()
        for(i in id) {
                rd <- read.csv(fn[i])   ##read data from the file
                good <- complete.cases(rd)  ##test if it is a complete case
                sum1 <- sum(good)
                iframe <- data.frame(id = i, sum1)
                table <- rbind(table, iframe)
        }
        table
}

##Part 3

corr <- function(directory, threshold = 0) {
        fn <- list.files(directory, full.names = TRUE)
        corrs <- numeric()
        for(i in 1:332) {
                rd <- read.csv(fn[i])
                good <- complete.cases(rd)
                dat <- rd[good, ]
                nrows <- sum(good)
                if(nrows > threshold) {
                        sul <- dat$sulfate
                        nit <- dat$nitrate
                        corrs[i] <- cor(sul, nit)
                } 
        }
        corrs[!is.na(corrs)]
}