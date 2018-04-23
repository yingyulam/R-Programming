##Revision of the best function

best <- function(state, outcome) {
        ##Read data
        dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ##Test the validity of the state argument
        statename <- unique(dat$State)  
        if(!(state %in% statename)) stop("invalid state") 
        
        ##Test the validity of the outcome argument
        outcomename <- c("heart attack", "heart failure", "pneumonia")
        if(!(outcome %in% outcomename)) stop("invalid outcome")
        
        ##Subset data.frame with 30-day death rates from the selected state.
        df <- subset(dat, State == state, select = c(2, 11, 17, 23))
        
        ##Rename the columns
        colnames(df) <- c("hospital", "heart attack", "heart failure", "pneumonia")
        
        ##Simplify the data frame
        dfout <- df[, c("hospital", outcome)]
        
        ##Convert the 30-day death rate into numeric
        dfout[, outcome] <- as.numeric(dfout[, outcome])
        
        ##Find the index of the lowest outcome
        i_min <- which.min(dfout[, outcome])
        
        ##Find the corresponding hospital name
        dfout$hospital[i_min]
}