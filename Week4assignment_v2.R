##These are the functions of the Week 4 Assignment of the R Programming from Coursera. 
##They give the same results as the original version I wrote. 
##This time, I modified some of the codes to make them easier to read.

##2. Finding the best hospital in a state

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

##3. Ranking hospitals by outcome in a state

rankhospital <- function(state, outcome, num = "best") {
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
        
        ##Delete missing values
        
        good <- complete.cases(dfout)
        dfgood <- dfout[good, ]
        
        ##Sort the data based on 30-day death rate, then the hospital name (A-Z)
        index <- order(dfgood[, outcome], dfgood[, "hospital"])
        ranklist <- dfgood[index, ]
        
        ##Count the number of rows
        num_row <- nrow(ranklist)
        
        ## Return hospital name in that state with the given rank 30-day death rate
        if(num == "best") {
                ranklist$hospital[1]
        } else {
                if(num == "worst") {
                        ranklist$hospital[num_row]
                } else {
                        if(num > num_row) {
                                NA
                        } else {
                                ranklist$hospital[num]
                        }
                }
        }
}

##4. Ranking hospitals in all states

rankall <- function(outcome, num = "best") {
        ##Read data
        dat <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ##Test the validity of outcome argument
        outcomename <- c("heart attack", "heart failure", "pneumonia")
        if(!(outcome %in% outcomename)) stop("invalid outcome")
        
        ##Subset data.frame with 30-day death rate
        df <- dat[, c(2, 7, 11, 17, 23)]
        
        ##Rename columns
        colnames(df) <- c("hospital", "state","heart attack", "heart failure", "pneumonia")
        
        ##Simplify the data.frame
        dfout <- df[, c("hospital", "state", outcome)]
        
        ##convert 30-day death rate to numeric
        dfout[, outcome] <- as.numeric(dfout[, outcome])
        
        ##Delete missing values
        good <- complete.cases(dfout)
        dfgood <- dfout[good, ]
        
        ##Sort data based on 1)state; 2)30-day death rate; 3)hospital(A-Z)
        index <- order(dfgood[, 2], dfgood[, 3], dfgood[, 1])
        rank <- dfgood[index, ]
        
        ##Split the data.frame by states
        statelist <- split(rank, rank$state)
        
        ##count the number of states
        statename <- unique(dat$State)  
        statenum <- length(statename)
        
        ##make a for loop to get data from each state
        hosrank <- data.frame()
        
        for(i in 1:statenum) {
                ##Extract the data.frame from the list
                ranki <- statelist[[i]]
                
                ##Count row numbers
                nrowi <- nrow(ranki)
                
                ##Make a vector of column names
                cname <- c("hospital", "state")
                
                ##Extract the data of interest
                if(num == "best") {
                        rowi <- ranki[1, cname]
                } else {
                        if(num == "worst") {
                                rowi <-ranki[nrowi, cname]
                        } else {
                                if(num > nrowi) {
                                        rowi <- data.frame(hospital = NA, state = ranki$state[1])
                                } else {
                                        rowi <- ranki[num, cname]
                                }
                        }
                }
                ##Combine data extracted from each state
                hosrank <- rbind(hosrank, rowi)
        }
        ##Return a data frame with the hospital names and the state name
        hosrank
}