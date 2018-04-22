best <- function(state, outcome) {
        ##Read the table from the csv file, and create a data.frame named 'dat'
        dat <- read.csv("outcome-of-care-measures.csv")
        ##Create a character vector, 'statename', containing the 2-character abbreviated names
        ##of all the states in the data.frame 'dat'; each state appears once.
        statename <- unique(dat[, 7])  
        ##Test if the validity of the state argument passed to the function.
        ##If the value is invalid, the function will throw an error with the message.
        if(!(state %in% statename)) stop("invalid state") 
        ##Create a character vector containg the three outcomes.
        outcomename <- c("heart attack", "heart failure", "pneumonia")
        ##Test the outcome argument.
        if(!(outcome %in% outcomename)) stop("invalid outcome")
        ##Extract columns from the 'dat' to create a data.frame named df.
        ##It contains 30-day mortality of three outcomes of hospital from the selected state.
        df <- subset(dat, State == state, select = c(2, 11, 17, 23))
        ##Rename the column names of the 'df'
        colnames(df) <- c("hospital", "heart attack", "heart failure", "pneumonia")
        ##Make a new data.frame 'df_out' containing only the hospital names and the 
        ##selected outcome.
        df_out <- df[, c("hospital", outcome)]
        ##Convert the second column (30-day mortality of selected outcome) into numeric type.
        df_out[, 2] <- as.numeric(as.character(df_out[, 2]))
        ##Sort the data according to 1) 30-day mortality (from lowest to highest); 2) alphabet.
        ##NAs are placed at the bottom.
        sort <- df_out[order(df_out[, outcome], df_out[, "hospital"]), ]
        ##Extract the 1st ranking (lowest 30-day mortality of certain outcome) hospital name (1st row, 1 column)
        sort[[1, 1]]
}


rankhospital <- function(state, outcome, num = "best") {
        ##Read the table from the csv file, and create a data.frame named 'dat'
        dat <- read.csv("outcome-of-care-measures.csv")
        ##Create a character vector, 'statename', containing the 2-character abbreviated names
        ##of all the states in the data.frame 'dat'; each state appears once.
        statename <- unique(dat[, 7])  
        ##Test if the validity of the state argument passed to the function.
        ##If the value is invalid, the function will throw an error with the message.
        if(!(state %in% statename)) stop("invalid state") 
        ##Create a character vector containg the three outcomes.
        outcomename <- c("heart attack", "heart failure", "pneumonia")
        ##Test the outcome argument.
        if(!(outcome %in% outcomename)) stop("invalid outcome")
        ##Extract columns from the 'dat' to create a data.frame named df.
        ##It contains 30-day mortality of three outcomes of hospital from the selected state.
        df <- subset(dat, State == state, select = c(2, 11, 17, 23))
        ##Rename the column names of the 'df'
        colnames(df) <- c("hospital", "heart attack", "heart failure", "pneumonia")
        ##Make a new data.frame 'df_out' containing only the hospital names and the 
        ##selected outcome.
        df_out <- df[, c("hospital", outcome)]
        ##Convert the second column (30-day mortality of selected outcome) into numeric type.
        df_out[, 2] = as.numeric(as.character(df_out[, 2]))
        ##Sort the data according to 1) 30-day mortality (from lowest to highest); 2) alphabet.
        ##Delete the rows with NAs to make a new data.frame 'dfgood'with no NAs
        good <- complete.cases(df_out)
        dfgood <- df_out[good, ]
        ##Sort the data according to 1) 30-day mortality (from lowest to highest); 2) alphabet.
        ##NAs are placed at the bottom.
        ranklist <- dfgood[order(dfgood[, 2], dfgood[, 1]), ]
        ##Count the number of rows
        num_row <- nrow(ranklist)
        ## Return hospital name in that state with the given rank 30-day death rate
        if(num == "best") {
                ranklist[[1, 1]]
        } else {
                if(num == "worst") {
                        ranklist[[num_row, 1]]
                } else {
                        if(num > num_row) {
                                NA
                        } else {
                                ranklist[[num, 1]]
                        }
                }
        }
}


rankall <- function(outcome, num = "best") {
        ##Read data
        dat <- read.csv("outcome-of-care-measures.csv")
        ##Test validity of outcome argument
        outcomename <- c("heart attack", "heart failure", "pneumonia")
        if(!(outcome %in% outcomename)) stop("invalid outcome")
        ##Extract data of 30-day death rate and rename columns
        df <- dat[, c(2, 7, 11, 17, 23)]
        colnames(df) <- c("hospital", "state","heart attack", "heart failure", "pneumonia")
        ##Extracted data of selected outcome
        dfout <- df[, c("hospital", "state", outcome)]
        ##convert outcome to numeric
        dfout[, 3] <- as.numeric(as.character(dfout[, 3]))
        ##Delete hospitals with NAs
        good <- complete.cases(dfout)
        dfgood <- dfout[good, ]
        ##Sort data
        rank <- dfgood[order(dfgood[, 2], dfgood[, 3], dfgood[, 1]), ]
        ##Split the data.frame by states
        statelist <- split(rank, rank$state)
        #### Return a data frame with the hospital names and the state name
        ##count the number of states
        statename <- unique(dat[, 7])  
        statenum <- length(statename)
        ##make a for loop
        hosrank <- data.frame()
        for(i in 1:statenum) {
                ##extract the data.frame from state[i]
                ranki <- statelist[[i]]
                ##count row numbers
                nrowi <- nrow(ranki)
                ##Assign the row of interest to a vector rowi
                if(num == "best") {
                        rowi <- ranki[1, 1:2]
                } else {
                        if(num == "worst") {
                                rowi <-ranki[nrowi, 1:2]
                        } else {
                                if(num > nrowi) {
                                        rowi <- data.frame(hospital = NA, state = ranki[[1, 2]])
                                } else {
                                        rowi <- ranki[num, 1:2]
                                }
                        }
                }
                hosrank <- rbind(hosrank, rowi)
        }
        hosrank
}