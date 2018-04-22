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