best <- function(state, outcome) {
        
        # This function prints out the best hospital of a specific state in terms of either
        # a) heart attack, b) heart failure, or c) pneumonia mortality rates. Firstly, subsets the
        # initial data frame by the desired state, and then sorts (ascending) the subset by the
        # desired outcome column. That way, the first element of the final data frame will be the
        # best hospital.
        
        # reading the initial data frame with hospitals from all over the states
        df <- read.csv("outcome-of-care-measures.csv")
        
        # validating the arguments input in order to prevent user from entering typos or 
        # arguments for non existing data (e.g. non existing abbreviation for a state)
        if (!(outcome=="heart attack" || outcome=="heart failure" || outcome=="pneumonia")) {
                stop("invalid outcome")
        }
        
        if (!state %in% df$State) {
                stop("invalid state")
        }
        
        # subseting the data frame in order to keep only the hospitals of the desired state
        idf <- subset(df, df$State==state)
        
        # creating a "clean" data frame without the unnecessary columns and renaming the final
        # column names in order to be more easily readable. Finally, setting the content of the
        # "outcome" columns to be considered as numeric. Initially, this content is considered as
        # factor by R, since it contains both characters, NAs and numbers.
        cleandf <- data.frame(idf$"Hospital.Name", idf$"State", idf$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", idf$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", idf$"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
        colnames(cleandf) <- c("Hospital Name", "state", "heart attack", "heart failure", "pneumonia")
        
        cleandf$"heart attack" <- as.numeric(as.character(cleandf$"heart attack"))
        cleandf$"heart failure" <- as.numeric(as.character(cleandf$"heart failure"))
        cleandf$"pneumonia" <- as.numeric(as.character(cleandf$"pneumonia"))
        
        # sorting the data frame by the given outcome.
        if (outcome=="heart attack"){
                ordf <- cleandf[order(cleandf$"heart attack", cleandf$"Hospital Name"),]
        }
        
        if (outcome=="heart failure"){
                ordf <- cleandf[order(cleandf$"heart failure", cleandf$"Hospital Name"),]
        }
        
        if (outcome=="pneumonia"){
                ordf <- cleandf[order(cleandf$"pneumonia", cleandf$"Hospital Name"),]
        }
        
        # returning the name of the best hotel
        ordf[1, "Hospital Name"]
        
}