rankhospital <- function(state, outcome, num="best") {
        
        # This function ranks the hospitals of a given state in terms of either a) heart attack,
        # b) heart failure, or c) pneumonia mortality rates. Then returns the name of the hospital
        # which ranked in the position that the user requested.
        
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
        
        
        # sorting the data frame by the given outcome and assigning the Rank column.
        if (outcome=="heart attack"){
                ordf <- cleandf[order(cleandf$"heart attack", cleandf$"Hospital Name"),]
                l <- nrow(ordf)
                ordf["Rank"] <- 1:l
                
        }
        
        if (outcome=="heart failure"){
                ordf <- cleandf[order(cleandf$"heart failure", cleandf$"Hospital Name"),]
                l <- nrow(ordf)
                ordf["Rank"] <- 1:l
        }
        
        if (outcome=="pneumonia"){
                ordf <- cleandf[order(cleandf$"pneumonia", cleandf$"Hospital Name"),]
                l <- nrow(ordf)
                ordf["Rank"] <- 1:l
        }
        
        # returning the name of the hotel that ranks in the requested position
        # The "best" and "worst" arguments are acceptable.
        if (num=="best"){
                ordf[1, "Hospital Name"]
        }
        
        else if (num=="worst"){
                finaldf <- na.omit(ordf)
                l <- nrow(finaldf)
                finaldf[l, "Hospital Name"]
        }
        
        else{
                ordf[ordf$Rank==num, "Hospital Name"]
                
        }
        
}