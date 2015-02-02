rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomes_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    
    # check outcome validity
    outcomes <-setNames(c(11, 17, 23), c("heart attack", "heart failure", "pneumonia"))
    if (outcome %in% names(outcomes)){
        # get diseases column
        diseases_col <- outcomes[[outcome]]
        
        # convert rate column to numeric values
        outcomes_data[, diseases_col] <- as.numeric(outcomes_data[, diseases_col]) 
        
    } else {
        # return error msg
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    
    # get states
    states <- unique(outcomes_data$State)
    
    # initate state and hospital character vectors
    state <- character()
    hospital <- character()
    
    # populate state and hospital character vectors
    for(s in states){
        state <- c(state, s)
        hospital <- c(hospital, rankhospital(s, outcome, num))
    }
    
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    # create rankall data frame
    rankall <- data.frame(hospital, state) 
    
    # return rankall data frame 
    return (rankall[order(rankall$state),])
}