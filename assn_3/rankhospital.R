rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcomes_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state and outcome are valid
    
    # uppercase coversion to accept lowercase values 
    state <- toupper(state)
    
    # check state validity
    states <- unique(outcomes_data$State)
    if (!(toupper(state) %in% states)) { 
        # return error msg
        stop("invalid state")
    }
    
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
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    
    # create subset of hospitals in specified state
    hospitals_in_state <- outcomes_data[which(outcomes_data$State == state),]
    
    # set column name
    diseases_col_name <- names(hospitals_in_state)[diseases_col]
    hospital_name_col <- "Hospital.Name"
    
    # order these hospitals by the lowest specfic diseases rate and by the hospital name 
    index <- order(hospitals_in_state[,diseases_col_name], hospitals_in_state[, hospital_name_col], na.last = NA)
    rankhospitals <- hospitals_in_state[index,]
    
    # calculate total number of hospitals in the state
    total_rankhospitals <- nrow(rankhospitals)
    
    # set numeric value of num 
    if(num == "best"){
        num <- 1
    } else if (num == "worst"){
        num <- total_rankhospitals
    } else if (num > total_rankhospitals){
        return(NA) 
    }
    
    # return Hospital according to its rank
    return(rankhospitals[num, hospital_name_col])
}