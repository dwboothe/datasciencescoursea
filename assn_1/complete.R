complete <- function(directory, id = 1:332){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    #store id value in new variable to use variable name
    ids <- id
    
    #get files
    files_list <- list.files(directory, full.names = TRUE)
    
    #generate id and nobs vector
    id <- integer(0)
    nobs <- integer(0)
    
    for(value in ids){
        case <- read.csv(files_list[value])
        #get complete cases
        is_complete <- complete.cases(case)
        
        #get number of complete cases
        number_complete <- nrow(case[is_complete,])
        
        #append values to respective vectors
        id <- c(id, value)
        nobs <- c(nobs, number_complete)
    }
    
    #create and return complete data frame
    complete <- data.frame(id, nobs)
    complete
}