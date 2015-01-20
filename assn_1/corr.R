corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    #get files
    files_list <- list.files(directory, full.names = TRUE)
    
    #get number of complete cases data frame
    complete_cases <- complete(directory)
    
    #get complete cases above threshold
    complete_cases_above_threshold <- complete_cases[which(complete_cases$nobs > threshold),]
    
    #create correlation numberic vector
    correlations <- numeric(0)
    
    for (id in complete_cases_above_threshold$id){
        #get current case
        case <- read.csv(files_list[id])
        
        #get nitrate ans sulfate values for case
        nitrate <- case$nitrate
        sulfate <- case$sulfate
        
        #calulate correllation
        correlation <- cor(nitrate, sulfate, use = "pairwise.complete.obs")
        
        #append current case correlation to list of correlations
        correlations <- c(correlations, correlation)
    }
    #return correlations
    correlations
     
}
