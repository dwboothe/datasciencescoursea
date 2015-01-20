pollutantmean <- function(directory, pollutant, id = 1:332){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    
    #get files
    files_list <- list.files(directory, full.names = TRUE)
    
    #generate data frame
    data <- data.frame()
    for(value in id){
        #append files together for specified range
        data <-rbind(data, read.csv(files_list[value]))
    }
    #filter data to specific pollutant
    pollutant_data <- data[, pollutant]
    
    #calculate mean of pollutant
    pollutant_mean <- round(mean(pollutant_data, na.rm = TRUE),3)
    
    #return pollutant mean
    pollutant_mean
}