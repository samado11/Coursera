complete <- function(directory, id){

  
  if(directory=="specdata"){
    #load csv files
    files <- dir(directory, pattern='\\.csv', full.names = T)
    result <- data.frame()
    for (i in id){
      id <- i
      dataset <- read.csv(files[i], header=T)
      nobs <- length(dataset[complete.cases(dataset),1])
      arrange <- c(id, nobs)
      result <- rbind(result, arrange)    
    }  
    names(result) <- c("id", "nobs")
    print(result)
  }
 else{
   print("Invalid directory name")
 }
} 

