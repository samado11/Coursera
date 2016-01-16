corr <- function(directory, th=NULL){
  #load csv files
  if(is.null(th)){
      th <- 0
  }
  if(directory=="specdata"){
    files <- dir(directory, pattern='\\.csv', full.names = T)
    
    output <- c()
    for(i in 1:332){
      dataset <- read.csv(files[i]) 
      dataset <- dataset[complete.cases(dataset),]
      if(length(dataset[,1]) >= th){
        output <- c(output, cor(dataset[,2], dataset[,3]))
        }  
     }
      
    options(digits=4)
    return(output)
  }
    else{
      print("Invalid direcotry name")
    }  
} 