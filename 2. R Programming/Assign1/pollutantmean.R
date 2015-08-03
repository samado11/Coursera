
pollutantmean <- function(directory, pollutant, id=1:332){
 
  #load csv files
  if(directory=="specdata"){
    files <- dir(directory, pattern='\\.csv', full.names = T)
    dataset <- data.frame()
    for (i in id){
      dataset <- rbind(dataset, read.csv(files[i]))
    }
    
    #Calculate mean
    if(pollutant=="sulfate"){
      print(mean(dataset[,2], na.rm=T), digits=4)
    }
    else if(pollutant=="nitrate"){
      print(mean(dataset[,3], na.rm=T), digits=4)
    }
    else{
      print("Incorrected Pollutant name")
    }  
  }
  else{
    print("unvalid directory name")
  }
}