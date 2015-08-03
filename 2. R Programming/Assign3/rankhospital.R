rankhospital <- function(state, outcome, num="best") {
        ## Read outcome data
        dataset <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        category <- matrix(c("heart attack", "heart failure", "pneumonia", 2, 3, 4), 3, 2)
                      
        ## Check that state and outcome are valid
        if(!(outcome %in% category[, 1])){
                stop("invalid outcome")
                
        }
        else if(!(state %in% dataset$State)){
                stop("invalid state")
                
        }
        else{       
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
                cal_num <- as.numeric(category[category[, 1]==outcome, 2])
                cal <- dataset[dataset$State==state, c(2, 11, 17, 23)]
                for(i in 2:4){
                        suppressWarnings(cal[, i] <- as.numeric(cal[, i]))
                }
                
                cal <- cal[complete.cases(cal[, cal_num]), c(1, cal_num)]
                cal <- cal[order(cal[, 1], decreasing = F), ] #text ordering
                cal <- cal[order(cal[, 2], decreasing = F), ] #rate ordering
                                
                if(num=="best"){
                        return(cal[1, 11])
                }
                else if(num=="worst"){
                        return(cal[length(cal[, 1]), 1])
                }
                
                else{
                        num <- as.numeric(num)
                        if(num <= length(cal[, 1])){
                                return(cal[num, 1]) 
                        }
                        else{
                                return(NA)
                        }
                }
                
        }
        
}