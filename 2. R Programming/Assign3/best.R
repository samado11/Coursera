best <- function(state, outcome){
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
                cal_num <- as.numeric(category[category[,1]==outcome, 2])
        ## Return hospital name in that state with lowest 30-day death
                cal <- dataset[dataset$State==state, c(2, 11, 17, 23)]
                for(i in 2:4){
                        suppressWarnings(cal[, i] <- as.numeric(cal[, i]))
                }
                
                cal <- cal[complete.cases(cal[, cal_num]), c(1, cal_num)]
                cal <- cal[order(cal[, 2], decreasing = F), ]
                return(cal[1,1])
        }
        
        
        
        ## rate
}