rankall <- function(outcome, num="best") {
        ## Read outcome data
        dataset <- read.csv("outcome-of-care-measures.csv", colClasses = "character", stringsAsFactors=F)
        category <- matrix(c("heart attack", "heart failure", "pneumonia", 11, 17, 23), 3, 2)
        
        ## Check that state and outcome are valid
        if(!(outcome %in% category[, 1])){
                stop("invalid outcome")
                
        }
        else{       
                cal_num <- as.numeric(category[category[, 1]==outcome, 2])
                for(i in c(11, 17, 23)){
                        suppressWarnings(dataset[, i] <- as.numeric(dataset[, i]))
                }
                #cal <- dataset[, c(2, 7, 11, 17, 23)]
                
                state.name <- unique(dataset$State) #state name without duplication
                state.name <- state.name[order(state.name, decreasing=F)]
               
                result.list <-list()
                for(i in 1:54){
                        temp <- NULL
                        temp <- dataset[dataset$State==state.name[i] , c(2, 7, cal_num)]
                        temp <- temp[complete.cases(temp[,3]), ]
                        temp <- temp[order(temp[, 1], decreasing = F), ] #text ordering
                        temp <- temp[order(temp[, 3], decreasing = F), ] #rate ordering
                        result.list[[i]] <- temp
                }
                result <- data.frame("<NA>", state.name)
                result[, 1] <- as.character(result[, 1])
                names(result) <- c("hospital", "state")
                #print(class(result[, 1]))
                if(num=="best"){
                        for(i in 1:54){
                                result[i, 1] <- result.list[[i]][1, 1]
                        }
                }
                else if(num=="worst"){
                        for(i in 1:54){
                                result[i, 1] <- result.list[[i]][length(result.list[[i]][, 1]), 1]
                        }
                }
                
                else{
                        num <- as.numeric(num)
                        #print(result.list[[54]][num, 1])
                        for(i in 1:54){
                                result[i, 1] <- result.list[[i]][num, 1]
                        }
                        
                }
                
        }
        return(result)     
}