rankall <- function(outcome, num = "best") {
        ## Read outcome data
        outcomeData <- read.csv("outcome-of-care-measures.csv",colClasses="character")
        
        ## Check that state and outcome are valid
        ##State_list <- outcomeData[,7]
        State_list <- state.abb
        Outcomes<- c("Heart Attack","Heart Failure","Pneumonia","heart attack","heart failure","pneumonia")
        oc <- ""
        ##oc_check <- outcome
        ##Outcome_list <- names(outcome)
       ## if (missing(state))
                ##stop("Need to specify 2 Letters State Code.")
        
        if (missing(outcome))
                stop("Need to specify outcome.")
        
        ##if (state%in%State_list) {stateCode <- state}
        ##else{
                ##stop("invalid state.")
       ## }        
        
        if (any(outcome==Outcomes)) {oc <- outcome}
        else{
                stop("invalid outcome.")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        
        ##decide what outcome to subset, only 11,17,23 col will be subseted in new dataframe        
        for (l in 1:length(Outcomes))
        {
                if (oc==Outcomes[l]){n <- l}
        } 
        ha <- c(1,4)
        hf <- c(2,5)
        p <- c(3,6)
        if(n %in% ha){k <- 11}
        if(n %in% hf){k <- 17}
        if(n %in% p){k <- 23}
        
        
        #print(n)
        #print(stateCode)
        ##print(k)
        newData <- outcomeData[,c(2,7,k)] ##subset with hospital name, state name, and outcome
        
        ##head(newData)
      Rank_ls <- data.frame()
       for (stateCode in  State_list){
       s <- split(newData,newData$State) ##split new data with state name-stateCode.
        ##head(s)
        dd <- data.frame(s[stateCode]) ##subset s and conver to dataframe in dd
        colnames(dd) <- c("a","b","z") ##change dd's colname to a b z for easy sorting
        dd[,3] <- as.numeric(dd[,3])
        ##class(d)
        ##head(d)
        ##class(d)
        ##newNameList <- names(d)
        ##z <- newNameList[3]
        ##best_H<- d[with(d, order(z)), ]
        ##best<- dd[with(dd, order(z,a)), ] ##sort dd with z col and assign to best
        r_c <- dim(dd)
        limit <- r_c[1]
        ## while(num =="best"){r <- 1
        ## sortdata<- dd[ order(dd[,3], dd[,1]), ]
        ##rank <- sortdata[r,1]
        ## }
        ##while(num < limit){
        r <- as.numeric(num)
        sortdata<- dd[ order(dd[,3], dd[,1]), ]
        rank <-data.frame(sortdata[r,1:2])
        ##                 }
        
        if(num =="worst"){r <- 1
                          rsortdata<- dd[ order(-dd[,3], dd[,1]), ]
                          rank <-data.frame( rsortdata[r,1:2])
                          
                        }
        ## while(num > limit){
        
        ## rank <- NA
        ## }
        ##best
        ##head(best_H)
        
        
        Rank_ls <- c(Rank_ls,rank)
       }
      colnames(Rank_ls) <- c("hospital","State")
      Rank_ls
        
}