complete <- function(directory, id = 1:332) 
        {
        ## decide the file name of csv file with one "0" or two "00" at the from. 
        nobs <-c()
        
        for (i in id){
                ##id is numernic, so I need to add "00" to match the file name below 100 
                if(i<10){
                        cid <- paste(c("00",i),collapse="")
                        
                }else {
                        if(i>=10 && i<=99){
                                cid <- paste(c("0",i),collapse="")
                                
                        }else{ cid <- paste(c(i),collapse="")}
                }
        ##assign file name with full directory
        filename <-paste(c(directory,"/",cid,".csv"), collapse = "") ##get work directory to add infront of target folder  
        pollutantData <- read.csv(filename, header = TRUE, sep = ",", quote = "",dec = ".", fill = TRUE, comment.char = "") ##read csv data into a df 
        ##print(names(pollutantData))
        ##print(nrow(pollutantData))
        ##polRow <- nrow(pollutantData)
        ##print(ncol(pollutantData))
        good<- complete.cases(pollutantData) ##remove NA values from slide 14/14
        cl_pol <- pollutantData[good,] 
        ##print(pollutantData[2:5,2:3]) ##Test for subseting a Datafram
        ##subset pollutantData or summary pollutantData
        ##   print (class(pollutantData)) 
        ##   print(filename) ## test for correct ID read
        ##   print (summary(pollutantData))
        
        nobs <-c(nobs,nrow(cl_pol))
        }
##print (nobs)
##print(id)
complete_case <- cbind(id,nobs)
complete_fin <- data.frame(complete_case)

complete_fin
##class(complete_fin)
     ##summary(pollutantData)  
}