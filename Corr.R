corr <- function(directory, threshold = 0) {
                
                k <- 1:332
                Test_cor <- c()
                for (i in k){
                                check<- complete(directory,i)
                                case <- check[1,2]
                                
                                 ##check for data big than threshold and calculate cor##
                                if (case > threshold){
                                 ##print(case)## 
                        
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
                                                        good<- complete.cases(pollutantData) ##remove NA values from slide 14/14
                                                        cl_pol <- pollutantData[good,] 
                                                        x <- c(cl_pol[,2])
                                                        y <- c(cl_pol[,3])
                                                        Test_cor <-c(Test_cor,cor(x,y))
                                                        ##fin_cor <- c(fin_cor,Test_cor)
                                                        #print(Test_cor)
                       
                                                        }
      
                                
                         }
        
        
Test_cor  
}