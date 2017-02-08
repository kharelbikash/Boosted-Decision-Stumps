DecisionStump <- function(Xvalue,RvalueLocal){
   Regionrm <- function(s){
    sumlessthan <- numlessthan <-numgreaterthan<-sumgreaterthan<-meanYlessthan<-meanYgreatethan <- 0
     #caclulating regions R
    for(i in 1:nrow(Xvalue)){
      #computing in each possible s
      if (Xvalue$rm[i] < s)
      {
        sumlessthan = sumlessthan+ RvalueLocal[i]
        numlessthan =numlessthan+1
      }
      if(Xvalue$rm[i]>s){
        sumgreaterthan = sumgreaterthan+ RvalueLocal[i]
        numgreaterthan = numgreaterthan+1
      }
      
    }
    meanYlessthan <- sumlessthan/numlessthan
    meanYgreatethan <- sumgreaterthan/numgreaterthan
    
    sumlessthanSquare <- sumgreaterthanSquare <- 0
    for(i in 1:nrow(Xvalue)){
      if (Xvalue$rm[i] < s )
      {
        sumlessthanSquare <- sumlessthanSquare + (RvalueLocal[i]-meanYlessthan)^2
      }
      if(Xvalue$rm[i] > s){
        sumgreaterthanSquare <-  sumgreaterthanSquare+(RvalueLocal[i]-meanYgreatethan)^2
      }
      
    }
    
    RSS <- sumlessthanSquare + sumgreaterthanSquare
    return(RSS)
    
  }
  Regionlstat <- function(s) {
    sumlessthan <- numlessthan <-numgreaterthan<-sumgreaterthan<-meanYlessthan<-meanYgreatethan <- 0
    #caclulating regions R
    for(i in 1:nrow(Xvalue)){
      #computing in each possible s
      
      if (Xvalue$lstat[i] < s)
      {
        sumlessthan = sumlessthan+ RvalueLocal[i]
        numlessthan =numlessthan+1
      }
      if(Xvalue$lstat[i]>s){
        sumgreaterthan = sumgreaterthan+ RvalueLocal[i]
        numgreaterthan = numgreaterthan+1
      }
      
    }
    meanYlessthan <- sumlessthan/numlessthan
    meanYgreatethan <- sumgreaterthan/numgreaterthan
    sumlessthanSquare <-  sumgreaterthanSquare <- 0
    for(i in 1:nrow(Xvalue)){
      if (Xvalue$lstat[i] < s )
      {
        sumlessthanSquare <- sumlessthanSquare + (RvalueLocal[i]-meanYlessthan)^2
      }
      if(Xvalue$lstat[i] > s){
        sumgreaterthanSquare <-  sumgreaterthanSquare+(RvalueLocal[i]-meanYgreatethan)^2
      }
      
    }
    RSS <- sumlessthanSquare + sumgreaterthanSquare
    
    return(RSS)
  }
  RssArraylstat <- matrix(nrow = nrow(Xvalue),ncol=1,byrow= TRUE)
  RssArrayrm <- matrix(nrow = nrow(Xvalue),ncol=1,byrow= TRUE)
  for(i in 1:nrow(Xvalue)){
    slstat <- Xvalue$lstat[i]
    RssArraylstat[i] <- Regionlstat(slstat)
    srm <- Xvalue$rm[i]
    RssArrayrm[i]<- Regionrm(srm)
  }
  returnArray = c()
  if(min(RssArraylstat) >min(RssArrayrm)){
    sIndex =which.min(RssArrayrm)
   # print(c("trainMSE:",min(RssArrayrm)/nrow(Xvalue)))
    S <- Xvalue$rm[sIndex]
    sumlessthan <- numlessthan <-numgreaterthan<-sumgreaterthan<-meanYlessthan<-meanYgreatethan <- 0
    #caclulating regions R
    for(i in 1:nrow(Xvalue)){
      #computing in each possible s
      if (Xvalue$rm[i] < S)
      {
        sumlessthan = sumlessthan+ RvalueLocal[i]
        numlessthan =numlessthan+1
      }
      if(Xvalue$rm[i]>S){
        sumgreaterthan = sumgreaterthan+ RvalueLocal[i]
        numgreaterthan = numgreaterthan+1
      }
      
    }
    meanYlessthan <- sumlessthan/numlessthan
    meanYgreatethan <- sumgreaterthan/numgreaterthan
    returnArray =c("rm",S,meanYlessthan,meanYgreatethan)
    #return(returnArray)
  }
  else{
    sIndex =which.min(RssArraylstat)
    S<- Xvalue$lstat[sIndex]
    sumlessthan <- numlessthan <-numgreaterthan<-sumgreaterthan<-meanYlessthan<-meanYgreatethan <- 0
     #caclulating regions R
    for(i in 1:253){
      #computing in each possible s
      if (Xvalue$lstat[i] < S)
      {
        sumlessthan = sumlessthan+ RvalueLocal[i]
        numlessthan =numlessthan+1
      }
      if(Xvalue$lstat[i] >S){
        sumgreaterthan = sumgreaterthan+ RvalueLocal[i]
        numgreaterthan = numgreaterthan+1
      }
      
    }
    meanYlessthan <- sumlessthan/numlessthan
    meanYgreatethan <- sumgreaterthan/numgreaterthan
    returnArray= c("lstat",S,meanYlessthan,meanYgreatethan)
    #return(returnArray)
  }
  
  
  }
  

#///////////////

BDS <- function(B){
  
  library(ISLR)
  library(MASS)
  library(tree)
  data("Boston")
  set.seed(0790)
  train <- sample(1:nrow(Boston),nrow(Boston)/2)
  trainData <- Boston[train,]
  test <- Boston[-train,]
  rTrain <- trainData$medv;
  
  DS <- matrix(nrow=B,ncol=4,byrow = TRUE )
 phat<- matrix(nrow=B,ncol=nrow(trainData),byrow = TRUE)
   for(i in 1:B){
    DS[i,]<- DecisionStump(trainData,rTrain)
      for(j in 1:nrow(trainData)){
          if(DS[i,1]=="lstat"){
            lstatS <- as.numeric(DS[i,2])
            lstatMeanLessthan <-as.numeric(DS[i,3])
            lstatMeanMorethan <- as.numeric(DS[i,4])
              if(trainData$lstat[j] < lstatS){
                phat[i,j] <- lstatMeanLessthan
                
              }else{
                phat[i,j] <- lstatMeanMorethan
              }
            
          }
          ###
        else if(DS[i,1]=="rm"){
          rmS <- as.numeric(DS[i,2])
          rmMeanLessthan <-as.numeric(DS[i,3])
          rmMeanMorethan <- as.numeric(DS[i,4])
          if(trainData$rm[j] < rmS){
            phat[i,j] <- rmMeanLessthan
            
          }else{
            phat[i,j] <- rmMeanMorethan
          }
          
        }
        ###
        rTrain[j] <- rTrain[j]-0.01*phat[i,j]
        
      }
    print(DS[i,])
   }

 

  
 #MSE
squareDifference <- 0
  for(i in 1:nrow(test)){
    predictionRule = sum(0.01*phat[,i])
    #print(phat[,i])
    squareDifference <- squareDifference+(test$medv[i]- (predictionRule))^2
    }
print(c("test MSE:" ,squareDifference/(nrow(test))))

}