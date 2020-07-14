#***This code implements §4 of the DSS paper:
#***Peak Cubes in Service Operations: Bringing Multidimensionality into Decision Support Systems
#***Note "SQI" in this code corresponds to "SQF" in the paper;
#***We used SQI here given that we were simulating the service database of a company we were working with;
#***in their service database, they showed service failures with different SQIs (Service Quality Indices)


#Libraries:
library(dplyr)
library(readr)

#***#############################
#***Create Embedded Patterns Module: 
#***implementing CreateEmbeddedPatterns algorithm,
#***See §4.1 in the paper for detailed a explanation
#***#############################
CreateEmbededPattern <- function(nPattern,minPatternSize,patternSQIMax,hiRegimeUpperBound){
  
  #Reminder:
  #minPatternSize is the MINIMUM number of Non-Zero SQIs in the churn pattern (e.g., between 2 to 5).
  #PatternSQIMax is the Max of each Non-Zero SQI in the churn pattern.
  embeddedPattern <- data.frame(
    SQI = c("SQI1","SQI2","SQI3","SQI4","SQI5"),
    Regime = c(0,0,0,0,0), 
    stringsAsFactors = FALSE
  );
  
  #Fix each embedded pattern purely by chance
  for(i in c(1:nPattern)) 
  {
    patternSize<-sample(c(minPatternSize:5),1);
    
  #Pick a RANDOM service failure value for the RANDOMLY selected number above, 
  #and the remaining ones (5-patternSize) will naturally be zero, 
  #Then shuffle the combination. 
    pattern <- sample(c(sample(1:patternSQIMax,patternSize,replace = TRUE), rep(0, 5- patternSize)),5);
    patternName <- paste0("Pattern",i);
    embeddedPattern[[patternName]] <-pattern;
  }
  
  #Reminder: we have three failure regimes. Regardless of churn or not, 
  #whether a SQI is zero or non-zero is determined by the regime it belongs to. 
  #It is easy to establish the range each regimen covers. 
  #Here is how it's done in the paper:
  hiRegimeLowerBound<-hiRegimeUpperBound-0.2
  medRegimeUpperBound<-hiRegimeLowerBound
  medRegimeLowerBound<-medRegimeUpperBound-0.2
  loRegimeUpperBound<-medRegimeLowerBound
  loRegimeLowerBound<-0

  #20% of (e.g., 1 out of 5) SQIs belongs to high-failure regime, 
  #60% of (e.g., 3 out of 5) SQIs belong to med-failure regime, 
  #and 20% of (e.g., 1 out of 5) SQIs belong to low-failure regime; 
  #Even here, the SQIs and probablities are selected RANDOMLY based on the regimes they are assigned to.
  
  highFailure1<-runif(1,min=hiRegimeLowerBound,max=hiRegimeUpperBound);
  mediumFailure1<-runif(1,min=medRegimeLowerBound,max=medRegimeUpperBound);
  mediumFailure2<-runif(1,min=medRegimeLowerBound,max=medRegimeUpperBound);
  mediumFailure3<-runif(1,min=medRegimeLowerBound,max=medRegimeUpperBound);
  lowFailure1<-runif(1,min=loRegimeLowerBound,max=loRegimeUpperBound);
  
  #Let's again rely on randomness:
  #The above random numbers are randomly assigned to random SQIs  
  regime<- sample(c(highFailure1,mediumFailure1,mediumFailure2,mediumFailure3,lowFailure1),5);
  
  embeddedPattern$Regime <- regime;
  
  return(embeddedPattern);
  }
#***#############################
#***End of Embedded Patterns Module
#***#############################



#***#############################
#***Create RANDOM patterns for nonChurners
#***Implementing the NonChurnServiceFailures algorithm
#***See §4.2 in the paper for detailed a explanation
#***#############################
nonChurnSQI <- function(SQIDF,nPattern,minPatternSize)
{
  #Note that that there is no upper limit for a SQL,
  #e.g., the SQI could be 100% (i.e., 10 out of 10).
  SQIDF$rand<- runif(nrow(SQIDF), min = 0, max = 1);
  SQIDF$result<-sample(1:10,nrow(SQIDF), replace = TRUE);
  SQIDF$result <- ifelse(SQIDF$rand> SQIDF$Regime, 0,SQIDF$result)
  
  
  #Below we implement lines (8) and (9) in §4.2 in the paper
  nonChurnMaxPatternSize<-minPatternSize-1;
  nonZero=length(SQIDF$result[SQIDF$result>0]);
  if (nonZero> nonChurnMaxPatternSize){
      deltaSize=nonZero-nonChurnMaxPatternSize;
      nonZeroSQI=SQIDF$SQI[SQIDF$result>0];
      randomSQI=nonZeroSQI[sample(1:length(nonZeroSQI),deltaSize)]
      SQIDF$result[SQIDF$SQI %in% randomSQI ] <- 0
  }
  
    return(SQIDF$result)
}
#***#############################
#***End of Random patterns for nonChurners
#***#############################


#***#############################
#***Create RANDOM SQI patterns for REMAINING SQIS of an embedded churn pattern
#***This is to ensure randomness even for those remaining SQIs!
#***Again, for those remaining non-zero SQIs, they could be between 0% to 100%
#***#############################
ChurnRemainingSQI <- function(SQIDF)
{
  SQIDF$rand<- runif(nrow(SQIDF), min = 0, max = 1);
  SQIDF$result<-sample(1:10,nrow(SQIDF), replace = TRUE);
  SQIDF$result <- ifelse(SQIDF$rand> SQIDF$Regime, 0,SQIDF$result)
  return(SQIDF$result)
}
#***#############################
#***End of ChurnRemainingSQI
#***#############################


#***#############################
#***Create CreateCustomers Module
#***Implementing the CreateCustomers algorithm in the paper
#***See §4.3 in the paper for detailed a explanation of the module and input parameters
#***#############################
createCustomers <- function(nPattern,nCustomer,nWeek,calibrationWindow,nChurnRate, minPatternSize,patternSQIMax,churnThChurners,churnThNonChurners,churnWhenPatternSeen,nonChurnerSeesChurnPattern,hiRegimeUpperBound){

#Use the input parameters to create embedded patterns:
embeddedPattern <- CreateEmbededPattern(nPattern,minPatternSize,patternSQIMax,hiRegimeUpperBound);
  
  #Establish the base for the customer table
  nChurn <- round(nChurnRate*nCustomer,0) #number of churners
  customerDataWeek0<-data.frame(
    customerID=rep(c(1:nCustomer),1)
  ) %>% arrange(customerID) %>% group_by(customerID) %>%
    mutate(weekID =0, 
           SQI1=0.1,
           SQI2=0.1,
           SQI3=0.1,
           SQI4=0.1,
           SQI5=0.1,
           churn=0);
  
  customerData <- data.frame(
    customerID=rep(c(1:nCustomer),nWeek )
  ) %>% arrange(customerID) %>% group_by(customerID) %>%
    mutate(weekID = row_number(), 
           SQI1=0,
           SQI2=0,
           SQI3=0,
           SQI4=0,
           SQI5=0,
           churn=0) 
  
  customerData <- rbind(customerDataWeek0,customerData) %>% arrange(customerID,weekID)
  

  #First, let's simulate our churners
  for (cus in 1:nChurn){
      for (week in 1:nWeek){
        if (week<=calibrationWindow){
      
        selectedSQI<-embeddedPattern$SQI;
        customerData[customerData$customerID==cus & 
                       customerData$weekID==week,selectedSQI] <- nonChurnSQI(embeddedPattern,nPattern,minPatternSize);
      }
      
      else if (runif(1, min = 0, max = 1) >churnThChurners){
        selectedSQI<-embeddedPattern$SQI;
      customerData[customerData$customerID==cus & 
                     customerData$weekID==week,selectedSQI] <- nonChurnSQI(embeddedPattern,nPattern,minPatternSize);
      }
      
      else{
        
        selectedPattern=sample(1:nPattern,1); #Randomly select a churn pattern 
        
        embeddedPattern$Pattern <- embeddedPattern[[paste0("Pattern",selectedPattern)]];
        churnSQI<-embeddedPattern[embeddedPattern$Pattern>0,];
        customerData[customerData$customerID==cus & 
                       customerData$weekID==week,churnSQI$SQI ] <- churnSQI$Pattern;
      
        actualChurnRandom <- runif(1, min = 0, max = 1)
        if(actualChurnRandom<=churnWhenPatternSeen){
          
          churnDay<-0#churn on the same week you can replace it with churnDay<- sample(0:3,1); in this case he churns in the same week or within 3 weeks after
          if (churnDay>0){
            
            for (i in 1:churnDay){
              
              selectedSQI<-embeddedPattern$SQI;
              customerData[customerData$customerID==cus & 
                             customerData$weekID==(week+i),selectedSQI] <- nonChurnSQI(embeddedPattern,nPattern,minPatternSize);
            }
          }
          customerData$churn[customerData$customerID==cus & 
                               customerData$weekID==(week+churnDay)] <- 1
          # remove other days for customer
          customerData <- customerData[!(customerData$customerID==cus &
                                           customerData$weekID > (week+churnDay)),]
          # break from week (inner loop)
          break
        }
      } 
    }
  }
  #Now let's simulate nonChurners
  for (cus in (nChurn+1):nCustomer){
     for (week in 1:nWeek){
      xxx<-runif(1, min = 0, max = 1);
      if(xxx<nonChurnerSeesChurnPattern){
        #***Potential NOISE:It is likely that a non-churner experience an embedded churn pattern-- but he does not churn***
        selectedPattern=sample(1:nPattern,1); 
        embeddedPattern$Pattern <- embeddedPattern[[paste0("Pattern",selectedPattern)]];
        churnSQI<-embeddedPattern[embeddedPattern$Pattern>0,];
        customerData[customerData$customerID==cus & 
                       customerData$weekID==week,churnSQI$SQI] <- churnSQI$Pattern;
        #fill the remainig SQIs randomly
        remainingSQI <- embeddedPattern[embeddedPattern$Pattern==0,];
        customerData[customerData$customerID==cus & 
                       customerData$weekID==week,remainingSQI$SQI ] <- ChurnRemainingSQI(remainingSQI);
      }
      else{
        selectedSQI<-embeddedPattern$SQI;
        customerData[customerData$customerID==cus & 
                       customerData$weekID==week,selectedSQI ] <- nonChurnSQI(embeddedPattern,nPattern,minPatternSize);# in this case he will see any pattern but the ones for embedded churn ones
          if(week>calibrationWindow & runif(1, min = 0, max = 1)< churnThNonChurners ){
          #***NOISE:It is likely that a non-churner churns (without seeing an embedded churn pattern***
          
          customerData$churn[customerData$customerID==cus & 
                               customerData$weekID==(week)] <- 1;
          
          # remove other days for customer
          customerData <- customerData[!(customerData$customerID==cus &
                                           customerData$weekID > (week)),];
          break;
        }
      }
    }
  } 
  
  result<- list("customerData"=customerData, "embeddedPattern"=embeddedPattern); 
  return(result);
}

#***#############################
#***End CreateCustomers Module
#***#############################


#***#############################
#***Sample code for running the above modules,
#***to randomly create customer bases...
#***#############################

#Set your output folder address below,
#Be careful how you set the parameter size; you don't want to run out of space on your PC ;)
output_path<-"C:/Users/X/your folder/" 

allRoundResult_minPatternSize<-data.frame()

#For sensitivity analysis on pattern size; i.e. number of non-zero SQIs in churn pattern:
#Run the following code for different minPatternSizes! See §4.1 in the paper for detailed a explanation
for (minPatternSize in 2:5){
  
for (i in 1:1000){#1000 is the number of customer bases! Each base can contain billions of customers!  
  
  #***Below you can set different parameters
  #For each customer base, create a number of RANDOM churn patterns!
  #If you set this parameter to 100, the code will create 100 random patterns for each base
  #You can also set it to random number of patterns: sample(c(30:100),1);
  #Note that these patterns will be saved on your PC in a CSV file!
  RandomPattern<-100
  PatternSQIMax=sample(c(2,4,6,8,10),1);
  ChurnThChurners=0.05
  ChurnThNonChurners=sample(c(0.0023,0.0045,0.0068,0.0091, 0.0114),1);#The probability that a Nonchurner churns is randomly picked from  10%/44 to 50%/44.
  ChurnWhenPatternSeen=sample(c(0.5,0.6,0.7,0.8,0.9,1),1)#the probability thereshold that a churners CHURNS whe she sees a churn pattern is randomly picked between 50% to 10%.
    NonChurnerSeesChurnPattern=sample(c(0.0023,0.0045,0.0068,0.0091, 0.0114),1);#The probability that a Nonchurner churns is randomly picked from  10%/44 to 50%/44.
  HiRegimeUpperBound=sample(c(0.50,0.55,0.60,0.65,0.7),1)
  result<-createCustomers(RandomPattern,1000,52,8,0.4, minPatternSize,PatternSQIMax,ChurnThChurners,ChurnThNonChurners,ChurnWhenPatternSeen,NonChurnerSeesChurnPattern,HiRegimeUpperBound)
  customerData<-result$customerData
  currentEmbeddedPattern<-result$embeddedPattern
  
  #merge customer data
  customerData$Round<-i
  customerData$nPattern=RandomPattern
  customerData$minPatternSize<-minPatternSize
  customerData$patternSQIMax<-PatternSQIMax
  customerData$churnThChurners<-ChurnThChurners
  customerData$churnThNonChurners<-ChurnThNonChurners
  customerData$churnWhenPatternSeen<-ChurnWhenPatternSeen
  customerData$nonChurnerSeesChurnPattern<-NonChurnerSeesChurnPattern
  customerData$hiRegimeUpperBound<-HiRegimeUpperBound
  allRoundResult_minPatternSize<-bind_rows(allRoundResult_minPatternSize,customerData)
  
  # The randomly created patterns will be saved below:
  write_csv(currentEmbeddedPattern, paste0(output_path,"EmbeddedPattern_","Size=",minPatternSize,"_round=",i,".csv"))
}
}
write_csv(allRoundResult_minPatternSize, paste0(output_path,"YourSimulation.csv"))

