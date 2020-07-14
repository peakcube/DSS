#***This code implements §3 of the DSS paper:
#***Peak Cubes in Service Operations: Bringing Multidimensionality into Decision Support Systems
#***The code can construct peak cubes for every customer in every customer base (still you can control it)!
#***Note that "SQI" in this code corresponds to "SQF" in the paper;
#***We used SQI here given that we were simulating the service database of a company we were working with;
#***in their service database, they showed service failures with different SQIs (Service Quality Indices)

#Libraries:
library(readr);
library(rPref);


#***retrieve your subspace profiles; see Table 1 in §3.2 for example and explanation 
#***You can find an example for 5-dimensional subspace profiles in GitHub: "SupSpaceProfiles.csv"
subSpacesProfile<- read_csv("2_SupSpaceProfiles.csv");
#Note 1:SubSpaceProfiles are sorted on the LEVEL (Bottom-Up) see §3.2 in the paper to learn why
#Note 2: TimeSequence (which always starts from 1) maps to PSEL.INDICES 


#***Read you multivariate timeseries data 
#***Below we are reading what we have simulated:
#***Different customer bases. Each base has thousands of customers. Each customer has 5-dimensional timeseries!
#***If you are using this for your company, you will only have one customer base obviously!
wholeTimeSeriesData<- read.csv("YourSimulation.csv");


#Reminder:wholeTimeSeriesData above contains all multivariate service experience timeseries;
#for all customers in all customer bases (could be a very very large simulated data!)  
#To see how peak cube works, you can first control the scope of your experiment to a few customers in a few bases; again if you are using this for your company, you will only have one customer base obviously!
#You can also pick a specific minPatternSize for the simulation (See §4.1 in the paper) to compare the resulting peak cubes. Note that you won't need that if you are using your own data!
#For example, let's compute the peak cubes for:
wholeTimeSeriesData<-wholeTimeSeriesData[wholeTimeSeriesData$Round<=10 & wholeTimeSeriesData$customerID<=20 & wholeTimeSeriesData$minPatternSize==3, ]

#Initiate the cubes first:
completeCube<- data.frame(customerID=-1, 
                          Round=-1, 
                          TimeSequence=0, 
                          weekID=-1, 
                          SubSpace=0, 
                          BinaryCode=1000000000, 
                          Level=0,
                          minPatternSize=0,
                          patternSQIMax=0,
                          churnThChurners=0,
                          churnThNonChurners=0,
                          churnWhenPatternSeen=0,
                          nonChurnerSeesChurnPattern=0,
                          hiRegimeUpperBound=0)

peakCube<- data.frame(customerID=-1, 
                            Round=-1, 
                            TimeSequence=0, 
                            weekID=-1, 
                            SubSpace=0, 
                            BinaryCode=1000000000, 
                            Level=0,
                            minPatternSize=0,
                            patternSQIMax=0,
                            churnThChurners=0,
                            churnThNonChurners=0,
                            churnWhenPatternSeen=0,
                            nonChurnerSeesChurnPattern=0,
                            hiRegimeUpperBound=0)

#Loop through the bases and customers
for (customerIndex in unique(wholeTimeSeriesData$customerID))
{
  print(customerIndex);
  timeSeriesDataX<- wholeTimeSeriesData[wholeTimeSeriesData$customerID==customerIndex &
                                          wholeTimeSeriesData$weekID>=0,]
    for(roundIndex in unique(timeSeriesDataX$Round))
  {
    print(roundIndex);
    timeSeriesData<- timeSeriesDataX[timeSeriesDataX$Round==roundIndex,]
    
    #Retrieve Simulation Parameters for the current data in the loop (for later sensitivity analysis) 
    minPatternSizeX<-head(timeSeriesData$minPatternSize,1);
    patternSQIMaxX<-head(timeSeriesData$patternSQIMax,1);
    churnThChurnersX<-head(timeSeriesData$churnThChurners,1);
    churnThNonChurnersX<-head(timeSeriesData$churnThNonChurners,1);
    churnWhenPatternSeenX<-head(timeSeriesData$churnWhenPatternSeen,1);
    nonChurnerSeesChurnPatternX<-head(timeSeriesData$nonChurnerSeesChurnPattern,1);
    hiRegimeUpperBoundX<-head(timeSeriesData$hiRegimeUpperBound,1);
    
    timeSeriesData$TimeSequence<- seq(1,nrow(timeSeriesData)) 
    sc_startTime<-Sys.time();
    
    #Building up the complete cube
    currentCompleteCube<-data.frame(); #Empty the current cube first
    
    for (i in 1:nrow(subSpacesProfile))#For each subSpace
    {
      #Let's construct a cynamic command below:
      dynamicCommand<-paste("completeSkylineX<-psel.indices(timeSeriesData,",
                            subSpacesProfile[i,2],")",sep="");
      #Reminder: SubSpacesProfile[i,2] in the above dynamic command contains the criteria, 
      #e.g., high(SQI1)*high(SQI2)
      #Also, you can have low(SQI1), which is useful for healthcare; e.g., low(bloodPressure)
      
      #Execute the command which was built dynamically:
      eval(parse(text=dynamicCommand));
      
      #The following loop adds the COMPLETE SKYLINE elements to the complete cube. 
      for (j in 1:length(completeSkylineX))
      {
        currentCompleteCube<-rbind(currentCompleteCube, 
                                   data.frame(customerID=customerIndex,
                                              Round=roundIndex,
                                              TimeSequence=completeSkylineX[j], 
                                              weekID=timeSeriesData$weekID[timeSeriesData$TimeSequence==completeSkylineX[j]], 
                                              SubSpace=subSpacesProfile[i,1], 
                                              SubSpaceCode=subSpacesProfile[i,3], 
                                              SubSpaceLevel=subSpacesProfile[i,4],
                                              minPatternSize=minPatternSizeX,
                                              patternSQIMax=patternSQIMaxX,
                                              churnThChurners=churnThChurnersX,
                                              churnThNonChurners=churnThNonChurnersX,
                                              churnWhenPatternSeen=churnWhenPatternSeenX,
                                              nonChurnerSeesChurnPattern=nonChurnerSeesChurnPatternX,
                                              hiRegimeUpperBound=hiRegimeUpperBoundX));
        
      }
    }
    #currentCompleteCube at this point contains the complete skyline elements for the CURRENT customer in the current customer base. We'll need it to construct our filter set!
    
    completeCube<-rbind(completeCube,currentCompleteCube);
    sc_EndTime<-Sys.time();
    print(paste("complete skyCube RunTime in mins: ", (sc_EndTime-sc_startTime)/60))
    
    #Building up the peak cube  
    for (k in 1:nrow(subSpacesProfile))#For each sub-dimension
    {
      
      dynamicCommand<-paste("completeSkylineX<-psel.indices(timeSeriesData,",
                            subSpacesProfile[k,2],")",sep="");
      
      eval(parse(text=dynamicCommand));
      
      #CompleteSkylineX now contains the COMPLETE SKYLINE elements 
      currentSubSpaceCode<-subSpacesProfile[k,"BinaryCode"]
      currentSubSpaceCodeX<-as.numeric(currentSubSpaceCode)
      
      #We have already built the Complete Cube for the CURRENT participant and CURRENT video in the loop; i.e. currentCompleteCube
      #See line (5) in Figure 4 and §3.2 in the paper to see the logic behind this 
      filterSet<-subset(currentCompleteCube,
                        bitwAnd(BinaryCode,currentSubSpaceCodeX)==BinaryCode & BinaryCode!=currentSubSpaceCodeX)
      
      filterObjects<-unique(filterSet[,"TimeSequence"])
      peakSkylineX<-setdiff(completeSkylineX,filterObjects)
      #peakSkylineX contains the Peak elements
      
      #The following FOR adds the Peak elements to the COMPLETE peak cube data (peak cubes for all customers and customer bases).
      
      if (length(peakSkylineX)>0){
        for (l in 1:length(peakSkylineX))
        {
          peakCube<-rbind(peakCube,
                                data.frame(customerID=customerIndex, 
                                           Round=roundIndex,
                                           TimeSequence=peakSkylineX[l], 
                                           weekID=timeSeriesData$weekID[timeSeriesData$TimeSequence==peakSkylineX[l]], 
                                           SubSpace=subSpacesProfile[k,1], 
                                           SubSpaceCode=subSpacesProfile[k,3], 
                                           SubSpaceLevel=subSpacesProfile[k,4],
                                           minPatternSize=minPatternSizeX,
                                           patternSQIMax=patternSQIMaxX,
                                           churnThChurners=churnThChurnersX,
                                           churnThNonChurners=churnThNonChurnersX,
                                           churnWhenPatternSeen=churnWhenPatternSeenX,
                                           nonChurnerSeesChurnPattern=nonChurnerSeesChurnPatternX,
                                           hiRegimeUpperBound=hiRegimeUpperBoundX));
        }
      }
    }
    rm(currentCompleteCube);
  }
  rm(timeSeriesDataX);
  rm(timeSeriesData);
 }
peak_endTime=Sys.time()
print(paste("peak Cube RunTime in mins: ", (peak_endTime-sc_EndTime)/60))

#Reminder: You don't need to save the complete cube; it's time and space consuming;
#But if you want to, just un-comment the line below
#write.csv(completeCube,"CompleteCubesPatternSize3.csv",row.names = FALSE)

write.csv(peakCube,"peakcubes.csv",row.names = FALSE)

