#***This code illustrates different classes of features that can be computed with peak cubes, 
#***For explanations, please see  §3.3 of the DSS paper:
#***Peak Cubes in Service Operations: Bringing Multidimensionality into Decision Support Systems
#***Note that "SQI" in this code corresponds to "SQF" in the paper;
#***We used SQI here given that we were simulating the service database of a company we were working with;
#***in their service database, they showed service failures with different SQIs (Service Quality Indices)

#load libraries
library(readr)
library(dplyr)
library(reshape2)

#load the necessary data
compressedSkyCube<- read_csv("peakcubes.csv");
allRounds <- read_csv("YourSimulation.csv");


#Find Benchmark TimeStamps and put them in a dataframe 
maxWeek <- allRounds %>% group_by(customerID,Round,minPatternSize) %>%
  summarise(benchmark=max(weekID));

#If last week is the benchmark; i.e., customer churns INSTANTLY, with NO lag:
maxWeek$benchmark <- maxWeek$benchmark

#If 4 weeks before the last week (potential churn): customer could churn in the same week (i.e., 0) or in the next 3 weeks, then:
#maxWeek$benchmark <- maxWeek$benchmark-3; 


#Add TimeStampMax to Every customer and every Round:
dataset <- compressedSkyCube %>% inner_join(maxWeek, by = c("customerID", "Round","minPatternSize"));

#Add id to the dataset; i.e. customer+Round+minPatternSize
dataset$id <- paste(dataset$customerID,dataset$Round,dataset$minPatternSize,sep='_');

#Before benchmark; i.e. "Course" (from episode's beginning to the benchmark)

#Now that Benchmark is the LAST week, "Course" basically concerns every week BUT Last Week.
courseDataset <- dataset[dataset$weekID < dataset$benchmark & dataset$weekID>0,];

#find number of hits on different LEVELs of dimensions in "Course":
levelHitCounts_course <- dcast(courseDataset, id~Level, value.var = 'Level', 
                               function(x) length(x))
colnames(levelHitCounts_course)[-1] <- paste0(colnames(levelHitCounts_course)[-1],"dHitsCourse")

subDimensionHitCounts_course<-dcast(courseDataset, id~BinaryCode, value.var = 'BinaryCode' , 
                             function(x) length(x))

colnames(subDimensionHitCounts_course)[-1] <- paste0(colnames(subDimensionHitCounts_course)[-1],"subDimensionHitsCourse")


# find number of hits on different LEVELs of dimensions After Benchmark to End; i.e. "END" 
endDataset <- dataset[dataset$weekID >= dataset$benchmark,]
levelHitCounts_end <- dcast(endDataset, id~Level, value.var = 'Level', 
                               function(x) length(x))
colnames(levelHitCounts_end)[-1] <- paste0(colnames(levelHitCounts_end)[-1],"dHitsEnd")

subDimensionHitCounts_end<-dcast(endDataset, id~BinaryCode, value.var = 'BinaryCode' , 
                                    function(x) length(x))
colnames(subDimensionHitCounts_end)[-1] <- paste0(colnames(subDimensionHitCounts_end)[-1],"subDimensionHitsEnd")


#find number of hits on different LEVELs of dimensions whole data; i.e. "Whole" (from beginning to end)
wholeDataset<- dataset[dataset$weekID>0,];
levelHitCounts_whole <- dcast(wholeDataset, id~Level, value.var = 'Level' , 
                            function(x) length(x))
colnames(levelHitCounts_whole)[-1] <- paste0(colnames(levelHitCounts_whole)[-1],"dHitsWhole")

subDimensionHitCounts_whole<-dcast(wholeDataset, id~BinaryCode, value.var = 'BinaryCode' , 
                                 function(x) length(x))
colnames(subDimensionHitCounts_whole)[-1] <- paste0(colnames(subDimensionHitCounts_whole)[-1],"subDimensionHitsWhole")

subDimensionHitMaxTime_whole<-dcast(wholeDataset, id~BinaryCode, value.var = 'weekID' , 
                                   function(x) max(x))
colnames(subDimensionHitMaxTime_whole)[-1] <- paste0(colnames(subDimensionHitMaxTime_whole)[-1],"subDimensionHitsWholeMaxTime")
subDimensionHitMaxTime_whole[subDimensionHitMaxTime_whole=="-Inf"] <- -1

#Sum of Failures on each dimension and unidimensional (absolute) peaks
failures<-allRounds[allRounds$weekID>=1,]
failures$id <- paste(failures$customerID,failures$Round,failures$minPatternSize,sep='_');
failures_sum <- failures %>% group_by(id, customerID,Round,minPatternSize) %>%
  summarise(SQI1Sum=sum(SQI1),
            SQI2Sum=sum(SQI2),
            SQI3Sum=sum(SQI3),
            SQI4Sum=sum(SQI4),
            SQI5Sum=sum(SQI5))

peakValues <- failures %>% group_by(customerID,Round,minPatternSize) %>% summarise(
  SQI1PeakValue=max(SQI1),
  SQI2PeakValue=max(SQI2),
  SQI3PeakValue = max(SQI3),
  SQI4PeakValue = max(SQI4),
  SQI5PeakValue = max(SQI5)
)

#Finding the number of times that we have ABSOLUTE PEAKS in each period (e.g., "course" or "end")
failures <- failures %>% inner_join(maxWeek, by = c(c("customerID", "Round","minPatternSize")));
courseFailures<-failures[failures$weekID>0 & failures$weekID < failures$benchmark,]
endFailures<-failures[failures$weekID >= failures$benchmark,]


coursePeaks <- courseFailures %>% group_by(customerID,Round,minPatternSize) %>% summarise(
  SQI1CoursePeakValue=max(SQI1),
  SQI2CoursePeakValue=max(SQI2),
  SQI3CoursePeakValue = max(SQI3),
  SQI4CoursePeakValue = max(SQI4),
  SQI5CoursePeakValue = max(SQI5)
)

endPeaks <- endFailures %>% group_by(customerID,Round,minPatternSize) %>% summarise(
  SQI1EndPeakValue=max(SQI1),
  SQI2EndPeakValue=max(SQI2),
  SQI3EndPeakValue = max(SQI3),
  SQI4EndPeakValue = max(SQI4),
  SQI5EndPeakValue = max(SQI5)
)


peaks <- coursePeaks %>% inner_join(endPeaks, by = c("customerID", "Round","minPatternSize"));

peaks$absoluteSQI1PeakEnd<-0
peaks$absoluteSQI2PeakEnd<-0
peaks$absoluteSQI3PeakEnd<-0
peaks$absoluteSQI4PeakEnd<-0
peaks$absoluteSQI5PeakEnd<-0

peaks$absoluteSQI1PeakEnd[peaks$SQI1EndPeakValue>peaks$SQI1CoursePeakValue] <- 1
peaks$absoluteSQI2PeakEnd[peaks$SQI2EndPeakValue>peaks$SQI2CoursePeakValue] <- 1
peaks$absoluteSQI3PeakEnd[peaks$SQI3EndPeakValue>peaks$SQI3CoursePeakValue] <- 1
peaks$absoluteSQI4PeakEnd[peaks$SQI4EndPeakValue>peaks$SQI4CoursePeakValue] <- 1
peaks$absoluteSQI5PeakEnd[peaks$SQI5EndPeakValue>peaks$SQI5CoursePeakValue] <- 1

peaks$numberOfAbsoluteEndPeaks<-peaks$absoluteSQI1PeakEnd+peaks$absoluteSQI2PeakEnd+peaks$absoluteSQI3PeakEnd+peaks$absoluteSQI4PeakEnd+peaks$absoluteSQI5PeakEnd

uniqueVariables<-failures%>% group_by(customerID, Round,minPatternSize) %>% distinct(hiRegimeUpperBound,churnThChurners,churnThNonChurners,churnWhenPatternSeen,nonChurnerSeesChurnPattern,minPatternSize,patternSQIMax)

churnData<- allRounds%>% group_by(Round, customerID,minPatternSize)%>%summarise(churn=max(churn))


# merge result
result <- failures_sum %>% 
          left_join(peakValues) %>%
          left_join(levelHitCounts_whole) %>% 
          left_join(levelHitCounts_course) %>%
          left_join(levelHitCounts_end) %>%
          left_join(subDimensionHitCounts_whole) %>%
          left_join(subDimensionHitCounts_course) %>%
          left_join(subDimensionHitCounts_end) %>%
          left_join(subDimensionHitMaxTime_whole) %>%
          left_join(churnData) %>%
          left_join(peaks) %>%
          left_join(uniqueVariables)

result[is.na(result)] <- 0

write_csv(result,"GoldTable.csv")

