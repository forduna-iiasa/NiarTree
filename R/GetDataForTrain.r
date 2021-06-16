library("dplyr")

GetTrainingData <- function(ChosenClass,filepath){
  load(filepath)
  Data = gl.df
  
  rm(gl.df)
#lo sig es solo para esta prueba sin no agregados
Data2007 <- data.frame(filter(Data, Data$YEAR == 2007))
NoYearData <- filter(Data, Data$YEAR != 2007)#remove year data
#lo sig solo para crear los dataframe
# notIncludedIR07 <- Data2007[1,]
# notIncludedRF07 <- Data2007[1,]
# notIncludedIR07 <- notIncludedIR07[-1,]
# notIncludedRF07 <- notIncludedRF07[-1,]
# 
# IRglobal <- unique(sort(NoYearData$YLDG_IR))
# RFglobal <- unique(sort(NoYearData$YLDG_RF))
# 
# IR2007 <- unique(sort(Data2007$YLDG_IR))
# RF2007 <- unique(sort(Data2007$YLDG_RF))
# 
# IR07notInGlobal <- setdiff(IR2007,IRglobal)
# RF07notInGlobal <- setdiff(RF2007,RFglobal)
# 
# IR2007rle <- rle(sort(Data2007$YLDG_IR))
# RF2007rle <- rle(sort(Data2007$YLDG_RF))
# 
# ClasseInstancesIR <- data.frame(ClassLabel = 0,NumInstances = 0)
# ClasseInstancesRF <- data.frame(ClassLabel = 0,NumInstances = 0)
# ClasseInstancesIR <- ClasseInstancesIR[-1,]
# ClasseInstancesRF <- ClasseInstancesRF[-1,]
# 
# for(i in 1:length(IR07notInGlobal)){
#   notIncludedIR07 <- rbind(notIncludedIR07,filter(Data, Data$YLDG_IR == IR07notInGlobal[i]))
#   #add the data not included to the training set
#   #NoYearData <- rbind(NoYearData,filter(Data, Data$YLDG_IR == IR07notInGlobal[i]))
# }
# 
# for(i2 in 1:length(RF07notInGlobal)){
#   notIncludedRF07 <- rbind(notIncludedRF07,filter(Data, Data$YLDG_RF == RF07notInGlobal[i2]))
#   #add the data not included to the training set
#   #NoYearData <- rbind(NoYearData,filter(Data, Data$YLDG_RF == RF07notInGlobal[i2]))
# }

samp.size <- floor(1 * nrow(NoYearData))
set.seed(123)
gl.spl <- sample(seq_len(nrow(NoYearData)), size = samp.size)

#-Define function to produce sets of features----

CreateFeatureVector <- function(num.mon = 0, incl.phu = F, incl.soil = F, 
                                incl.sd = F,  incl.gs = F, incl.GS = T, 
                                incl.cal = F, excl.sk = T, dframe = NoYearData){
  foi <- "" #  to make it possible to append the following features
  foi <- c(foi, "SimUID")
  foi <- c(foi, "YEAR")
  if (num.mon > 0) {
    mon <- num.mon
    foi <- c(foi,
             paste("TMX", c(1:mon), sep="_"),
             paste("TMN", c(1:mon), sep="_"),
             paste("PRCP", c(1:mon), sep="_"),
             paste("PET", c(1:mon), sep="_"),
             paste("GDD", c(1:mon), sep="_"),
             paste("RAD", c(1:mon), sep="_"),
             paste("WET", c(1:mon), sep="_"),
             paste("CMD", c(1:mon), sep="_"))
  }
  if (incl.phu == T) {
    foi <- c(foi, "PHU")
  }
  if (num.mon > 0 && incl.sd == T) {
    mon <- num.mon
    foi <- c(foi,
             paste("TMXsd", c(1:mon), sep="_"),
             paste("TMNsd", c(1:mon), sep="_"),
             paste("PRCPsd", c(1:mon), sep="_"),
             paste("RADsd", c(1:mon), sep="_"),
             paste("PETsd", c(1:mon), sep="_"),
             paste("GDDsd", c(1:mon), sep="_"),
             paste("WETsd", c(1:mon), sep="_"),
             paste("CMDsd", c(1:mon), sep="_"))
  }
  if (incl.soil == T) {
    foi <- c(foi,
             "SLP", "DEPTH", "SAND", "CLAY", 
             "PAW", "LVP", "HG", "PH", "CEC",
             "BD", "SB", "CARB", "ROK", "OC")
  }
  if (incl.GS == T) {
    foi <- c(foi, names(dframe)[grep('GS', names(dframe))])
  }
  if (incl.gs == T) {
    foi <- c(foi, names(dframe)[grep('gs', names(dframe))])
  }
  if (incl.cal == T) {
    foi <- c(foi, names(dframe)[grep('cal', names(dframe))])
  }
  if ( (excl.sk == T) & (incl.GS == T) ) {
    foi <- foi[- grep("sk", foi)]
  }
  foi <- c(foi, "YLDG_IR")
  foi <- c(foi, "YLDG_RF")
  foi <- foi[grep("GSET", foi, invert=T)]
  foi <- foi[!foi == ""]
  if (length(foi) == 0) {
    print("Features are not defined!!!")
  }
  return(foi)
}

## Construct dataframe of feature subsets as defined in Table 2 of the manuscript

feature_sets <- c("monthly climate", "annual climate", "growing season climate", "complete climate")
features.df <- data.frame(feature_sets=feature_sets, nummo=c(6,0,0,6), phu=as.logical(c("T", "T", "T", "T")), soil=as.logical(c("T", "T", "T", "T")), sd=as.logical(c("F", "F", "F", "T")),
                          gs=as.logical(c("F", "F", "F", "T")), GS=as.logical(c("F", "F", "T", "T")), calYR=as.logical(c("F", "T", "F", "T")), sk=as.logical(c("T", "T", "T", "F")))

## Select feature subset (here monthly climate)

k <- 1

## Generate list of features

features.fit <- CreateFeatureVector(num.mon = features.df$nummo[k], incl.phu = features.df$phu[k], 
                                    incl.soil = (features.df$soil[k]), incl.sd = (features.df$sd[k]),
                                    incl.gs = (features.df$gs[k]), incl.GS = (features.df$GS[k]),
                                    incl.cal = (features.df$calYR[k]), excl.sk = (features.df$sk[k]), dframe = NoYearData)


## Split global dataframe in train and test data
traindb <- NoYearData[gl.spl,  features.fit]
if(ChosenClass == "YLDG_IR")
  traindb <- within(traindb, rm("YEAR","YLDG_RF","SimUID"))
if(ChosenClass == "YLDG_RF")
  traindb <- within(traindb, rm("YEAR","YLDG_IR","SimUID"))

return(traindb)
}
#
#d<-rle(sort(traindb$YEAR))
#d$values
