require(data.table)
require(dplyr)
library("dplyr")

GetTestingData <- function(GivenYear,ChosenClass,filepath){
  
load(filepath)
Data <- gl.df
#............................................................................
#configuration section
#ChosenClass <- "YLDG_IR"
#GivenYear <- 2007
# 0 to 1  ... 0.75 is the 75% of the db .... 1 >is the 100% of the db ....
AvgDB = 1

## Select water management  j = 1 for RF j=2 for IR
j <- 1

## Select feature subset (here monthly climate)
k <- 1
#............................................................................

## Define function to produce sets of features
CreateFeatureVector <- function(num.mon = 0, incl.phu = F, incl.soil = F, 
                                incl.sd = F,  incl.gs = F, incl.GS = T, 
                                incl.cal = F, excl.sk = T, dframe = gl.df){ #mex.df) {
  foi <- "" #  to make it possible to append the following features
  #just to include year and then remove data from 2007
  
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

## Generate list of features

features.fit <- CreateFeatureVector(num.mon = features.df$nummo[k], incl.phu = features.df$phu[k], 
                                    incl.soil = (features.df$soil[k]), incl.sd = (features.df$sd[k]),
                                    incl.gs = (features.df$gs[k]), incl.GS = (features.df$GS[k]),
                                    incl.cal = (features.df$calYR[k]), excl.sk = (features.df$sk[k]), dframe = gl.df)

## Split global dataframe in train and test data
#foo bd 100%
yearData <- filter(Data, Data$YEAR == GivenYear)

samp.size <- floor(1 * nrow(yearData))
set.seed(123)
gl.spl <- sample(seq_len(nrow(yearData)), size = samp.size)

testdb <- yearData[gl.spl,  features.fit]

if(ChosenClass == "YLDG_IR")
  testdb <- within(testdb, rm("YEAR","YLDG_RF","SimUID"))
if(ChosenClass == "YLDG_RF")
  testdb <- within(testdb, rm("YEAR","YLDG_IR","SimUID"))

return(testdb)
}
