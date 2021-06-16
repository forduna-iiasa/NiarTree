source("GetDataForTrain.r")
source("GetDataForTest.r")
selectedClass <- 1 # 1 ="YLDG_RF"; 2 ="YLDG_IR"
class_vec <- c("YLDG_RF", "YLDG_IR") # RF=rainfed; IR=irrigated
CurrentClass <- class_vec[selectedClass]

traindb <- GetTrainingData(CurrentClass,"H:/Git/NiarTree/gl.df")
testdb <- GetTestingData(2007,CurrentClass,"H:/Git/NiarTree/gl.df")

#trainset <- data.table(traindb, keep.rownames = F)
#testset <-  data.table(testdb, keep.rownames = F)

#rm(traindb)
#rm(testdb)

#a <- sum(trainset[,37])
NiarTreeModel <-NiarTreeAttDisc(traindb)
save(NiarTreeModel, file = "NiarTreeModel.df")
Prediction <- NiarPredictionAtt(testset,NiarTreeModel)


# #-----Main execution example----
# irisdb <- read.csv("iriscorto.txt", sep=",")
# irisdbnoclass <- irisdb[1:4]
# #----full tree
# set.seed(1676)
# dbrand <- runif(nrow(irisdb))
# irisrand <- irisdb[order(dbrand),]
#  trainset <- irisrand[1:100,]
#  testset <- irisrand[101:150,]
#
# NiarTreeModel <-NiarTreeAttDisc(trainset)
# Resultado <- NiarPredictionAtt(trainset,NiarTreeModel)
# son<-trainset[,5]
# re <- list()
# totalTrue<-0
# for(u in 1:length(son)){
#   if(son[u] == Resultado[u])
#   {
#     totalTrue <- totalTrue + 1
#     re[u] <- TRUE
#   }
#   else re[u] <- FALSE
# }
#
# avgPred <- totalTrue/length(son)
# print(avgPred)
#
#
#
# #----separate the tree by initial sides----
# # div_at <- findRoot(irisdbnoclass,AttDicriminant(irisdbnoclass))
# # Leftdata <- irisdbnoclass[1:div_at-1,]
# # Rightdata <- irisdbnoclass[-seq(div_at),]
# #
# # RootCase <- list(irisdbnoclass[div_at,])
# # LeftTree <- NiarTreeG(Leftdata)
# # RightTree <- NiarTreeG(Rightdata)




