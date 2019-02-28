library(factoextra)
library(FactoMineR)
library(graphics)

example <- read_csv("C:/Users/jonathan/Desktop/DAPT Docs/spring 2019/Statistics/stat_assignments_spring19_R/stat_assignment3_R/tutorial scripts & data/CorrespondenceAnalysis.txt")

cont.table <- xtabs(example$Count ~ example$`Eye Color` +example$`Hair Color`)

mosaicplot(cont.table,shade=TRUE, las=1)
chisq <- chisq.test(cont.table)
chisq

ca <- CA(cont.table,graph=FALSE) #correspondence analysis
summary(ca)

eigenvalues <- get_eigenvalue(ca)
fviz_screeplot(ca)
fviz_ca_biplot(ca, arrows=c(TRUE, TRUE)) #Biplot

fviz_contrib(ca,choice="row",axes=1)
fviz_contrib(ca,choice="col",axes=1)

#################################################################

crime <- read_csv("C:/Users/jonathan/Desktop/DAPT Docs/spring 2019/Statistics/stat_assignments_spring19_R/stat_assignment3_R/tutorial scripts & data/CityCrimeData.txt")
crime.table <- xtabs(crime$Rate ~ crime$City + crime$Crime)

mosaicplot(crime.table,shade=TRUE,las=2)
chisq <- chisq.test(crime.table)
chisq

ca <- CA(crime.table,graph=FALSE) #correspondence analysis
summary(ca)

eigenvalues <- get_eigenvalue(ca)
fviz_screeplot(ca)
fviz_ca_biplot(ca) #Biplot

fviz_contrib(ca,choice="row",axes=1)
fviz_contrib(ca,choice="col",axes=1)

##################################################################
#Multiple Correspondence Analysis

diy <- read_csv("C:/Users/jonathan/Desktop/DAPT Docs/spring 2019/Statistics/stat_assignments_spring19_R/stat_assignment3_R/tutorial scripts & data/DoItYourself.txt")
diy <- diy[rep(1:nrow(diy),diy$Count),]

diy.mca <- MCA(diy[,1:5],graph=FALSE)
summary(diy.mca)

fviz_screeplot(diy.mca,addlabels=TRUE)
fviz_mca_biplot(diy.mca)
fviz_mca_var(diy.mca)
