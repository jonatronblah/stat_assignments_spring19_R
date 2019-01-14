#Cars Data
cars <- read.table("C:/Users/jonathan/Desktop/car_data.txt",sep=',',header=TRUE)
cars_sub <- na.omit(cars[,c("body.style", "length","width","height","city.mpg", "highway.mpg", "price")])

pca <- prcomp(cars_sub[,c("length","width","height","city.mpg", "highway.mpg","price")], scale=TRUE)

#Get variance explained by each component
summary(pca)

#Scree plot
plot(pca, type='l')

#Loadings
loadings <- pca$rotation
loadings

#Biplot
biplot(pca)

#Nicer plot of scores
library(ggplot2)
scores <- as.data.frame(pca$x)
p <- ggplot(data=scores, aes(x=PC1, y=PC2,colour=cars_sub$body.style)) 
p + geom_hline(yintercept=0) + geom_vline(xintercept=0) + geom_point()


#HATCO data
hatco <- read.table("C:/Users/jonathan/Desktop/HATCO data.txt",sep=',',header=TRUE)
hatco_sub <- na.omit(hatco[,c(2:11)])

hatco_pca <- prcomp(hatco_sub[,c(1:7,9:10)],scale=TRUE)
summary(hatco_pca)
plot(hatco_pca, type='l')
loadings <- hatco_pca$rotation
biplot(hatco_pca)
scores <- as.data.frame(hatco_pca$x)
p <- ggplot(data=scores, aes(x=PC1, y=PC2,colour=as.factor(hatco_sub$Size))) 
p + geom_hline(yintercept=0) + geom_vline(xintercept=0) + geom_point()
