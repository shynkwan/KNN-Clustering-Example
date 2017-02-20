##############################
### KNN-Clustering Example ###
##############################


library(class)

### Training data for 3 ###

zip3 = read.table("zipcode-train-3.txt", sep=",")

tt1 = matrix(as.numeric(zip3[1,]), ncol = 16 )
image(tt1)

### Training data for 8 ###

zip8 = read.table("zipcode-train-8.txt", sep=",")

tt2 = matrix(as.numeric(zip8[1,]), ncol = 16 )
image(tt2)

### Prepare for k-NN ###

zip = rbind(zip3, zip8)
label = as.numeric(c(rep(3, nrow(zip3)), rep(8, nrow(zip8))))
data2 = cbind(label, zip)

neighbors = c(1,2,5,10,15)

## ff fold cross validation ##
ff = 3
enum = sample(1:ff, nrow(data2), replace=TRUE)
errorCV = array(0, c(length(neighbors), ff))
rownames(errorCV) = neighbors
for (k in neighbors) {
	for (i in 1:ff) {  ## Test set is i. The other ones are training sets. 
		pred = knn(data2[which(enum != i), -1], data2[which(enum == i), -1], data2[which(enum != i), 1], k)
		errorCV[which(neighbors == k),i] = sum(pred != data2[which(enum==i),1]) / nrow(data2[which(enum==i),-1])
	}	
}

plot(y=apply(errorCV, 1, mean), x=neighbors, ylab="Estimated Test Error Rate", xlab="number of neighbors (k)", type='b', pch=19, cex=1, lwd=2)

## Optimal number of neighbors is 1, based on 3 fold cross validation ##

### Test the Model on Testing Data ###

test2 = read.table("zipcode-test.txt", sep=" ", header=FALSE)
test2 = test2[test2[,1] %in% c(3,8),]
testModel = knn(data2[,-1], test2[,-1], data2[,1], neighbors[which.min(apply(errorCV, 1, mean))])
testError = sum(testModel != test2[,1]) / nrow(data2)

### Check how it Work on An Individual Observation ##

test1 = matrix(as.numeric(test2[8,-1]), ncol = 16 )
image(test1)

print(testModel[8])