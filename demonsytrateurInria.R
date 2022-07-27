library(cfda)
data(care)
duration<-compute_duration(care)
idToKeep<-names(duration[duration>=18])
data<-cut_data(care[care$id %in% idToKeep,],18)
data


##statistiques
##Table
time_spent<-compute_time_spent(data)
nJump<-compute_number_jumps(data)
jump_gp <- data.frame(jump = as.vector(nJump))
mark<-estimate_Markov(data)

#plots
pnjump <-ggplot(data.frame(jump_gp), aes(x = jump)) + geom_bar(fill = "lightblue", color = "black")
pTimeSpent<-boxplot(time_spent)
pMark<-plot(mark,main="")



##factorial Analysis


basis<-create.bspline.basis(c(0,18),nbasis=4)
fmca<-compute_optimal_encoding(data,basis)
dim1SortIncre <-sort(fmca$pc[,1])
dim1SortDecre <- sort(dim1SortIncre, decreasing = TRUE)
minpc1 <- names(dim1SortIncre[1:ceiling(1317 *0.05)])
maxpc1 <- names(dim1SortDecre[1:ceiling(1317 *0.05)])


##Clustering

str(results)
results$


hc <- hclust(dist(fmca$pc), method = "ward.D2")
class <- cutree(hc, k = 4)

results<-list(data=data,fmca=fmca,class=class,hc=hc,plotMark=pMark,minpc1=minpc1,maxpc1=maxpc1,plot_jump=pnjump,plot_time=pTimeSpent)

save(results,file=" patientpath.Rdata")

rm(list=ls())

load("Downloads/resCluDemo.RData")

results_clustering$data

library(cfda)

mark<-estimate_Markov(results_clustering$data[,c("id","time","state")])
mark

results_clustering$dataByCluster
plot(mark,box.lcol=c("black","black","black","red"))
str(results_clustering$dataByCluster)
plot(fmca)    