#Some multivariate methods compiled by Helena

rm(list=ls(all=TRUE)) #clear environment

## load libraries ##
library(vegan)

## load data ##
setwd("C:/users/User/Desktop/6-Multivar-Stats-Helena")
DOM <- as.data.frame(read.csv("HE361-DOM-shorter.csv", sep=",", row.names=1))
ENVR <- as.data.frame(t(read.csv("HE361-ENVR-short.csv", sep=",", row.names=1)))
ENVR.short <- cbind(ENVR$Temp, ENVR$Sal, ENVR$chla, ENVR$TDN.uM)
rownames(ENVR.short) <- rownames(ENVR); colnames(ENVR.short) <- c("Temp", "Sal", "chla", "TDN")
ENVR.short <- as.data.frame(ENVR.short)
## plots to familiarize yourself with the data ##
plot(ENVR$Sal, ENVR$DOC.uM, cex=2, pch=21)
plot(ENVR$chla, ENVR$Temp, cex=2, pch=21)
cor.test(ENVR$Sal, ENVR$DOC.uM, method="pearson")
hist(DOM$X1_3m)
plot(DOM$X1_3m, type="l")
#corrplot to investigate correlations between environmental variables
library(corrplot)
col1 <- colorRampPalette(c("green4","white","dodgerblue3"))
envr.pearson <- cor(ENVR, use= "complete.obs", method="pearson")
corrplot(envr.pearson, type="full", order ="hclust", addrect=4, title="Pearson Correlation", 
         method="square", col=col1(20))
corrplot(envr.pearson, type="upper", order ="hclust", title="Spearman Correlation", 
         sig.level=0.001, insig="blank")

## normalize/standardize data ##
dom <- decostand(DOM, method="total", MARGIN = 2); colSums(dom)
dom.hell <- decostand(dom, method="hellinger"); colSums(dom.hell)
envr <- decostand(ENVR, method="normalize", MARGIN = 2)
envr.short <- decostand(ENVR.short, method="normalize", MARGIN = 2)
#envr <-as.data.frame(scale(ENVR, center = T, scale=T))

#test for normal distribution
hist(rnorm(1000, mean = 5, sd = 3))
shapiro.test(rnorm(1000, mean = 5, sd = 3)) #if p>0.05 -> normal distribution
hist(envr$Sal, breaks=25)
shapiro.test(envr$Sal) #if p<0.05 -> no normal distribution

## cluster analysis ##
#hclust
dom.dist <- vegdist(t(dom.hell), method="bray")
dom.clust.complete <- hclust(dom.dist, method="complete")
plot(dom.clust.complete)
dom.clust.average <- hclust(dom.dist, method="average") #=UPGMA
plot(dom.clust.average)
clusters <- cutree(dom.clust.complete, k=3)
dom.clust.complete$order
#bootstrap
library(fpc)
boot <- clusterboot(dom.dist, B=200, distances=T, clustermethod=hclustCBI, 
                    method="complete", k=3)
boot$bootmean #values close to 1 indicate stable clusters
boot$bootrecover
#kmeans
dom.kmeans <- kmeans(t(dom), centers=3)
dom.kmeans$cluster
#plotcluster(t(dom), dom.kmeans$cluster) #discriminant projection plot

## NMDS ##
#just in case you come across isoMDS: meta and iso are basically the same functions (metaMDS
# (of vegan) uses isoMDS (of MASS), only meta provides some additional possibilities)
dom.nmds <- metaMDS(dom.dist, distance="bray", k=3, try=20)
dom.nmds$stress #stress <0.05 good fit, <0.1 fair, <0.2 suspect, <0.3 arbitrary
my.color.ramp.fct<-colorRamp(c("red","snow","dodgerblue3"))
as.rgb.channels<-my.color.ramp.fct(decostand(ENVR$Sal,method="range"))
lat.color<-rgb(as.rgb.channels,maxColorValue=255)
plot(dom.nmds$points[,1], dom.nmds$points[,2], cex=2, pch=21, bg=lat.color)

#varimax rotation
nmds.varimax <- varimax(dom.nmds$points, normalize=T)
plot(nmds.varimax$loadings[,1], nmds.varimax$loadings[,2], cex=2, pch=21, bg=lat.color)

## PCA ##
#other functions: vegan's rda() without a matrix of explanatory variables is PCA,
#pca() from labdsv, princomp (more samples than variables)
dom.pca <- prcomp(t(dom));str(dom.pca)
summary(dom.pca)
biplot(dom.pca)
plot(dom.pca$rotation[,1], dom.pca$rotation[,2], main="Loadings")
text(dom.pca$rotation[,1], dom.pca$rotation[,2], labels=rownames(dom))
plot(dom.pca$x[,1] , dom.pca$x[,2], pch=21, cex=2, bg=lat.color, main="Scores")

#how many prinicpal coordinates are important?
evplot <- function(ev)
{
  # Broken stick model (MacArthur 1957)
  n <- length(ev)
  bsm <- data.frame(j=seq(1:n), p=0)
  bsm$p[1] <- 1/n
  for (i in 2:n) bsm$p[i] <- bsm$p[i-1] + (1/(n + 1 - i))
  bsm$p <- 100*bsm$p/n
  # Plot eigenvalues and % of variation for each axis
  op <- par(mfrow=c(2,1))
  barplot(ev, main="Eigenvalues", col="bisque", las=2)
  abline(h=mean(ev), col="red")
  legend("topright", "Average eigenvalue", lwd=1, col=2, bty="n")
  barplot(t(cbind(100*ev/sum(ev), bsm$p[n:1])), beside=TRUE, 
          main="% variation", col=c("bisque",2), las=2)
  legend("topright", c("% eigenvalue", "Broken stick model"), 
         pch=15, col=c("bisque",2), bty="n")
  par(op)
}

#using prcomp, standard deviations are proportional to eigenvalues
#https://stat.ethz.ch/pipermail/r-help/2005-August/076610.html
evplot(dom.pca$sdev^2)

#scree plot
plot(dom.pca$sdev^2/sum(dom.pca$sdev^2), xlab= "# of Components", ylab="% Variance")

## PCoA ##
dom.pcoa<-cmdscale(dom.dist,eig=TRUE,add=FALSE,k=7) # set a reasonable value for k<n-1 (but eigenvalues should be positive)
cumsum(dom.pcoa$eig)/sum(dom.pcoa$eig)
plot((dom.pcoa$eig)/sum(dom.pcoa$eig), main="Scree")
evplot(dom.pcoa$eig)
plot(dom.pcoa$points[,1], dom.pcoa$points[,2], pch=21, cex=2, bg=lat.color)
#fit surface variables
ordisurf(dom.pcoa$points,y=ENVR$Sal, main="DOM with Sal isolines",add=TRUE,col="black")
grp <- c(rep(1,6), rep(2,6))
ordihull(dom.pcoa$points, grp)

#vector fitting (least squares fit (Y~ scores1+scores2))
envfit.pcoa <- envfit(dom.pcoa, choices = c(1,2),envr, na.rm=T, permutations=999)
envfit.pcoa
plot(envfit.pcoa, col = "black", font=2)

#As PCoA is not a linear method, we don't get loadings. We can, however, approximate
#loadings by correlating the original data to the scores
loadings <- as.data.frame(t(cor(dom.pcoa$points, t(dom))))
plot(loadings$V1, loadings$V2)

## CA ##
#other: ca() from mva, ca()labdsv, cca from vegan without explanatory matrix
library(FactoMineR)
dom.ca <- CA(dom, ncp=5, graph=T)
dom.ca
plot(dom.ca$col$coord[,1], dom.ca$col$coord[,2], pch=21, cex=2, bg=lat.color)
plot(dom.ca$row$coord[,1], dom.ca$row$coord[,2])
summary(dom.ca)

## RDA ##
dom.rda.short <- rda(t(dom)~., envr.short, scale=T)
dom.rda.short
dom.rda <- rda(t(dom)~., envr, scale=T)
dom.rda
summary(dom.rda)
plot(dom.rda, scaling="species")
plot(dom.rda, scaling="sites")
plot(dom.rda, scaling="symmetric")

## CCA ##
dom.cca <- cca(t(dom)~., ENVR, scale=T)
dom.cca
summary(dom.cca)
plot(dom.cca, scaling="symmetric")

## CCorA ##
#read bacteria data - bacteria can influence DOM composition, but DOM availability can also
#influence bacterial community composition
BAC <- read.csv("HE361-BAC-short.csv", sep=",", row.names=1)
bac <- decostand(BAC, method = "total", MARGIN = 2); colSums(bac)
bac.hell <- decostand(bac, method="hellinger", MARGIN = 2)
bac.dist <- vegdist(t(bac.hell), method="bray")

ccora <- CCorA(Y=t(bac.hell), X=t(dom.hell), permutations = 99)
#not enough degrees of freedom...

bac.pcoa <- cmdscale(bac.dist,eig=TRUE,add=FALSE,k=7)
evplot(bac.pcoa$eig)
plot(bac.pcoa$points, cex=2, pch=21, bg=lat.color)

# CCorA using PCoA dimensions
ccora <- CCorA(Y=bac.pcoa$points[,1:3],X=dom.pcoa$points[,1:3], permutations = 999) 
# the number of PCoA axes taken from scree plots
ccora$CanCorr #Canonical correlation coefficient
ccora$p.Pillai; ccora$p.perm
ccora$Cy[,1] # scores on canonical variate in BAC-space (use to compute colors for the phylogenetic tree)
ccora$Cx[,1] # scores on canonical variate in DOM-space
ccora$RDA.adj.Rsq # bimultivariate redundance coefficients of RDAs of Y|X and X|Y
# for further analysis, correlate back to original data, i.e.:
str.DOMonDNA<-cor(t(dom),ccora$Cx) # "structure", i.e., correlations of molecules with the canonical variates in DOM-space
dim(str.DOMonDNA)
permutest(rda<-rda(bac.pcoa$points[,1:3]~dom.pcoa$points[,1:3]),permutations=999)
# testing all axes sequentially (preceding axes are taken as constraints)
anova(rda,by="axis",model="direct",perm.max=999)

## Mantel test ##
mantel(bac.dist, dom.dist)
# partial Mantel ##
mantel.partial(bac.dist, dom.dist, dist(envr$Lat) , method="pearson")

## Procrustes rotation ##
protest <- protest(bac.pcoa$points, dom.pcoa$points)
summary(protest)
plot(protest, kind=1)
plot(protest, kind=2)
#can also be run on original data

## variable selection ##
ordistep.cca.back <- ordistep(dom.cca, perm.max=100, direction="backward")
#returns oxygen, phaeo, bp, doc
ordistep.rda.back <- ordistep(dom.rda, perm.max=100, direction="backward")
#returns sal, fluores, celcounts, doc, tdn

ordistep.rda.both <- ordistep(dom.rda, perm.max=100, direction="both") #set trace=F to suppress output during model building
#returns Fluores, oxygen, chla, phaeo, TDN, dist

#test multicollinearity using variance inflation factors,
#should be done before ordistep selection
vif.cca(dom.cca)
vif.cca(dom.rda.short)







