library(devtools)
#install_github("vqv/ggbiplot")
library(ggbiplot)

iris <- iris            
pca_iris <- prcomp(iris[,1:4], scale=T)
species <- iris$Species
pca_iris
summary(pca_iris)
ggbiplot(pca_iris, choices = 1:2,obs.scale = 1, var.scale = 1, groups = species)+
  theme_bw()

