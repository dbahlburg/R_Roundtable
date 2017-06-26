library(Cairo)
library(drc)
library(picante)
setwd("C:/Users/icbmadmin/Desktop/")
#Table Format: Samples in rows, Scpecies in columns!
OTU_Table = t(read.delim("AllSamples_otu_table.txt", row.names = 1)) 

# Produces a phylogenetic tree from .fasta sequence files
# system("muscle3.8.31_i86win32.exe -in AllSamples_otus.fasta -out AllSamples_otus.afa")
# system("muscle3.8.31_i86win32.exe -maketree -in AllSamples_otus2_red.afa -out AllSamples_otus2_red.phy -cluster neighborjoining")
# OTU_TREE = read.tree("AllSamples_otus.phy")

col = c("#ED5565", "#DA4453", "#B22222","#FF7F50", "#FF6347", "#FF4500","#FFCE54", "#F6BB42","#A0D468", "#8CC152", "#6B8E23","#48CFAD", "#37BC9B", "#4FC1E9", "#3BAFDA", "#00BFFF", "#5D9CEC", "#4A89DC", "#AC92EC", "#967ADC")
sort(rowSums(OTU_Table))
MIN = 2000
boots = 10 # number of bootstraps for the diversity calculation
# OTU_Table_red = OTU_Table[which(rowSums(OTU_Table) >= MIN),]

# Erstellen von Platzhaltern für die verschiedenen Diversitätsidices
SPR = NULL # = Species Richness
#FPD = NULL # = Phylogenetic Diversity 
SID = NULL # = Schannon Diversity
SIM = NULL # = Simpson Siversity
ISI = NULL # = Inverse simpson index
CHA = NULL # = Chao1 diversity index
MMF = NULL # = Michaelis-Menten "expected richness"

for(i in 1:boots) 
{
  SUB = rrarefy(x = OTU_Table, sample = MIN) # random resampling of the community
  SPR = rbind(SPR, specnumber(SUB)) 
  #FPD = rbind(FPD, pd(samp = SUB, tree = TRE)$PD)
  SID = rbind(SID, diversity(SUB))
  SIM = rbind(SIM, diversity(SUB, index = "simpson"))
  ISI = rbind(ISI, diversity(SUB, index = "invsimpson"))
  CHA = rbind(CHA, estimateR(x = SUB)[2,])

    MM = rarecurve(SUB, step=floor(MIN/100), sample=MIN, col = col, lwd =2, label = F ) 
  print(i) # fortschrittsanzeige, wie weit die berechnung ist
  lMMF = NULL
  for(j in 1:dim(SUB)[1]) 
  {
    S = as.numeric(attributes(MM[[1]])$Subsample)
    v = as.numeric(MM[[j]][1:length(MM[[1]])])
    model.drm <- drm(v ~ S, fct = MM.2())
    lMMF = c(lMMF, model.drm$coefficients[1])
  }
  MMF = rbind(MMF,lMMF)
}

# Average diversity calculation 
SPR_MEAN = colMeans(SPR)
MMF_MEAN = colMeans(MMF)
COV = (SPR_MEAN/MMF_MEAN)*100 
#FPD_MEAN = log(colMeans(exp(FPD)))
SID_MEAN = log(colMeans(exp(SID)))
CHA_MEAN = colMeans(CHA)
SIM_MEAN = colMeans(SIM)
ISI_MEAN = colMeans(ISI)

# Fasst alles zusammen und Speichet es in einer Tabelle im WD
div3 = data.frame(Richness=SPR_MEAN, Michaelis=MMF_MEAN, Coverage=COV, Shannon=SID_MEAN, Simpson=SIM_MEAN, iSimpson = ISI_MEAN, Chao1=CHA_MEAN)
write.table(div, file = "AlphaDiversityL.txt", append = F, quote = F, sep = "\t", row.names = T, col.names = T)

#Rarecurve plot
SUB_large = rrarefy(OTU_Table, sample = 2000)

# enable Cairo to export Rarefraction-plot
# Cairo(file = "Rarecurve.svg", type = "svg", width = 12, height = 8, bg = "white", units = "in", dpi = 96)
rarecurve(SUB_large, step=floor(MIN/200), col = col, lwd =2, label = F)
# dev.off()

