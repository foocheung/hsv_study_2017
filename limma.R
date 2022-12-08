library(limma)

library(tidyverse)
library(fgsea)
library(grid)
library(edgeR)
library(cowplot)

b<-read_tsv("testdemo.txt")

c <- b %>% rename('Subject' = Sample.ID) %>%  unite(Sample,c(Subject,TimePoint),sep="_Day", remove = FALSE)

d<-c%>% rename("samples" = Sample) %>% mutate("TimePoint"="Day0")


e<-d %>% mutate("time"="Day1") %>% unite(s,c(Subject,time),sep="_",remove=FALSE) %>% select(s,time,Subject,Group,Gender,Ethnicity,Age) %>% rename("samples" = s)%>% rename("TimePoint" =time )
d<- d %>% mutate("time"="Day0") %>% rename("TimePoint" =time )
d<-d %>% select(c("samples", "TimePoint", "Subject", "Group", "Gender", "Ethnicity", 
                  "Age"))

f<-rbind(d,e)
g<-f %>% arrange(TimePoint,Subject)
rownames(g)<-g$samples

rownames(g)


h<-g
j<-unite(h,combo,c(2,4:5), sep = "_", remove = FALSE, na.rm = FALSE)


##MODEL MATRIX DESIGN
design<- model.matrix(  ~ 0 + combo , data=j)




###LOAD COUNTS AND PROCESS THE DELTA DAY1 - DAY0

counts_df <- read_tsv("./raw_counts.txt")


counts_df2 <- counts_df  %>% dplyr:: select(contains(c("Day0","Day1")) & -contains(c("Day180","Day187") ) )

counts <- as.matrix(counts_df2)
rownames(counts) <- counts_df[[1]]

#dgelist <- calcNormFactors(DGEList(counts))

isexpr = rowSums(cpm(counts)>0.1) >= 1
#isexpr = rowSums(cpm(counts)>0.5) >= 1
dgelist <- calcNormFactors(DGEList(counts[isexpr,]))



log2_cpms <- cpm(dgelist, log=TRUE, prior.count=0.25)

rna<-as.tibble(log2_cpms)

rownames(rna)<-rownames(log2_cpms)


###RUN LIMMA
corfit <- duplicateCorrelation(rna,design,block=j$Subject)
corfit$consensus





cont.matrix = makeContrasts(
  ## pnnD1=(comboDay1_Group1_Female + comboDay1_Group1_Male) - (comboDay0_Group1_Female + comboDay0_Group1_Male),
  ppnD11=((comboDay1_Group1_Female - comboDay0_Group1_Female) -(comboDay1_Group1_Male - comboDay0_Group1_Male)) - ((comboDay1_Group3_Female - comboDay0_Group3_Female) -(comboDay1_Group3_Male - comboDay0_Group3_Male)),
  ppnD12=((comboDay1_Group1_Female - comboDay0_Group1_Female) -(comboDay1_Group1_Male - comboDay0_Group1_Male)) - ((comboDay1_Group2_Female - comboDay0_Group2_Female) -(comboDay1_Group2_Male - comboDay0_Group2_Male)),
  ppnD13=((comboDay1_Group2_Female - comboDay0_Group2_Female) -(comboDay1_Group2_Male - comboDay0_Group2_Male)) - ((comboDay1_Group3_Female - comboDay0_Group3_Female) -(comboDay1_Group3_Male - comboDay0_Group3_Male)),
  ppnD14=((comboDay1_Group1_Female - comboDay0_Group1_Female) -(comboDay1_Group1_Male - comboDay0_Group1_Male)) - ((comboDay1_Placebo_Female - comboDay0_Placebo_Female) -(comboDay1_Placebo_Male - comboDay0_Placebo_Male)),
  ppnD15=((comboDay1_Group2_Female - comboDay0_Group2_Female) -(comboDay1_Group2_Male - comboDay0_Group2_Male)) - ((comboDay1_Placebo_Female - comboDay0_Placebo_Female) -(comboDay1_Placebo_Male - comboDay0_Placebo_Male)),
  # ppnD1=(comboDay1_Group2_Female + comboDay1_Group2_Male) - (comboDay0_Group2_Female + comboDay0_Group2_Male),
  # pplaceboD1=(comboDay1_Placebo_Female + comboDay1_Placebo_Male) - (comboDay0_Placebo_Female + comboDay0_Placebo_Male),
  #pfnnD1=(comboDay1_Group1_Female ) - (comboDay0_Group1_Female),
  #pfnnD2=(comboDay1_Group1_Male ) - (comboDay0_Group1_Male),
  pfnnD3=(comboDay1_Group1_Female - comboDay0_Group1_Female) -(comboDay1_Group1_Male - comboDay0_Group1_Male),
  pfnnD4=(comboDay1_Group2_Female - comboDay0_Group2_Female) -(comboDay1_Group2_Male - comboDay0_Group2_Male),
  pfnnD5=(comboDay1_Group3_Female - comboDay0_Group3_Female) -(comboDay1_Group3_Male - comboDay0_Group3_Male),
  pfnnD6=(comboDay1_Placebo_Female - comboDay0_Placebo_Female) -(comboDay1_Placebo_Male - comboDay0_Placebo_Male),
  levels=design)

fit <- lmFit(rna,design,block=j$Subject,correlation=corfit$consensus)

vfit <- contrasts.fit(fit, contrasts=cont.matrix)
efit<-eBayes(vfit,trend=TRUE)





