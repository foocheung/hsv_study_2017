library("DESeq2")

 b<-read.table("args", as.is=TRUE,header=TRUE, sep="\t")
 a<-read.table("matrix_count", header=TRUE, row.names=1,sep="\t")
 dds<-DESeqDataSetFromMatrix(countData =a,b,design=~SubjectID + Timepoint)
 dds<-DESeq(dds)

saveRDS(dds, file="dds.RData")


rld <- rlog(dds)

##Example Day 1 Versus Day 0

saveRDS(rld, file="rld.RData")
res<-results(dds, contrast=c("Timepoint","Day1","Day0"))
saveRDS(res, file="D1.RData")

resOrdered <- res[order(res$padj),]
saveRDS(resOrdered, file="D1_ordered.RData")



##Example Day 30 Versus Day 0

saveRDS(rld, file="rld.RData")
res<-results(dds, contrast=c("Timepoint","Day30","Day0"))
saveRDS(res, file="D30.RData")

resOrdered <- res[order(res$padj),]
saveRDS(resOrdered, file="D30_ordered.RData")
