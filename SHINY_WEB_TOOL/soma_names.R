f<-read_adat("./SS-200062.ADat")
g<-as.tibble(f)
 
t<-getFeatureData(f) 
###t$EntrezGeneSymbol[t$EntrezGeneSymbol==''] <- NA

u<-t %>% filter(EntrezGeneSymbol !="")
u$EntrezGeneSymbol[match(colnames(f), u$AptName)]
data.table::setnames(g, old = u$AptName, new = u$EntrezGeneSymbol,skip_absent = TRUE)

