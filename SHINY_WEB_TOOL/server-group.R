source("chooser.R")


f <- function(x,y){
  test <- t.test(x,y, paired=FALSE)
  out <- data.frame(
    pval = as.numeric(test$p.value)                                  
  )
  return(out)
}



fold <- function(x, y) {
  
  fc<-foldchange(median(x,na.rm = TRUE),median(y,na.rm = TRUE))
  
  out <- data.frame(
    # fc1 = round(as.numeric(fc),digits=4)                                  
    fc1 <-log2((abs(fc))^(sign(fc)) ) 
  )
  
  
}




output$group_stats <- renderUI({
  
  ## NOT USED DELETE?
  ## grp_dat<-datmat()
  #datacheck2<-se()
  #testmat<<-input$mychooser2$right
  #datacheck2 %>% filter(SampleID %in% (testmat))
  #MERGE COLUMNS VERY USEFUL unite(datacheck3, me,c(SampleID,Subject))
  ##datacheck2 <-se()
  
  datacheck2<-mdg()$myD2
  # isolate({
  testmat<-input$mychooser2$right
  datacheck3 <- datacheck2 %>% filter(SampleID %in% (testmat))
  
  
  if (is.null(input$showtp) || input$showtp == 'FALSE') {
    ann<-unite(datacheck3, ID,c(SampleID,Subject,TimePoint,Group),sep='~') %>% select(ID)
  }
  else{
    
    ann<-unite(datacheck3, ID,c(SampleID)) %>% select(ID)  
  }
  
  
  
  tagList(  {
    HTML("<div id= 'page3'>")
    box(
      
      title="dplyr::select 2 Groups",width = 12,status = "primary", solidHeader = TRUE,
      
            box( 
        
        
        chooserInput("mychooser3", "Available frobs2", "Selected frobs2",
                     unique(ann$ID) , c(), size = 10, multiple = TRUE),
        
        br(),br(),br(),br(),
        br(),br(),br(),br(),
        
        br(),
        br()
        
        
      ),
      
      box(
        id="grp",
        valueBoxOutput( "number_stat_grp_left"),
        
        valueBoxOutput("number_stat_grp_right")
      ),
      box(width=4,
          radioButtons("tfilt2", "Filter By:",
                       c( 
                         "ttest"= 2,
                         "Wilcoxin Mann-Whitney" =3,
                         "t-test (fdr)" = 4,
                         "Wilcoxin Mann-Whitney (fdr)" =5     )) ,
          sliderInput(
            'tfilt',
            'Filter pvalue',
            min = 0,
            max = 1,
            value = 0.05,
            step = 0.01
          ) 
          ,
          radioButtons("vcp", "Volcano Plot Filter By:",
                       c( 
                         "t-test (fdr)" = 1,
                         "Wilcoxin Mann-Whitney (fdr)" =2     )) ,
          
          actionButton("help3", "Press for instructions"),
          
          actionButton(
            'ttestgo','Submit')
      )
      
      
    )
  })
  
  
  
})





ttest<-reactive({
  
 req(input$ttestgo)
 # if (is.null(input$ttestgo))
#    return()        
  
 # if (input$ttestgo == 0)
#    return()        
isolate(  {
  pca_dat<-mdg()$myD2
  
  
  names(pca_dat)<-  sub("  ", " ", names(pca_dat))
  pcadat2<-as.data.frame(pca_dat)
  
#  pcadat2 <- pca_dat  
  
  #pcadat2$value<-as.numeric(pcadat2$value)
  #pn<-unique(pca_dat$panel)
  
  #pca_all<-foreach (i=1:length(pn),.combine="c") %do% { 
  #  pcres<-pcadat2%>% filter(panel == pn[i]) %>% spread(protein,value)
  #  pcres
  #}
  
  #all1<-pca_all
  all1<-pcadat2
  library(ggfortify); library(ggplot2)
  
  
  
  #all2<-as.data.frame(all1) %>% dplyr::select(-matches('Sample|Subject|panel|TimePoint|Group'))
  names(all1)<-gsub("\\-", "\\_\\_", names(all1))
  
  all2<-as.data.frame(all1) %>% dplyr::select(-matches('SampleID.|Subject.|TimePoint.|Group.'))
  names(all2)<-gsub("\\_\\_", "\\-", names(all2))
  
  
  mat<-as.data.frame(all1) %>% dplyr::select(-matches('SampleID.|Subject.|TimePoint.|Group.'))
  names(mat)<-gsub("\\_\\_", "\\-", names(mat))
  
  cccccc3<<-mat
  cccccc<<-all1
  cccccc2<<-all2
  rownames(all2)<-all1$SampleID
  
  
  all3<-all2 %>% rownames_to_column() %>% filter(rowname %in% c(input$mychooser2$right))
  
  
  all3<-column_to_rownames(all3, 'rowname')
  eee<-input$mychooser$right
  
  all4<-all3 %>% dplyr::select(dput(eee))
  
  myMatrix2<-all4
  
  m1<-myMatrix2[grep(paste(gsub("\\~.*","",input$mychooser3$left), collapse='|'), rownames(myMatrix2), ignore.case=FALSE),]
  m2<-myMatrix2[grep(paste(gsub("\\~.*","",input$mychooser3$right), collapse='|'), rownames(myMatrix2), ignore.case=FALSE),]
  
  
  
  m1<-as.matrix(m1)
  
  m2<-as.matrix(m2)
  
  tfoldchange1<-sapply(seq(ncol(m1) ), function(x) fold(m1[,x], m2[,x]) ) 
  
  ttestmat1<-sapply(seq(ncol(m1)), function(x) f(m1[,x], m2[,x]) ) 
  
  
  wilcoxmat1<-sapply(seq(ncol(m1)), function(x) w(m1[,x], m2[,x]) ) 
  
  wilcoxmat1<-unlist(wilcoxmat1)
  ttestmat1<-unlist(ttestmat1)
  
  padj<-p.adjust(sapply(ttestmat1, function(x) paste(unlist(x),collapse="")), method="fdr" )
  
  wilpadj<-p.adjust(sapply(wilcoxmat1, function(x) paste(unlist(x),collapse="")), method="fdr" )
  
  somamers<-unlist(colnames(m1))
  
  foo<- list(df11=data_frame(tfoldchange1),df2=data_frame(ttestmat1),df4=data_frame(wilcoxmat1), df1 = data.frame(padj),df5 = data.frame(wilpadj), ff3=data.frame(somamers) )
  
  
  
  ttestmat<- do.call("cbind", foo)
  colnames(ttestmat) <- c('Fold Change', 'ttest', 'Wilcoxin Mann-Whitney' , 't-test (fdr)', 'Wilcoxin Mann-Whitney (fdr)', 'id')
  
  #my2Matrix <-selection()$select_myMatrix
  
  
  
  
  ########Threshold
  ######
  
  #   fil_ttest<-ttestmat %>% filter(.[[as.numeric(input$tfilt2)]] < input$tfilt)
  
  
  fil_ttest<-ttestmat %>% filter(.[[as.numeric(input$tfilt2)]] <= input$tfilt)
  
  # tttt<-ttestmat %>% filter(.[[as.numeric(input$tfilt2)]] < input$tfilt) %>% select(id) 
  #  aa<-data.frame(c("Sample.ID","Groups"))
  #  colnames(aa)<-make.names("id")
  #  eeee<-rbind(tttt, aa)
  
  
  
  
  # lllll<-paste(t(eeee), sep="|")
  
  
  # pp<-colnames(my2Matrix)
  #  filter_myMatrix<-my2Matrix[,grepl(paste0("^",lllll,  collapse='|'), colnames(my2Matrix), ignore.case=TRUE),]
  
  
  # return(list("ttestmat"=fil_ttest, "filter_myMatrix"=filter_myMatrix))
  # return(list("ttestmat"=fil_ttest))
  return(list("ttestmat"=ttestmat,"ttestmatgrp1"=m1,"ttestmatgrp2"=m2, "fil_ttest"=fil_ttest))
})
})




output$ttestout <-  DT::renderDataTable(
  datatable( 
    # ttest()$ttestmat,
    
    ttest()$fil_ttest,
    extensions = 'Buttons', options = list(
      dom = 'Blfrtip',lengthMenu = c(50, 100, 10000),pageLength = 10,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      scrollX='400px'
    )
  )
)



output$ttestoutgroup1 <- renderDataTable(
  datatable( 
    ttest()$ttestmatgrp1,
    
    extensions = 'Buttons', options = list(
      dom = 'Blfrtip',lengthMenu = c(50, 100, 200),pageLength = 10,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      scrollX='400px'
    )
  )
)

output$ttestoutgroup2 <- renderDataTable(
  datatable( 
    ttest()$ttestmatgrp2,
    
    extensions = 'Buttons', options = list(
      dom = 'Blfrtip',lengthMenu = c(50, 100, 200),pageLength = 10,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      scrollX='400px'
    )
  )
)






w <- function(x,y){
  
  test <- wilcox.test(x,y, paired=FALSE)
  out <- data.frame(
    pval = as.numeric(test$p.value)                                  
    
  )
  
  return(out)
}



pw <- function(x,y){
  
  test <- wilcox.test(x,y, paired=TRUE)
  out <- data.frame(
    pval = as.numeric(test$p.value)                                  
    
  )
  
  return(out)
}


f <- function(x,y){
  test <- t.test(x,y, paired=FALSE)
  out <- data.frame(
    pval = as.numeric(test$p.value)                                  
  )
  return(out)
}






output$volca_plot2<-renderPlot ({
  if (is.null(input$ttestgo))
    return()
  if (input$ttestgo==0)
    return()
  
  #   req(input$file1)
  # req(input$etime)
  stat_matrixt<-ttest()$ttestmat
  #fil_pttest2
  library(dplyr)
  
  
  if (input$vcp == 2){
    
    
    ttest2a <- mutate(stat_matrixt, sig=ifelse(stat_matrixt$`Wilcoxin Mann-Whitney (fdr)` < 0.05, "FDR<0.05", "Not Sig"))
    p <- ggplot(ttest2a, aes(as.numeric(`Fold Change`), -log10(`Wilcoxin Mann-Whitney`))) + 
      geom_point(aes(col=sig)) +
      scale_color_manual(values=c("red", "black")) 
    
    p <- p + geom_text_repel(min.segment.length = unit(0, "lines"),parse=TRUE,
                             data=filter(ttest2a, `Wilcoxin Mann-Whitney (fdr)` < 0.05) %>% arrange(`Wilcoxin Mann-Whitney (fdr)`) %>% slice(1:100),aes(label=id))
  }
  else{
    
    
    
    ttest2a <- mutate(stat_matrixt, sig=ifelse(stat_matrixt$`t-test (fdr)` < 0.05, "FDR<0.05", "Not Sig"))
    p <- ggplot(ttest2a, aes(as.numeric(`Fold Change`), -log10(`ttest`))) + 
      geom_point(aes(col=sig)) 
    
    
    p <- p + geom_text_repel(min.segment.length = unit(0, "lines"),
                             data=filter(ttest2a, `t-test (fdr)` < 0.05) %>% arrange(`t-test (fdr)`) %>% slice(1:100),aes(label=id)) +
      scale_color_manual(values=c("red", "black")) 
    
    
    #   p <- p + geom_text_repel(min.segment.length = unit(0, "lines"),
    #                           data=filter(ttest2a, `t-test (fdr)` < 0.10 & `t-test (fdr)` >= 0.05) %>% arrange(`t-test (fdr)`) %>% slice(1:100),aes(label=id)) +
    #    scale_color_manual(values=c("orange", "black")) 
    
    
    
  }
  
  
  print(p)
  
}, width = 700, height = 600)




