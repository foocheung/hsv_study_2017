
output$chooser_stats2 <- renderUI({
  
  
  #myMatrix <-datmat()
  
  # box(width=4,   
  #myMatrix <- myMatrix[ order(colnames(myMatrix)), ]
  tagList(
    box( width = 12,status = "primary", solidHeader =TRUE,
         selectInput(
           #  'e0', 'dplyr::select Column Containing Paired Information', choices = colnames(myMatrix),selected = "colnames(myMatrix)[2]"Time,
           "e0", 'dplyr::select Column Containing Paired Information', choices = "TimePoint",selected = "TimePoint",
           multiple = FALSE
         )
    )
    
    
  )
  
} )



output$chooser_stats2b <- renderUI({
  ### first time runs is null 
  if (is.null(input$e0)) return()
  
  myMatrix <-mdg()$myD2
  
  
  
  
  testmat<-input$mychooser2$right
  myMatrix <- myMatrix %>% filter(SampleID %in% (testmat))
  
  
  if (is.null(input$showtp) || input$showtp == 'FALSE') {
    ann<-unite(myMatrix, ID,c(SampleID,Subject,TimePoint,Group),sep='~') %>% select(ID)
  }
  else{
    
    ann<-unite(myMatrix, ID,c(SampleID)) %>% select(ID)  
  }
  
  
  
  for_paired1<-input$e0
  pairedselected<-unique(myMatrix[,grepl(input$e0,  names(myMatrix))]) 
  ##pairedselected<-myMatrix %>% select(input$e0) %>% collect()
  ##  pairedselected<-subset(myMatrix, names(myMatrix)==input$e0)
  
  #tps<- as.data.frame(table(myData$TimePoint))
  #tpg1<- tps %>% filter(Freq > 1) %>% arrange(desc(Freq) )
  #colnames(tpg1)<-c("TimePoint", "Frequency")
  
  #  pairedselected <-pairedselected %>% filter(Freq > 1) %>% arrange(desc(Freq) )
  #  colnames(pairedselected)<-c("", "Frequency")
  aaaa<-unique(myMatrix$TimePoint)
  tagList(   
    HTML("<div id= 'page4'>"),
    
    box(title="",width = 8,status = "primary", solidHeader =FALSE,
        
        #      checkboxInput("select_ptest", "Compare Multiple TimePoint", TRUE),
        #     conditionalPanel(
        #      "input.select_ptest == 0 ",
        
        #   selectizeInput(
        #      'e1', 'dplyr::select a Pair', choices = unique(pairedselected),
        #      multiple = TRUE, options = list(maxItems = 2)
        #    )
        #    ),
        #    conditionalPanel(
        #      "input.select_ptest >0 ",
        #     
        #              if (is.numeric(tgp()$timepoints)  == "TRUE")
        #             {
        selectizeInput(
          'etime', 'Select Multiple TimePoints', choices =  unique(myMatrix$TimePoint),
          selected =  myMatrix$TimePoint,  
          multiple = TRUE)
        
        
    ),
    box(width=4,    radioButtons("tfilt3", "Filter By:",
                                 c( "paired t-test"= 3,
                                    "Wilcoxin Rank Sum Test" =4,
                                    "paired t-test (fdr)" = 5,
                                    "Wilcoxin Rank Sum Test (fdr)" =6     )) ,
        sliderInput(
          'tfilt4',
          'Filter pvalue',
          min = 0,
          max = 1,
          value = 0.05,
          step = 0.01
        ),
        actionButton("help4", "Press for instructions"),
        actionButton("goButton", "Submit")
        
    )
    
  )
  
  
})









output$pttestoutgr1 <- DT::renderDataTable(
 ## pttest()$myMatrix2,
  pttest1()$mmm[[3]],
  extensions = 'Buttons', options = list(
    lengthMenu = c(50, 100, 200,10000), pageLength = 50,
    dom = 'Blfrtip'
    ,
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    
  )
  
)

output$pttestoutgr2 <- DT::renderDataTable(
  ### pttest()$_myMatrix2,
  pttest1()$mmm[[2]],
  extensions = 'Buttons', options = list(
    lengthMenu = c(50, 100, 200,2000), pageLength = -1,
    dom = 'Blfrtip'
    ,
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    
  )
  
)

##pairedtt
output$pttestout <- DT::renderDataTable(
  ### pttest()$_myMatrix2,
  #pttest()$pttestmat,
  #pttest()$fil_pttest,
  pttest1()$fil_pttest2,
  extensions = 'Buttons', options = list(
    lengthMenu = c(50, 100, 200), pageLength = -1,
    dom = 'Blfrtip',
    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    
  )
  
)


pttest1 <-reactive({
  
  #  all<-observe({
  if (is.null(input$goButton))
    return()
  if (input$goButton==0)
    return()
  
  #req(input$file1)
  req(input$etime)
  
  # pca_dat<-md()$myD2
  # names(pca_dat)<-  sub("  ", " ", names(pca_dat))
  # all2<-as.data.frame(pca_dat)
  # 
  # 
  # 
  # library(ggfortify); library(ggplot2)
  # alltt<<-input$mychooser2$right
  # allll2<<-all2  
  # all3<-all2  %>% dplyr::filter(SampleID %in% c(input$mychooser2$right))
  # 
  # 
  # eee<-input$mychooser$right
  # eee1<<-eee
  # 
  # all4<-all3 %>%  dplyr::select(1,2,3,4,dput(eee))
  # 
  # myMatrix<-all4  
  myMatrix<-dataset()
  
  myM<<-myMatrix
  
  
  tp<-"TimePoint"

  tmpts<-input$etime
  tt<<-tmpts
  test<-embed(tmpts,2)
  ttt<<-test
  
  row_list<-input$mychooser2$right
  col_list<- input$mychooser$right
  
  rl<<-row_list
  cl<<-col_list
  
  f3<-input$tfilt3
  f4<-input$tfilt4
  
  f33<<-f3
  f44<<-f4
  
  mmm <- sapply(seq(nrow(test)), function(x)
    pairedtt(myMatrix , tp,test[x,2], test[x,1],row_list,col_list,f3,f4))
    #pairedtt(myMatrix , tp,test[2], test[1],row_list,col_list,f3,f4))
  ll<<-mmm
  
  df_total = data.frame()
  for(i in 1:ncol(mmm)){ 
    aaa <- mmm[,i]$pttestmat
    
    df_total <- rbind(df_total,aaa)
  }
  
  fff<<-df_total
  fil_pttest1<-df_total
  
  
  fil_pttest2 <-fil_pttest1 %>% filter(.[[as.numeric(f3)]] <= f4)
  ffff<<-mmm
  fffff<<-fil_pttest2
  return(list("fil_pttest2"=fil_pttest2, "mmm"=mmm)) 
})







output$volca_plot<-renderPlot ({
  if (is.null(input$goButton))
    return()
  if (input$goButton==0)
    return()
  
  #   req(input$file1)
  req(input$etime)
  stat_matrix<-pttest1()$fil_pttest2
  #fil_pttest2
  library(dplyr)
  
  fil_pttest2a <- mutate(stat_matrix, sig=ifelse(stat_matrix$`Wilcoxin Rank Sum Test (fdr)` < 0.05, "FDR<0.05", "Not Sig"))
  
  #  p <- ggplot(fil_pttest2a, aes(`Fold Change`, -log10(`Wilcoxin Rank Sum Test`))) +
  #   geom_point(aes(col=sig)) +
  #  scale_color_manual(values=c("red", "black"))
  
  p <- ggplot(fil_pttest2a, aes(`Fold Change`, -log10(`Wilcoxin Rank Sum Test (fdr)`))) + 
    geom_point() +  geom_point(size=3,data=filter(fil_pttest2a, `Wilcoxin Rank Sum Test (fdr)` < 0.05), aes(col=TimePoint)) +
    geom_hline(yintercept=-log10(.05), linetype="dashed",  color = "red", size=0.5) 
  
  
  
  
  
  p <- p + geom_text_repel(min.segment.length = unit(0, "lines"),parse = TRUE,
                           data=filter(fil_pttest2a, `Wilcoxin Rank Sum Test (fdr)` < 0.05), aes(label=id) ) + expand_limits(y = c(0, 3))
  
  
  print(p)
  
})




















