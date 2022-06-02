###NEED TO FIX CODE TO HANDLE GROUP COLUMNS!!!!!



dirrr<-function(t1,t2,timep,hits2){
  
  rownames(t1)<-t1$Subject
  rownames(t2)<-t2$Subject 
  list_of_data2<-list(t1,t2)
  
  ttt1<-list_of_data2
  
  common_names2 <- Reduce(intersect, lapply(list_of_data2, row.names))
  lx2<-common_names2    
  list_of_data2 <- lapply(list_of_data2, function(x) { x[row.names(x) %in% common_names2,] })
  
  t111<-list_of_data2[[1]]
  t222<-list_of_data2[[2]]
  
  names(t111)<-  sub("  ", " ", names(t111))
  names(t222)<-  sub("  ", " ", names(t222))
 # all2<-as.data.frame(pca_dat)
  
  
  print("line 21")
  print(t222)
  
   bbbb<<-t222
   aaaa<<-t111
  eee<-input$mychooser$right
  eee1<<-eee
  
  ##all4<-all3 %>%  dplyr::select(1,2,3,4,dput(eee))
  
  #a1<-t111[6:length(t111)] 
  #b1<-t222[6:length(t222)]
  library(robustbase)
  a1<-t111 %>% select(dput(eee))
  b1<-t222 %>% select(dput(eee))
  
  fc1<-robustbase::colMedians(as.matrix(b1),keep.names=TRUE)/robustbase::colMedians(as.matrix(a1),keep.names=TRUE)
  
  fc_med<- log2((abs(fc1))^(sign(fc1)) )
  
  fc_medr1<-rownames_to_column(as.data.frame(fc_med))
  
  sss5<<-fc_medr1
  
  ff1<-b1 - a1
  up1<-colSums(ff1 > 0)
  upr1<-rownames_to_column(as.data.frame(up1))
  # upres1 <-upr1 %>% filter(up1 >= hits/100 * length(common_names2))
  upres1 <-upr1 %>% filter(up1 >= hits2)
  upres22 <- merge(upres1, fc_medr1, by = "rowname", by.x="rowname")
  upres22<-upres22%>% mutate("direction"="Up")%>% rename("Subj_count"="up1") %>% mutate("TimePoint"=timep)
  
  down1<-colSums(ff1 < 0)
  
  
  
  dr1<-rownames_to_column(as.data.frame(down1))
  ss2<<-dr1
  #dres1<-  dr1 %>% filter(down1 >=  hits/100 * length(common_names2))
  
  dres1<-  dr1 %>% filter(down1 >=  hits2)
  ss3<<-dres1
  dres22<- merge(dres1, fc_medr1, by = "rowname", by.x="rowname")
  ss4<<-dres22
  dres22<-dres22%>% mutate("direction"="Down")%>% rename("Subj_count"="down1")%>% mutate("TimePoint"=timep)
  ss5<<-dres22
  #  newlist <- list(upres22,dres22,list_of_data2)
  newlist <- list(upres22,dres22)
  return(newlist)
}


output$text6 <- renderDataTable({
  #res<-sliderValues()$results
  req(input$goButtono)
  
  #req(input$integer)
 ## ttttt<<-se()$table
 # require(md()$myD2) 
  dres<-se6()
  ddddd<<-dres
  summary_table<- bind_rows(dres) %>% select(rowname,TimePoint,direction) %>% spread(TimePoint,direction)
  dddd<<-summary_table
  
  isolate({
   
    datatable(summary_table,extensions = 'Buttons', options = list(
      dom = 'Blfrtip',lengthMenu = c(10,50, 100, 200),pageLength = 100,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
    
  })
  
})







output$plotup<-renderPlot ({
 # require(input$file1)
  dat<-(mdg()$myD2) 
  
  ttttttt<<-se6()
  dres<-se6()
  
  summary_table<- bind_rows(dres) %>% select(rowname,TimePoint,direction) %>% spread(TimePoint,direction)
  
  st<<-summary_table
  dat2plot<- dat %>% select(1,2, dput(summary_table$rowname))
  aaaaa<<-dat2plot
  
  dat2plot<-dat2plot %>% tidyr::gather(3:length(dat2plot),key="protein",value="value")
  
  p <- ggplot(dat2plot,aes(x=as.factor(TimePoint),y=value,color=Subject,group=Subject)) + geom_line()+  facet_wrap(~protein,scales = "free")
  
  
  print(p)
  
  
})




se6 <- reactive({
#  require(md()$myD2) 
 # if (is.null(input$goButtonosat2) )
    
  #  return()
  
  req(input$goButtono)
  isolate({
    aaa<<-input$timep  
    dat<-(mdg()$myD2)  
    dataaaa<<-dat
   dddddd<- length(input$timep)
  #  pca_all<-foreach (i=1:length(unique(dat$TimePoint)-1),.combine="c") %do% {
   pca_all<-""
   #l<-length(unique(dat$TimePoint))-1
   l<-length(unique(input$timep))-1
   pca_all<-foreach (i=1:l,.combine="c") %do% {
  #  tp<-sort(unique(dat$TimePoint))
      j<- i +1
    bbb<<-j
  #  baseline<-dat %>% filter(TimePoint ==tp[[i]]) %>%  arrange(SampleID)
    baseline<-dat %>% dplyr::filter(TimePoint ==input$timep[[i]]) %>% dplyr::filter(SampleID %in% c(input$mychooser2$right)) %>%  arrange(Subject)
    
    ba<<-baseline
    hhhh<<-input$hits2
    #select(SampleId,SampleDescription,TimePoint,33:length(dat)) %>% arrange(SampleDescription)
    
  #  t1<-dat %>% filter(TimePoint == tp[[j]])  %>%  arrange(SampleID)
    t1<-dat %>% dplyr::filter(TimePoint == input$timep[[j]])  %>% dplyr::filter(SampleID %in% c(input$mychooser2$right)) %>%  arrange(Subject)
    ta<<-t1
    
    rownames(t1)<-t1$Subject
    rownames(baseline)<-baseline$Subject
    
    outdir2<-dirrr(baseline,t1,input$timep[[j]],input$hits2)
    #aaa<<-outdir2
    
    #dres2<-outdir2[[1]]
    
  #  upres2<-outdir2[[2]]
    outdir2
   # list_of_data<-outdir2[[3]]
   # outdir3<-dir3(baseline,t1,input$tp[[j]],input$hits)
    
    
    }
    
    
    
    
    
  })

  #allup_res<-rbind(allup_res,alldown_res)
  #upres2<-rbind(upres2,dres2)
##  upres2<-rbind(upres2,dres2)
#  summary_table<- upres2 %>% select(rowname,TimePoint,direction) %>% spread(TimePoint,direction)
  
  #ht_list = NULL
  #table_hits<-for(i in 1:l) {
    
  #  j=i+1
  #  ht_list = rbind(ht_list,pca_all[[i]],pca_all[[j]])
  #}
  
  eeeeee<<-pca_all
 # return(list("upres2"=pca_all[[1]],"dres2"=pca_all[[2]], "allup_res"=pca_all[[3]], "alldown_res"=pca_all[[4]]))
  
#  return(list("table"=outdir2,"plot"=outdir3))
 # return(list("table"=outdir2))
})






output$fil_tp6<- renderUI({
 #require(md()$myD2) require(input$file1)
#  require(md()$myD2) 
  dat<-(mdg()$myD2)  
  
  tagList(  {
    box( 
      #numericInput("hits", "Filter By Number of Subjects:",
                #   min=1, max=100, value=100) ,
       #         min=1, max=length(unique(dat$Subject)),value=length(unique(dat$Subject)) ) ,
      
    #  sliderInput(inputId="hits2", label = "Filter By Number of Subjects:", min=length(unique(dat$Subject))*0.5+1, max=length(unique(dat$Subject)), value=length(unique(dat$Subject))*0.5+1, width=300,step = 1, ticks=T),
      sliderInput(inputId="hits2", label = "Filter By Number of Subjects:", min=1, max=length(unique(dat$Subject)), value=length(unique(dat$Subject))*0.5+1, width=300,step = 1, ticks=T),
      selectInput(
        inputId = "timep",
        label = "Filter By TimePoints (3 Max)",
        unique(dat$TimePoint),
       # selected = unique(dat$TimePoint)[1:3],
       selected = unique(dat$TimePoint),
        selectize = TRUE,
        multiple = TRUE
      )
      ,
      
     
      actionButton("goButtono", "Go!")
      
      
    )
  })
  
})








##dddd %>% gather(2:4,key="key",value="value") %>% filter(value != 'NA')

