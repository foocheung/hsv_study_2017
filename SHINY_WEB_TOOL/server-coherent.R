###NEED TO FIX CODE TO HANDLE GROUP COLUMNS!!!!!


options(warn=-1)
#library(OSAT)
library(DT)
library(ggforce)
library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ComplexHeatmap)
library(readr)
library("genefilter")
library(stats)

library(htmltools)
library(readxl)
source("chooser.R")


output$text2 <- renderDataTable({
  #res<-sliderValues()$results
  req(input$goButtonosat)
  
  #req(input$integer)
  
  dres<-se()$dres2
  dddd<<-dres
  isolate({
    
    datatable(dres,extensions = 'Buttons', options = list(
      dom = 'Blfrtip',lengthMenu = c(10,50, 100, 200),pageLength = 100,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
    
  })
  
})

output$text2B <- renderDataTable({
  #res<-sliderValues()$results
  
  # if (is.null(input$goButtonosat) | input$goButtonosat == 0 |input$goButtonosat == 'FALSE')
  #  return()
  req(input$goButtonosat)
  
  
  res<-se()$upres2
  isolate({
    datatable(res,extensions = 'Buttons', options = list(
      dom = 'Blfrtip',lengthMenu = c(10,50, 100, 200),pageLength = 5,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ))
    
    
  })
  
})



se <- reactive({
  
  if (is.null(input$goButtonosat) )
    
    return()
  
  
  isolate({
    
    dat<-(mdg()$myD2)  
    dd<<-dat
   dat<- dat %>% select(-c(Subject,Group,Panel))
    ##  bbb<<-input$tp
    
    # bb<<-dat %>% filter(TimePoint %in% c(input$tp[[1:length(input$tp)]])) %>% select(SampleId,SampleDescription,TimePoint,33:length(dat))
    
    ## b<<-dat %>% filter(TimePoint %in% c(input$tp[[1]],input$tp[[2]])) 
    #%>% select(SampleId,SampleDescription,TimePoint,33:length(dat))
    
    
   ba<<-input$tp[[1]]
   tt1<<-input$tp[[2]]
    
    baseline<-dat %>% filter(TimePoint ==input$tp[[1]]) %>%  arrange(SampleID)
    #select(SampleId,SampleDescription,TimePoint,33:length(dat)) %>% arrange(SampleDescription)
    bas<<-baseline
    t1<-dat %>% filter(TimePoint == input$tp[[2]])  %>%  arrange(SampleID)
    #%>% select(SampleId,SampleDescription,TimePoint,33:length(dat)) %>% arrange(SampleDescription)
    t11<<-t1
    if(length(input$tp) < 3 ) {
      
    }
    else if (length(input$tp) == 3){
      
      
      t1<-dat %>% filter(TimePoint == input$tp[[2]])  %>%  arrange(SampleID)
      t2<-dat %>% filter(TimePoint == input$tp[[3]])  %>%  arrange(SampleID)
      
      outdir<-dir(t1,t2,input$tp[[3]],input$hits)
      
      aa<<-outdir
      dres22<-outdir[[1]]
      upres22<-outdir[[2]]
      
      list_of_data2<-outdir[[3]]
      
    }
    else if (length(input$tp) == 4){
      
      
      t3<-dat %>% filter(TimePoint == input$tp[[3]])  %>%  arrange(SampleID)
      t4<-dat %>% filter(TimePoint == input$tp[[4]])  %>%  arrange(SampleID)
      
      outdir3<-dir(t3,t4,input$tp[[4]],input$hits)
      
      aaa<<-outdir3
      dres222<-outdir3[[1]]
      upres222<-outdir3[[2]]
      
      list_of_data3<-outdir3[[3]]
      
    }
    
    
    rownames(t1)<-t1$SampleID
    rownames(baseline)<-baseline$SampleID
    bbb<<-baseline
    t111<<-t1
    hhhh<<- input$hits
    tpppp<-input$tp[[2]]
    
    outdir2<-dirrr(baseline,t1,input$tp[[2]],input$hits)
  #  outdir2<-dirrr(baseline,t1,"0",10)
    aaa<<-outdir2
    
    dres2<-outdir2[[1]]
    
    upres2<-outdir2[[2]]
    
    list_of_data<-outdir2[[3]]
    
    
    
    #############################################  
    #############################################
    dow<-"DOWN"
    dir3<-dir2(list_of_data,dow,dres2)
    alldown_res <-dir3   
    adr<<-alldown_res
    
    #############################################  
    #############################################
    
    dow<-"UP"
    dir4<-dir2(list_of_data,dow,upres2)
    allup_res <-dir4   
    aur<<-allup_res
    #############################################  
    #############################################
    
    ##ADD IF STATEMENT FOR 3 TIMEPOINTS
    if(length(input$tp) == 3 ){
      
      dow<-"DOWN"
      dir5<-dir2(list_of_data2,dow,dres22)
      alldown_res2 <-dir5   
      
      
      #############################################  
      #############################################
      dow<-"UP"
      dir6<-dir2(list_of_data2,dow,upres22)
      allup_res2 <-dir6  
      
      allup_res<-rbind(allup_res,alldown_res,allup_res2,alldown_res2)
      upres2<-rbind(upres2,dres2,upres22,dres22 )
      
      
    }
    else if(length(input$tp) == 4 ){
      
      dow<-"DOWN"
      dir6<-dir2(list_of_data3,dow,dres222)
      alldown_res3 <-dir6   
      
      
      #############################################  
      #############################################
      dow<-"UP"
      dir7<-dir2(list_of_data3,dow,upres222)
      allup_res3 <-dir7  
      
      allup_res<-rbind(allup_res,alldown_res,allup_res2,alldown_res2,allup_res3,alldown_res3)
      upres2<-rbind(upres2,dres2,upres22,dres22,upres222,dres222  )
      
      
    }
    else
    {
      allup_res<-rbind(allup_res,alldown_res)
      upres2<-rbind(upres2,dres2)
      
    }
    #############################################  
    #############################################
    
    
    
    summary_table<- upres2 %>% select(rowname,TimePoint,direction) %>% spread(TimePoint,direction)
    
    aaa<<-upres2
    aaa1<<- dres2   
    aaa2<<-allup_res
    aaa3<<-alldown_res
    
    return(list("upres2"=upres2,"dres2"=summary_table, "allup_res"=allup_res, "alldown_res"=alldown_res))
    
    
    
    
    
  })
  
})

output$fil_tp<- renderUI({
#  require(input$file1)
  dat<-(mdg()$myD2)  
  
  tagList(  {
   # HTML("<div id= 'page3'>")
    box( 
      numericInput("hits", "Filter By Percentage of Subjects:",
                   min=1, max=100, value=100),
      selectInput(
        inputId = "tp",
        label = "Filter By TimePoints (3 Max)",
        unique(dat$TimePoint),
        selected = unique(dat$TimePoint)[1:3],
        selectize = TRUE,
        multiple = TRUE
      ),
      
      br(),br(),br(),br(),
      actionButton("goButtonosat", "Go!"),
      br(),br(),br(),br(),
      
      br(),
      br()
      
      
    )
  })
  
})

output$plotup<-renderPlot ({
  
  
  allup_res<-se()$allup_res
  
  p <- ggplot(allup_res,aes(x=as.factor(TimePoint),y=value,color=SampleID,group=SampleID)) + geom_line()+  facet_wrap(~protein,scales = "free")
  
  
  print(p)
  
  
})




output$plotdown<-renderPlot ({
  
  
  alldown_res<-se()$alldown_res
  fff  <<-alldown_res
  p <- ggplot(alldown_res,aes(x=as.factor(TimePoint),y=value,color=SampleID,group=SampleID)) + geom_line()+  facet_wrap(~protein,scales = "free")
  
  
  print(p)
  
  
})

