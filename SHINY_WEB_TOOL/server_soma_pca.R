output$pca_col <- renderUI({
  #req(input$soma_tbl)
 # if (is.null(input$dataInput_soma) |input$dataInput_soma == 0 |input$dataInput_soma == 'FALSE')
#  return()
#    req(input$qc_soma_go)
  
  # 
   sdata <- soma_data2()
   ssss<<-sdata


  soma_meta<-sdata %>% select(-starts_with("seq."))

  tagList(

    box( status = "primary", solidHeader = FALSE, width=12,
         selectInput(inputId="som_pca_color2", label="Color", names(soma_meta), names(soma_meta)[1]),
         sliderInput(inputId="soma_pca_wid", label = "Image Width:", min=8, max=15, value=10, width=300,step = 1, ticks=T),
         
         actionButton("qc_soma_go","Go")
        )


  )

  
})


output$pca1 <- renderPlotly({
  
  req(input$qc_soma_go)
  
  isolate({
  
    sdata <- soma_data2()  
 if(input$dataInput_soma==2){
  
  sd2<<-sdata
  soma_meta<-sdata %>% select(-starts_with("seq."))
  sm1<<-soma_meta
  soma_data<-sdata %>% select(starts_with("seq."))
  sm2<<-soma_data
  p<-autoplot(prcomp(soma_data,scale=TRUE), data=sdata, colour=input$som_pca_color2,text = paste("ID:", sdata$SampleID ), label = FALSE, label.size = 3, na.action = na.omit )
  
  }
    
  else if(input$dataInput_soma ==1){
  sdata<-as.tibble(sdata)
  s1<<-sdata
  soma_ensem_col<-which(colnames(sdata)=="Subject")
  s2<<-soma_ensem_col
  soma_d<-sdata[(soma_ensem_col+1):length(sdata)]
  s3<<-soma_d
  soma_d2<-na.omit(soma_d) 
  s4<<-soma_d2
#  soma_d3<-soma_d[ , which(apply( soma_d, 2, var) != 0)]
  #sdata[1:length(sdata)] <- lapply(sdata[1:length(sdata)], factor)
 # p<-autoplot(prcomp(myMatrix,scale=TRUE), data=myData ,colour=input$color2,text = paste("ID:", rownames(myMatrix) ),  na.action = na.omit )
  p<-autoplot(prcomp(soma_d2,scale=TRUE), data=sdata, colour=input$som_pca_color2,text = paste("ID:", sdata$SampleID ), label = FALSE, label.size = 3, na.action = na.omit )
   }
  
  cccc2<-input$color2
#   
#  # s5<<-soma_d3
#  
#   
# #  sdata[1:length(sdata)] <- lapply(sdata[1:length(sdata)], factor)
  
#   #   p<-autoplot(prcomp(myMatrix,scale=TRUE), data=myData ,colour=input$color2,text = paste("ID:", rownames(myMatrix) ),  na.action = na.omit )+
#   #+
#   #  p <- autoplot(prcomp(myMatrix,scale=TRUE),size=0) + geom_path(arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed"), size = 0.2, data=prcomp(myMatrix,scale=TRUE) ,aes(colour=myData$Sample.ID,group=myData$Sample.ID) )
#   
#   #    geom_path(prcomp(myMatrix,scale=TRUE), data=myData, arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed"), aes(colour=myData$Sample.ID,group=myData$Sample.ID) )
#   #              ###, size = 0.1, data=prcomp(myMatrix,scale=TRUE) ,aes(colour=myData$Sample.ID,group=myData$Sample.ID) )
  p
  
  })
})


#  output$downloadData2 <- downloadHandler(
#    filename = function() { paste("stats_", input$file1  , '.csv', sep='') },
#    content = function(ile) {
#      write.csv(ttest()$ttestmat, file)
#    }
#  )

#output$sidepca <- renderUI({
#tagList(
#uiOutput('pca_col'),
#box(title="PCA", status = "primary", solidHeader = TRUE, width=12,plotlyOutput("pca1", height=300),
#    textOutput("text5") 
    
#)

#)
  
#})



