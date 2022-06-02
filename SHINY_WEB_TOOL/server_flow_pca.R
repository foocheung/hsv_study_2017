output$gpca_col <- renderUI({
  
  
  #if (is.null(mdg()$myD2)) return()
  myData<-dataset_flow()
  
  tagList(
    
    tagList(
      
      box( status = "success", solidHeader = FALSE, width=4,selectInput(inputId="pca_g_color2", label="Color", names(myData), names(myData)[1]),
           actionButton("qc_generic_go","Go"))
      
    )    
)
  
})


output$gpca1 <- renderPlotly({
req(input$qc_generic_go)
 
  
isolate({
  myData<-dataset_flow()
 
  
  
  myData1<-myData %>%dplyr::select(where(is.numeric))
  myData2<-myData1[ , which(apply(myData1, 2, var) != 0)]
  myData[1:length(myData)] <- lapply(myData[1:length(myData)], factor)
  #s33<<-odata3
  p<-autoplotly(autoplot(prcomp(na.omit(myData2),scale=TRUE), data=na.omit(myData) ,text = paste("ID:", myData$SampleID ),colour=input$pca_g_color2, label = FALSE, label.size = 2, na.action = na.omit ) )
  
  #   p<-autoplot(prcomp(myMatrix,scale=TRUE), data=myData ,colour=input$color2,text = paste("ID:", rownames(myMatrix) ),  na.action = na.omit )+
  #+
  #  p <- autoplot(prcomp(myMatrix,scale=TRUE),size=0) + geom_path(arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed"), size = 0.2, data=prcomp(myMatrix,scale=TRUE) ,aes(colour=myData$Sample.ID,group=myData$Sample.ID) )
  
  #    geom_path(prcomp(myMatrix,scale=TRUE), data=myData, arrow = arrow(length=unit(0.30,"cm"), ends="last", type = "closed"), aes(colour=myData$Sample.ID,group=myData$Sample.ID) )
  #              ###, size = 0.1, data=prcomp(myMatrix,scale=TRUE) ,aes(colour=myData$Sample.ID,group=myData$Sample.ID) )
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



