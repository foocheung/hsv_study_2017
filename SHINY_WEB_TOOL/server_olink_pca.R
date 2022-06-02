output$opca_col <- renderUI({
  odata <- ol_d()
  tagList(
  box(width=4,status = "primary",selectInput( inputId = "s_opanel",
               label = "Choose dataset",
               unique(odata$Panel),
               selected =  unique(odata$Panel),
               selectize = TRUE,
               multiple = FALSE),
      actionButton("qc_olink_go","Go")
  )
)

})



output$odist_col <- renderUI({
  odata <- ol_d()
  tagList(
    box(width=4,status = "primary",selectInput( inputId = "s_opanel2", 
                                                label = "Choose dataset",
                                                unique(odata$Panel),
                                                selected =  unique(odata$Panel),
                                                selectize = TRUE, 
                                                multiple = FALSE),
        actionButton("qc_olink_go2","Go")
    )
  )
  
})

output$opca1 <- renderPlotly({
req(input$qc_olink_go)
 
  
isolate({
   odata <- ol_d()
 #s4<<-soma_d2
  
  odata1<-odata %>% filter(Panel == input$s_opanel) %>% select(SampleID,Panel,NPX,Assay) %>% pivot_wider(names_from = Assay, values_from = NPX)  %>% unnest()
  s22<<-odata
  s11<<-odata1
  odata2<-odata1 %>%dplyr::select(where(is.numeric))
  #odata3<-odata2[ , which(apply( odata2, 2, var) != 0)]
  #cccc2<-input$ocolor2
  s22<<-odata2
  
  
  #s33<<-odata3
  p<-autoplotly(autoplot(prcomp(na.omit(odata2),scale=TRUE), data=na.omit(odata1) ,text = paste("ID:", odata1$SampleID ),colour="SampleID", label = FALSE, label.size = 2, na.action = na.omit ) )
  
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



