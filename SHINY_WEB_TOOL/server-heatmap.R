library(heatmaply)
library(shinyHeatmaply)



#output$heatmap <- renderPlotly({
output$heatmap <- renderPlotly({
  #  myMatrixdat <-datmat()
  #rownames(myMatrixdat)<-t(myMatrixdat[1])
  
  ##myMatrix2 <-selection()$select_myMatrix
  #### myMatrix <-dataM()$myData
  
  ###      nums<-5
  # nums<-3
  #  myMatrix<-myMatrix[ -c(1:nums) ]
  
  #    myMatrix<-myMatrix[ c(4) ]
  
  #  my2Matrix2<-my2Matrix2[ -c(1:nums) ]
  
  #  myMatrix<-na.omit(myMatrixdat)
  
  # rownames(myMatrix)<-myMatrix$Sample.ID
  #  myMatrix2 <- as.tibble(myMatrix) %>%  select(-matches('Sample|panel|TimePoint|Group'))
  
  
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
  # 
  # 
  # myMatrix2<-all4  
  myMatrix2<-dataset()
  # tmatrix<-myMatrix2
  #heatmaply(percentize(as.matrix(myMatrix2)))
  heatmaply(percentize(as.matrix(myMatrix2[5:length(myMatrix2)])), plot_method="plotly",row_side_colors =myMatrix2[2:3] )
  
})