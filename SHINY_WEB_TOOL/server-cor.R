

# 
# dataset <- reactive({
#   #req(input$gocor)
#   
#   
#   
#   pca_dat<-md()$myD2
#   names(pca_dat)<-  sub("  ", " ", names(pca_dat))
#   all2<-as.data.frame(pca_dat)
#   
#   
#   
#   library(ggfortify); library(ggplot2)
#   alltt<<-input$mychooser2$right
#   allll2<<-all2  
#   all3<-all2  %>% dplyr::filter(SampleID %in% c(input$mychooser2$right))
#   
#   
#   eee<-input$mychooser$right
#   eee1<<-eee
#   
#   all4<-all3 %>%  dplyr::select(1,2,3,4,dput(eee))
#   
#   #myData3<-all4  
#   
#   all4
#   
#   
#   
# })

numericColumns <- reactive({
  req(input$gocor)
  df <- dataset()
  colnames(df)[sapply(df, is.numeric)]
  
})

correlation <- reactive({
  req(input$gocor)
  
  isolate({
  data <- dataset()
  #variables <- input$variables
  # variables <- numericColumns()
  ##  aaaa<<-data
  
  #data<- as.tibble(data) %>% select(-c(1:4))
  nums <- sapply(data, is.numeric)
  
  
  data<-data[ , nums]
  aaaa<<-data
  bbb<<-input$fil_cor
  if(is.null(data) ) {
    NULL
  } else {
    #  cor(dataset()[,input$variables], use = input$corUse, method = input$corMethod)
    
    data<- as.tibble(data) %>% dplyr::select(input$fil_cor)
    cor(data, use = input$corUse, method = input$corMethod)
  }
  
  })

  })

sigConfMat <- reactive({
  req(input$gocor)
  val <- correlation()
  if(!is.null(val))
    corTest(val, input$confLevel)
  # cor.mtest(val, conf.level = input$confLevel)
})

## Data and Correlation Validation and UI Updates ##########

#Update hclust rect max
observe({
  req(input$gocor)
  val <- correlation()
  if(!is.null(val))
    updateNumericInput(session, "plotHclustAddrect", max = nrow(val))
})



output$warning <- renderUI({
  req(input$gocor)
  val <- correlation()
  if(is.null(val)) {
    tags$i("Waiting for data input...")
  } else {
    isNA <- is.na(val)
    if(sum(isNA)) {
      tags$div(
        tags$h4("Warning: The following pairs in calculated correlation have been converted to zero because they produced NAs!"),
        helpText("Consider using an approriate NA Action to exclude missing data"),
        renderTable(expand.grid(attr(val, "dimnames"))[isNA,]))
    }
  }
})




output$fil_corr <- renderUI({

  ##req(input$gocor)
  
  data <- dataset()
#  nums <- sapply(data, is.numeric)
#  data<-data[ , nums]
  
myData1<- data %>% select_if(is.numeric) %>% dplyr::select(-matches('SampleID|Subject|TimePoint|Group')) 
tagList(
  selectInput(
    inputId = "fil_cor",
    label = "Filter Numeric Columns",
    colnames(myData1),
    colnames(myData1[1:6]),
    selectize = TRUE,
    multiple = TRUE
  )
)


})


output$outcorrplot <- renderUI({
  req(input$gocor)  
  

  
  tagList(
    plotOutput("corrPlot",height=input$cor_size,width=input$cor_wsize)
    # plotOutput("distPlot",height=input$size)
  )
  
})
output$corrPlot <- renderPlot({
  req(input$gocor)
  
  val <- correlation()
  if(is.null(val)) return(NULL)
  
  val[is.na(val)] <- 0
  args <- list(val,
               order = if(input$plotOrder == "manual") "original" else input$plotOrder, 
               hclust.method = input$plotHclustMethod, 
               addrect = input$plotHclustAddrect,
               
               p.mat = sigConfMat()[[1]],
               sig.level = if(input$sigTest) input$sigLevel else NULL,
               insig = if(input$sigTest) input$sigAction else NULL,
               
               lowCI.mat = sigConfMat()[[2]],
               uppCI.mat = sigConfMat()[[3]],
               plotCI = if(input$showConf) input$confPlot else "n")
  
  if(input$showConf) {
    do.call(corrplot, c(list(type = input$plotType), args))
  } else if(input$plotMethod == "mixed") {
    do.call(corrplot.mixed, c(list(lower = input$plotLower,
                                   upper = input$plotUpper),
                              args))
  } else {
    do.call(corrplot, c(list(method = input$plotMethod, type = input$plotType), args))
  }
})




