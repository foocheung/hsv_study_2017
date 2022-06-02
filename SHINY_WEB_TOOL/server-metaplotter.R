library(shiny)
library(glue)
library(ComplexHeatmap)

output$out1 <- renderUI({
  #if (is.null(input$goButtonp) |input$goButtonp == 0 |input$goButtonp == 'FALSE')
   req(input$goButtonp) 
  #  return()        
  
  req(dataset())
  
    tagList(
    plotOutput("distPlot",height=input$size,width=input$wsize)
    # plotOutput("distPlot",height=input$size)
  )
  
})

output$xdat <- renderUI({
  
  #a<-md()$myD2
  a<-dataset()
  myData2<- a %>% select_if(negate(is.numeric))
  myData2<- a %>% select(SampleID,TimePoint,Group,Subject)
  myData1<- a %>% select_if(is.numeric) %>% dplyr::select(-matches('SampleID|Subject|TimePoint|Group')) 
  
  
  tagList(
    actionButton("goButtonp", "Go!"),
    selectInput(
      inputId = "flt_samp",
      label = "Filter Samples",
      a[1],
      a[1],
      selectize = TRUE,
      multiple = TRUE
    ),
    
    selectInput(
      inputId = "xv",
      label = "Filter Numeric Columns",
      colnames(myData1),
      colnames(myData1[1:6]),
      selectize = TRUE,
      multiple = TRUE
    ),
    selectInput(
      inputId = "xy",
      label = "Filter Non-Numeric Columns",
      colnames(myData2),selected = c("TimePoint", "Group"),
     
      selectize = TRUE,
      multiple = TRUE
    ),
    selectInput(
      inputId = "cbyc",
      label = "cluster By Column",
      c("TimePoint","Subject","Group","NA"),
      
      selectize = TRUE,
      multiple = FALSE
    ),
    # selectInput(
    #   inputId = "plot",
    #   label = "Plot Type",
    #   c("anno_barplot", "anno_points", "anno_lines"),
    #   "anno_points",
    #   selectize = TRUE,
    #   multiple = FALSE
    # ), selectInput(
    #   inputId = "col",
    #   label = "Color",
    #   c(1:20),
    #   1,
    #   selectize = TRUE,
    #   multiple = FALSE
    # ),
    # selectInput(
    #   inputId = "point",
    #   label = "Point Type",
    #   c(1:24),
    #   19,
    #   selectize = TRUE,
    #   multiple = FALSE
    # ),
    
    sliderInput(inputId="pos", label = "Label POS:", min=-100, max=-1, value=-40, width=300,step = -10, ticks=T),
    sliderInput(inputId="sgaps", label = "Gap Size:", min=1, max=10, value=1, width=300,step = 0.5, ticks=T),
    sliderInput(inputId="wsize", label = "Plot Width Size:", min=300, max=2000, value=1000, width=300,step = 10, ticks=T),
    sliderInput(inputId="size", label = "Height Size:", min=300, max=2000, value=600, width=300,step = 10, ticks=T),
    
    sliderInput(inputId="sdata2", label = "Non Numeric Width Size:", min=1, max=3, value=1, width=300,step = 0.02, ticks=T),
    sliderInput(inputId="sdata1", label = "Numeric Width Size:", min=0.5, max=4, value=1, width=300,step = 0.02, ticks=T),
    textInput("title", "Title", "Heatmap"),
    
    checkboxInput('returnpdf', 'Output Plot To PDF?', FALSE),
    
    
    conditionalPanel(
      condition = "input.returnpdf == true",
      
      
      strong("PDF size (inches):"),
      div(style="display:inline-block",  sliderInput(inputId="w", label = "width:", min=3, max=20, value=8, width=100, ticks=F)),
      div(style="display:inline-block", sliderInput(inputId="h", label = "height:", min=3, max=20, value=6, width=100, ticks=F)),
      br(),
      downloadLink('pdflink'),
      br(),
      br()
    )
  )
  
})


output$pdflink <- downloadHandler(
  filename <- "myplot.pdf",
  content <- function(file) {
    file.copy("plot.pdf", file)
  }
)






output$distPlot <- renderPlot({
  if (is.null(input$goButtonp) |input$goButtonp == 0 |input$goButtonp == 'FALSE')
  
    return()        
  
  req(dataset())
  ## myData1 %>% dplyr::select(-Age)
  
  
  # a<-read_tsv("nn.txt")
  
  isolate({
    
    a<-dataset()
    ##a <- a %>% dplyr::arrange(desc(get(input$xz)))
   ## a <- a %>% dplyr::arrange(get(input$xz))
    
    
    
    
    if (is.null(input$flt_samp)){
      
    }
    
    else{
      
      ssss<<-a
      sel_samp<-input$flt_samp
      a <-  a %>% dplyr::filter(a[,1] %in% dput(sel_samp))
     aaa<<-a 
    }
    
    
    myData2<- a  %>% select(SampleID,TimePoint,Group,Subject)%>% dplyr::select(input$xy)
   ## myData2<- a %>% select_if(negate(is.numeric))%>% dplyr::select(input$xy)
    ##   col = list(Gender = c("Male" = "blue", "M"="blue","F"="red","Female" = "red"),
    ##           #   Responder=c("F" = "yellow", "S" = "orange"), 
    ##              Ethnicity=c("Not Hispanic or Latino" ="gray", "Hispanic or Latino" = "darkred"),
    ##           Race=c("White" ="gray", "African American" = "blue", "Hispanic"="green", "Asian"="Orange"),
    ##          FluVaccine=c("Yes" ="red", "No" = "blue"),
    ##           Age_group=c("O"= "lightblue", "Y" ="purple"),
    ##          Kit=c("IL"= "blue", "NG" ="red"),
    ##          Group=c("Colchicine"="cyan", "Placebo"="purple"),
    ##          PlateId=c("Set 001" = "darkred", "Set 002" = "green"),
    ##          Set=c("Set1"= "orange", "Set2" ="cyan", "Set3" = "green")
    
    
    ##              )
    cccc<- input$xv
    ttt<<-myData2
  ##  myData1<- a %>% select_if(is.numeric) %>% dplyr::select(input$xv)
    myData1<- a  %>% select_if(is.numeric) %>% dplyr::select(-matches('SampleID|Subject|TimePoint|Group')) %>% dplyr::select(input$xv)
     
    Ha<-HeatmapAnnotation(  df=  data.frame(myData2,pch = 1)
    )
    
    
    
    
    
    ann<<-rep(input$sdata2:input$sdata2, length(myData2))
    anno_list = list() 
    ## unit(c(ann,input$sdata1,input$sdata1,input$sdata1,input$sdata1,input$sdata1), "cm"),
    #  anno_list = list(df=data.frame(myData2),
    #annotation_height = unit(c(ann,input$sdata1,input$sdata1,input$sdata1,input$sdata1,input$sdata1), "cm"),
    #                  col = col ,show_annotation_name = FALSE) 
    #annotation_height = unit(c(ann,input$sdata1,input$sdata1), "cm"
    anno_list = list(
      df=data.frame(myData2),show_annotation_name = FALSE,gap = unit(input$sgaps, "mm"),
      #,#unit(c(rep(1, times = length(ttt))),"cm") 
      #  annotation_height = unit(ann,c(rep(input$sdata1, times = length(myData1))),"cm") 
      #, 
      annotation_height = unit(rep(input$sdata2,(length(myData1) + length(myData2))), "cm")
      
    )
    
    ## annotation_height = unit(1:3, "cm")
    #anno_list=list(annotation_height = unit(rep(c(input$sdata1), times = length(myData1)),"cm"))
    
    #annotation_height = unit(c(ann,input$sdata1,input$sdata1,input$sdata1,input$sdata1,input$sdata1), "cm"),
    #col = col ,show_annotation_name = FALSE
    
    #,
    #  annotation_height = unit(c(ann,input$sdata1,input$sdata1,input$sdata1,input$sdata1,input$sdata1), "cm"),
    #  col = col ,show_annotation_name = FALSE
    # ) 
   # for(i in 1:length(myData1)) {
   #   anno_list[[ paste0(colnames(myData1[i])) ]] = get(input$plot)(myData1[, i], axis = TRUE,
   #                                                                 pch = as.numeric(input$point), 
   #                                                                 gp = gpar(col = input$col,fill=input$col),
    #                                                                add_points = TRUE)
   # }
    
    dddd<<-anno_list 
    
    
    #annotation_height = unit(c(ann,input$sdata1,input$sdata1,input$sdata1,input$sdata1,input$sdata1), "cm"),
    #  col = col ,show_annotation_name = FALSE
    # cc<<-myData1
    
    # aa<<-anno_list
    # bb<<-myData2
    
    #Ha2<<-HeatmapAnnotation(df=data.frame(myData2),
    #                         do.call(HeatmapAnnotation, anno_list) ,
    # annotation_height = unit(c(ann,input$sdata1,input$sdata1,input$sdata1,input$sdata1,input$sdata1), "cm"),
    # col = col ,show_annotation_name = FALSE
    # )
    Ha2<- do.call(HeatmapAnnotation, anno_list) 
    
    
    ##H_nn<-do.call(HeatmapAnnotation, dddd) %v% NULL
    #  HeatmapAnnotation(df = data.frame(myData2))
    ## do.call(HeatmapAnnotation, anno_list)
    qqqqq<<-myData1
    qqqq<<-myData2
    zero_row_mat = matrix(nrow = 0, ncol = length(t(a[1]) )) 
    qq<<-a
    qqq<<-zero_row_mat 
    colnames(zero_row_mat) = t(a[1])
    
    hhhh<<-Ha2
    tttt<<-input$cbyc
    
    rownames(myData1) = a$SampleID
    if (input$cbyc == 'NA'){
    H_nn <-Heatmap(t(myData1), top_annotation = Ha2, name = "aa" #column_title = input$title
                   
                   ) 
    }
    else{
      H_nn <-Heatmap(t(myData1), top_annotation = Ha2, name = "aa",# column_title = input$title,
                   #  column_split = myData2[,as.numeric(input$cbyc)]
                   column_split =eval(parse(text = paste0("qqqq$", input$cbyc)))
                   ) 
      
    }
    
    
    
    draw(H_nn, padding = unit(c(40, 40, 20, 20), "mm"))
    
    
    for(i in 1:length(myData2) ) {
      decorate_annotation(colnames(myData2[i]), {grid.text( colnames(myData2[i]), unit(input$pos, "mm"), just = "left")  }) 
    }
    
    
   # for(i in 1:length(myData1) ) 
  #  {
  #    decorate_annotation(colnames(myData1[i]), {grid.text( colnames(myData1[i]), unit(input$pos, "mm"), just = "left")  }) 
  #  }
    
  })
  
  
  
  if(input$returnpdf){
    pdf("plot.pdf", width=as.numeric(input$w), height=as.numeric(input$h))
    
    
    zero_row_mat = matrix(nrow = 0, ncol = length(t(a[1]) )) 
    colnames(zero_row_mat) = t(a[1])
    
    
    H_nn <-Heatmap(zero_row_mat, top_annotation = Ha2, name = "aa", column_title = input$title) 
    draw(H_nn, padding = unit(c(40, 40, 20, 20), "mm"))
    
    
    for(i in 1:length(myData2) ) {
      decorate_annotation(colnames(myData2[i]), {grid.text( colnames(myData2[i]), unit(input$pos, "mm"), just = "left")  }) 
    }
    
    
  #  for(i in 1:length(myData1) ) 
  #  {
  #    decorate_annotation(colnames(myData1[i]), {grid.text( colnames(myData1[i]), unit(input$pos, "mm"), just = "left")  }) 
  #  }
    
    
    dev.off()
  }
  
  
  
})





