




output$moreControls2 <- renderUI({
  
  
  

  
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
  # myData3<-all4  
   
  myData3<-dataset()

  if (is.null(myData3))
    return(NULL)
  
  myData3 <- myData3[order(myData3$SampleID), ]
  
  
  if (length(input$mychooser1$right) < 2)
    plot_myData3 <-
    myData3[grep(paste("", collapse = '|'), myData3$SampleID, ignore.case =
                   TRUE),]
  else{
    plot_myData3 <-
      myData3[grep(
        paste0("^", input$mychooser1$right, "$",  collapse = '|'),
        myData3$SampleID,
        ignore.case = TRUE
      ),]
    
  }
  
  myData3 <- plot_myData3
  
  
  
  tagList(fluidRow(
    width = 12,
    
     box(
        width = 5,
        title = "Box Plots",
        plotlyOutput("plot"),
        height = 820,
        status = "info",
        solidHeader = TRUE
      ),
    
    box(
      width = 4,
      height = 820,
      solidHeader = TRUE,
      title = "Options",
      status = "info",
      selectInput(
        inputId = "x",
        label = "X",
        names(myData3),
        names(myData3)[[1]],
        selectize = FALSE
      ) ,
      selectInput(
        inputId = "y",
        label = "Select Protein",
        sort(names(myData3[, 4:ncol(myData3)])),
        names(myData3)[[6]],
        selectize = FALSE
      ) ,
      checkboxInput("showcolor", "Add Colors", TRUE),
      conditionalPanel(
        "input.showcolor > 0",
        selectInput(
          inputId = "color",
          label = "color",
          names(myData3),
          names(myData3)[[1]],
          selectize = FALSE
        )
      ) ,
      checkboxInput("showshape", "Add Shapes") ,
      conditionalPanel(
        "input.showshape > 0",
        selectInput(
          inputId = "shape",
          label = "shape",
          names(myData3[, 0:4]),
          names(myData3)[[1]],
          selectize = FALSE
        )
      ) ,
      selectInput(
        "ggplot_scaletype",
        "Scale type",
        c(
          "normal" = "normal",
          "Reverse direction of y axis " = "reverse",
          "Plot y on log10 scale" = "log10",
          "Plot y on log2 scale)" = "log2",
          "log10 (coord_trans())" = "log10_trans",
          "log2 (coord_trans())" = "log2_trans",
          "coord_cartesian()" = "coord_cartesian",
          "coord_flip()" = "coord_flip",
          "coord_polar() (doesn't work)" = "coord_polar",
          "x factor" = "x_factor",
          "date and time" = "datetime"
        ),
        selectize = FALSE
      ),
      
      
      
      textInput('title', 'title', "Title")  ,
      textInput('xlab', 'x-axis label', "X"),
      textInput('ylab', 'y-axis label', "Y"),
      checkboxInput("joindp", "Join Datapoints"),
      sliderInput(
        'jitter',
        'Jitter',
        min = 0,
        max = 2,
        value = 0,
        step = 0.1
      ),
      
      actionButton("show2", "Help", icon("question-circle"),
                   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    )
    
  ))
  
  
})







output$plot <- renderPlotly({
  
 
  
  
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
  
  myData3<-dataset()
  
  if (is.null(myData3))
    return(NULL)
  
  myData3 <- myData3[order(myData3$SampleID), ]
  
  
  if (length(input$mychooser1$right) < 2)
    plot_myData3 <-
    myData3[grep(paste("", collapse = '|'), myData3$SampleID, ignore.case =
                   TRUE),]
  else{
    plot_myData3 <-
      myData3[grep(
        paste0("^", input$mychooser1$right, "$",  collapse = '|'),
        myData3$SampleID,
        ignore.case = TRUE
      ),]
    
  }
  
  myData3 <- plot_myData3
  
  
  
  
  if (is.null(myData3))
    return(NULL)
  
  myData3 <- myData3[order(myData3$SampleID), ]
  
  
  if (length(input$mychooser1$right) < 2)
    plot_myData3 <-
    myData3[grep(paste("", collapse = '|'), myData3$SampleID, ignore.case =
                   TRUE),]
  else{
    plot_myData3 <-
      myData3[grep(
        paste0("^", input$mychooser1$right, "$",  collapse = '|'),
        myData3$SampleID,
        ignore.case = TRUE
      ),]
    
  }
  
  myData3 <- plot_myData3
  
  
  
  
  
  if (input$showcolor && input$showshape) {
    gp = geom_point(data = myData3,
                    aes(
                      x = factor(get(input$x)),
                      y = as.numeric(as.character(get(input$y))) ,
                      color = factor(get(input$color)),
                      shape = as.factor(get(input$shape))
                    ))
  }
  
  else if (input$showcolor && !input$showshape) {
    gp = geom_point(data = myData3, aes(
      x = factor(get(input$x)),
      y = as.numeric(as.character(get(input$y)))  ,
      color = factor(get(input$color))
    ))
    
  }
  else if (!input$showcolor && input$showshape) {
    gp = geom_point(data = myData3, aes(
      x = factor(get(input$x)),
      y = as.numeric(as.character(get(input$y))) ,
      shape = as.factor(get(input$shape))
    ))
  }
  else{
    gp = geom_point(data = myData3, aes(x = factor(get(input$x)), y = as.numeric(as.character(get(
      input$y
    )))))
    
  }
  
  
  
  
  if (input$joindp) {
    p <-
      ggplot(data = myData3, aes(
        text = paste(
          "SampleId:",
          myData3$SampleID,
          "<BR>",
          "SampleDescription:",
          myData3$SampleDescription,
          "<BR>",
          "SampleGroup:",
          myData3$SampleGroup,
          "<BR>",
          "Value:",
          as.numeric(as.character(get(input$y))),
          "<BR>"
        ),
        x = factor(get(input$x)),
        y = as.numeric(as.character(get(input$y)))
      )) +
      geom_line(data = myData3,
                aes(
                  x = factor(get(input$x)),
                  y = as.numeric(as.character(get(input$y)))  ,
                  color = get(input$color),
                  group = deparse(substitute(get(input$color)))
                  
                ))   +
      
      geom_point(data = myData3,
                 aes(color = get(input$color), shape = as.factor(get(input$shape)))) +
      
      theme(
        text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank()
      )  +  ggtitle(input$title) +  xlab(input$xlab) +
      ylab(input$ylab)
  }
  else{
    p <-
      ggplot(data = myData3, aes(
        x  = factor(get(input$x)),
        y = as.numeric(as.character(get(input$y))),
        text = paste(
          "SampleId:",
          myData3$SampleID,
          "<BR>",
          "SampleDescription:",
          myData3$SampleDescription,
          "<BR>",
          "SampleGroup:",
          myData3$SampleGroup,
          "<BR>",
          "Value:",
          as.numeric(as.character(get(input$y))),
          "<BR>"
        )
      ))     +
      gp +
      
      theme(
        text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank()
      )  +  ggtitle(input$title) +  xlab(input$xlab) +
      ylab(input$ylab)  + geom_boxplot()
  }
  
  
  
  
  if (input$jitter) {
    p  <-
      p +  geom_jitter(data = myData3,
                       aes(
                         x = factor(get(input$x)),
                         y = as.numeric(as.character(get(input$y)))  ,
                         color = get(input$color)
                       ))
  }
  else{
    
  }
  
  # p<- p + theme(legend.title = element_blank() )
  ## p<- p + theme(legend.title = element_text(paste(input$color) ) )
  
  p <- switch(
    input$ggplot_scaletype,
    normal =
      p,
    reverse =
      p + scale_y_reverse(),
    log10 =
      p + scale_y_log10(),
    log2 =
      p +
      scale_y_continuous(trans = scales::log2_trans()),
    log10_trans =
      p + coord_trans(y = "log10"),
    log2_trans =
      p + coord_trans(y = "log2"),
    coord_cartesian =
      p + coord_cartesian(xlim = c(2, 4), ylim = c(0, 50)),
    coord_flip =
      p + coord_flip(),
    coord_polar =
      p + coord_polar(),
    # Discrete x, continuous y
    x_factor =
      p,
    # Datetime x, Date y
    datetime =
      p
  )
  
  
  
  
  
  p <- ggplotly(p, tooltip = "text")
    hide_legend(p)
  
  
})