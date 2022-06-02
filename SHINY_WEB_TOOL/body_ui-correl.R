tabItem(tabName = "item5A" ,
        box(
                width = 3,
                title = "Correlation", 
                closable = TRUE, 
                status = "warning", 
                solidHeader = FALSE, 
                collapsible = TRUE,
                actionButton("gocor", "Submit"),
                uiOutput('fil_corr'),
                sliderInput(inputId="cor_wsize", label = "Plot Width Size:", min=300, max=2000, value=540, width=300,step = 10, ticks=T),
                sliderInput(inputId="cor_size", label = "Height Size:", min=300, max=2000, value=600, width=300,step = 10, ticks=T),
                selectInput("corMethod", "Correlation Method",
                            eval(formals(cor)$method)),
                selectInput("corUse", "NA Action",
                            c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs")),
                tags$hr(),
                conditionalPanel("!input.showConf",
                                 selectInput("plotMethod", "Plot Method",
                                             list("mixed", all = eval(formals(corrplot)$method)), "circle"),
                                 conditionalPanel("input.plotMethod === 'mixed'",
                                                  wellPanel(
                                                          selectInput("plotLower", "Lower Method", eval(formals(corrplot)$method)),
                                                          selectInput("plotUpper", "Upper Method", eval(formals(corrplot)$method)))
                                 )
                ),
                conditionalPanel("input.showConf || input.plotMethod !== 'mixed'",
                                 selectInput("plotType", "Plot Type",
                                             eval(formals(corrplot)$type))),
                
                selectInput("plotOrder", "Reorder Correlation",
                            eval(formals(corrplot)$order)),
                conditionalPanel("input.plotOrder === 'hclust'",
                                 wellPanel(
                                         selectInput("plotHclustMethod", "Method",
                                                     eval(formals(corrplot)$hclust.method)),
                                         numericInput("plotHclustAddrect", "Number of Rectangles", 3, 0, NA))),
                
                tags$hr(),
                checkboxInput("sigTest", "Significance Test"),
                conditionalPanel("input.sigTest",
                                 numericInput("sigLevel", "Significane Level",
                                              0.05, 0, 1, 0.01),
                                 selectInput("sigAction", "Insignificant Action",
                                             eval(formals(corrplot)$insig))),
                checkboxInput("showConf", "Show Confidence Interval"),
                conditionalPanel("input.showConf",
                                 selectInput("confPlot", "Ploting Method",
                                             eval(formals(corrplot)$plotCI)[-1]),
                                 numericInput("confLevel", "Confidence Level",
                                              0.95, 0, 1, 0.01)
                )
                
        ),

box(
        width = 8,
        
        title = "box", 
        closable = TRUE, 
        status = "warning", 
        solidHeader = FALSE, 
        collapsible = TRUE,
        uiOutput('outcorrplot')%>% withSpinner(color="#0dc5c1")
        ##plotOutput("corrPlot",width = 1200, height = 1200)%>% withSpinner(color="#0dc5c1")
) )
        
        
        
        
