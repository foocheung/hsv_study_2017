
tabItem(tabName = "item3A" ,
#                  tabsetPanel(
                          conditionalPanel(
                                  condition = "output.fileUploaded ==true",
                         tabBox(
                                 width = 12,
                         tabPanel("Statistic Output",
                                  fluidRow(
                                          
                                          ##
                                            #          column(width = 12,
                                                 checkboxInput("showtp", "Label By SampleIDs Only",TRUE)
                                                 ),
                                                 uiOutput('group_stats'),
                                                 dataTableOutput('ttestout')
                                          
                                  #)
                         ),
                         tabPanel("Volcano Plot",
                                  fluidRow(
                                        #  column(width = 12,
                                                 plotOutput('volca_plot2')    
                                         # ) 
                                  )
                         ),
                         tabPanel("Group1", dataTableOutput('ttestoutgroup1')),
                         tabPanel("Group2", dataTableOutput('ttestoutgroup2'))
                         
                         
                  #Close inner tabsetPanel
        
                         )

                  ))
#)
   

