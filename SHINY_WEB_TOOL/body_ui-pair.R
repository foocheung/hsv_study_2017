
        
tabItem(tabName = "item3B" ,
        
                tabPanel("Paired Test",
                        
                         tabsetPanel(
                        
                         tabPanel("Statistics2",
                                  fluidRow(
                                          column(
                                                  width=12,
                                                  box(
                                                         width=12,
                                                 uiOutput('chooser_stats2'),
                                                 uiOutput('chooser_stats2b'),
                                                 dataTableOutput('pttestout')
                                               )
                                       )
                                  )
                         ),
                         tabPanel("Pair1", dataTableOutput('pttestoutgr1')),
                         tabPanel("Pair2", dataTableOutput('pttestoutgr2')),
                         tabPanel("Volcano Plot",  plotOutput('volca_plot') 
                                  
                         )
                 ) #Close inner tabsetPanel
        )
        
        
        
)