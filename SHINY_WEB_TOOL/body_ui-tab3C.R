tabItem(tabName = "item3C" ,
        box(width = 6,
                sliderInput(
                  "miss", 
                  "Filter Proteins By Percent Of Missing Sample Values:",
                  min = 0.001, 
                  max = 1, 
                  value = 0.25
                ),
                actionButton("goButtonosat", "Filter")
        ),
        uiOutput('filter_miss'),
        box(
          width = 12,
          title = "Filtering Proteins By Percent Of Missing Sample Values (25% Default)", 
          closable = TRUE, 
          status = "warning", 
          solidHeader = FALSE, 
          collapsible = TRUE,
          enable_sidebar = TRUE,
          sidebar_width = 25,
          sidebar_start_open = FALSE,
          
          #sidebar_content = tagList(
          
          # uiOutput('filter_miss'),
          # checkboxInput("somevalue", "Some value", FALSE),
          # verbatimTextOutput("value"),
          #   sliderInput(
          #     "miss", 
          #     "Filter Proteins By Percent Of Missing Sample Values:",
          #     min = 0.001, 
          #     max = 1, 
          #     value = 0.25
          #   ),
          #   actionButton("goButtonosat", "Filter")
          
          # ),
          
          
          plotOutput("qc2")
        ))