tabItem(tabName = "upload_generic" ,
        
       fluidRow(
         column(12, offset = 0.1,
        box(
          title="Step2:Upload Your re-formated file as Tab Delimited Text File (.txt) from Step1",
         status="danger",
         solidHeader = TRUE,
          width = 6,
          #tags$h2("Step2: Upload Your Generic .xlsx file here"),
          
          source("body2_ui_generic.R",local=TRUE)$value,
          checkboxInput("info","Click Here for more Information",value=FALSE)
         
         ),
       #  tags$br(),
      # tags$strong("An Example is shown Above")
        
      conditionalPanel(
        condition = "output.fileUploaded2filter ==true",
        fluidRow(
          column(12,
                 box(
                   strong("Filter Columns and Rows below and then proceed to the next step:"),
                   #tags$style(type='text/css', "button#go_stepIII {margin-left: 70%;}"),
                   actionButton("go_stepIII", "Go To Step 3" ,class = "btn-primary")
                 )
          )
        ),
        tabBox(
        width = 12,
        tabPanel("Filter Data",
                 fluidRow(
                   uiOutput('filter_sample')%>% withSpinner(color="#0dc5c1"),
                   uiOutput('filter_sampleII')%>% withSpinner(color="#0dc5c1")
                 )),
        tabPanel("Data Upload",
                 fluidRow(
                   uiOutput('filter_miss')   %>% withSpinner(color="#0dc5c1")
                   ) 
                 )
        
       
      )  
     
),
conditionalPanel(
  condition = "input.info ==true",
box(
  width = 6,
  img(src = "generic_screenshot.png", height = 140, width = 400),
  tags$br("Information"),
  tags$strong("Required Columns:"),
  tags$ol(
    tags$li("SampleID"),
    tags$li("TimePoint"), 
    tags$li("Group"),
    tags$li("Subject") 
  )
)
)
)
)
)