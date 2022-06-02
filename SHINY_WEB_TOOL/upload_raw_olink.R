upload3UI <- function(id,myData3) {
  #ns <- NS(id)
  
  
  
  tagList(
    box(
      title="Step 1 Olink" , 
      # color="yellow",
      status = "info", 
      solidHeader = TRUE,
     width=8, #height = 800,
     tabsetPanel(id = "tabs",
     tabPanel(strong("Upload Your file"),
    
      tags$br(),
     
      conditionalPanel(condition="input.dataInput_ol==2",
                      fileInput('file3', "",
                    accept = c("text/csv",
                    "text/comma-separated-values,text/plain",
                   ".xlsx")
                   )
     ),
      
                   
       radioButtons('dataInput_ol', "", list("Upload Your Data"=2,
                                             "Load Example Data"=1)
       ),
    tags$a(
      href = 'Explore_Demo_NPX.xlsx',
      target = "_blank",
      class = "btn",
      icon("download"),
      'Download Example Olink data'
    ) ,
 
    tags$hr() ,
    checkboxInput(inputId = "show_olink_vid",label = "View Help Video",value = 0),
  #  tabPanel("Help",
    conditionalPanel(condition="input.show_olink_vid=='1'",
                       
                       HTML('<iframe width="600" height="400" src="https://www.youtube.com/embed/8LJtLv-AT-w" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                     
    
    
   )
  ),
  tabPanel("",value="QC-PCA",
           #   uiOutput('pca_col'),
           #   box(title="PCA", status = "primary", solidHeader = FALSE, width=12,
           #       plotlyOutput("pca1" ) %>% withSpinner(color="#0dc5c1")
           #   )
  ),
  tabPanel("",value="Download Matrix",
           #  uiOutput('filter_miss2') %>% withSpinner(color="#0dc5c1")
           
           
  )

   )
  
  
  
  
  
  
  
  
  
  ) 
  )
}



upload3 <- function(input, output, session, myData3) {
  
  
  
  
  filedata <- reactive({
    
   req(input$file3)
    
    ddda<<-input$dataInput_ol
    dddab<<-ns(input$dataInput_ol)
    
    inFile <- input$file3
   # is.null(inFile)
   # di<<-input$dataInput_ol
    
   ## myData<-read_excel(inFile$datapath)
   # myData<-read_excel("20190387_Tsang_NPX.xlsx")
   # if (input$dataInput_ol == 2)
   #   {
   #      myData<-read_NPX(filename = "www/20190387_Tsang_NPX.xlsx")
   #  }
   #  else {
    myData<-read_NPX(filename = inFile$datapath)
  #  }
  #  print (myData)
    
    
   
    
    
   
     myDatah<<-myData
    
    
    
    
   # return(list("myD2"=myDatah ))
  })
  
  
  
}

