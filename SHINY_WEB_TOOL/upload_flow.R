uploadfUI <- function(id,myData3) {
  ns <- NS(id)
  
  
  tagList(
    box(
      title="Step 1 Generic",
      status = "success", 
      solidHeader = TRUE,
      width=8,
      
      tabsetPanel(id = "tabs_flow",
                  tabPanel(strong("Upload Your file"),
                           
                           tags$br(),
                           
                           conditionalPanel(condition="input.dataInput_flow=='2'",
      fileInput('file4',strong(""),
                accept=c(
                  '.xlsx')   )
      ),
      
      
      radioButtons("dataInput_flow", "", list("Upload Your Data"=2,
                                              "Load Example Data"=1)
      ),
      tags$a(
        href = 'generic_flow.xlsx',
        target = "_blank",
        class = "btn",
        icon("download"),
        'Download Matrix Example file'
      ),
      tags$hr(),
      
    

      checkboxInput(inputId = "show_flow_vid","View Help Video",FALSE),
       
      conditionalPanel(condition="input.show_flow_vid=='1'",
                       
                       collapsed = FALSE,
                       HTML('<iframe width="350" height="250" src="https://www.youtube.com/embed/V6yJLILUmWA" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
                       
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
  
    ))
  
}



uploadf <- function(input, output, session, myData3) {
  
  
  
  
  filedata <- reactive({
    
    req(input$file4)
    
    
    inFile <- input$file4
    # is.null(inFile)
    
    
     myData<-read_excel(inFile$datapath)
    # myData<-read_excel("20190387_Tsang_NPX.xlsx")
    
   
    
    myDatah<-myData
    
    if("SampleID" %in% colnames(myDatah))
    {
     
    } else
    {
      
      myDatah<-myDatah %>% dplyr::mutate("SampleID"=0)
    }
    if("Subject" %in% colnames(myDatah))
    {
      cat("Yep, Subject it's in there!\n");
    } else
    {
      myDatah<-myDatah %>% dplyr::mutate(Subject=0)
    }
    
    if("TimePoint" %in% colnames(myDatah))
    {
      cat("Yep, TimePoint it's in there!\n");
    } else
    {
      myDatah<-myDatah %>% dplyr::mutate(TimePoint=0)
    }
    
    
    if("Group" %in% colnames(myDatah))
    {
      cat("Yep, Group it's in there!\n");
    } else
    {
      myDatah<-myDatah %>% dplyr::mutate(Group=0)
    }
    
    
    
    
    myDatah <- myDatah %>% select(SampleID,TimePoint,Group,Subject,everything())
    
    
    
    return(list("myD2"=myDatah ))
  })
  
  
  
}

