uploadgUI <- function(id,myData3) {
  ns <- NS(id)
  
  
  tagList(
  
    
   
    fileInput(ns('fileg'), p(""),
              accept=c(
                '.txt')   ) ,
    tags$a(
      href = 'example.txt',
      target = "_blank",
      class = "btn",
      icon("download"),
      'Download Example Data')
    #,
      
    # checkboxInput(inputId = "file_filter_col", label = "Filter Columns By File Containing a List of Column Names", value = FALSE),
    # 
    # conditionalPanel(condition = "input.file_filter_col == 1",
    #                  fileInput(ns('file_filter1'), label='',placeholder ="Upload Your .xlsx file here Containing a list of Column Names",
    #                            accept=c(
    #                              '.xlsx')   )            
    #                  
    #                  
    # ),
    # checkboxInput(inputId = "file_filter_col2",label="Filter Rows By File Containing a List of SampleID Names", value = FALSE),
    # 
    # conditionalPanel(condition = "input.file_filter_col2 == 1",
    #                  fileInput(ns('file_filter2'),label="",placeholder = "Upload Your .xlsx file here Containing a list of SampleID",
    #                            accept=c(
    #                              '.xlsx')   )            
    #                  
    #                  
    # )
    
    
    
    
    
    
  )
  
  
  
}



uploadg <- function(input, output, session, myData3) {
  
 

  
  filedata <- reactive({
    
   req(input$fileg)
    
   # return(!is.null(dataset()))
    inFile <- input$fileg
   # is.null(inFile)
   
    
    myDatah<-read_tsv(inFile$datapath)
    ttt<<-myDatah
   
  
    
     
     
    check<<-myDatah
    
    
    return(list("myD2"=myDatah ))
  })
  
  
  
}

