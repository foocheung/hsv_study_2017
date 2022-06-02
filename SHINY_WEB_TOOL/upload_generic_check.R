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
      'Download Example TEST Data')
    # ,
    # 
    # checkboxInput(inputId = "file_filter_col", label = "Filter Columns By File Containing a List of Column Names", value = FALSE),
    # 
    # conditionalPanel(condition = "input.file_filter_col == 1",
    #                  fileInput(ns('file_filter1'), label='',placeholder ="Upload Your Text File here Containing a list of Column Names",
    #                            accept=c(
    #                              '.txt') 
    #                  )
    # ),
    # checkboxInput(inputId = "file_filter_col2",label="Filter Rows By File Containing a List of SampleID Names", value = FALSE),
    # 
    # conditionalPanel(condition = "input.file_filter_col2 == 1",
    #                  fileInput(ns('file_filter2'),label="",placeholder = "Upload Your Text File here Containing a list of SampleID",
    #                            accept=c(
    #                              '.txt')   )
    # 
    # 
    # )
    # 
    
    
    
    
    
  )
  
  
  
}



uploadg <- function(input, output, session, myData3) {
  
 

  
  filedata <- reactive({
    
    req(input$fileg)
    
    # return(!is.null(dataset()))
    inFile <- input$fileg
    # is.null(inFile)
    
    myDatah<-read_tsv(inFile$datapath)
    myDatah$SampleID <-as.factor(myDatah$SampleID)
    check1<<-myDatah
    
    # if (length(input$file_filter1)==0){
    #   #  myDatah<-read_excel(inFile$datapath)
    #  
    # }
    # 
    # else if (input$file_filter1 > 1){
    #   print("line 63")
    #   inFile2 <- input$file_filter1
    #   # is.null(inFile)
    #   
    #   dd<<-inFile2
    #   myDatah2<-read_tsv(inFile2$datapath, col_names = FALSE)
    #   
    #   check2<-myDatah2
    #   
    #   myDatah<- myDatah %>% dplyr::select(-one_of(dput(check2$X1)))
    # }
    # 
    # if (length(input$file_filter2)==0){
    #   
    # }
    # else if (input$file_filter2 > 1){
    #   print("line 83")
    #   inFile3 <- input$file_filter2
    #   # is.null(inFile)
    #   
    #   ddd<<-inFile3
    #   myDatah3<-read_tsv(inFile3$datapath, col_names = FALSE)
    #   
    #   check3<-myDatah3
    #   
    #   myDatah<- myDatah %>% dplyr::filter(!SampleID %in% dput(as.factor(check3$X1))
    #                                       )
    #}
    
    
    
     
     
    check<<-myDatah
    
    
    return(list("myD2"=myDatah ))
  })
  
  
  
}

