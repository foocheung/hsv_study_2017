uploadgUI <- function(id,myData3) {
  ns <- NS(id)
  
  
  tagList(
  
    
   
    fileInput(ns('fileg'), p(""),
              accept=c(
                '.xlsx')   ) ,
    tags$a(
      href = 'example.xlsx',
      target = "_blank",
      class = "btn",
      icon("download"),
      'Download Example Data'),
      
    checkboxInput(inputId = "file_filter_col", label = "Filter Columns By File Containing a List of Column Names", value = FALSE),
    
    conditionalPanel(condition = "input.file_filter_col == 1",
                     fileInput(ns('file_filter1'), label='',placeholder ="Upload Your .xlsx file here Containing a list of Column Names",
                               accept=c(
                                 '.xlsx') 
                               )            
                     
                     
    ),
    checkboxInput(inputId = "file_filter_col2",label="Filter Rows By File Containing a List of SampleID Names", value = FALSE),
    
    conditionalPanel(condition = "input.file_filter_col2 == 1",
                     fileInput(ns('file_filter2'),label="",placeholder = "Upload Your .xlsx file here Containing a list of SampleID",
                               accept=c(
                                 '.xlsx')   )            
                     
                     
    )
    
    
    
    
    
    
  )
  
  
  
}



uploadg <- function(input, output, session, myData3) {
  
 
  filedata <- reactive({
    
   req(input$fileg)
    
   # return(!is.null(dataset()))
    inFile <- input$fileg
   # is.null(inFile)
    if (length(input$file_filter1)==0){
    #  myDatah<-read_excel(inFile$datapath)
      myDatah<-read_excel(inFile$datapath)
      myDatah$SampleID <-as.factor(myDatah$SampleID)
      check1<<-myDatah
        }
    
    else if (input$file_filter1 > 1){
      print("line 63")
      inFile2 <- input$file_filter1
      # is.null(inFile)
      
      dd<<-inFile2
      myDatah2<-read_excel(inFile2$datapath, col_names = FALSE)
      
      check2<-myDatah2
      myDatah<-read_excel(inFile$datapath)
      check2<<-myDatah
      myDatah<- myDatah %>% dplyr::select(-dput(check2$...1))
      }
    
   
    if (length(input$file_filter2)==0){
     
    }
    else if (input$file_filter2 > 1){
      print("line 83")
      inFile3 <- input$file_filter2
      # is.null(inFile)
      
      ddd<<-inFile3
      myDatah3<-read_excel(inFile3$datapath, col_names = FALSE)
      
      check3<-myDatah3
     
      myDatah<- myDatah %>% dplyr::filter(!SampleID %in% dput(as.factor(check3$...1)))
    }
    
    
   
  
    check<<-myDatah
    
    
    return(list("myD2"=myDatah ))
  })
  
  
  
}

