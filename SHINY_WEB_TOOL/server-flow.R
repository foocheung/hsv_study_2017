

md4<-callModule(uploadf, "test")

output$filter_miss4 <-renderUI({
  #if (is.null(md4()$myD2)) return()
 
tagList(
 

  dataTableOutput('text4flow')


)
  
})

dataset_flow<-reactive({
  
  
  
  
  if(input$dataInput_flow == 1){
    myData<-read_excel("www/generic_flow.xlsx")
    
    myDatah<-myData
   
  }
  
  else if (input$dataInput_flow == 2){
  req(input$file4)
  
  
  inFile <- input$file4
  # is.null(inFile)
  
  
  myData<-read_excel(inFile$datapath)
  
  myDatah<-myData
  }
  
  
  
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

  
  
  
  myDatah<-colClean(myDatah)
  myDatah<-colClean2(myDatah)
  myDatah<-colClean3(myDatah)
  myDatah<-colClean4(myDatah)
  
myDatah %>% select(SampleID,TimePoint,Group,Subject,everything())
  

 # return(list("myD2"=myDatah ))
  
})




observeEvent(input$dataInput_flow,
             
             if (input$dataInput_flow == 1){
               removeTab(inputId = "tabs_flow",target = "Download Matrix")
               removeTab(inputId = "tabs_flow",target = "QC-PCA")
              
               appendTab(inputId = "tabs_flow",  select = TRUE,
                         tabPanel("QC-PCA",
                                  uiOutput('gpca_col'),
                                  box(title="PCA", status = "success", solidHeader = FALSE, width=8,
                                      plotlyOutput("gpca1") %>% withSpinner(color="#0dc5c1")
                                  )
                         )
               )
               
               appendTab(inputId = "tabs_flow",  select = TRUE,
                         tabPanel("Download Matrix",
                                  
                                  # uiOutput('filter_miss4')   %>% withSpinner(color="#0dc5c1")
                                  
                                  tags$br(),
                                  downloadButton('flow_txtlink','Download Matrix') %>% withSpinner(color="#0dc5c1") , 
                                  tags$br(), 
                                  tags$p('Download and populate the SampleID, TimePoint, Subject and Group Columns in your favourite software eg Excel. Upload this annotated file in Step 2'),
                                  tags$br(),
                                  tags$hr(),
                                  checkboxInput('flow_tbl', tags$span("View First 10 Rows"), FALSE),
                                  checkboxInput('ready4step2flow', tags$span("Ready To Proceed To Step 2?", style = "color: red;"), FALSE),
                                  conditionalPanel(
                                    condition = "input.ready4step2flow == true",
                                    tags$strong('Click on the button and proceed to next step '),
                                    #tags$style(type='text/css', "button#go_step2 {margin-left: 60%;}"),
                                    actionButton("go_step2c","Go To Step 2",class = "btn-primary")
                                  ),
                                  conditionalPanel(
                                    condition = "input.flow_tbl == true",
                                    dataTableOutput('text4flow')  %>% withSpinner(color="#0dc5c1")
                                    
                                  )
                                  
                         )
               )
               
             }
else if (input$dataInput_flow == 2){
#  removeTab(inputId = "tabs_flow",target = "Example Data")
#  removeTab(inputId = "tabs_flow",target = "QC-PCA")
}

)




observeEvent(input$go_step2c, {
  # newtab <- switch(input$tabs, "one" = "two","two" = "one")
  updateTabItems(session, "sidebarmenu2", "upload_generic")
 # updateTabItems(session, "Home", "home")
#  updateTabItems(session, "sidebarmenu3", "step3")
  
   })


observeEvent(input$file4,
             
             if (input$dataInput_flow == 2){
               removeTab(inputId = "tabs_flow",target = "Download Matrix")
               removeTab(inputId = "tabs_flow",target = "QC-PCA")
               
              
               appendTab(inputId = "tabs_flow",  select = TRUE,
                         tabPanel("QC-PCA",
                                              uiOutput('gpca_col'),
                                                     box(title="PCA", status = "success", solidHeader = FALSE, width=8,
                                                         plotlyOutput("gpca1") %>% withSpinner(color="#0dc5c1")
                                                     )
                                            )
                                  )              
                                          
                                  
                         
               appendTab(inputId = "tabs_flow",  select = TRUE,
                         tabPanel("Download Matrix",
                                  
                                  # uiOutput('filter_miss4')   %>% withSpinner(color="#0dc5c1")
                                  
                                  tags$br(),
                                  downloadButton('flow_txtlink','Download Matrix') %>% withSpinner(color="#0dc5c1") , 
                                  tags$br(), 
                                  tags$p('Download and populate the SampleID, TimePoint, Subject and Group Columns in your favourite software eg Excel. Upload this annotated file in Step 2'),
                                  tags$br(),
                                  tags$hr(),
                                  checkboxInput('flow_tbl', tags$span("View First 10 Rows"), FALSE),
                                  checkboxInput('ready4step2flow', tags$span("Ready To Proceed To Step 2?", style = "color: red;"), FALSE),
                                  conditionalPanel(
                                    condition = "input.ready4step2flow == true",
                                    tags$strong('Click on the button and proceed to next step '),
                                    #tags$style(type='text/css', "button#go_step2 {margin-left: 60%;}"),
                                    actionButton("go_step2c","Go To Step 2",class = "btn-primary")
                                  ),
                                  conditionalPanel(
                                    condition = "input.flow_tbl == true",
                                    dataTableOutput('text4flow')  %>% withSpinner(color="#0dc5c1")
                                  )       
                         )
               )
               
             }
             else if (input$dataInput_flow == 1){
             #  removeTab(inputId = "tabs_flow",target = "Download Matrix")
            #   removeTab(inputId = "tabs_flow",target = "QC-PCA")
               
               
               
               
             }
             
             #else{
             #  removeTab(inputId = "tabs",target = "Step 1B") 
             
            # }
             
)







colClean <- function(x){ colnames(x) <- gsub("\\+", "plus", colnames(x)); x } 
colClean2 <- function(x){ colnames(x) <- gsub("non\\-", "non", ignore.case = TRUE,colnames(x)); x } 

colClean3 <- function(x){ colnames(x) <- gsub("\\-", "minus", colnames(x)); x } 
colClean4 <- function(x){ colnames(x) <- gsub("\\s+", "_", colnames(x)); x } 

#a<-read_tsv("./ba_27C_dat4analysis.txt")



output$text4flow <- DT::renderDataTable({
  #res<-sliderValues()$results
  
  #if (is.null(input$goButtonosat))
  #  return()
 
  #if (is.null(dataset_flow())) return()
  res4<-dataset_flow()
 da<<-res4
  # res4<-(md4()$myD2)
  ch_fil<<-input$file4
  #DOWNLOADS WRONG SEE IF CAN FIX....
 
 a2<-colClean(res4)
a3<-colClean2(a2)
a4<-colClean3(a3)
 res4flow<-colClean4(a4)
  

  isolate({
   # DT::datatable(  res4flow,editable = TRUE,selection = 'none',
   #             extensions = 'Buttons',rownames=FALSE, options = list(
   #               lengthMenu = c(50, 100, 200,2000), pageLength = 1000,
   #               dom = 'Blfrtip',
   #               scrollX='400px',
   #               buttons = list(
   #                 'copy', 
   #                 'csv', 
   #                 list(extend = 'excel', title = NULL), 
   #                 'pdf', 
   #                 'print'
   #               )
   #               
   #    
   #  )
   #  )
   #  
    datatable(head(res4flow, 10),width=700,
              options = list(pageLength = 5, dom = 'tip', scrollX='400px'), rownames = FALSE
    )    
  })
  
})




output$flow_txtlink <- downloadHandler(
  
  
  filename <- paste0("Step1_Matrix_",ts(),".txt",spe=""),
  
  content <- function(file) {
    
    
    write_tsv(dataset_flow(),file) 
  }
)




#output$flow_info <- renderInfoBox({
#  infoBox(
#    "Generic Step 1", "", icon = icon("thumbs-up", lib = "glyphicon"),
#    color = "green", fill = TRUE,width = 12
#  )
#})

