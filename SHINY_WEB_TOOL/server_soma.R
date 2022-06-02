source("upload_raw_soma2.R")

md<-callModule(upload, "test")

ts<-reactive(
  
  timestmp<-format(Sys.time(), "%d-%b-%Y_%H_%M_%S")
  
)


output$filter_miss2 <-renderUI({
 # if (is.null(md()$myD2)) return()
  #res<-(md()$myD2)
res<-soma_data()
  tagList(
  
 # box(
  #  width = 6,height = 400,
  #  title = strong("Step 1B: Download Your Data Matrix Here. "),
    tags$br(),
   
    downloadButton('soma_txtlink3','Step1_Matrix') %>% withSpinner(color="#0dc5c1") , 
    downloadButton('soma_txtlink2','Step1_Temp1') %>% withSpinner(color="#0dc5c1") , 
    downloadButton('soma_txtlink','Step1_Temp2') %>% withSpinner(color="#0dc5c1") , 
   tags$br(),  tags$br(), 
   tags$p('Download and populate the SampleID, TimePoint, Subject and Group Columns in your favourite software eg Excel. Upload this annotated file in Step 2'),
    tags$br(),
   tags$hr(),
    checkboxInput('soma_tbl', tags$span("View First 10 Rows"), FALSE),
    checkboxInput('ready4step2', tags$span("Ready To Proceed To Step 2?", style = "color: red;"), FALSE),
    conditionalPanel(
      condition = "input.ready4step2 == true",
      tags$strong('Click on the button and proceed to next step '),
      #tags$style(type='text/css', "button#go_step2 {margin-left: 60%;}"),
      actionButton("go_step2","Go To Step 2",class = "btn-primary")
    ),
  conditionalPanel(
    condition = "input.soma_tbl == true",
    datatable(head(res, 10),width=700,
      options = list(pageLength = 5, dom = 'tip', scrollX='400px'), rownames = FALSE
    
    )) %>% withSpinner(color="#0dc5c1")
     
   )


  
})



observeEvent(input$go_step2, {
 # newtab <- switch(input$tabs, "one" = "two","two" = "one")
  updateTabItems(session, "sidebarmenu2", "upload_generic")
})


# observeEvent(input$dataInput_soma, {
#   if (input$dataInput_soma == 1){
#   updateTabsetPanel(session, inputId = "tabs_soma",
#                     selected = "Example Data"
#   )
#   }
# })
# 
# testin<-reactive({
#   req(input$dataInput_soma)
#   if (input$dataInput_soma == 1){
#     updateTabsetPanel(session, inputId = "tabs_soma",
#                       selected = "Example Data"
#     )
#   }
#   
# })

observeEvent(input$dataInput_soma,
             # tagList(
          
           #  isolate(
             if (input$dataInput_soma == 1){
               removeTab(inputId = "tabs_soma",target = "Download Matrix")
               removeTab(inputId = "tabs_soma",target = "QC-PCA")
              
               appendTab(inputId = "tabs_soma",  select = TRUE,
                         tabPanel("QC-PCA",
                                  uiOutput('pca_col'),
                                  box(title="PCA", status = "primary", solidHeader = FALSE, width=8,
                                      plotlyOutput("pca1") %>% withSpinner(color="#0dc5c1")
                                  )
                         )
               )
               appendTab(inputId = "tabs_soma",  select = TRUE,
                         tabPanel("Download Matrix",
                                  uiOutput('filter_miss2')
                         )
               )
                 }
            # else if (input$dataInput_soma == 2){
            #   removeTab(inputId = "tabs_soma",target = "Example Data")
            #   removeTab(inputId = "tabs_soma",target = "QC-PCA")
            # }


             )






observeEvent(input$file1,
         isolate(
             if (input$dataInput_soma == 2){
               removeTab(inputId = "tabs_soma",target = "Download Matrix")
               removeTab(inputId = "tabs_soma",target = "QC-PCA")

               appendTab(inputId = "tabs_soma",  select = TRUE,
                         tabPanel("QC-PCA",
               uiOutput('pca_col'),
               box(title="PCA", status = "primary", solidHeader = FALSE, width=12,
                   plotlyOutput("pca1" ) %>% withSpinner(color="#0dc5c1")
               )
               )
               )
               appendTab(inputId = "tabs_soma",  select = TRUE,
                         tabPanel("Download Matrix",

                                  uiOutput('filter_miss2') %>% withSpinner(color="#0dc5c1")


                         )


               )
             }
             else if (input$dataInput_soma == 1){
            #   removeTab(inputId = "tabs_soma",target = "Download Matrix")
            #   removeTab(inputId = "tabs_soma",target = "QC-PCA")



             }

)
  )
#})




make_dat<-reactive({
  
  
  req(input$file1)
  
  
  inFile <- input$file1
  # is.null(inFile)
  
  
  # myData<-read_excel(inFile$datapath)
  # myData<-read_excel("20190387_Tsang_NPX.xlsx")
  
  
  file<-inFile$datapath
  
  f<-SomaDataIO::read_adat(file)
  print(f)
  
  soma_meta<-SomaDataIO::getFeatureData(f)
  ssss<<-soma_meta
  g<-as.tibble(f)
#  r<-g
  r<-as.tibble(SomaDataIO::read_adat(file))
  
  start_prot_index<-min(grep("^seq", colnames(r)) )
  rrr<<-r
  t<-getFeatureData(f) 
  ###t$EntrezGeneSymbol[t$EntrezGeneSymbol==''] <- NA
  tt<<-t
  u<-t %>% filter(EntrezGeneSymbol !="")
  u$EntrezGeneSymbol[match(colnames(f), u$AptName)]
  data.table::setnames(g, old = u$AptName, new = u$EntrezGeneSymbol,skip_absent = TRUE)

  colnames(g)<-make.names(colnames(g), unique = TRUE)
  myDatah<-g
  
  

  
  
    
  #}
  #else{
  ## myDatah<-mat %>% dplyr::filter(SampleType == 'Sample')
  
  #}
  
  if("SampleId" %in% colnames(myDatah))
  {
    myDatah<-myDatah %>% dplyr::rename("SampleID"=SampleId)
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
  
  
  if("SampleGroup" %in% colnames(myDatah))
  {
    myDatah<-myDatah %>% dplyr::rename("Group"=SampleGroup)
  } else
  {
    myDatah<-myDatah %>% dplyr::mutate(Group=0)
  }
  
 if(unique(is.na(myDatah$TimePoint)) == 'TRUE'){
   
   myDatah<-myDatah %>% dplyr::mutate(TimePoint=0)
 }
  
  #res2<-as.data.frame(myDatah)
  # dddddd<<-res2
  
  # if (is.null(input$filtersomasamples) |input$filtersomasamples == 0 |input$filtersomasamples == 'FALSE')
  # {
  res2<-as.data.frame(myDatah)
  # }
  # else{
  #   res2<-as.data.frame(myDatah) %>% dplyr::filter(SampleType == 'Sample')
  
  # }
  samples_only<-res2%>% dplyr::filter(SampleType =="Sample") %>% dplyr::select(SampleID) %>% pull()
  
   res2 <- res2 %>% select(SampleID,TimePoint,Group,Subject,start_prot_index:length(res2))
  
 
  
  samples_res2<- res2 %>% filter(SampleID %in% samples_only)
 
  HCE<-soma_meta  %>% filter(grepl("Hybridization",Type,ignore.case = TRUE)) %>% dplyr::select(EntrezGeneSymbol) %>% dplyr::pull()
 
  clean_res2<-samples_res2 %>% dplyr::select(!contains(HCE)) 
  results<<-clean_res2
  # openxlsx::write.xlsx(res2,"mydata.xlsx")  
  timestmp<-format(Sys.time(), "%d-%b-%Y_%H_%M_%S")
#  write_tsv(res2,paste0(timestmp, ".txt")) 
#  write_tsv( soma_meta,paste0(timestmp, "_meta.txt")) 
 
  return(list("myD2"=res2, "stmp"=timestmp , "rawsoma"=r, "meta"= soma_meta, "full"=myDatah,"cleaned"=clean_res2))
  
  
  
})



soma_data4<-reactive({
  req(input$dataInput_soma)
  
  if(input$dataInput_soma ==1){
    read.table("www/input_ex_adat.txt", header=T, fill=TRUE, sep="\t")
 # print("This is a Test Dataset")
    } 
  else if(input$dataInput_soma==2){
    aaa11<<- make_dat()$cleaned
    make_dat()$cleaned
  }   
  
  
})

soma_data3<-reactive({
  req(input$dataInput_soma)
  
  if(input$dataInput_soma ==1){
    read.table("www/input_ex_adat.txt", header=T, fill=TRUE, sep="\t")
  } 
  else if(input$dataInput_soma==2){
    aaa<<- make_dat()$meta
    make_dat()$meta
  }   
  
  
})

soma_data2<-reactive({
  req(input$dataInput_soma)
  
  if(input$dataInput_soma ==1){
    read.table("www/input_ex_adat.txt", header=T, fill=TRUE, sep="\t")
  } 
  else if(input$dataInput_soma==2){
    aaa<<- make_dat()$rawsoma
    make_dat()$rawsoma
  }   
  
  
})


soma_data<-reactive({
  
  req(input$dataInput_soma)
 
  if(input$dataInput_soma ==1){
read.table("www/input_ex_adat.txt", header=T, fill=TRUE, sep="\t")
  } 
  else if(input$dataInput_soma==2){
   
    # myData <-md()$myD2
 
    #make_dat()$myD2
    make_dat()$full
    
  }
  
})


output$text2 <- DT::renderDataTable({
  #res<-sliderValues()$results
  
  #if (is.null(input$goButtonosat))
  #  return()
  
  #if (is.null(md()$myD2)) return()
  
 #res<-(md()$myD2)
  req(input$soma_tbl)
  
  
  
  
  
  #DOWNLOADS WRONG SEE IF CAN FIX....
 
  
 # cl_res<-res %>% clean_names()
  #cl_res %>% remove_empty(c("rows", "cols"))
   
#  cl_res <- cl_res %>% remove_empty(c("rows", "cols"))
  isolate({
    res<-soma_data()
    res<- head(res)
    ch_fil<<-input$file1
    
   DT::datatable( res,
                  #,selection = 'none',
               #extensions = 'Buttons',rownames=FALSE, 
               options = list(serverSide=TRUE,
              #   lengthMenu = c(50, 100, 200,2000), pageLength = 10,
                 #dom = 'Blfrtip',
                scrollX='400px')
                 #buttons = list(
             
                  # 'excel'
                 #)
                 
      
    #)
    )
    
  })
  
 # if (input$table){
 ## res2<-as.data.frame(res)
##  dddddd<<-res2
 # openxlsx::write.xlsx(res2,"mydata.xlsx")  
  
##  write_tsv(res2,"mydata.txt") 
 # }
  
})
#observe(
  
 # res<-(md()$myD2),
  
  
#)



output$soma_txtlink <- downloadHandler(
    
  
  filename <- paste0("Step1_Temp2_", ts(),".txt",spe=""),
   
  content <- function(file) {
    

    write_tsv(soma_data(),file) 
    }

  
  
  
  )

output$soma_txtlink2 <- downloadHandler(
  
  
  filename <- paste0("Step1_Temp1_", ts(),".txt",spe=""),
  
  content <- function(file) {
    
    
    write_tsv(soma_data3(),file) 
  }
  
  
)

  output$soma_txtlink3 <- downloadHandler(
    
    
    filename <- paste0("Step1_Matrix_",ts(),".txt",spe=""),
    
    content <- function(file) {
      
      
      write_tsv(soma_data4(),file) 
    }
  
  
  
)








#output$soma_info <- renderInfoBox({
#  infoBox(
#    "SomaLogic Step 1", "", icon = icon("thumbs-up", lib = "glyphicon"),
#    color = "yellow", fill = TRUE,width = 12
#  )
#})

