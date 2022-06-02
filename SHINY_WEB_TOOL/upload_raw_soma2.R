uploadUI <- function(id,myData3) {
  ns <- NS(id)
  
  
  tagList(
    box(
     # color="yellow",
    status = "warning", 
    solidHeader = TRUE,
     title = "Step 1 SomaLogic",
      width=8,#height = 800,
      tabsetPanel(id = "tabs_soma",
                  tabPanel(strong("Upload Your file"),
     
      tags$br(),
      conditionalPanel(condition="input.dataInput_soma=='2'",
    fileInput('file1',"",
              accept=c(
                '.adat')   )
    
    ),
    
    
    radioButtons("dataInput_soma", "", list("Upload Your Data"=2,
                                            "Load Example Data"=1)
    ),
    tags$a(
      href = 'PLASMA.1.3k.HybNorm.MedNorm.Cal.20151030.adat',
      target = "_blank",
      class = "btn",
      icon("download"),
      'Download SomaLogic Example adat file'
    ),
    
    
    tags$hr(),
    checkboxInput(inputId = "show_soma_vid","View Help Video",FALSE),
  #  conditionalPanel(condition="input.show_soma_vid=='1'",
  #  HTML('<iframe width="350" height="250" src="https://www.youtube.com/embed/V6yJLILUmWA" frameborder="0" allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')
  #  ),
   ## checkboxInput("filtersomasamples", "Filter Samples", TRUE),
 
  conditionalPanel(condition="input.show_soma_vid=='1'",
     
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



)
  ) 
}






upload <- function(input, output, session, myData3) {
  
  
  
  
  filedata <- reactive({
    
     req(input$file1)
     inFile <- input$file1
    # 
     file<-inFile$datapath
    # lab<-"EntrezGeneSymbol"
    # rhit<-read_tsv(file) %>% dplyr::mutate(row_name = row_number())  %>% dplyr::filter_at(1, all_vars(. =="^TABLE_BEGIN" )) %>% dplyr::select(row_name)
    # tab_st<-rhit +2
    # 
    # idcols  <-  read_tsv(file,skip=tab_st$row_name,n_max = 0) 
    # 
    # ensem_cols_st<-read_tsv(file,skip=tab_st$row_name)  %>% dplyr::mutate(row_name = row_number())  %>% dplyr::filter_all(any_vars(str_detect(., pattern = lab)))%>% dplyr::select(row_name)
    # idcols2  <-  read_tsv(file,skip=tab_st$row_name + ensem_cols_st$row_name,n_max = 0) 
    # 
    # sample_in<-grep(lab, colnames(idcols2)) - 1
    # 
    # sample_st <-  read_tsv(file,skip=tab_st$row_name)  %>% dplyr::mutate(row_name = row_number()) %>% filter(X1 == 'PlateId') %>% dplyr::select(row_name)
    # 
    # smpcols <-  read_tsv(file,skip=tab_st$row_name+sample_st$row_name,n_max = 0) 
    # 
    # col<-cbind(smpcols[c(1:sample_in)],idcols2[-c(1:sample_in)])
    # 
    # 
    # soma_data<-read_tsv(file,skip=tab_st$row_name+sample_st$row_name+1,col_names = FALSE) 
    # 
    # colnames(soma_data)<-col
    # mat<-read_tsv(file,skip=tab_st$row_name+sample_st$row_name+1,col_name=colnames(col))
    #   myDatah<-mat
    
    
    f<-read_adat(file)
    
    g<-as.tibble(f)
    gg<<-g
    
    t<-getFeatureData(f) 
    ###t$EntrezGeneSymbol[t$EntrezGeneSymbol==''] <- NA
    tt<<-t
    u<-t %>% filter(EntrezGeneSymbol !="")
    u$EntrezGeneSymbol[match(colnames(f), u$AptName)]
    data.table::setnames(g, old = u$AptName, new = u$EntrezGeneSymbol,skip_absent = TRUE)
    gg<-g
    colnames(g)<-make.names(colnames(g), unique = TRUE)
    myDatah<-g
    
    
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
     
     
     if("Group" %in% colnames(myDatah))
     {
       cat("Yep, Group it's in there!\n");
     } else
     {
       myDatah<-myDatah %>% dplyr::mutate(Group=0)
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
       res2 <- res2 %>% select(SampleID,TimePoint,Group,Subject,everything())
       

     
     # openxlsx::write.xlsx(res2,"mydata.xlsx")  
     timestmp<-format(Sys.time(), "%d-%b-%Y_%H_%M_%S")
     write_tsv(res2,paste0(timestmp, ".txt")) 
     
    
    
     
     
    
    
    return(list("myD2"=res2, "stmp"=timestmp ))
  })
  
  
  
}

