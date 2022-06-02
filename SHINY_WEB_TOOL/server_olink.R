source("upload_raw_olink.R")

ol_d<-reactive({
 # req(input$file3)

if (input$dataInput_ol == 1){
  #input$file3 <- NULL
  
  read_NPX(filename = "www/20190387_Tsang_NPX.xlsx")

}else if (input$dataInput_ol == 2){
  

  ffff<<-input$file3
  inFile <- input$file3
  
read_NPX(filename = inFile$datapath)

}
 
})

# #output$filter_miss3A <-renderUI({
#  
#   #req(input$file3)
#   req(ol_d())
#   #  if (is.null(se3()))
#   
#  # if (is.null(se3())) return()
# #if (input$dataInput_ol == 1 | is.null(md3()$myD2))
# #  return()
# 
#     tagList(
# tags$br(),
# 
#    # box(width = 6,
#             sliderInput(
#               "miss3",
#               "Filter Proteins By Percent Of Missing Sample Values:",
#               min = 1,
#               max = 100,
#               value = 25
#             )
# 
#   #  )
#     )
# })


#output$merge_ol_ui <-renderUI({
  #req(ol_d())

observeEvent(input$dataInput_ol,
             # tagList(
             
              if (input$dataInput_ol == 1){
                removeTab(inputId = "tabs",target = "Download Matrix")
                removeTab(inputId = "tabs",target = "QC-PCA")
                 
                 appendTab(inputId = "tabs",  select = TRUE,
                           tabPanel("QC-PCA",
                                    tabsetPanel(
                                      tabPanel("PCA",
                                               uiOutput('opca_col'),
                                               box(title="PCA", status = "primary", solidHeader = FALSE, width=12,
                                                   plotlyOutput("opca1")%>% withSpinner(color="#0dc5c1")
                                               )
                                      ),
                                      tabPanel("NPX Distribution",
                                               uiOutput('odist_col'),
                                               plotOutput('pl_npx'))
                                      
                                    )                  
                           )
                 )
                 appendTab(inputId = "tabs",  select = TRUE,
                           tabPanel("Download Matrix",
                                    # # box(width=6,height=600,title = strong("Step 1B: Download Your Data Matrix Here. "),
                                    
                                    
                                #    uiOutput('filter_miss3A')%>% withSpinner(color="#0dc5c1"),
                                    uiOutput('filter_miss3')%>% withSpinner(color="#0dc5c1"),
                                    uiOutput('filter_miss3b')%>% withSpinner(color="#0dc5c1")
                           )
                           
                 )
                 
              }
             else if (input$dataInput_ol == 2){
             #  removeTab(inputId = "tabs",target = "QC-PCA")
            #   removeTab(inputId = "tabs",target = "Example Data")
               
             }
             #if (input$dataInput_ol == 2){
             # appendTab(inputId = "tabs",  select = TRUE,
             #           tabPanel("Step 1B",
             #                    # box(width=6,height=600,title = strong("Step 1B: Download Your Data Matrix Here. "),
             #                    
             #                    
             #                    uiOutput('filter_miss3A'),
             #                    uiOutput('filter_miss3'),
             #                    uiOutput('filter_miss3b')
             #                    #            )
             #                    
             #                    
             #           )
             #)
)
                       
observeEvent(input$file3,
 # tagList(
 
 if (input$dataInput_ol == 2){
   removeTab(inputId = "tabs",target = "Download Matrix")
   removeTab(inputId = "tabs",target = "QC-PCA")
   
             
   appendTab(inputId = "tabs",  select = TRUE,
             tabPanel("QC-PCA",
                      tabsetPanel(
                      tabPanel("PCA",
                        uiOutput('opca_col'),
                      box(title="PCA", status = "primary", solidHeader = FALSE, width=12,
                         plotlyOutput("opca1")%>% withSpinner(color="#0dc5c1")
                         )
                         ),
                        tabPanel("NPX Distribution",
                          uiOutput('odist_col'),
                          plotOutput('pl_npx'))
                          
                      )                  
   )
                      
     )
   
   
   appendTab(inputId = "tabs",  select = TRUE,
   tabPanel("Download Matrix",
            # box(width=6,height=600,title = strong("Step 1B: Download Your Data Matrix Here. "),
            
            
            #uiOutput('filter_miss3A')%>% withSpinner(color="#0dc5c1"),
            uiOutput('filter_miss3')%>% withSpinner(color="#0dc5c1"),
            uiOutput('filter_miss3b')%>% withSpinner(color="#0dc5c1")
            #            )
            
            
   )
   
   )
   }
 else if (input$dataInput_ol == 1){
  # removeTab(inputId = "tabs",target = "Download Matrix")
  # removeTab(inputId = "tabs",target = "QC-PCA")
   

  
 }

#else{
#  removeTab(inputId = "tabs",target = "Step 1B") 
  
#}

)
   #  )
#})


output$filter_miss3 <-renderUI({
  # req(input$file4)
  #if (is.null(input$miss3)) return()
  
  
  myData<-se3()
 ddddd<<-se3()
  #isolate(
  tagList(
   
            
            selectInput( inputId = "dl_dat3",
                         label = "Choose dataset",
                         unique(myData$Panel),
                         selected =  unique(myData$Panel),
                         selectize = TRUE, 
                         multiple = FALSE#,
                         
    ),
   
        
        
        downloadButton('olink_txtlink','Step1_Matrix') %>% withSpinner(color="#0dc5c1") ,
        downloadButton('olink_txtlink2','Step1_Temp') %>% withSpinner(color="#0dc5c1") ,
        tags$br(), 
        tags$p('Download and populate the SampleID, TimePoint, Subject and Group Columns in your favourite software eg Excel. Upload this annotated file in Step 2'),
        tags$br(),
        tags$hr(),
    checkboxInput('olink_tbl', tags$span("View First 10 Rows"), FALSE),
   checkboxInput('ready4step2olink', tags$span("Ready To Proceed To Step 2?", style = "color: red;"), FALSE),
   conditionalPanel(
     condition = "input.ready4step2olink == true",
     tags$strong('Click on the button and proceed to next step '),
     
     #tags$style(type='text/css', "button#go_step2 {margin-left: 60%;}"),
     actionButton("go_step2b","Go To Step 2",class = "btn-primary"),
     tags$hr()
   ))
   
          
})  

observeEvent(input$go_step2b, {
  # newtab <- switch(input$tabs, "one" = "two","two" = "one")
  updateTabItems(session, "sidebarmenu2", "upload_generic")
})

output$filter_miss3b <-renderUI({
  req(input$olink_tbl)
  myData<-dat3()
  tagList(
#box(width=6,height=600,
        conditionalPanel(
          condition = "input.olink_tbl == true",
          # dataTableOutput('text2')%>% withSpinner(color="#0dc5c1")
          datatable(head(myData, 10),width=700,
                    options = list(pageLength = 5, dom = 'tip', scrollX='400px'), rownames = FALSE
                    
#          )
        )
        
)
)
})




output$table3text <- DT::renderDataTable({
  req(input$dl_dat3)
  # req(input$file4)
  # if (is.null(input$goButtonosat4) |input$goButtonosat4 == 0 |input$goButtonosat4 == 'FALSE')
  #    return()
  
  res2<-dat3()
  
  #DOWNLOADS WRONG SEE IF CAN FIX....
  
  
  # cl_res<-res %>% clean_names()
  #cl_res %>% remove_empty(c("rows", "cols"))
  
  #  cl_res <- cl_res %>% remove_empty(c("rows", "cols"))
  #  isolate({
  DT::datatable( res2,editable = TRUE,
                 extensions = 'Buttons',rownames=FALSE, options = list(
                   lengthMenu = c(50, 100, 200,2000), pageLength = 1000,
                   dom = 'Blfrtip',
                   scrollX='400px',
                   buttons = list(
                   #  'copy', 
                  #   'csv', 
                     list(extend = 'excel', title = NULL)#, 
                  #   'pdf', 
                  #   'print'
                   )
                   
                   
                 )
  )
  
  # })
  
})



dat3<- reactive({
  req(input$dl_dat3)

    #req(md3()$myD2)
  req(ol_d())
  #  if (is.null(input$goButtonosat4) |input$goButtonosat4 == 0 |input$goButtonosat4 == 'FALSE')
  #    return()
  
#   isolate({

  data<-se3()
  
    ffff<<-data
 chhh<-data
  chhh$value<-as.numeric(chhh$value)
  ## da<-chhh %>% filter(panel==input$dl_dat4) %>% spread(key="protein",value="value") %>% mutate_if(is.numeric, round, digits=3)
  ##da<-chhh %>% filter(Panel==input$dl_dat3) %>% pivot_wider(names_from = protein, values_from = value) %>% mutate_if(is.numeric, round, digits=3)
  da<-chhh %>% filter(Panel==input$dl_dat3) %>% pivot_wider(names_from = protein, values_from = value) %>% unnest() %>% mutate_if(is.numeric, round, digits=3)
  da
  
  #   })
  ##mutate(row = row_number()) %>% 
  
})





##Do PCA od the data on a separate tab


##



se3 <- reactive({
  #req(md3()$myD2)
  req(ol_d())
 ## req(input$file3)
  
  #req(input$miss3)
 # if (is.null(input$goButtonosat) )
    
  #  return()
  
  
  #if (input$dataInput_ol == 1)
  #{
  #  a<-read_NPX(filename = "www/20190387_Tsang_NPX.xlsx")
  #}
  #else if (input$dataInput_ol == 2)
  #{
   # a<-md3()$myD2
  a<-ol_d()
    
   # a<-(md3()$myD2)  
    st<<-a
    
    
    
      
   
    typ<-as.tibble(a %>% select(Panel) %>% unique())
    
    oper<-foreach (i=1:length(t(typ)),.combine="rbind") %do% { 
      
    
    ##  missing_proteins <-  a %>% select(Assay,MissingFreq,Panel) %>% filter(Panel == typ$Panel[i]) %>% filter(MissingFreq >= input$miss3) %>% unique() %>% select(Assay)
      missing_proteins <-  a %>% select(Assay,MissingFreq,Panel) %>% filter(Panel == typ$Panel[i])  %>% unique() %>% select(Assay)
      m<-dput(missing_proteins$Assay)
      
    #  mm<-a %>% filter(Assay %in% (m)) %>% filter(Panel == typ$Panel[i])
    #  e<-a %>% filter(!Assay %in% (m))%>% filter(Panel == typ$Panel[i])
      
     # check<-a %>% filter(!Assay %in% (m))%>% filter(Panel == typ$Panel[i]) %>% select(SampleID,Panel,Panel_Lot_Nr,Panel_Lot_Nr,NPX,Assay,PlateID) %>% pivot_wider(names_from = Assay, values_from = NPX) 
     #DONT FILTER check<-a %>% filter(!Assay %in% (m))%>% filter(Panel == typ$Panel[i]) %>% select(SampleID,Panel,NPX,Assay) %>% pivot_wider(names_from = Assay, values_from = NPX) 
       check<-a %>%  filter(Panel == typ$Panel[i]) %>% select(SampleID,Panel,NPX,Assay) %>% pivot_wider(names_from = Assay, values_from = NPX)  %>% unnest()
      
       f23<<-check
      
      ###www<-d
      if("Subject" %in% colnames(check))
      {
        cat("Yep, Subject it's in there!\n");
      } else
      {
        check<-check %>% dplyr::mutate(Subject=0)
      }
      
      if("TimePoint" %in% colnames(check))
      {
        cat("Yep, TimePoint it's in there!\n");
      } else
      {
        check<-check %>% dplyr::mutate(TimePoint=0)
      }


      if("Group" %in% colnames(check))
      {
        cat("Yep, Group it's in there!\n");
      } else
      {
        check<-check %>% dplyr::mutate(Group=0)
      }

      if("SampleID" %in% colnames(check))
      {
        check<-check %>% dplyr::rename("SampleID"=SampleID)
      } else
      {
        
        check<-check %>% dplyr::mutate("SampleID"=0)
      }
      
      
      
      check1<-check %>% dplyr::select("SampleID","TimePoint", "Group","Subject", 2:(length(check)-3))
     # check<-check %>% rename("SampleID" = Assay) %>% dplyr::select("SampleID", 4:length(e))
      
      
      
      check2<-gather(check1,6:length(check1),key="protein",value="value")
   #   check2<-gather(check1,9:length(check1),key="protein",value="value")
      
 fffffff<<-check2
      check2
     
     
    }
    
  })
  
 # ttttt<-check2
  #  oper2<-check2
#  return("oper2" = check2 )
  #return(l("oper" = oper, "check2" = check2))
  #oper
#check2
  


output$olink_txtlink2 <- downloadHandler(
  
  
  filename <- paste0("Step1_Temp1_", ts(),".txt",spe=""),
  
  content <- function(file) {
    
    
    write_tsv(ol_d(),file) 
  }
)


output$olink_txtlink <- downloadHandler(
  
  
  filename <- paste0("Step1_Matrix_", ts(),".txt",spe=""),
  
  content <- function(file) {
    
    
    write_tsv(dat3(),file) 
  }
)



output$pl_npx<-renderPlot({
  req(input$qc_olink_go2)
  
  
  isolate({
    odata <- ol_d()
    
  olink_dist_plot(odata %>% dplyr::filter(Panel ==  input$s_opanel2)) +
    theme(axis.text.x = element_text(angle = 90)) +
    #  theme(axis.text.x = element_blank(),
#        axis.ticks.x = element_blank()) +
  scale_fill_manual(values = c('turquoise3', 'red'))
})

  })



