
source("chooser.R")

mdg<-callModule(uploadg, "test")

fileloaded<-reactive({
  paste( ("File loaded: "), input$file1$name, sep= "")
})


#output$filter_sampleIII <- renderUI({
  
#  fff<-selection_g()
  
#})
output$filter_sampleII <- renderUI({
  
  if (is.null(mdg()$myD2)) return()
  
  myData<-(mdg()$myD2)
  
  
  
    tagList(
    #  box(
    tabBox(
      tabPanel(  
        title = "Panel 1",
      width = 4,
        status="danger",
        selectInput(
          inputId = "fil_by_g",
          label = "Filter By Group",
          unique(myData$Group),
          selected =  unique(myData$Group),
          selectize = TRUE,
          multiple = TRUE
        ),
        selectInput(
          inputId = "fil_by_t",
          label = "Filter By TimePoint",
          unique(myData$TimePoint),
          selected =  unique(myData$TimePoint),
          selectize = TRUE,
          multiple = TRUE
        )               
                       
      #)
      # ,
      # checkboxInput(inputId = "file_filter_col2",label="Filter Rows By File Containing a List of SampleID Names", value = FALSE),
      # 
      # conditionalPanel(condition = "input.file_filter_col2 == 1",
      #                  fileInput('file_filter2',label="",placeholder = "Upload Your .xlsx file here Containing a list of SampleID",
      #                            accept=c(
      #                              '.xlsx')   )            
      #                  
                       
      #)
  
  
       
      )#,
      
    #  tabPanel(
    #    title = "Panel 2",
    #    actionButton("test", "test"),
        
    #  )
    )
)
})

observeEvent(input$go_stepIII, {
  # newtab <- switch(input$tabs, "one" = "two","two" = "one")
  #updateTabItems(session, "Home", "home")
 updateTabItems(session, "sidebarmenu3", "step3")
updateTabItems(session, "Meta-Plotter", "item6A")
})



# col_filt<-reactive({
#   req(input$file_filter1)
#  # if (is.null(mdg()$myD2)) return()
#   inFile2 <- input$file_filter1
#   print("line 93")
#   print(input$file_filter1)
#   myData_fil1<-read_excel(inFile2$datapath, col_names = FALSE) 
#   ssssss<<-myData_fil1
#   myDatah<- check %>% select(-dput(ssssss$...1))
#   myDatah
# 
# })



filter_g<-reactive({
  
  
  if (is.null(mdg()$myD2)) return()
  
  myData<-(mdg()$myD2)
  fil_g<-input$fil_by_g
  
  if (all(c("Group") %in% colnames(myData))){
    
    myData4 <-myData %>% filter(Group %in% c(fil_g)) ##%>% select(`Sample ID`) %>% unique()
    myData3 <-myData %>% filter( !(Group %in% c(fil_g)))## %>% select(`Sample ID`)%>% unique()
    
  }
  
  
  return(list("myData3"=myData3, "myData4"=myData4))
})

filter_t<-reactive({
  if (is.null(mdg()$myD2)) return()
  myData<-mdg()$myD2
  
  fil_t<-input$fil_by_t
  
  if (all(c("Group") %in% colnames(myData))){
    
    myData6 <-myData %>% filter(TimePoint %in% c(fil_t)) #%>% select(`Sample ID`) %>% unique()
    myData5 <-myData %>% filter( !(TimePoint %in% c(fil_t)))# %>% select(`Sample ID`)%>% unique()
    
  }
  
  
  return(list("myData5"=myData5, "myData6"=myData6))
})








output$filter_sample <- renderUI({
  
  if (is.null(mdg()$myD2)) return()
  
  #if (is.null(input$gof) )
  
  #    return()
  #  if (input$gof==0)
  #    return()
  # isolate({
  ####FIX THIS !!!
  
 
  # if (is.null(input$file_filter1) |input$file_filter1 == 0 |input$file_filter1 == 'FALSE'){
  #   
  # }
  # else{
  #  # sssss2<<-col_filt() 
  #   
  # }
    
  myData<-(mdg()$myD2)
  #myData1 <- filter_p()$myData1
  #myData2 <- filter_p()$myData2
  
  myData3 <- filter_g()$myData3
  myData4 <-filter_g()$myData4
  # 
  myData5 <- filter_t()$myData5
  myData6 <-filter_t()$myData6
  # 
  sss<<-myData
  tttt<-input$file_filter1
  
  
  
  nm_col2<-myData %>%  dplyr::select(-c("TimePoint","Group","Subject","SampleID")) %>%  dplyr::select(where(is.numeric))%>% colnames()
  
  nm_col1<-myData  %>%  dplyr::select(one_of(c("TimePoint","Group","Subject","SampleID"))  | -where(is.numeric)) %>% colnames()
  
  if (length(t(myData5)) > 0 &  length(t(myData3)) > 0){ 
    s1<-dput(unique(c(myData3$SampleID,myData5$SampleID)))
    myData_s_m<- myData %>% dplyr::select(SampleID) %>% unique %>% dplyr::filter(SampleID %in% s1 )
    myData_s<- myData %>% dplyr::select(SampleID) %>% unique %>% dplyr::filter(!(SampleID %in% s1 ))
  }
  else if (length(t(myData5)) == 0 &  length(t(myData3)) > 0){   
    s1<-dput(unique(c(myData4$SampleID)))
    myData_s<- myData %>% dplyr::select(SampleID) %>% unique %>% dplyr::filter(SampleID %in% s1 )
    myData_s_m<- myData %>% dplyr::select(SampleID) %>% unique %>% dplyr::filter(!(SampleID %in% s1 ))
  }
  else if (length(t(myData3)) == 0 &  length(t(myData5)) > 0){   
    s1<-dput(unique(c(myData6$SampleID)))
    myData_s<- myData %>% dplyr::select(SampleID) %>% unique %>%dplyr:: filter(SampleID %in% s1 )
    myData_s_m<- myData %>% dplyr::select(SampleID) %>% unique %>% dplyr::filter(!(SampleID %in% s1 ))
  }
  else if (length(t(myData3)) == 0 &  length(t(myData5)) == 0){   
    #s1<-dput(myData5)
    myData_s<- myData %>% dplyr::select(SampleID)  %>% unique ##%>% filter(`Sample ID` %in% s1$`Sample ID` )
    myData_s_m<- NULL
  }
  
  
  
  tagList(
    
   
    fluidRow(
      column(
        12,
        align = "center",
    box(
      width = 8,
      title = "Filter Columns",
      status = "warning",
      solidHeader = TRUE,
      chooserInput(
        "mychooser",
        "Available frobs",
        "Selected frobs",
        nm_col1,
        nm_col2,
        size = 15,
        multiple = TRUE
      )),
    box(
      width = 4,
      
      title = "Filter Rows",
      status = "success",
      solidHeader = TRUE,
      chooserInput(
        "mychooser2",
        "Available frobs",
        "Selected frobs",
        unique(sort(myData_s_m$SampleID)),
        unique(sort(myData_s$SampleID)),
        
        size = 15,
        multiple = TRUE
      )
    ) )
    ))
  
  # })
  
})




#md<-callModule(upload, "test")

dataset <- reactive({
  #req(input$gocor)
  #if(is.null(input$fileg)) return(NULL)
  if (is.null(mdg()$myD2)) return()
  
  pca_dat<-mdg()$myD2
  names(pca_dat)<-  sub("  ", " ", names(pca_dat))
  all2<-as.data.frame(pca_dat)
  
  
  
  library(ggfortify); library(ggplot2)
  alltt<<-input$mychooser2$right
  allll2<<-all2
  all3<-all2  %>% dplyr::filter(SampleID %in% c(input$mychooser2$right))
  
  
  eee<-input$mychooser$right
  eee1<<-eee
  
  all4<-all3 %>%  dplyr::select(1,2,3,4,dput(eee))
  
  #myData3<-all4
 ##allll4<<-all4

#  del <- function(x) (x - first(x))
  
 # all4<-all4 %>% group_by(Subject)   %>%
#    arrange(TimePoint,.by_group=TRUE) %>% mutate_at(dput(eee), del) 
  
    all4
  
  
  
})
