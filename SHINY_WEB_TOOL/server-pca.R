
#mdg<-callModule(uploadg, "test")


#outputOptions(output, 'fileg', suspendWhenHidden=FALSE)

# output$in_boxpca <-renderUI({
# 
# #  if(is.null(input$go2 )|input$go2 == 0 |input$go2 == 'FALSE')
#  # if (is.null(input$fileg) |input$fileg == 0 |input$fileg == 'FALSE')
#   ##all4<-dataset() 
#   pca_dat<-mdg()$myD2
#   names(pca_dat)<-  sub("  ", " ", names(pca_dat))
#   all4<-as.data.frame(pca_dat)  
#   
#   
#   nonum <-all4  %>% select_if(negate(is.numeric)) 
#   cmeta <-all4  %>% select(matches('SampleID|Subject|TimePoint|Group')) 
#   pca_cols<-cbind(nonum,cmeta) %>% colnames() %>% unique()
# tagList(
#   box(
#           width = 3,
# 
#           title = "box",
#           closable = TRUE,
#           status = "warning",
#           solidHeader = FALSE,
#           collapsible = TRUE,
#          
# 
#           #sidebar_content = tagList(
#                   #radioButtons("pca_color", label = h3("Color By"),
#                   #             choices = list("SampleID" = "SampleID", "Subject" = "Subject", "Group" = "Group", "TimePoint"="TimePoint"),
#                   #             selected = "TimePoint"),
#                  #selectInput(colnames(myData2),
#                 #             colnames(myData2),
#                 #             selectize = TRUE,
#                 #             multiple = TRUE)
#             selectInput(inputId = "pca_color",choices = pca_cols, label = h3("Color By"),selected = "TimePoint"),
#           selectInput(inputId = "show_labels",choices = c("TRUE","none"), label = h3("Show Labels"),selected = "none")
#                  # hr(),
#                 # fluidRow(column(3, verbatimTextOutput("value"))),
# 
#               #    actionButton("go_pca", "Submit")
# 
#           #)
#   )
# )
# 
# })

#####VERY NICE TO USE WITH CONIDITIONAL PANEL ON UI

output$fileUploaded2c <- reactive({
  req(input$go_step2c)
  #return(!is.null(mdg()$myD2))
  return(1)
})

outputOptions(output, 'fileUploaded2c', suspendWhenHidden=FALSE)



output$fileUploaded2b <- reactive({
  req(input$go_step2)
   #return(!is.null(mdg()$myD2))
return(1)
  
  })

outputOptions(output, 'fileUploaded2b', suspendWhenHidden=FALSE)

output$fileUploaded2 <- reactive({
  req(input$go_step2b)
  #return(!is.null(mdg()$myD2))
  return(1)
  
})

outputOptions(output, 'fileUploaded2', suspendWhenHidden=FALSE)

output$fileUploaded2filter <- reactive({
  ##req(input$go_step2_filter)
  return(!is.null(mdg()$myD2))
  ##return(1)
})

outputOptions(output, 'fileUploaded2filter', suspendWhenHidden=FALSE)

############################################
#go_stepIII

output$fileUploaded <- reactive({
   # return(!is.null(input$go_stepIII))
  
  # if (is.null(input$go_stepIII) |input$go_stepIII == 0 |input$go_stepIII == 'FALSE')
  # {
  #   
  # }
  # else{
  #   return(1)
  # }
  req(input$go_stepIII)
  return(1)
  })

outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  




output$pca2<- renderPlot({
 # if (is.null(input$fileg) |input$fileg == 0 |input$fileg == 'FALSE')
#    return()
 #if(is.null(input$go2 )|input$go2 == 0 |input$go2 == 'FALSE')
#  return()
#require(input$go_pca)
 
  
  
  
  #isolate ({
  pca_dat<-mdg()$myD2
  ###REMOVE EXTRA SPACES IN COLUMN NAMES LIKE aaaaa$`IGHD  IGK@ IGL@`
  names(pca_dat) <- sub("  ", " ", names( pca_dat))


  ttttest<<-pca_dat

  pcadat2 <- pca_dat

##  pcadat2$value<-as.numeric(pcadat2$value)


  all1<-pcadat2

  library(ggfortify); library(ggplot2)

  eeee<-input$mychooser2$right
  eee<-input$mychooser$right

  ff<<-all1
  fff2<<-eeee
  fff<<-eee

  mat<-all1 %>% dplyr::select(-matches('SampleID.|Subject.|TimePoint.|Group.'))

  aaa<<-mat

  all3<-mat %>% filter(SampleID %in% c(input$mychooser2$right))
  aaaaa<<-all3
  all4<-all3 %>% dplyr::select(1,2,3,4,dput(eee))
   all4 <- dataset()
 
  # pca_dat<-mdg()$myD2
  # names(pca_dat)<-  sub("  ", " ", names(pca_dat))
  # all4<-as.data.frame(pca_dat)  
  # 
  # 
  #    rownames(all4)<-all4$SampleID
  #   
  #     
  #  # pcol <-input$pca_color
  # #  pcollll<<-pcol
  # 
     IDS<-as.tibble(na.omit(as.matrix(all4[c(1,5:length(all4))])))  %>% select(SampleID)
  #   
  #   iiiiddds<<-IDS
  #   
   all444<<-all4
   rownames(all4)<-all4$SampleID  
   pp<-dput(IDS, control = NULL)
     bb<-all4 %>% filter(SampleID %in% pp$SampleID) %>% dplyr::select(input$pca_color)
  #   
  #   
     bbbb<<-bb
    ccccc<<- input$show_labels
     if (length(t(unique(bb))) ==1){
       fviz_pca_ind(prcomp(
         na.omit(all4[5:length(all4)])), addEllipses=input$show_eclipse,ellipse.level=0.95 ,label=input$show_labels )
       
       
     }
     else{
       fviz_pca_ind(prcomp(
         na.omit(all4[5:length(all4)])), habillage=as.vector(t(bb)) ,label=input$show_labels,addEllipses=input$show_eclipse, ellipse.level=0.95  )
       
       
     }
     
    
   # fviz_pca_ind(prcomp(
  #    na.omit(all4[5:length(all4)])), addEllipses=TRUE, ellipse.level=0.95  )
#})



})



