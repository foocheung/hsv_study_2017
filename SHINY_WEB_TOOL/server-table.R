source("upload_generic.R")

mdg<-callModule(upload, "test")






output$filter_miss <-renderUI({
  if (is.null(mdg()$myD2)) return()
 
tagList(
  
box(
  width = 12,
  title = "Matrix Version", 
  closable = TRUE, 
  status = "warning", 
  solidHeader = FALSE, 
  collapsible = TRUE,
  dataTableOutput('text1')
)
)
  
})




output$text1 <- DT::renderDataTable({
  #res<-sliderValues()$results
  
  #if (is.null(input$goButtonosat))
  #  return()
  
  if (is.null(mdg()$myD2)) return()
  
  res<-(mdg()$myD2)
###  ch_fil<<-input$file1
  #DOWNLOADS WRONG SEE IF CAN FIX....
 
  
 # cl_res<-res %>% clean_names()
  #cl_res %>% remove_empty(c("rows", "cols"))
   
#  cl_res <- cl_res %>% remove_empty(c("rows", "cols"))
  isolate({
    
   DT::datatable( res,editable = TRUE,selection = 'none',
               extensions = 'Buttons',rownames=FALSE, options = list(
                 lengthMenu = c(50, 100, 200,2000), pageLength = 1000,
                 dom = 'Blfrtip',
                 scrollX='400px',
                 buttons = list(
                  
                   list(extend = 'excel', title = NULL)#, 
                   
                 )
                 
      
    )
    )
    
  })
  
})



