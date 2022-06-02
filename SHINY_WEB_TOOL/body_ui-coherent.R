tabItem(tabName = "item5B" ,
        
        
        
        
        
  #    uploadUI("test"),
  box(
    width = 8,
     uiOutput('fil_tp6')%>% withSpinner(color="#0dc5c1")
    ),
    box(
      width = 8,
  dataTableOutput('text6')%>% withSpinner(color="#0dc5c1")
    )
        
        
        
        
        
)