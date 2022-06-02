tabItem(tabName = "ui_olink" ,
        
      #  infoBoxOutput("olink_info"),
          source("body2_ui_olink.R",local=TRUE)$value,
          
  
        #  plotOutput('qc2'),    
        uiOutput('merge_ol_ui')
       )
        
        
        
        
     

