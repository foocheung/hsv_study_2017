
tabItem(tabName = "item6A" ,
        box(
          width = 3,
        uiOutput('xdat')%>% withSpinner(color="#0dc5c1")
        ),
        box(
          width = 8,
        uiOutput('out1')#%>% withSpinner(color="#0dc5c1")
        )
       # uiOutput('filter_sample')%>% withSpinner(color="#0dc5c1"),
      #  uiOutput('filter_sampleII')%>% withSpinner(color="#0dc5c1")
        
)
