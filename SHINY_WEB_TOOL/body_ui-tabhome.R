#Different Proteomics platforms represent protein expression in different formats.
#Here re-format these files into a tab delimited matrix like structure which can be easily parsed for further analysis.

tabItem(tabName = "home" ,
        
        box(
          width = 12,
          #includeHTML("intro_text.html")
         # includeMarkdown("Untitled.Rmd")
         tabPanel("Reference", 
                  tags$iframe(style="height:800px; width:100%; scrolling=yes", 
                              src="Untitled.pdf"))
          
)

)
