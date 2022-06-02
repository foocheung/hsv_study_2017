tabItem(tabName = "item2B" ,
        
        
        box(
                width = 8,

                title = "box",
                closable = TRUE,
                status = "warning",
                solidHeader = FALSE,
                collapsible = TRUE,
                enable_sidebar = TRUE,
                sidebar_width = 25,
                sidebar_start_open = TRUE,

              #  sidebar_content = tagList(
               sidebar = boxSidebar(
                       id = "mycardsidebar",
                        radioButtons("pca_color", label = "Color By",
                                     choices = list("SampleID" = "SampleID", "Subject" = "Subject", "Group" = "Group", "TimePoint"="TimePoint"),
                                     selected = "TimePoint"),
                        selectInput(inputId = "show_labels",choices = c("all","none"), label = "Show Labels",selected = "none"),
                        selectInput(inputId = "show_eclipse",choices = c("TRUE","FALSE"), label = "Show Ellipses",selected = "FALSE"),
                        actionButton("pcago", "Submit")

                ),
       #uiOutput("in_boxpca"),
        
                plotOutput("pca2")%>% withSpinner(color="#0dc5c1")
        )
)