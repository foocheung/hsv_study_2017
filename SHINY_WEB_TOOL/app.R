#<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css" integrity="sha256-eZrrJcwDc/3uDhsdt61sL2oOBY362qM3lon1gyExkL0=" crossorigin="anonymous" />

rm(list=ls())
options(repos = BiocManager::repositories()) 
options(shiny.maxRequestSize = 3000 * 1024 ^ 2)
options(expressions=10000)
library(autoplotly)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(slickR)
library(vembedr)
library(openxlsx)
library(rio)
#install_version("shinydashboardPlus", version="2.0.2",repos = "http://cran.us.r-project.org")
###library('shinyglide')
library(OlinkAnalyze)
library(flashCard)
source("corTest.R", local = TRUE)
##Add correlation
##Add 
#install.packages("remotes")
#remotes::install_github("dgrtwo/drlib")
library(waiter)
#library(drlib)
library(corrplot)
options(digits=4)
library(shiny)
library(DT)
library(shinycssloaders)
#library(anytime)
#library(mailR)
library(tidyverse)
#library(leaflet)
#library(slickR)
#library(xml2)
library(foreach)
library(lubridate)
library(shinydashboard)
source('global.R')
##source("chooser.R")
library(ggrepel)
library(matrixStats) 
library(plotly)
options(warn=-1)
#library(OSAT)
#library(DT)
#library(ggforce)
library(shiny)
library(gtools)
library(ggplot2)
#library(plyr)
#library(dplyr)
library(readxl)
library(tidyr)
#source('corTest.R')
library(readr)
library("genefilter")
library(janitor)
library(here)
library(robustbase)
library("FactoMineR")
library("factoextra")
#library(stats)

#library(htmltools)

#library(shiny)
#library(DT)
#library(pool)
#library(DBI)
#library(RPostgreSQL)

##library(dplyr)
#library(glue)
#library(purrr)

library(shinythemes)
#library(tools)



library(OSAT)
library(DT)
library(ggforce)
library(shiny)
#library(shinyauthr)

library(ggplot2)
library(dplyr)
library(tidyr)

library(readr)
library("genefilter")
library(stats)

library(htmltools)
source("chooser.R")

source("upload_raw_soma2.R")
source("upload_flow.R")
source("upload_raw_olink.R")
source("upload_generic.R")
library(shinyjs)

library('corrplot')
library(ggfortify)
library('rintrojs')
library('SomaDataIO')
#devtools::install_github("carlganz/rintrojs"

slickOpts=list(autoplay=FALSE)

ll <- as.vector(c(
  "http://lenkiefer.com/img/charts_sep_23_2019/inari.png",
  "http://lenkiefer.com/img/charts_2019_10_08/gganimate_ma.gif"
))
#ll<-c(embed_url("https://www.youtube.com/watch?v=uV4UpCq2azs"))
shinyApp(
  ui = dashboardPage(
    
    header = dashboardHeader(
  #    enable_rightsidebar = TRUE,
  #    rightSidebarIcon = "gears"
      
    ),
    
    
    
    sidebar <- dashboardSidebar(
      sidebarMenu(
        id = 'sidebarmenu',
        introjsUI(),
        
        menuItem('Home', tabName = 'home', icon = icon('home')),
        
        
        source("ui-step1.R",local=TRUE)$value 
      ),
      
      conditionalPanel(
        condition = "output.fileUploaded2b  == true || output.fileUploaded2  == true || output.fileUploaded2c  == true  ",
        sidebarMenu(
          id = 'sidebarmenu2',
          menuItem('Step 2', tabName = 'upload_generic', icon = icon('dice-two')
                   
          )
          
          #   source("ui_generic.R",local=TRUE)$value
          
        )
      ),
      # conditionalPanel(
      #   condition = "output.fileUploaded2filter ==true",
      # sidebarMenu(
      #   id = 'sidebarmenu2b',
      #   source("ui_filter.R",local=TRUE)$value
      # )
      # ),
      # 
      conditionalPanel(
        condition = "output.fileUploaded ==true",
        sidebarMenu(
          id = 'sidebarmenu3',
          menuItem('Step 3', tabName = 'step3', icon = icon('dice-three') ) ,
          source("ui-pca.R",local=TRUE)$value,
          source("ui-group.R",local=TRUE)$value,
          source("ui-paired.R",local=TRUE)$value,
          source("ui-plot.R",local=TRUE)$value,
          
          source("ui-correl.R",local=TRUE)$value,
          source("ui-coherent.R",local=TRUE)$value,
          source("ui-metaplotter.R",local=TRUE)$value
          
          
          #)   
          
          
        )
      ) 
    ),
    
    ##  sidebar = dashboardSidebar(),
    body = dashboardBody(
   ##   tags$style("@import url(https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css);"),
      fluidRow(
        use_waiter(), # 
        use_hostess(),
        tabItems(
          source("body_ui-tabhome.R",local=TRUE)$value,
          source("body_ui_soma.R",local=TRUE)$value,
          source("body_ui_olink.R",local=TRUE)$value,
          
          source("body_ui_upload_generic.R",local=TRUE)$value,
          # source("body_ui_filter.R",local=TRUE)$value,
          source("body_ui-pca.R",local=TRUE)$value,
          source("body_ui-group.R",local=TRUE)$value,
          source("body_ui-pair.R",local=TRUE)$value ,
          source("body_ui-plot.R",local=TRUE)$value,
          source("body_ui_flow.R",local=TRUE)$value,
          source("body_ui-correl.R",local=TRUE)$value,
          source("body_ui-metaplotter.R",local=TRUE)$value,
          source("body_ui-coherent.R",local=TRUE)$value
          
          
          
        )
      )
      
    ),
    
    
    
    title = "Web Tool"
    
  ),
  
  
  server = function(input, output,session) { 
    hostess <- Hostess$new()
    observeEvent(input$help2, {
      introjs(session, options = list(steps = steps2()))
    })
    
    
    steps2 <- reactive(data.frame(element=c("#test_soma","#go_step2"
    ), 
    intro =c(
      "Click on the Side Bar 'Step 1'<BR>",
      "View The User Manuel<BR>"
      
    ),
    position = "bottom"))
    
    # set the loader as html
    waiter <- Waiter$
      new(
        "plot", 
        html = hostess$get_loader(stroke_color = "#ffffff")
      )
    source("general.R",local = TRUE)
    


 
    source("server_soma.R",local = TRUE)
    source("server_soma_pca.R",local = TRUE)
       source("server_olink.R",local = TRUE)
    source("server_olink_pca.R",local = TRUE)
    source("server_flow_pca.R",local = TRUE)
    source("server-table.R",local = TRUE)
    source("server-filter.R",local = TRUE)
    source("server-pca.R",local = TRUE)
    
    source("server-group.R",local = TRUE)
    source("server-pair.R",local = TRUE)
    source("server-plot.R",local = TRUE)
    source("server-flow.R",local = TRUE)
    source("server-cor.R",local = TRUE)
    
    source("hits.R",local=TRUE)
    source("server-metaplotter.R",local=TRUE)
    
    
    
    
    
    library(ComplexHeatmap)
    
  }
)