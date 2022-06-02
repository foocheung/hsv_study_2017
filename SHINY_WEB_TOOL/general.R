#steps <- read_csv2("help.csv")
#intro <- read_csv2("intro.csv")


observeEvent("", {
  showModal(modalDialog(size = "l",
    includeHTML("intro_text2.html"),
    easyClose = TRUE,
    footer = tagList(
      actionButton(inputId = "intro", label = "Start", icon = icon("info-circle"))
    )
  ))
})

observeEvent(input$intro,{
  removeModal()
})

# show intro tour
#observeEvent(input$intro,
#             introjs(session, options = list("nextLabel" = "Continue",
#                                           "prevLabel" = "Previous",
#                                            "doneLabel" = "Alright. Let's go"))
#)






observeEvent(input$intro, {
  introjs(session, options = list(steps = steps2()))
})


steps2 <- reactive(data.frame(element=c("#sidebarmenu", "#step2"
), 
intro =c(
  "Click on the Side Bar 'Step 1'<BR>",
  "View The User Manuel<BR>"
),


position = "bottom"))