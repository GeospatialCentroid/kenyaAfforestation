pageButtonUi <- function(id) {
  actionButton(NS(id, "page_change"),
               label="View Scenario",
               options = list(openOnFocus = FALSE))
}


pageButtonServer <- function(id, parentSession, pageName) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$page_change, {
      updateNavbarPage(session=parentSession,
                       inputId="pages",
                       selected= pageName) # this is the part that need to change based on input button selected
    })
  })
}