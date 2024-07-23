
overview_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("overview"))
  )
}

overview_server <- function(id) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Text
    output$overview_text <- renderText({page_information(tbl=page_text, page_name="Overview", page_section = "Overview", page_info = "description")})
    
    # Overview UI
    output$overview <- renderUI({
      tagList(
        br(),
        textOutput(ns("overview_text")),
        br(),
        
      )
    })
  })  # end moduleServer
}
