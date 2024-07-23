shinyUI(
  
  fluidPage(
    tags$head(
      tags$script(src = HTML("js-functions.js"))
    ),
    
    tags$style(HTML("
    .tabbable > .nav > li > a {background-color: #005753;  color:white}
    .tabbable > .nav > li.active > a {background-color: #91268F; color:white}
  ")),
  
  id = "AppID", # Shiny App ID
  tags$style("@import url(https://use.fontawesome.com/releases/v6.3.0/css/all.css);"),
  title = "Schools for Summer Planning Academy", # Internal Shiny title
  
  theme = "styles.css", # Loads in the custom CSS
  
  # This section adds the PSRC logo on the top left of the page and the Page Title
  fluidRow(column(4, tags$a(div(tags$img(src='psrc-logo.png',
                                         style="margin-top: 10px; padding-left: 40px;",
                                         height = "80")
  ), href="https://www.psrc.org", target="_blank")),
  column(8, br(), strong(tags$div(class="mainpage_title", HTML(paste0("Neighborhood Schools:", "<br/>", "2024 Summer Planning Academy")))))),
  
  hr(style = "border-top: 1px solid #000000;"),
  fluidRow(column(12, style='padding-left:75px; padding-right:75px;', overview_ui('OVERVIEW'))),
  fluidRow(column(12, style='padding-left:75px; padding-right:75px;',  place_ui('SCHOOL'))),
  hr(style = "border-top: 1px solid #000000;"),

  tags$footer(footer_ui('psrcfooter'))
  
  ) # End of fluid page
) # end of shiny app
