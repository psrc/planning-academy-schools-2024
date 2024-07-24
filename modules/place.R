
place_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    uiOutput(ns("place"))
  )
}

place_server <- function(id, place_type) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Place based inputs
    places <- place_shape |> st_drop_geometry() |> filter(geography_type == place_type)

    place_list <- NULL
    for (c in unique(places$category)) {
      
      # Names associated with category
      nms <- places |> select("name", "category") |> filter(category == c) |> select("name") |> pull() |> unique()
      i <- list(nms)
      names(i) <- c
      
      if(is.null(place_list)) {place_list <- i} else {place_list <- append(place_list, i)}
      
    }
    
    random_place <- place_list[[sample(1:length(place_list), 1)]]
    if (place_type == rgc_title) {all_places <- "All RGCs"} else {all_places <- "All Schools"}
    
    # Text
    output$description <- renderText(pull_place_information(place_name=input$place_name, place_type=place_type, place_info = "information"))
    
    # Charts & Maps
    output$map <- renderLeaflet(create_place_map(place_name=input$place_name, place_type=place_type))
    
    output$summary_table <- renderDataTable(create_summary_table(place_name=input$place_name, place_type = place_type))
    
    output$pop_chart <- renderEcharts4r({
      create_bar_chart(df = pop_hh_hu_data |> 
                         filter(geography %in% c(input$place_name) & grouping == "Population") |>
                         arrange(desc(year)),
                       x = "year", y = "estimate", fill = "geography", toggle = "grouping", dec = -1,
                       color = c("#8CC63E", "#F05A28", "#91268F"), legend = TRUE, left_align='15%')})
    
    output$race_chart <- renderEcharts4r({
      echart_multi_bar_chart(df = race_data |> 
                               filter(geography %in% c(input$place_name, "Region", all_places) & grouping != "Total") |>
                               arrange(desc(grouping)),
                             x = "grouping", y = "share", fill="geography", tog = "year",
                             dec = 0, esttype = "percent", color = "jewel", left_align = '20%')})
    
    output$age_chart <- renderEcharts4r({
      echart_multi_column_chart(df = age_data |>
                                  filter(geography %in% c(input$place_name, "Region", all_places) & grouping != "Total"),
                                x = "grouping", y = "share", fill="geography", tog = "year", 
                                dec = 0, esttype = "percent", color = "jewel", left_align = '15%')})
    
    output$education_chart <- renderEcharts4r({
      echart_multi_bar_chart(df = education_data |>
                               filter(geography %in% c(input$place_name, "Region", all_places) & grouping != "Total"),
                             x = "grouping", y = "share", fill="geography", tog = "year", 
                             dec = 0, esttype = "percent", color = "jewel", left_align = '20%')})
    
    output$housing_chart <- renderEcharts4r({
      create_bar_chart(df = pop_hh_hu_data |> 
                         filter(geography %in% c(input$place_name) & grouping != "Population") |>
                         arrange(desc(year)),
                       x = "year", y = "estimate", fill = "geography", toggle = "grouping", dec = -1,
                       color = c("#8CC63E", "#F05A28", "#91268F"), legend = TRUE, left_align='15%')})
    
    output$tenure_chart <- renderEcharts4r({
      echart_multi_column_chart(df = tenure_data |> 
                                  filter(geography %in% c(input$place_name, "Region", all_places) & grouping != "Total"),
                                x = "grouping", y = "share", fill="geography", tog = "year", 
                                dec = 0, esttype = "percent", color = "jewel", left_align = '15%')})
    
    output$type_chart <- renderEcharts4r({
      echart_multi_bar_chart(df = type_data |>
                               filter(geography %in% c(input$place_name, "Region", all_places) & grouping != "Total") |>
                               arrange(desc(grouping)),
                             x = "grouping", y = "share", fill="geography", tog = "year", 
                             dec = 0, esttype = "percent", color = "jewel", left_align = '20%')})
    
    output$renter_burden_chart <- renderEcharts4r({
      echart_multi_column_chart(df = burden_data |>
                                  filter(geography %in% c(input$place_name, "Region", all_places) & grouping != "Total" & concept == "Renter Cost Burden"),
                                x = "grouping", y = "share", fill="geography", tog = "year", 
                                dec = 0, esttype = "percent", color = "jewel", left_align = '15%')})
    
    output$owner_burden_chart <- renderEcharts4r({
      echart_multi_column_chart(df = burden_data |>
                                  filter(geography %in% c(input$place_name, "Region", all_places) & grouping != "Total" & concept == "Owner Cost Burden"),
                                x = "grouping", y = "share", fill="geography", tog = "year", 
                                dec = 0, esttype = "percent", color = "jewel", left_align = '15%')})
    
    output$job_chart <- renderEcharts4r({
      create_bar_chart(df = jobs_data |> 
                         filter(geography %in% c(input$place_name) & grouping == "Total") |>
                         arrange(desc(year)),
                       x = "year", y = "estimate", fill = "geography", toggle = "grouping", dec = -1,
                       color = c("#8CC63E", "#F05A28", "#91268F"), legend = TRUE, left_align='15%')})
    
    output$job_sector_table <- renderDataTable(create_jobs_by_sector_table(place_name = input$place_name, place_type = place_type))
    
    output$income_chart <- renderEcharts4r({
      echart_multi_column_chart(df = income_data |> 
                                  filter(geography %in% c(input$place_name, "Region", all_places) & grouping != "Total"),
                                x = "grouping", y = "share", fill="geography", tog = "year", 
                                dec = 0, esttype = "percent", color = "jewel", left_align = '15%')})
    
    output$resident_mode_chart <- renderEcharts4r({
      echart_multi_column_chart(df = mode_data |> 
                                  filter(geography %in% c(input$place_name, "Region", all_places) & grouping != "Total"),
                                  x = "grouping", y = "share", fill="geography", tog = "year", 
                                  dec = 0, esttype = "percent", color = "jewel", left_align = '15%')})
    
    output$resident_vehicles_chart <- renderEcharts4r({
      echart_multi_column_chart(df = vehicles_data |> 
                                  filter(geography %in% c(input$place_name, "Region", all_places) & grouping != "Total"),
                                x = "grouping", y = "share", fill="geography", tog = "year", 
                                dec = 0, esttype = "percent", color = "jewel", left_align = '15%')})
    
    output$stops_table <- renderDataTable(create_transit_stop_table(place_name=input$place_name, place_type = place_type))
    
    output$stop_map <- renderLeaflet({create_transit_map(place_name=input$place_name, place_type = place_type)})
    
    # Tab layout
    output$place <- renderUI({
      tagList(
        br(),
        fluidRow(column(12, selectInput(ns("place_name"), label="Select Neighborhood School:", choices=place_list, selected = random_place, width = '100%'))),
        fluidRow(column(6, leafletOutput(ns("map"))),
                 column(6, strong("Summary Statistics"),
                        br(),
                        dataTableOutput(ns("summary_table")))),
        br(),
        fluidRow(column(12, strong("Description:"),
                        br(),
                        textOutput(ns("description")))),
        hr(style = "border-top: 1px solid #000000;"),
        
        tabsetPanel(type = "pills",
                    tabPanel("Demographics", 
                             
                             # Total Population
                             br(),
                             strong(tags$div(class="chart_title","Total Population")),
                             fluidRow(column(12, echarts4rOutput(ns("pop_chart")))),
                             br(),
                             tags$div(class="chart_source","Source: Office of Financial Managment SAEP Program & PSRC Parcelization"),
                             br(),
                             
                             # Race
                             br(),
                             strong(tags$div(class="chart_title","Share of Total Population by Race & Ethnicity")),
                             fluidRow(column(12, echarts4rOutput(ns("race_chart"), height=500))),
                             br(),
                             tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B03002"),
                             br(),
                             
                             # Age Group
                             br(),
                             strong(tags$div(class="chart_title","Share of Total Population by Age Group")),
                             fluidRow(column(12, echarts4rOutput(ns("age_chart")))),
                             br(),
                             tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B01001"),
                             br(),
                             
                             # Educational Attainment
                             br(),
                             strong(tags$div(class="chart_title","Share of Total Population 25+ by Educational Attainment")),
                             fluidRow(column(12, echarts4rOutput(ns("education_chart"), height=500))),
                             br(),
                             tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B15002"),
                             br()
                             
                             ),
                    
                    tabPanel("Housing", 
                             
                             # Total Housing
                             br(),
                             strong(tags$div(class="chart_title","Total Households & Housing Units")),
                             fluidRow(column(12, echarts4rOutput(ns("housing_chart")))),
                             br(),
                             tags$div(class="chart_source","Source: Office of Financial Managment SAEP Program & PSRC Parcelization"),
                             br(),
                             
                             # Housing Tenure
                             br(),
                             strong(tags$div(class="chart_title","Share of Households by Housing Tenure")),
                             fluidRow(column(12, echarts4rOutput(ns("tenure_chart")))),
                             br(),
                             tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B25003"),
                             br(),
                             
                             # Housing Unit Type
                             br(),
                             strong(tags$div(class="chart_title","Share of Households by Housing Unit Type")),
                             fluidRow(column(12, echarts4rOutput(ns("type_chart"), height=500))),
                             br(),
                             tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B25024"),
                             br(),
                             
                             # Renter Cost Burden
                             br(),
                             strong(tags$div(class="chart_title","Share of Renter Households by Cost Burden")),
                             fluidRow(column(12, echarts4rOutput(ns("renter_burden_chart")))),
                             br(),
                             tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B25070"),
                             br(),
                             
                             # Owner Cost Burden
                             br(),
                             strong(tags$div(class="chart_title","Share of Owner Households by Cost Burden")),
                             fluidRow(column(12, echarts4rOutput(ns("owner_burden_chart")))),
                             br(),
                             tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B25091"),
                             br()
                             
                             ),
                    
                    tabPanel("Jobs & Income", 
                             
                             # Total Jobs
                             br(),
                             strong(tags$div(class="chart_title","Total Jobs")),
                             fluidRow(column(6, echarts4rOutput(ns("job_chart"), height = 500)),
                                      column(6, strong("Jobs by Sector"),
                                             br(),
                                             dataTableOutput(ns("job_sector_table")))),
                             br(),
                             tags$div(class="chart_source","Source: US Census Bureau LEHD Origin-Destination Employment Statistics (LODES)"),
                             br(),
                             
                             # Income
                             br(),
                             strong(tags$div(class="chart_title","Share of Total Households by Household Income")),
                             fluidRow(column(12, echarts4rOutput(ns("income_chart")))),
                             br(),
                             tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B19001"),
                             br()
                             
                            ),
                    
                    tabPanel("Transportation", 
                             
                             # Transit Stops
                             br(),
                             strong(tags$div(class="chart_title","Transit Stops by Mode")),
                             fluidRow(column(6, leafletOutput(ns("stop_map"))),
                                      column(6, strong("Transit Service"),
                                             br(),
                                             dataTableOutput(ns("stops_table")),
                                             tags$div(class="chart_source","Source: Spring 2024 GTFS Service by Transit Agency"),
                                             br())),
                             
                             fluidRow(column(12, div(img(src="transit-legend.png", width = "75%", style = "padding-left: 0px;")))),
                             tags$div(class="chart_source", "Note: Many stations show stops for each direction of travel."),
                             
                             br(),
                             
                             # Mode to work
                             br(),
                             strong(tags$div(class="chart_title","Mode to Work for people 16+")),
                             fluidRow(column(12, echarts4rOutput(ns("resident_mode_chart")))),
                             br(),
                             tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B08301"),
                             br(),
                             
                             # Vehicle Availability by Household
                             br(),
                             strong(tags$div(class="chart_title","Vehicle Availability by Household")),
                             fluidRow(column(12, echarts4rOutput(ns("resident_vehicles_chart")))),
                             br(),
                             tags$div(class="chart_source","Source: US Census Bureau American Community Survey (ACS) 5yr Data Table B08201"),
                             br()
                             
                             
                             )
                    ),
        
        
        hr(style = "border-top: 1px solid #000000;")
        
        
      )
    }) 
  })  # end moduleServer
}
