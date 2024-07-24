
echarts4r::e_common(font_family = "Poppins")

# General Information -------------------------------------------------------------
page_information <- function(tbl, page_name, page_section=NULL, page_info) {
  
  if(is.null(page_section)) {
    
    t <- tbl |>
      filter(page == page_name) |>
      select(all_of(page_info)) |>
      pull()
    
  } else {
    
    t <- tbl |>
      filter(page == page_name & section == page_section) |>
      select(all_of(page_info)) |>
      pull()
    
  }
  
  
  if(is.na(t)) {f <- ""} else {f <- t}
  
  return(f)
  
}

# Charts ------------------------------------------------------------------

tooltip_js <- "
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"percent\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return(params.seriesName + '<br>' + 
      params.marker + ' ' +\n
      params.name + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }"

format_opts <- function(e, esttype, dec, title) {
  if(esttype == "number") {
    e <- e |> e_tooltip(trigger = "item")
    
  } else {
    
    if(esttype == "currency") {
      curr <- "USD"
    } else {
      curr <- NULL
    }
    
    e <- e |>
      e_y_axis(name = title, 
               nameLocation = "middle", 
               nameGap = 50,
               nameTextStyle = list(fontSize=14),
               axisLabel=list(margin=10),
               formatter = e_axis_formatter(esttype, digits = dec)) |>
      e_tooltip(trigger = "item",
                formatter =  e_tooltip_item_formatter(style = esttype, digits = 0, currency = curr)) |>
      e_tooltip(formatter =  htmlwidgets::JS(tooltip_js))
  }
  return(e)
}

e_basics <- function(e, top_padding, bottom_padding, legend, left_align) {
  e <- e |>
    e_grid(left = left_align, top = top_padding, bottom = bottom_padding) |>
    e_x_axis(axisTick=list(show = FALSE)) |>
    e_show_loading()
  
  e <- e |> e_legend(show = legend, bottom=0)
  
  return(e)
}

timeline_opts <- function(e, right_toggle, left_toggle) {
  e |>
    echarts4r::e_timeline_opts(autoPlay = FALSE,
                               tooltip = list(show=FALSE),
                               axis_type = "category",
                               top = 15,
                               right = right_toggle,
                               left = left_toggle,
                               currentIndex = 0,
                               controlStyle=FALSE,
                               lineStyle=FALSE,
                               label = list(show=TRUE,
                                            interval = 0,
                                            color='#4C4C4C',
                                            fontFamily = 'Poppins'),
                               itemStyle = list(color='#BCBEC0'),
                               checkpointStyle = list(label = list(show=FALSE),
                                                      color='#4C4C4C',
                                                      animation = FALSE),
                               progress = list(label = list(show=FALSE),
                                               itemStyle = list (color='#BCBEC0')),
                               emphasis = list(label = list(show=FALSE),
                                               itemStyle = list (color='#4C4C4C')))
  
}

create_bar_chart <- function(df, x, y, fill, toggle, esttype="number", dec=0, color, bar_column="column", legend=TRUE, left_align='20%', top_padding=100, bottom_padding=75, title=NULL, right_toggle = 200, left_toggle = 200) {
  
  # Determine the number of Series to Plot
  chart_fill <- df |> select(all_of(fill)) |> distinct() |> pull() |> unique() |> as.character()
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  # Format the tibble so that each series is in its own column - necessary to assigning colors in echarts via a palette
  chart_df <- df |>
    filter(.data[[fill]] %in% chart_fill) |>
    mutate(!!y:= round(.data[[y]], num_dec)) |>
    select(all_of(fill), all_of(x), all_of(y), all_of(toggle)) |>
    pivot_wider(names_from = all_of(fill), values_from = all_of(y))
  
  # Create the most basic chart
  c <- chart_df |>
    group_by(.data[[toggle]]) |>
    e_charts_(x, timeline = TRUE) |>
    e_toolbox_feature("dataView") |>
    e_toolbox_feature("saveAsImage")
  
  # Add a bar for each series
  for (fill_items in chart_fill) {
    c <- c |> e_bar_(fill_items, name = fill_items)
  }
  
  # Set series colors and set the basics for padding and leged
  c <- c |> e_color(color) |> e_basics(top_padding, bottom_padding, legend = legend, left_align = left_align)
  
  # Format the Axis and Hover Text
  c <- format_opts(c, esttype, dec, title)
  
  # Add in the Timeseries Selector
  c <- timeline_opts(c, right_toggle, left_toggle)
  
  # Rotate for bar chart
  if (bar_column == "bar") {
    c <- c |> e_flip_coords() |> e_legend(show = legend, top=0)
  }
  
  return(c)
  
}

create_line_chart <- function(df, x, y, fill, esttype="number", dec=0, color, legend=TRUE, left_align='20%', top_padding=100, bottom_padding=75, title=NULL) {
  
  # Determine the number of Series to Plot
  chart_fill <- df |> select(all_of(fill)) |> distinct() |> pull() |> unique() |> as.character()
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  # Format the tibble so that each series is in its own column - necessary to assigning colors in echarts via a palette
  chart_df <- df |>
    filter(.data[[fill]] %in% chart_fill) |>
    mutate(!!y:= round(.data[[y]], num_dec)) |>
    select(all_of(fill), all_of(x), all_of(y)) |>
    pivot_wider(names_from = all_of(fill), values_from = all_of(y))
  
  # Create the most basic chart
  c <- chart_df |>
    e_charts_(x, timeline = FALSE) |>
    e_toolbox_feature("dataView") |>
    e_toolbox_feature("saveAsImage")
  
  # Add a bar for each series
  for (fill_items in chart_fill) {
    c <- c |> e_line_(fill_items, smooth = FALSE)
  }
  
  # Set series colors and set the basics for padding and leged
  c <- c |> e_color(color) |> e_basics(top_padding, bottom_padding, legend = legend, left_align = left_align)
  
  # Format the Axis and Hover Text
  c <- format_opts(c, esttype, dec, title)
  
  return(c)
  
}

create_place_map <- function(place_name, place_type) {
  
  if(place_type == rgc_title) {
    
    place_shp <- rgc_shape |> filter(name %in% place_name)
    
  } else {
    
    place_shp <- school_shape |> filter(name %in% place_name)
    
  }
  
  m <- leaflet(options = leafletOptions(zoomControl=FALSE)) |>
    addProviderTiles(providers$Esri.NatGeoWorldMap) |>
    addPolygons(data = place_shp,
                fillColor = "76787A",
                weight = 4,
                opacity = 1.0,
                color = "#EB4584",
                dashArray = "4",
                fillOpacity = 0.0)
  
  return(m)
  
}

pull_place_information <- function(place_name, place_type, place_info) {
  
  if(place_type == rgc_title) {
    
    place_desc <- "Regional Growth Center"
    
  } else {
    
    place_desc <- "Planning Academy School"
    
  }
  
  t <- place_information |>
    filter(type_of_place == place_desc & name == place_name) |>
    select(all_of(place_info)) |>
    pull()
  
  if(is.na(t)) {f <- ""} else {f <- t}
  
  return(f)
  
}

echart_multi_column_chart <- function(df, x, y, fill, tog, dec, esttype, color, left_align='20%') {
  
  if (color == "blues") {chart_color <- psrcplot::psrc_colors$blues_inc}
  if (color == "greens") {chart_color <- psrcplot::psrc_colors$greens_inc}
  if (color == "oranges") {chart_color <- psrcplot::psrc_colors$oranges_inc}
  if (color == "purples") {chart_color <- psrcplot::psrc_colors$purples_inc}
  if (color == "jewel") {chart_color <- psrcplot::psrc_colors$pognbgy_5}
  
  # If the value is a percentage, round differently
  ifelse(esttype == "percent", num_dec <- 4, num_dec <- dec)
  
  # Determine the number of Series to Plot
  bar_fill_values <- df %>% 
    select(all_of(fill)) %>% 
    dplyr::distinct() %>% 
    dplyr::pull() %>% 
    unique
  
  chart_fill <- as.character(bar_fill_values)
  
  top_padding <- 100
  title_padding <- 75
  bottom_padding <- 75
  
  # Create the most basic chart
  chart_df <- df %>%
    dplyr::filter(.data[[fill]] %in% chart_fill) %>%
    dplyr::mutate(!!y:= round(.data[[y]], num_dec)) %>%
    dplyr::select(tidyselect::all_of(fill), tidyselect::all_of(x), tidyselect::all_of(y), tidyselect::all_of(tog)) %>%
    tidyr::pivot_wider(names_from = tidyselect::all_of(fill), values_from = tidyselect::all_of(y))
  
  c <- chart_df %>%
    dplyr::group_by(.data[[tog]]) %>%
    echarts4r::e_charts_(x, timeline = TRUE) %>%
    e_toolbox_feature("dataView") %>%
    e_toolbox_feature("saveAsImage")
  
  for(fill_items in chart_fill) {
    c <- c %>%
      echarts4r::e_bar_(fill_items, smooth = FALSE)
  }
  
  c <- c %>% 
    echarts4r::e_color(chart_color) %>%
    echarts4r::e_grid(left = left_align, top = top_padding, bottom = bottom_padding) %>%
    echarts4r::e_x_axis(axisTick=list(show = FALSE)) %>%
    echarts4r::e_show_loading() %>%
    echarts4r::e_legend(show = TRUE, bottom=0)
  
  # Add in the Timeseries Selector
  c <- c %>%
    echarts4r::e_timeline_opts(autoPlay = FALSE,
                               tooltip = list(show=FALSE),
                               axis_type = "category",
                               top = 15,
                               right = 200,
                               left = 200,
                               #currentIndex = 2,
                               controlStyle=FALSE,
                               lineStyle=FALSE,
                               label = list(show=TRUE,
                                            interval = 0,
                                            color='#4C4C4C',
                                            fontFamily = 'Poppins'),
                               itemStyle = list(color='#BCBEC0'),
                               checkpointStyle = list(label = list(show=FALSE),
                                                      color='#4C4C4C',
                                                      animation = FALSE),
                               progress = list(label = list(show=FALSE),
                                               itemStyle = list (color='#BCBEC0')),
                               emphasis = list(label = list(show=FALSE),
                                               itemStyle = list (color='#4C4C4C')))
  
  # Format the Axis and Hover Text
  if (esttype == "percent") {
    c <- c %>%
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter("percent", digits = dec)) %>%
      echarts4r::e_tooltip(trigger = "item", formatter =  echarts4r::e_tooltip_item_formatter("percent", digits = 0)) %>%
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"percent\",\"minimumFractionDigits\":1,\"maximumFractionDigits\":1,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return(params.seriesName + '<br>' + 
      params.marker + ' ' +\n
      params.name + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }")
      )
    
  } # end of percent format
  
  if (esttype == "currency") {
    c <- c %>%
      echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style="currency", digits = dec, currency = "USD")) %>%
      echarts4r::e_tooltip(trigger = "item", formatter =  echarts4r::e_tooltip_item_formatter(style="currency", digits = 0, currency = "USD")) %>%
      echarts4r::e_tooltip(formatter =  htmlwidgets::JS("
      function(params, ticket, callback) {
      var fmt = new Intl.NumberFormat('en',
      {\"style\":\"currency\",\"minimumFractionDigits\":0,\"maximumFractionDigits\":0,\"currency\":\"USD\"});\n
      var idx = 0;\n
      if (params.name == params.value[0]) {\n
      idx = 1;\n        }\n
      return('<strong>' + params.seriesName '<br>' + 
      params.marker + ' ' +\n
      params.name + ': ' + fmt.format(parseFloat(params.value[idx]))
      )
      }")
      )
    
  } # end of currency format
  
  if (esttype == "number") {
    c <- c %>%
      echarts4r::e_tooltip(trigger = "item")
  }
  
  return(c)
  
}

echart_multi_bar_chart <- function(df, x, y, fill, tog, dec, esttype, color, left_align = '20%') {
  
  c <- echart_multi_column_chart(df=df, x=x, y=y, fill=fill, tog=tog, dec=dec, esttype=esttype, color=color, left_align=left_align) 
  
  c <- c %>%
    e_flip_coords()
  
  return(c)
  
}

create_source_table <- function(d=glossary_text) {
  
  # Table with Titles as first row
  t <- rbind(names(d), d)
  
  headerCallbackRemoveHeaderFooter <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('display', 'none');",
    "}"
  )
  
  summary_tbl <- datatable(t,
                           options = list(paging = FALSE,
                                          pageLength = 30,
                                          searching = FALSE,
                                          dom = 't',
                                          headerCallback = JS(headerCallbackRemoveHeaderFooter),
                                          columnDefs = list(list(targets = c(0,2), className = 'dt-left'))),
                           selection = 'none',
                           callback = JS(
                             "$('table.dataTable.no-footer').css('border-bottom', 'none');"
                           ),
                           class = 'row-border',
                           filter = 'none',              
                           rownames = FALSE,
                           escape = FALSE
  ) 
  
  # # Add Section Breaks
  # 
  # summary_tbl <- summary_tbl %>%
  #   formatStyle(0:ncol(t), valueColumns = "Data Point",
  #               `border-bottom` = styleEqual(c("Work from Home: City", 
  #                                              "Traffic Related Deaths and Serious Injuries: Day of Week", 
  #                                              "Population, Housing Units and Jobs: Near High Capacity Transit",
  #                                              "Transit Mode to Work: City",
  #                                              "Bike to Work: City",
  #                                              "Departure Time to Work: Metro Areas"), "solid 2px"))
  # 
  # summary_tbl <- summary_tbl %>%
  #   formatStyle(0:ncol(t), valueColumns = "Data Point",
  #               `border-top` = styleEqual(c("Vehicle Registrations: Region"), "solid 2px"))
  
  return(summary_tbl)
  
}

create_jobs_by_sector_table <- function(place_name, place_type) {
  
  sectors <- jobs_data |> select("grouping") |> distinct() |> pull()
  data <- jobs_data |> filter(geography == place_name & geography_type == place_type) |> mutate(year = as.integer(as.character(year)))
  max_year <- max(data$year)
  
  # Jobs by Sector
  t <- data |>
    filter(year == max_year) |>
    select("grouping","estimate", "share") |>
    mutate(estimate = format(estimate, big.mark = ",")) |>
    mutate(share = round(share, 4)) |>
    mutate(share = label_percent(accuracy=1)(share))
  
  headerCallbackRemoveHeaderFooter <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('display', 'none');",
    "}"
  )
  
  summary_tbl <- datatable(t,
                           options = list(paging = FALSE,
                                          pageLength = 15,
                                          searching = FALSE,
                                          dom = 't',
                                          headerCallback = JS(headerCallbackRemoveHeaderFooter),
                                          columnDefs = list(list(targets = c(1,2), className = 'dt-right'),
                                                            list(targets = c(0), className = 'dt-left'))),
                           selection = 'none',
                           callback = JS(
                             "$('table.dataTable.no-footer').css('border-bottom', 'none');"
                           ),
                           class = 'row-border',
                           filter = 'none',              
                           rownames = FALSE,
                           escape = FALSE
  ) 
  
  # Add Section Breaks
  summary_tbl <- summary_tbl %>%
    formatStyle(0:ncol(t), valueColumns = "grouping",
                `border-bottom` = styleEqual(c("Total"), "solid 2px"))
  
  summary_tbl <- summary_tbl %>%
    formatStyle(0:ncol(t), valueColumns = "grouping",
                `border-top` = styleEqual(c("Construction / Resources"), "solid 2px"))
  
  return(summary_tbl)
  
}

create_summary_table <- function(df = summary_data, place_name, place_type) {
  
  t <- df |> 
    filter(geography == place_name & geography_type == place_type) |>
    mutate(pic = case_when(
      grouping == "Land Area (acres)" ~ as.character(icon("layer-group", lib = "font-awesome")),
      grouping == "Year" ~ as.character(icon("calendar-check", lib = "font-awesome")),
      grouping == "Category" ~ as.character(icon("city", lib = "font-awesome")),
      grouping == "Population" ~ as.character(icon("users", lib = "font-awesome")),
      grouping == "Housing Units" ~ as.character(icon("building", lib = "font-awesome")),
      grouping == "Total Employment" ~ as.character(icon("briefcase", lib = "font-awesome")),
      grouping == "Activity Units per Acre" ~ as.character(icon("people-group", lib = "font-awesome")),
      grouping == "Jobs per Resident" ~ as.character(icon("person-shelter", lib = "font-awesome")))) |>
    select("pic", "grouping", "estimate")

    
  headerCallbackRemoveHeaderFooter <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('display', 'none');",
    "}"
  )
  
  summary_tbl <- datatable(t,
                           options = list(paging = FALSE,
                                          pageLength = 15,
                                          searching = FALSE,
                                          dom = 't',
                                          headerCallback = JS(headerCallbackRemoveHeaderFooter),
                                          columnDefs = list(list(targets = c(2), className = 'dt-right'),
                                                            list(targets = c(0), className = 'dt-center'))),
                           selection = 'none',
                           callback = JS(
                             "$('table.dataTable.no-footer').css('border-bottom', 'none');"
                           ),
                           class = 'row-border',
                           filter = 'none',              
                           rownames = FALSE,
                           escape = FALSE
  ) 
  
  # Add Section Breaks
  summary_tbl <- summary_tbl %>%
    formatStyle(0:ncol(t), valueColumns = "grouping",
                `border-bottom` = styleEqual(c("Category", "Total Employment", "Jobs per Resident"), "solid 2px"))
  
  summary_tbl <- summary_tbl %>%
    formatStyle(0:ncol(t), valueColumns = "grouping",
                `border-top` = styleEqual(c("Land Area (acres)"), "solid 2px"))
  
  return(summary_tbl)
  
}

create_transit_stop_table <- function(place_name, place_type) {
  
  t <- stops_data |> 
    filter(geography == place_name & geography_type == place_type) |>
    select("mode", "stops") |>
    mutate(stops = format(stops, big.mark = ","))
  
  headerCallbackRemoveHeaderFooter <- c(
    "function(thead, data, start, end, display){",
    "  $('th', thead).css('display', 'none');",
    "}"
  )
  
  summary_tbl <- datatable(t,
                           options = list(paging = FALSE,
                                          pageLength = 15,
                                          searching = FALSE,
                                          dom = 't',
                                          headerCallback = JS(headerCallbackRemoveHeaderFooter),
                                          columnDefs = list(list(targets = c(1), className = 'dt-right'),
                                                            list(targets = c(0), className = 'dt-left'))),
                           selection = 'none',
                           callback = JS(
                             "$('table.dataTable.no-footer').css('border-bottom', 'none');"
                           ),
                           class = 'row-border',
                           filter = 'none',              
                           rownames = FALSE,
                           escape = FALSE
  ) 
  
  # Add Section Breaks
  summary_tbl <- summary_tbl %>%
    formatStyle(0:ncol(t), valueColumns = "mode",
                `border-bottom` = styleEqual(c("Light Rail or Streetcar"), "solid 2px"))
  
  summary_tbl <- summary_tbl %>%
    formatStyle(0:ncol(t), valueColumns = "mode",
                `border-top` = styleEqual(c("All Transit Stops"), "solid 2px"))
  
  return(summary_tbl)
  
}

create_transit_map <- function(place_name, place_type) {
  
  transit_pal <- colorFactor(
    palette = c("#BCBEC0", "#8CC63E", "#91268F", "#00A7A0", "#F05A28"),
    levels = c("Bus", "BRT", "Commuter Rail", "Ferry", "Light Rail or Streetcar"))
  
  if(place_type == rgc_title) {
    
    place_shp <- rgc_shape |> filter(name %in% place_name) |> st_transform(wgs84)
    
  } else {
    
    place_shp <- school_shape |> filter(name %in% place_name) |> st_transform(wgs84)
    
  }
  
  data <- st_intersection(stop_shape, place_shp) |> select("stop_id", "type_name")

  lrt_stops <- data |>
    filter(type_name == "Light Rail or Streetcar") |>
    select(Stop="stop_id", Mode="type_name") |>
    drop_na()
  
  brt_stops <- data |>
    filter(type_name == "Bus Rapid Transit") |>
    select(Stop="stop_id", Mode="type_name") |>
    drop_na()
  
  crt_stops <- data |>
    filter(type_name == "Commuter Rail") |>
    select(Stop="stop_id", Mode="type_name") |>
    drop_na()
  
  ferry_stops <- data |>
    filter(type_name == "Ferry") |>
    select(Stop="stop_id", Mode="type_name") |>
    drop_na()
  
  bus_stops <- data |>
    filter(type_name == "Bus") |>
    select(Stop="stop_id", Mode="type_name") |>
    drop_na()
  
  m <- leaflet() |>
    
    addProviderTiles(providers$CartoDB.Positron) |>
    
    addLayersControl(baseGroups = c("Base Map"),
                     overlayGroups = c("Bus",
                                       "BRT", 
                                       "Commuter Rail",
                                       "Ferry",
                                       "Light Rail or Streetcar", 
                                       "School"),
                     options = layersControlOptions(collapsed = TRUE)) |>
    
    addPolygons(data = place_shp,
                fillColor = "76787A",
                weight = 4,
                opacity = 1.0,
                color = "#EB4584",
                dashArray = "4",
                fillOpacity = 0.0,
                group="School") |>
    
    addCircles(data=bus_stops, 
               group="Bus",
               color = "#BCBEC0",
               opacity = 1.0,
               fillOpacity = 1.0) |>
    
    addCircles(data=brt_stops, 
               group="BRT",
               color = "#8CC63E",
               opacity = 1.0,
               fillOpacity = 1.0) |>
    
    addCircles(data=crt_stops, 
               group="Commuter Rail",
               color = "#91268F",
               opacity = 1.0,
               fillOpacity = 1.0) |>
    
    addCircles(data=ferry_stops, 
               group="Ferry",
               color = "#00A7A0",
               opacity = 1.0,
               fillOpacity = 1.0) |>
    
    addCircles(data=lrt_stops, 
               group="Light Rail or Streetcar",
               color = "#F05A28",
               opacity = 1.0,
               fillOpacity = 1.0)
  
  return(m)
  
  
}