ui_ncuber <- function(id) {
  ns <- NS(id)
  tagList(h2("Speedcubing has been growing in recent years"),
          uiOutput(ns("warning_text")),
          uiOutput(ns("results_text")),
          uiOutput(ns("container1"))%>%withSpinner(),
          uiOutput(ns("container2")),
          uiOutput(ns("container3")),
          uiOutput(ns("container4"))
  )
}

server_ncuber <- function(input, output, session, graphData, select_num_event, select_num_type, appOptions, refresh) {
  ns <- session$ns
  
  textData <- reactive({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    c(
      graphData() %>% 
        ungroup() %>% 
        filter(type == "cum", gender == "t") %>% 
        summarise(start = year(min(end_date, na.rm = TRUE)), cumm = max(n, na.rm = TRUE)),
      graphData() %>%
        ungroup() %>% 
        mutate(year = year(end_date)) %>%
        filter(type != "cum", year == metadata$refYr) %>%
        mutate(gender = case_when(gender == "" ~ "nk", gender == "o" ~ "nb", TRUE ~ gender)) %>%
        select(gender, type, n) %>%
        pivot_wider(names_from = c(gender, type), values_from = c(n), names_sep = "_"),
      graphData()%>%
        ungroup() %>% 
        filter(type != "cum", gender == "t") %>%
        mutate(peak = year(end_date)) %>%
        arrange(desc(n)) %>%
        filter(row_number() == 1) %>%
        select(peak)
    )
  })

  output$warning_text <- renderUI({
    if(is.null(graphData())) {
      return(p(strong("No results found. Please select another region and/or event from the menu")))
    } else if(nrow(graphData()) == 0) {
      return(p(strong("No results found. Please select another region and/or event from the menu")))
    } else {
      return(NULL)
    }
  })
  
  output$results_text <- renderUI({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    selected <- isolate(appOptions())
    
    if(select_num_event() %in% TRUE) selected$event_name <- "speedcubing"
    if(select_num_event() %in% TRUE) selected$event <- "all"
    selected$total_tourn <- no_events %>% 
      ungroup() %>%
      filter(championship_type == selected$region, eventId == selected$event) %>%
      summarise(n = sum(n)) %>%
      pull(n)
    
    
    
    results_text <- tagList(
      p("Between", textData()$start, "and", year(metadata$updated), "there have been", 
        strong(comma(textData()$cumm, 1), "cubers", selected$region_from), "that have competed in an official",
        selected$event_name, "competition event, and there have been", comma(selected$total_tourn, 1),
        selected$event_name, "competitions held across",paste0(selected$region_name, ".")), 
      p(ifelse(textData()$peak >= 2018, "Numbers have been growing in recent years, and in", "In"),
        paste0(metadata$refYr, ","), comma(textData()$t_nCuber, 1), "cubers", selected$region_from,
        "participated in at least one", selected$event_name, "event. On average they competed in",
        strong(comma(textData()$t_nTourn / textData()$t_nCuber, 0.1), "competitions"), "throughout the ",
        HTML(ifelse(select_num_event() %in% TRUE, 
                    paste0("year, and at each competition cubers took part in an average of", strong(comma(textData()$t_nEvent / textData()$t_nTourn, 0.1), " events"), "at each competition."),
                    "year."))),
      p(case_when(textData()$f_nCuber / textData()$t_nCuber < 0.4 & textData()$t_nCuber >= 20 ~
                    paste0("In ", metadata$refYr, ", a majority of cubers ",selected$region_from, " who participated in official ", selected$event_name,
                           " competition events were men/boys. There were"),
                  textData()$f_nCuber / textData()$t_nCuber  > 0.6 & textData()$t_nCuber >= 20 ~
                    paste0("In ", metadata$refYr, ", a majority of cubers ",selected$region_from, " who participated in official ", selected$event_name,
                           " competition events were women/girls. There were"),
                  TRUE ~ paste0("In ", metadata$refYr, " there were")),
        comma(textData()$f_nCuber, 1), "female cubers", percent(textData()$f_nCuber/textData()$t_nCuber, 1, prefix = "(", suffix = "%)"),
        "compared with",comma(textData()$m_nCuber, 1), "male cubers", percent(textData()$m_nCuber/textData()$t_nCuber, 1, prefix = "(", suffix = "%)."),
        case_when(textData()$o_nCuber > 0 & textData()$nk_nCuber > 0 ~
                    paste0("In addition there were ", comma(textData()$nk_nCuber, 1), 
                           " cubers of other genders (", percent(textData()$o_nCuber/textData()$t_nCuber, 0.1),"), and ",
                           comma(textData()$nk_nCuber, 1), " cubers (", percent(textData()$nk_nCuber/textData()$t_nCuber, 0.1),") where their gender wasn't recorded."),
                  textData()$o_nCuber > 0 ~
                    paste0("In addition there were ", comma(textData()$nk_nCuber, 1), 
                           " cubers of other genders (", percent(textData()$o_nCuber/textData()$t_nCuber, 0.1),")."),
                  textData()$nk_nCuber > 0 ~
                    paste0("In addition there were ", comma(textData()$nk_nCuber, 1), " cubers (", 
                           percent(textData()$nk_nCuber/textData()$t_nCuber, 0.1),") where their gender wasn't recorded."),
                  TRUE ~ ""),
        case_when(select_num_event() %in% TRUE & textData()$f_nCuber >= 10 & textData()$m_nCuber >= 10 ~
                    paste0("For female cubers who attended competitions in ", metadata$refYr, " on average they attended ",
                           comma(textData()$f_nTourn / textData()$f_nCuber, 0.1), " competitions, with women taking part in ",
                           comma(textData()$f_nEvent / textData()$f_nTourn, 0.1)," events at each competition. Whilst men attended ",
                           comma(textData()$m_nTourn / textData()$m_nCuber, 0.1), " competitions, and participated in an average of ",
                           comma(textData()$m_nEvent / textData()$m_nTourn, 0.1)," events at each competition."),
                  textData()$f_nCuber >= 10 & textData()$m_nCuber >= 10 ~
                    paste0("For female cubers who took part in ", selected$event_name, " events at competitions in ", metadata$refYr, "on average they competed ",
                           comma(textData()$f_nTourn / textData()$f_nCuber, 0.1), " times, compared with ",
                           comma(textData()$m_nTourn / textData()$m_nCuber, 0.1), " for male cubers."),
                  TRUE ~ "")
      )
    )
  })
  
  output$chart1 <- renderDygraph({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(is.null(appOptions())) return(NULL)
    message("Updating Cubers chart and data")
    
    #widen data
    select_type <- "cum"
    graph_data <- graphData() %>%
      filter(type == select_type) %>%
      filter(gender != "") %>%
      mutate(n = as.numeric(n),
             end_date = as_datetime(end_date),
             gender = case_when(gender == "f" ~ "Women", 
                                gender == "m" ~ "Men", 
                                gender == "o" ~ "Other", 
                                gender == "t" ~ "Total cubers")) %>%
      pivot_wider(names_from = gender, values_from = n)
    key <- c("Women", "Men", "Total cubers", "Other") %>% intersect(names(graph_data))
    
    #x axis dates/years
    xValueFormatter <- "function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'short', day: 'numeric' });}"
    
    #y axis labels
    yValueFormatter <- "function(d) {return '<br>'+formatCommas(d,0)+' total number of cubers';}"
    
    xts(graph_data[key], graph_data$end_date) %>%
      dygraph(group = "ncubers") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindow())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(connectSeparatedPoints = TRUE,
                titleHeight=40,drawGrid=F,rightGap = 25,strokeWidth=2,digitsAfterDecimal = 2, includeZero = TRUE,
                drawPoints = F, labelsKMB = TRUE, colors = colourPal$colours) %>%
      dyAxis("x", rangePad = 20,valueFormatter = xValueFormatter) %>% #"function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'numeric', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = yValueFormatter) %>%
      dyLegend(show = "follow",labelsSeparateLines = TRUE,width=225)#,labelsDiv="legenddiv")
    
  })
  
  output$chart2 <- renderDygraph({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(is.null(appOptions())) return(NULL)
    message("Updating Cubers chart and data")
    
    #widen data
    select_type <- "nCuber"
    graph_data <- graphData() %>%
      filter(type == select_type) %>%
      filter(gender != "") %>%
      mutate(n = as.numeric(n),
             end_date = as_datetime(end_date),
             gender = case_when(gender == "f" ~ "Women", 
                                gender == "m" ~ "Men", 
                                gender == "o" ~ "Other", 
                                gender == "t" ~ "Total cubers")) %>%
      pivot_wider(names_from = gender, values_from = n)
    key <- c("Women", "Men", "Total cubers", "Other") %>% intersect(names(graph_data))
    
    #x axis dates/years
    xValueFormatter <- "function(d) {x = new Date(d); return x.getFullYear();}"
    
    #y axis labels
    yValueFormatter <- "function(d) {return '<br>'+formatCommas(d,0)+' cubers attended a competition';}"
    
    xts(graph_data[key], graph_data$end_date) %>%
      dygraph(group = "ncubers") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindow())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(connectSeparatedPoints = TRUE,
                titleHeight=40,drawGrid=F,rightGap = 25,strokeWidth=2,digitsAfterDecimal = 2, includeZero = TRUE,
                drawPoints = F, labelsKMB = TRUE, colors = colourPal$colours) %>%
      dyAxis("x", rangePad = 20,valueFormatter = xValueFormatter) %>% #"function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'numeric', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = yValueFormatter) %>%
      dyLegend(show = "follow",labelsSeparateLines = TRUE,width=225)#,labelsDiv="legenddiv")
    
  })
  
  output$chart3 <- renderDygraph({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(is.null(appOptions())) return(NULL)
    message("Updating Cubers chart and data")
    
    #widen data
    graph_data <- graphData() %>%
      filter(type %in% c("nCuber", "nTourn")) %>%
      filter(!gender %in% c("", "o")) %>%
      spread(type, n) %>%
      mutate(n = nTourn / nCuber) %>%
      select(-nTourn, -nCuber) %>%
      mutate(n = as.numeric(n),
             end_date = as_datetime(end_date),
             gender = case_when(gender == "f" ~ "Women", 
                                gender == "m" ~ "Men", 
                                gender == "o" ~ "Other", 
                                gender == "t" ~ "Total cubers")) %>%
      pivot_wider(names_from = gender, values_from = n)
    key <- c("Women", "Men", "Total cubers", "Other") %>% intersect(names(graph_data))
    
    #x axis dates/years
    xValueFormatter <- "function(d) {x = new Date(d); return x.getFullYear();}"
    
    #y axis labels
    yValueFormatter <- "function(d) {return '<br>'+formatCommas(d,2)+' average competitions attended';}"
    
    xts(graph_data[key], graph_data$end_date) %>%
      dygraph(group = "ncubers") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindow())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(connectSeparatedPoints = TRUE,
                titleHeight=40,drawGrid=F,rightGap = 25,strokeWidth=2,digitsAfterDecimal = 2, includeZero = TRUE,
                drawPoints = F, labelsKMB = TRUE, colors = colourPal$colours) %>%
      dyAxis("x", rangePad = 20,valueFormatter = xValueFormatter) %>% #"function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'numeric', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = yValueFormatter) %>%
      dyLegend(show = "follow",labelsSeparateLines = TRUE,width=225)#,labelsDiv="legenddiv")
    
  })

  output$chart3a <- renderDygraph({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(is.null(appOptions())) return(NULL)
    message("Updating Cubers chart and data")
    
    #widen data
    select_type <- "nTourn"
    graph_data <- graphData() %>%
      filter(type == select_type) %>%
      filter(gender != "") %>%
      mutate(n = as.numeric(n),
             end_date = as_datetime(end_date),
             gender = case_when(gender == "f" ~ "Women", 
                                gender == "m" ~ "Men", 
                                gender == "o" ~ "Other", 
                                gender == "t" ~ "Total cubers")) %>%
      pivot_wider(names_from = gender, values_from = n)
    key <- c("Women", "Men", "Total cubers", "Other") %>% intersect(names(graph_data))
    
    #x axis dates/years
    xValueFormatter <- "function(d) {x = new Date(d); return x.getFullYear();}"
    
    #y axis labels
    yValueFormatter <- "function(d) {return '<br>'+formatCommas(d,0)+' total competitions attended';}"
    
    xts(graph_data[key], graph_data$end_date) %>%
      dygraph(group = "ncubers") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindow())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(connectSeparatedPoints = TRUE,
                titleHeight=40,drawGrid=F,rightGap = 25,strokeWidth=2,digitsAfterDecimal = 2, includeZero = TRUE,
                drawPoints = F, labelsKMB = TRUE, colors = colourPal$colours) %>%
      dyAxis("x", rangePad = 20,valueFormatter = xValueFormatter) %>% #"function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'numeric', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = yValueFormatter) %>%
      dyLegend(show = "follow",labelsSeparateLines = TRUE,width=225)#,labelsDiv="legenddiv")
    
  })
  
  output$chart4 <- renderDygraph({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(! "nEvent" %in% graphData()$type) return(NULL)
    if(is.null(appOptions())) return(NULL)
    message("Updating Cubers chart and data")
    
    #widen data
    graph_data <- graphData() %>%
      filter(type %in% c("nTourn", "nEvent")) %>%
      filter(!gender %in% c("", "o")) %>%
      spread(type, n) %>%
      mutate(n = nEvent / nTourn) %>%
      select(-nEvent, -nTourn) %>%
      mutate(n = as.numeric(n),
             end_date = as_datetime(end_date),
             gender = case_when(gender == "f" ~ "Women", 
                                gender == "m" ~ "Men", 
                                gender == "o" ~ "Other", 
                                gender == "t" ~ "Total cubers")) %>%
      pivot_wider(names_from = gender, values_from = n)
    key <- c("Women", "Men", "Total cubers", "Other") %>% intersect(names(graph_data))
    
    #x axis dates/years
    xValueFormatter <- "function(d) {x = new Date(d); return x.getFullYear();}"
    
    #y axis labels
    yValueFormatter <- "function(d) {return '<br>'+formatCommas(d,2)+' average events taken part in';}"
    
    xts(graph_data[key], graph_data$end_date) %>%
      dygraph(group = "ncubers") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindow())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(connectSeparatedPoints = TRUE,
                titleHeight=40,drawGrid=F,rightGap = 25,strokeWidth=2,digitsAfterDecimal = 2, includeZero = TRUE,
                drawPoints = F, labelsKMB = TRUE, colors = colourPal$colours) %>%
      dyAxis("x", rangePad = 20,valueFormatter = xValueFormatter) %>% #"function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'numeric', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = yValueFormatter) %>%
      dyLegend(show = "follow",labelsSeparateLines = TRUE,width=225)#,labelsDiv="legenddiv")
    
  })
  
  output$chart4a <- renderDygraph({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(is.null(appOptions())) return(NULL)
    message("Updating Cubers chart and data")
    
    #widen data
    select_type <- replace("nEvent", select_num_event() %in% FALSE, "nTourn")
    graph_data <- graphData() %>%
      filter(type == select_type) %>%
      filter(gender != "") %>%
      mutate(n = as.numeric(n),
             end_date = as_datetime(end_date),
             gender = case_when(gender == "f" ~ "Women", 
                                gender == "m" ~ "Men", 
                                gender == "o" ~ "Other", 
                                gender == "t" ~ "Total cubers")) %>%
      pivot_wider(names_from = gender, values_from = n)
    key <- c("Women", "Men", "Total cubers", "Other") %>% intersect(names(graph_data))
    
    #x axis dates/years
    xValueFormatter <- "function(d) {x = new Date(d); return x.getFullYear();}"
    
    #y axis labels
    yValueFormatter <- "function(d) {return '<br>'+formatCommas(d,0)+' total events taken part in';}"
    
    xts(graph_data[key], graph_data$end_date) %>%
      dygraph(group = "ncubers") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindow())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(connectSeparatedPoints = TRUE,
                titleHeight=40,drawGrid=F,rightGap = 25,strokeWidth=2,digitsAfterDecimal = 2, includeZero = TRUE,
                drawPoints = F, labelsKMB = TRUE, colors = colourPal$colours) %>%
      dyAxis("x", rangePad = 20,valueFormatter = xValueFormatter) %>% #"function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'numeric', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = yValueFormatter) %>%
      dyLegend(show = "follow",labelsSeparateLines = TRUE,width=225)#,labelsDiv="legenddiv")
    
  })
  
  
  
  output$container1 <- renderUI({
    if(is.null(textData())) return(NULL)
    tagList(h3(paste(comma(textData()$cumm, case_when(textData()$cumm > 100000 ~ 10000, textData()$cumm > 10000 ~ 1000,textData()$cumm > 1000 ~ 100, textData()$cumm > 100 ~ 10,TRUE~1)),
                     "cubers have entered", replace(appOptions()$event_name, select_num_event() %in% TRUE, "speedcubing"), "competitions")),
            h4("Cummulative number of speedcubers who have entered a competition, by gender"),
            div(
              style="position:relative",
              dygraphOutput(ns("chart1")),
              div(id=ns("legend1"),class="legend0 legend-single",
                  span(id=ns("legendtext1")),
                  div(id=ns("legenddiv1"),class="legenddiv"))))
  })
  output$container2 <- renderUI({
    if(is.null(textData())) return(NULL)
    tagList(br(), br(), 
            h3(ifelse(textData()$peak >= 2018, "Until 2019, the numbers competing each year was growing",
                      "")),
            h4(paste("Number of speedcubers", appOptions()$region_from, "who entered a ", replace(appOptions()$event_name, select_num_event() %in% TRUE, "speedcubing"), "competition each year, by gender")),
            div(
              style="position:relative",
              dygraphOutput(ns("chart2")),
              div(id=ns("legend2"),class="legend0 legend-single",
                  span(id=ns("legendtext2")),
                  div(id=ns("legenddiv2"),class="legenddiv"))))
  })
  output$container3 <- renderUI({
    if(is.null(textData())) return(NULL)
    tagList(br(), br(), 
            h4(paste("Average number of ", replace(appOptions()$event_name, select_num_event() %in% TRUE, "speedcubing"), " competitions entered by speedcubers", appOptions()$region_from, "each year, by gender")),
            div(
              style="position:relative",
              dygraphOutput(ns("chart3")),
              div(id=ns("legend3"),class="legend0 legend-single",
                  span(id=ns("legendtext3")),
                  div(id=ns("legenddiv3"),class="legenddiv"))))
  })
  output$container4 <- renderUI({
    if(is.null(textData())) return(NULL)
    if(select_num_event() %in% FALSE) return(NULL)
    tagList(br(), br(), 
            h4(paste("Average number of events entered by speedcubers ", appOptions()$region_from, " each competition, by gender")),
            div(
              style="position:relative",
              dygraphOutput(ns("chart4")),
              div(id=ns("legend4"),class="legend0 legend-single",
                  span(id=ns("legendtext4")),
                  div(id=ns("legenddiv4"),class="legenddiv"))))
  })
  
  dateWindowRange <- reactive({
    if(is.null(graphData())) return(NULL)
    if("end_date" %in% names(graphData())) {
      return(as_datetime(range(graphData()$end_date)))
    } else {
      return(as_datetime(paste0(range(graphData()$year),"-07-01")))
    }
  })
  
  updateDateWindow <- reactiveVal(NULL)
  
  observeEvent(input$chart1_date_window, {
    message("...updating window range")
    if(input$chart1_date_window[1] > dateWindowRange()[2] | input$chart1_date_window[2] < dateWindowRange()[1]) {
      message("...resetting window range")
      updateDateWindow(NULL)
    } else {
      updateDateWindow(input$chart1_date_window)
    }
  })
  observeEvent(input$chart2_date_window, {
    message("...updating window range")
    if(input$chart2_date_window[1] > dateWindowRange()[2] | input$chart2_date_window[2] < dateWindowRange()[1]) {
      message("...resetting window range")
      updateDateWindow(NULL)
    } else {
      updateDateWindow(input$chart2_date_window)
    }
  })
  observeEvent(input$chart3_date_window, {
    message("...updating window range")
    if(input$chart3_date_window[1] > dateWindowRange()[2] | input$chart3_date_window[2] < dateWindowRange()[1]) {
      message("...resetting window range")
      updateDateWindow(NULL)
    } else {
      updateDateWindow(input$chart3_date_window)
    }
  })
  observeEvent(input$chart4_date_window, {
    message("...updating window range")
    if(input$chart4_date_window[1] > dateWindowRange()[2] | input$chart4_date_window[2] < dateWindowRange()[1]) {
      message("...resetting window range")
      updateDateWindow(NULL)
    } else {
      updateDateWindow(input$chart4_date_window)
    }
  })
  

  
}