ui_rank <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("text")) %>%withSpinner(),
    uiOutput(ns("container"))
  )
}

server_rank <- function(input, output, session, graphData, select_avg, appOptions, refresh) {
  ns <- session$ns
  
  output$container <- renderUI({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    tagList(
      tabsetPanel(id = ns("panels"),
                  tabPanel("Ranking", dygraphOutput(ns("chart_rank"))),
                  tabPanel("Percentile", dygraphOutput(ns("chart_pct"))),
                  tabPanel("Personal Best", dygraphOutput(ns("chart_pb")))
      ),
      br(),
      br(),
      p("WCA rankings are based on the fastest single/average achieved at an WCA competition. The chart shows the rankings for the selected region and excludes any cubers who haven't set a competition time during the selected years. Dates correspond to the end of a tournament when rankings are recalculated, even if a personal best was set on an earlier day in the competition")
    )    
    
  })
  
  output$text <- renderUI({
    selected <- isolate(appOptions())
    selected$region_name <- case_when(
      selected$region_name == "the World" ~ "World",
      selected$region_name == "Africa" ~ "African continental",
      selected$region_name == "Asia" ~ "Asian continental",
      selected$region_name == "Europe" ~ "European continental",
      selected$region_name == "North America" ~ "North American continental",
      selected$region_name == "Oceania" ~ "Oceanian continental",
      selected$region_name == "South America" ~ "South American continental",
      TRUE ~ paste(selected$region_name, "national") %>% sub("^the ", "", ., ignore.case = TRUE))
    
    selected$region_from <- paste0(case_when(selected$gender %in% "m" ~ "male cuber",
                                             selected$gender %in% "f" ~ "female cuber",
                                             selected$gender %in% "o" ~ "cuber of another gender",
                                             TRUE ~ "cubers"),
                                   ifelse(selected$region == "world", "",
                                          paste(" from",regions$region_name[match(selected$region, regions$championship_type)])))
    
    
    ranking_text <- NULL
    if(is.null(graphData())) {
      warning_text <- "No results found. Please select a cuber from the menu"
    } else if(nrow(graphData()) == 0) {
      warning_text <- "No results found. Please select a cuber from the menu"
    } else {
      warning_text <- "Additional cubers can be selected from the menu"
      ranking_text <- tagList(
        graphData() %>% arrange(end_date) %>% group_split(personId) %>% map(~{
          p(HTML(paste0(
            strong(.x$personName[1]), " is currently ranked ", .x$rank[which.max(.x$end_date)], 
            " of all ", selected$region_from, ", with a personal best ", 
            case_when(selected$event %in% "333fm" ~ paste0("fewest moves (",select_avg(),") of ",comma(min(.x$pb, na.rm = TRUE), 1)),
                      selected$event %in% c("333mbf", "333mbo") ~ paste0("net solves (",select_avg(),") of ", format_mbd(min(.x$pb, na.rm = TRUE))),
                      TRUE ~ paste0("time (",select_avg(),") of ",strong(format_time(min(.x$pb, na.rm = TRUE))))),
            ", placing ", case_when(.x$gender[1] == "m" ~ "him", .x$gender[1] == "f" ~ "her", TRUE ~ "them"),
            " in the top ", 
            .x$pct[which.max(.x$end_date)] %>% (function(x){case_when(x<0.1~comma(ceiling(x*100)/100,0.01),x<1~comma(ceiling(x*10)/10,0.1),TRUE~comma(ceiling(x),1))}), "%. ",
            case_when(.x$gender[1] == "m" ~ "His", .x$gender[1] == "f" ~ "Her", TRUE ~ "Their"),
            " best ranking is ", min(.x$rank), ", first achieved on ", 
            format(min(.x$end_date[.x$rank == min(.x$rank)]), "%d %B %Y"), "."
          )))
          
        })
      )
    }
    tagList(h2(paste0(selected$region_name %>% capwords(), " Rankings", 
                      case_when(selected$gender == "m" ~ ": Men",
                                selected$gender == "f" ~ ": Women",
                                selected$gender == "o" ~ ": Other genders",
                                TRUE ~ ""))),
            p(strong(warning_text)),
            ranking_text
    )
  })
  
  graphDataPB <- reactive({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(is.null(appOptions())) return(NULL)
    selected <- isolate(appOptions())
    message("Updating Rankings chart data")
    
    if(selected$event %in% c("333mbf", "333mbo")) {
      graph_data <- graphData() %>%
        mutate(pb = (99 - (pb %/% 10000000))*10000000 + (pb %% 10000000)) %>%
        select(end_date, personLabel, value = pb) %>%
        mutate(value = as.numeric(value)) %>%
        spread(personLabel, value) %>%
        tidyr::fill(-end_date)
      #   valueFormatter  <- "function(d) {if(d<0) {return -d+' net loses';} else {return d+' net wins';}}"
      #   axisLabelFormatter <- "function(d) {if(d<0) {return -d;} else {return d;}}"
    } else {
      graph_data <- graphData() %>%
        select(end_date, personLabel, value = pb) %>%
        mutate(value = as.numeric(value)) %>%
        spread(personLabel, value) %>%
        tidyr::fill(-end_date)
    }
    graph_data
  })
  
  output$chart_pb <- renderDygraph({
    if(is.null(graphDataPB())) return(NULL)
    if(nrow(graphDataPB()) == 0) return(NULL)
    if(is.null(refresh())) tmp <- 1
    if(is.null(appOptions())) return(NULL)
    message("Updating Rankings chart")
    
    selected <- isolate(appOptions())
    if(selected$event %in% c("333mbf", "333mbo")) {
      yValueFormatter <- "function(d) {return '<br>'+formatWcaMbd(d);}"
      yAxisFormatter <- "function(d) {return d/10000000;}"
    } else if(selected$event %in% "333fm") {
      yValueFormatter <- "function(d) {return '<br>'+formatWcaMove(d);}"
      yAxisFormatter <- NULL
    } else {
      yValueFormatter <- "function(d) {return '<br>'+formatWcaTime(d);}"
      yAxisFormatter <- NULL
    } 
    
    key <- isolate(graphData()) %>% distinct(personId, personLabel, r) %>%
      arrange(r)
    
    xts(graphDataPB() %>% select(key$personLabel), 
        as_datetime(graphDataPB()$end_date)) %>%
      dygraph(group = "rankings") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindowAll())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(titleHeight=40,drawGrid=F,rightGap = 25,strokeWidth=2,digitsAfterDecimal = 2, 
                drawPoints = F,colors=colourPal$colours, includeZero = TRUE, labelsKMB = TRUE) %>%
      dyAxis("x", rangePad = 20,valueFormatter = "function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'short', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = yValueFormatter,
             axisLabelFormatter=yAxisFormatter) %>%
      dyLegend(show = "follow",labelsSeparateLines = TRUE,width=225)#,labelsDiv="legenddiv")
  })
  
  graphDataRank <- reactive({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(is.null(appOptions())) return(NULL)
    selected <- isolate(appOptions())
    message("Updating Rankings chart data")
    
    graphData() %>%
      select(end_date, personLabel, value = rank) %>%
      mutate(value = as.numeric(value)) %>%
      spread(personLabel, value) %>%
      tidyr::fill(-end_date)
    
  })
  
  output$chart_rank <- renderDygraph({
    if(is.null(graphDataRank())) return(NULL)
    if(nrow(graphDataRank()) == 0) return(NULL)
    if(is.null(refresh())) tmp <- 1
    if(is.null(appOptions())) return(NULL)
    message("Updating Rankings chart")
    
    selected <- isolate(appOptions())
    yValueFormatter <- "function(d) {return '<br>'+formatCommas(d,0);}"
    yAxisFormatter <- NULL
    
    key <- isolate(graphData()) %>% distinct(personId, personLabel, r) %>%
      arrange(r)
    
    xts(graphDataRank() %>% select(key$personLabel), 
        as_datetime(graphDataRank()$end_date)) %>%
      dygraph(group = "rankings") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindowAll())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(titleHeight=40,drawGrid=F,rightGap = 25,strokeWidth=2,digitsAfterDecimal = 2, 
                drawPoints = F,colors=colourPal$colours, includeZero = TRUE, labelsKMB = TRUE) %>%
      dyAxis("x", rangePad = 20,valueFormatter = "function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'short', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = yValueFormatter,
             axisLabelFormatter=yAxisFormatter) %>%
      dyLegend(show = "follow",labelsSeparateLines = TRUE,width=225)#,labelsDiv="legenddiv")
  })
  
  graphDataPct <- reactive({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(is.null(appOptions())) return(NULL)
    selected <- isolate(appOptions())
    message("Updating Rankings chart data")
    
    graphData() %>%
      select(end_date, personLabel, value = pct) %>%
      mutate(value = as.numeric(value)) %>%
      spread(personLabel, value) %>%
      tidyr::fill(-end_date)
  })
  
  output$chart_pct <- renderDygraph({
    if(is.null(graphDataPct())) return(NULL)
    if(nrow(graphDataPct()) == 0) return(NULL)
    if(is.null(refresh())) tmp <- 1
    if(is.null(appOptions())) return(NULL)
    message("Updating Rankings chart")
    
    selected <- isolate(appOptions())
    yValueFormatter <- NULL
    yAxisFormatter <- NULL
    
    key <- isolate(graphData()) %>% distinct(personId, personLabel, r) %>%
      arrange(r)
    
    xts(graphDataPct() %>% select(key$personLabel), 
        as_datetime(graphDataPct()$end_date)) %>%
      dygraph(group = "rankings") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindowAll())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(titleHeight=40,drawGrid=F,rightGap = 25,strokeWidth=2,digitsAfterDecimal = 2, 
                drawPoints = F,colors=colourPal$colours, includeZero = TRUE, labelsKMB = TRUE) %>%
      dyAxis("x", rangePad = 20,valueFormatter = "function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'short', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = yValueFormatter,
             axisLabelFormatter=yAxisFormatter) %>%
      dyLegend(show = "follow",labelsSeparateLines = TRUE,width=225)#,labelsDiv="legenddiv")
  })
  
  dateWindowRangeAll <- reactive({
    if(is.null(graphDataRank())) return(NULL)
    if(is.null(graphDataPct())) return(NULL)
    if(is.null(graphDataPB())) return(NULL)
    as_datetime(range(c(graphDataPct()$end_date, graphDataRank()$end_date, graphDataPB()$end_date)))
  })
  
  updateDateWindowAll <- reactiveVal(NULL)
  
  observeEvent(input$chart_pb_date_window, {
    if(is.null(input$chart_pb_date_window)) return(NULL)
    if(is.null(dateWindowRangeAll())) return(NULL)
    message("...updating window range")
    if(input$chart_pb_date_window[1] > dateWindowRangeAll()[2] | input$chart_pb_date_window[2] < dateWindowRangeAll()[1]) {
      message("...resetting window range")
      updateDateWindowAll(NULL)
    } else {
      updateDateWindowAll(input$chart_pb_date_window)
    }
  })
  
  observeEvent(input$chart_rank_date_window, {
    if(is.null(input$chart_rank_date_window)) return(NULL)
    if(is.null(dateWindowRangeAll())) return(NULL)
    message("...updating window range")
    if(input$chart_rank_date_window[1] > dateWindowRangeAll()[2] | input$chart_rank_date_window[2] < dateWindowRangeAll()[1]) {
      message("...resetting window range")
      updateDateWindowAll(NULL)
    } else {
      updateDateWindowAll(input$chart_rank_date_window)
    }
  })
  
  observeEvent(input$chart_pct_date_window, {
    if(is.null(input$chart_pct_date_window)) return(NULL)
    if(is.null(dateWindowRangeAll())) return(NULL)
    message("...updating window range")
    if(input$chart_pct_date_window[1] > dateWindowRangeAll()[2] | input$chart_pct_date_window[2] < dateWindowRangeAll()[1]) {
      message("...resetting window range")
      updateDateWindowAll(NULL)
    } else {
      updateDateWindowAll(input$chart_pct_date_window)
    }
  })
  
}