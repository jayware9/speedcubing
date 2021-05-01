ui_event_vs <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("text")),
          div(
            style="position:relative",
            dygraphOutput(ns("chart"))%>%withSpinner(),
            div(id=ns("legend"),class="legend0",
                span(id=ns("legendtext")),
                div(id=ns("legenddiv"),class="legenddiv")))
  )
}

server_event_vs <- function(input, output, session, graphData, select_avg, select_event2, appOptions, refresh) {
  ns <- session$ns
  
  output$text <- renderUI({
    text <- ""
    if(is.null(graphData())) {
      text <- "No results found. Please select a cuber from the menu"
    } else if(nrow(graphData()) == 0) {
      text <- "No results found. Please select a cuber from the menu"
    }
    tagList(h3("WCA Competitions"),
            p(strong(text)))
  })
  
  ####Filter data####
  
  filter_events <- reactive({
    if(length(select_event2()) == 0) return(tibble())
    message("Filtering events")
    #tourn_results[[input$select_event]]
    readRDS(paste0("tourn_results",select_event2(),".rds"))
  })
  
  filter_years <- reactive({
    if(is.null(appOptions()$year)) return(tibble())
    if(nrow(filter_events()) == 0) return(tibble())
    
    message("Filtering years")
    if(appOptions()$year[1] == year(metadata$min) & appOptions()$year[2] == year(metadata$max)) {
      return(filter_events())
    } else if(appOptions()$year[1] == year(metadata$min)) {
      return(filter_events() %>% filter(year <= appOptions()$year[2]))
    } else if(appOptions()$year[2] == year(metadata$max)) {
      return(filter_events() %>% filter(year >= appOptions()$year[1]))
    } else {
      return(filter_events() %>% filter(year >= appOptions()$year[1], year <= appOptions()$year[2]))
    }
  })
  
  filter_gender <- reactive({
    if(is.null(appOptions()$gender)) return(tibble())
    if(nrow(filter_years()) == 0) return(tibble())
    message("Filtering gender")
    
    if(appOptions()$gender == "All") return(filter_years())
    
    cubers %>%
      filter(gender %in% appOptions()$gender) %>%
      select(personId) %>%
      inner_join(filter_years(), by = "personId")
  })
  
  
  filter_countries <- reactive({
    if(is.null(appOptions()$region)) return(tibble())
    if(nrow(filter_gender()) == 0) return(tibble())
    message("Filtering countries")
    
    if(appOptions()$region == "world") return(filter_gender())
    
    countries <- regions %>% filter(championship_type %in% appOptions()$region) %>% select(personCountryId = countryId)
    filter_gender() %>%
      inner_join(countries, by = "personCountryId") %>%
      mutate(championship_type = appOptions()$region)
  })
  
  personal_bests <- reactive({
    if(nrow(filter_countries()) == 0) return(NULL)
    if(is.null(select_avg())) return(NULL)
    message("Calculating personal bests")
    filter_countries() %>%
      select(personId, time = all_of(select_avg())) %>%
      filter(! time %in% NA) %>%
      #group_by(personId) %>%
      #summarise(time = min(time)) %>%
      arrange(time) %>%
      distinct(personId, .keep_all = TRUE) %>%
      ungroup()
  })
  
  graphDataFinal <- reactive({
    if(is.null(graphData())) return(NULL)
    if(is.null(personal_bests())) return(NULL)
    message("Updating Events Vs chart data")
    #browser()
    graph_data <- inner_join(
      filter(graphData(), !is.na(time)) %>% rename(time1 = time),
      filter(personal_bests(), !is.na(time)) %>% rename(time2 = time),
      #filter(graphData(), time < sort(time, partial = floor(n()*0.95))[floor(n()*0.95)]) %>% rename(time1 = time),
      #filter(personal_bests(), time < sort(time, partial = floor(n()*0.95))[floor(n()*0.95)]) %>% rename(time2 = time),
      by = "personId"
    ) #%>% 
    #mutate(time1 = round(time1, 1), time2 = round(time2, 1))
    #mutate(x2 = time1^2, x3 = time1^3) %>%
    
    bestfit <- lm(graph_data$time2 ~ graph_data$time1)
    print(summary(bestfit)$r.squared)
    
    graph_data %>%
      mutate(bestfit = predict(bestfit)) %>%
      #mutate(bestfit = predict(lm(time2 ~ time1 + x2 + x3))) %>%
      #mutate(bestfit = predict(lm(time2 ~ time1))) %>%
      distinct(time1, time2, bestfit)
  })
  
  output$chart <- renderDygraph({
    if(is.null(graphDataFinal())) return(NULL)
    if(is.null(refresh())) tmp <- 1
    message("Updating Events Vs chart")
    
    graphDataFinal() %>%
      select(time1, time2, bestfit) %>%
      dygraph() %>%
      dyRangeSelector(height = 40, strokeColor = "") %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(includeZero = TRUE, retainDateWindow = FALSE, strokeWidth = 0, drawPoints = TRUE,
                titleHeight=40,drawGrid=F,rightGap = 25,digitsAfterDecimal = 2) %>%
      dySeries("bestfit", strokeWidth = 2, drawPoints = FALSE) %>%
      dyAxis("x", rangePad = 20,
             valueFormatter = NULL) %>% #"function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'short', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = NULL,#graphDataFinal()$yValueFormatter, 
             axisLabelFormatter = NULL) %>%#graphDataFinal()$yAxisFormatter) %>%
      dyLegend(show = "follow",labelsSeparateLines = T,width=150)#,labelsDiv="legenddiv")%>%
    # dyLegend(show = "always",labelsSeparateLines = FALSE,labelsDiv=ns("legenddiv"),width=225) %>%
    # dyCallbacks(
    #   highlightCallback = paste0("function(event, x, points, row, seriesName) {$('#",ns("legend"),"').css({'display':'block','top':event.clientY+10+'px','left':event.clientX+10+'px'});$('#",ns("legendtext"),"').text([",paste0("\"",graphDataFinal()$data$label,"\"",collapse=", "),"][row]);}"),
    #   unhighlightCallback = paste0("function(event) {$('#",ns("legend"),"').css('display','none');}")
    # )
  })
  
  dateWindowRange <- reactive({
    if(is.null(graphData())) return(NULL)
    c(min(graphData()$time), max(graphData()$time))
  })
  
  updateDateWindow <- reactive({
    if(is.null(input$chart_date_window)) return(NULL)
    if(is.null(dateWindowRange())) return(NULL)
    message("...updating window range")
    if(input$chart_date_window[1] > dateWindowRange()[2] | input$chart_date_window[2] < dateWindowRange()[1]) {
      message("...resetting window range")
      return(NULL)
    } 
    return(input$chart_date_window)
  })
  
}