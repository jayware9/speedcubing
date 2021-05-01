ui_progression <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("text")),
          div(
            style="position:relative",
            dygraphOutput(ns("chart"))%>%withSpinner(),
            div(id=ns("legend"),class="legend0 legend-single",
                span(id=ns("legendtext")),
                div(id=ns("legenddiv"),class="legenddiv")))
  )
}

server_progression <- function(input, output, session, graphData, appOptions, refresh) {
  ns <- session$ns
  
  output$text <- renderUI({
    text <- ""
    title <- ""
    results_text <- NULL
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
    
    if(is.null(graphData())) {
      text <- "No results found. Please select another ranking region/event from the menu"
    } else if(nrow(graphData()) == 0) {
      text <- "No results found. Please select another ranking region/event from the menu"
    } else {
      title <- paste(selected$event_name, selected$region_name, "record progression")
      
      most_records <- graphData() %>% 
        filter(key == "recordStart") %>% 
        group_by(personName, type) %>% 
        tally() %>%
        mutate(total = sum(n, na.rm = TRUE)) %>%
        ungroup() %>%
        pivot_wider(names_from = "type", values_from = "n") %>%
        rename(n = total) %>%
        #mutate(n = replace_na(average, 0) + replace_na(single, 0)) %>%
        filter(n == max(n)) 
      if(! "average" %in% names(most_records)) most_records <- most_records %>% mutate(average = NA)
      
      longest_standing <- graphData() %>%
        filter(year(dateRecord) > 2000) %>%
        group_by(type) %>%
        arrange(dateRecord) %>%
        mutate(length = difftime(lead(as.Date(dateRecord)), (as.Date(dateRecord)),units = "days"),
               imputedLength = imputedTime | lead(imputedTime))
      
      current_holder <- graphData() %>%
        group_by(type) %>%
        arrange(desc(key), desc(dateRecord)) %>%
        summarise_all(first)
      
      results_text <- tagList(
        p(paste0(
          "Between ", graphData() %>% pull(dateRecord) %>% min %>% year,
          " and ", graphData() %>% pull(dateRecord) %>% max %>% year,
          " ",
          graphData() %>% filter(key == "recordStart", type == "single") %>% nrow(),
          " single solve ", 
          selected$region_name, " ", selected$event_name,
          " records and ",
          graphData() %>% filter(key == "recordStart", type == "average") %>% nrow(),
          " records for round average were set for ", selected$event_name, ". ",
          if_else(nrow(most_records) == 1,
                  paste0(most_records %>% pull(personName)  %>% first(),
                         " has set the most ", selected$event_name, " ", selected$region_name, " records, with ",
                         most_records %>% pull(single) %>% first(), " single solve records",
                         if_else(! most_records %>% pull(average)  %>% first() %in% NA,
                                  paste0(" and ",
                                         most_records %>% pull(average)  %>% first(), " records for round average."),
                                  ".")),
                  paste0(most_records %>% pull(personName) %>% text_list(),
                         " have set the most ", selected$event_name, " ", selected$region_name, " records, with ",
                         most_records %>% pull(n) %>% max(), " records each."))
        )),
        p(if_else(
          longest_standing %>% filter(type == "single") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% nrow() == 0,
          "",
          paste0("The longest standing ", selected$event_name, " ", selected$region_name, 
                 " record since 2003 for a single solve was a",
                 case_when(selected$event %in% "333fm" ~ 
                             paste0(" fewest moves of ",
                                    longest_standing %>% filter(type == "single") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(time)),
                           selected$event %in% c("333mbf", "333mbo") ~ 
                             paste0(" best result of ",
                                    longest_standing %>% filter(type == "single") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(time) %>% format_mbd()),
                           TRUE ~
                             paste0(" time of ",
                                    longest_standing %>% filter(type == "single") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(time) %>% format_time())
                 ),
                 " set by ",
                 longest_standing %>% filter(type == "single") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(personName),
                 if_else(longest_standing %>% filter(type == "single") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(imputedTime),
                         " around ",
                         " on "),
                 longest_standing %>% filter(type == "single") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(dateRecord) %>% format("%d %B %Y"),
                 if_else(longest_standing %>% filter(type == "single") %>% arrange(desc(length), dateRecord) %>% mutate(curr = time == min(time, na.rm = TRUE)) %>% filter(row_number() == 1) %>% pull(curr),
                         " which currently stands at ",
                         " which stood for "),
                 if_else(longest_standing %>% filter(type == "single") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(imputedLength),
                         "approximately ",
                         ""),
                 longest_standing %>% filter(type == "single") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(length),
                 " days.")),
          if_else(
            longest_standing %>% filter(type == "average") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% nrow() == 0,
            "",
            paste0(" The longest standing record for a round average was a",
                   case_when(selected$event %in% "333fm" ~ 
                               paste0(" fewest moves of ",
                                      longest_standing %>% filter(type == "average") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(time)),
                             selected$event %in% c("333mbf", "333mbo") ~ 
                               paste0(" best result of ",
                                      longest_standing %>% filter(type == "average") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(time) %>% format_mbd()),
                             TRUE ~
                               paste0(" time of ",
                                      longest_standing %>% filter(type == "average") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(time) %>% format_time())
                   ),
                   " set by ",
                   longest_standing %>% filter(type == "average") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(personName),
                   if_else(longest_standing %>% filter(type == "average") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(imputedTime),
                           " around ",
                           " on "),
                   longest_standing %>% filter(type == "average") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(dateRecord) %>% format("%d %B %Y"),
                   if_else(longest_standing %>% filter(type == "average") %>% arrange(desc(length), dateRecord) %>% mutate(curr = time == min(time, Inf, na.rm = TRUE)) %>% filter(row_number() == 1) %>% pull(curr),
                           " which currently stands at ",
                           " which stood for "),
                   if_else(longest_standing %>% filter(type == "average") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(imputedLength),
                           "approximately ",
                           ""),
                   longest_standing %>% filter(type == "average") %>% arrange(desc(length), dateRecord) %>% filter(row_number() == 1) %>% pull(length),
                   " days."))),
        p(if_else(
          current_holder %>% filter(type == "single") %>% nrow() == 0,
          "",
          paste0("The current ", selected$event_name, " ", selected$region_name, 
                 " record (single) is a",
                 case_when(selected$event %in% "333fm" ~ 
                             paste0(" fewest moves of ",
                                    current_holder %>% filter(type == "single") %>% pull(time)),
                           selected$event %in% c("333mbf", "333mbo") ~ 
                             paste0(" best result of ",
                                    current_holder %>% filter(type == "single") %>% pull(time) %>% format_mbd()),
                           TRUE ~
                             paste0(" time of ",
                                    current_holder %>% filter(type == "single") %>% pull(time) %>% format_time())
                 ),
                 " set by ",
                 current_holder %>% filter(type == "single") %>% pull(personName),
                 if_else(current_holder %>% filter(type == "single") %>% pull(imputedTime),
                         " around ",
                         " on "),
                 current_holder %>% filter(type == "single") %>% pull(dateRecord) %>% format("%d %B %Y"),
                 " at the ",
                 current_holder %>% filter(type == "single") %>% pull(competitionName)))),
        p(if_else(
          current_holder %>% filter(type == "average") %>% nrow() == 0,
          "",
          paste0("The current ", selected$event_name, " ", selected$region_name, 
                 " record (average) is a",
                 case_when(selected$event %in% "333fm" ~ 
                             paste0(" fewest moves of ",
                                    current_holder %>% filter(type == "average") %>% pull(time)),
                           selected$event %in% c("333mbf", "333mbo") ~ 
                             paste0(" best result of ",
                                    current_holder %>% filter(type == "average") %>% pull(time) %>% format_mbd()),
                           TRUE ~
                             paste0(" time of ",
                                    current_holder %>% filter(type == "average") %>% pull(time) %>% format_time())
                 ),
                 " set by ",
                 current_holder %>% filter(type == "average") %>% pull(personName),
                 if_else(current_holder %>% filter(type == "average") %>% pull(imputedTime),
                         " around ",
                         " on "),
                 current_holder %>% filter(type == "average") %>% pull(dateRecord) %>% format("%d %B %Y"),
                 " at the ",
                 current_holder %>% filter(type == "average") %>% pull(competitionName))))
      )
      
    }
    tagList(h3("WCA Record Progression"),
            p(strong(text)),
            results_text,
            h4(title))
  })
  
  graphDataFinal <- reactive({
    if(is.null(graphData())) return(NULL)
    if(is.null(appOptions())) return(NULL)
    selected <- isolate(appOptions())
    message("Updating Progression chart data")
    
    graphData() %>%
      left_join(events %>% select(eventId = id, eventName = name), by = "eventId") %>%
      group_by(dateRecord) %>%
      arrange(desc(time)) %>%
      mutate(adj = row_number()*10) %>%
      ungroup() %>%
      mutate(time = case_when(selected$event %in% c("333mbf", "333mbo") ~ 
                                (99 - (time %/% 10000000))*10000000 + (time %% 10000000), 
                              TRUE ~time),
             timeRecord = case_when(selected$event %in% c("333mbf", "333mbo") ~ 
                                      (99 - (timeRecord %/% 10000000))*10000000 + (timeRecord %% 10000000), 
                                    TRUE ~timeRecord)) %>%
      mutate(time = ifelse(time == timeRecord, NA, time)) %>%
      pivot_wider(names_from = type, values_from = c(time, timeRecord)) %>%
      mutate(dateRecord = as_datetime(dateRecord),
             personLabel = paste0(personName, " (", personId, ")"),
             label = case_when(key == "recordStart" ~paste("<strong>", personName, "</strong><br>", competitionName, roundName,"<br>",format(dateRecord, "%d %b %Y:")),
                               TRUE ~ paste("<strong>", personName, "</strong><br>Current record holder<br> ",format(dateRecord, "%d %b %Y:")))) %>%
      mutate(dateRecord = dateRecord + 12 * 3600 + adj) %>%
      arrange(dateRecord)
  })
  
  output$chart <- renderDygraph({
    if(is.null(graphDataFinal())) return(NULL)
    if(is.null(refresh())) tmp <- 1
    if(is.null(appOptions())) return(NULL)
    message("Updating Progression chart")
    
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
    
    plot <- xts(graphDataFinal()%>%select(starts_with("time")), graphDataFinal()$dateRecord) %>%
      dygraph() %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindow())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(connectSeparatedPoints = TRUE, stepPlot = TRUE,
                titleHeight=40,drawGrid=F,rightGap = 25,digitsAfterDecimal = 2, includeZero = TRUE,
                drawPoints = TRUE, pointSize = 4) %>%#, labelsKMB = TRUE, colors = colourPal$colours) %>%
      dySeries("time_single", label = "Single", strokeWidth = 0, color = colourPal$colours[1]) %>%
      dySeries("timeRecord_single", label = "Record (Single)", strokeWidth = 2, color = colourPal$colours[1]) %>%
      
      dyAxis("x", rangePad = 20,valueFormatter = "function(d){return '';}") %>% #"function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'numeric', day: 'numeric' });}") %>%
      dyAxis("y",valueFormatter = yValueFormatter, axisLabelFormatter = yAxisFormatter) %>% #"function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'numeric', day: 'numeric' });}") %>%
      #dyLegend(show = "follow",labelsSeparateLines = T,width=150)#,labelsDiv="legenddiv")
      dyLegend(show = "always",labelsSeparateLines = FALSE,labelsDiv=ns("legenddiv"),width=225) %>%
      dyCallbacks(
        #zoomCallback = paste0("function(minDate, maxDate, yRanges) {Shiny.onInputChange('",ns("zoom"),"',yRanges);}"),
        #underlayCallback = paste0("function(c,a,d) {Shiny.onInputChange('",ns("area"),"',a);}"),
        highlightCallback = paste0("function(event, x, points, row, seriesName) {$('#",ns("legend"),"').css({'display':'block','top':event.clientY+10+'px','left':event.clientX+10+'px'});$('#",ns("legendtext"),"').html([",paste0("\"",graphDataFinal()$label,"\"",collapse=", "),"][row]);}"),
        unhighlightCallback = paste0("function(event) {$('#",ns("legend"),"').css('display','none');}")
        #pointClickCallback = paste0("function(e,p) {Shiny.onInputChange('",ns("click"),"',p);}")
      )
    if("time_average" %in% names(graphDataFinal())) {
      plot <- plot %>% 
        dySeries("time_average", label = "Average", strokeWidth = 0, color = colourPal$colours[2]) %>%
        dySeries("timeRecord_average", label = "Record (Average)", strokeWidth = 2, color = colourPal$colours[2])
    }
    plot
  })
  
  dateWindowRange <- reactive({
    if(is.null(graphData())) return(NULL)
    as_datetime(range(graphData()$dateRecord))
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