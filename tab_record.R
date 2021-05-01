ui_record <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("text1")),
          div(style="position:relative",
              dygraphOutput(ns("chart1"))%>%withSpinner(),
              div(id=ns("legend1"),class="legend0",
                  span(id=ns("legendtext1")),
                  div(id=ns("legenddiv1"),class="legenddiv"))),
          br(),
          br(),
          br(),
          br(),
          uiOutput(ns("text2")),
          div(style="position:relative",
              dygraphOutput(ns("chart2")),
              div(id=ns("legend2"),class="legend0",
                  span(id=ns("legendtext2")),
                  div(id=ns("legenddiv2"),class="legenddiv")))
  )
}

server_record <- function(input, output, session, graphData, select_cuber, appOptions, refresh) {
  ns <- session$ns
  
  output$text1 <- renderUI({
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
    ref_yr <- year(Sys.Date()) - 2
    
    warning_text <- NULL
    record_text <- NULL
    graph_text <- NULL
    if(is.null(graphData())) {
      warning_text <- "No results found. Please select another ranking region from the menu"
    } else if(nrow(graphData()) == 0) {
      warning_text <- "No results found. Please select another ranking region from the menu"
    } else {
      most_records <- graphData1() %>%
        group_by(personId) %>%
        summarise(totRecords = max(numRecords)) %>%
        filter(totRecords == max(totRecords)) %>%
        left_join(graphData1(), by = "personId") %>%
        group_split(personId)
      recent_records <- graphData1() %>%
        filter(year(dateRecord) >= ref_yr)
      
      record_type <- case_when(length(selected$record_type) == 2 ~
                                 "record",
                               "average" %in% selected$record_type ~ "average record",
                               "single" %in% selected$record_type ~ "single record")
      
      
      record_text <- tagList(
        p(paste0(
          map_chr(most_records, ~.x$personName[1]) %>% text_list(),
          if_else(length(most_records) == 1, " has", " have"),
          " set the most ",
          selected$region_name, " ", record_type,
          "s with ",
          if_else(length(most_records) == 1,
                  paste0(
                    max(most_records[[1]]$numRecords),
                    case_when(length(selected$record_type) == 2 ~
                                paste0(" records (", sum(most_records[[1]]$type == "average"), " average records; ", sum(most_records[[1]]$type == "single"), " single records). "),
                              "average" %in% selected$record_type ~ " average records. ",
                              "single" %in% selected$record_type ~ " single records. ")),
                  paste0(max(most_records[[1]]$numRecords)," records each. ")
          ),
          paste0(map_chr(most_records, ~paste0(
            .x$personName[1],
            " set ",
            case_when(.x$gender[1] == "m" ~ "his ", .x$gender[1] == "f" ~ "her ", TRUE ~ "their "),
            "first ",
            selected$region_name,
            " record at the ",
            .x$competitionName[which.min(.x$dateRecord)],
            " with the most recent set at the ",
            .x$competitionName[which.max(.x$dateRecord)],
            if_else(.x$imputedTime[which.max(.x$dateRecord)],
                    " around ",
                    " on "),
            format(max(.x$dateRecord), "%d %B %Y"),
            ". The events in which ",
            .x$personName[1],
            " has set the highest number of records were ",
            .x %>% group_by(eventId) %>% tally() %>% arrange(desc(n)) %>% filter(row_number() < 4) %>% pmap_chr(~{
              paste0(event_list$name[event_list$id == ..1], " (", ..2, " records)")
            }) %>% text_list(),
            ".")), collapse = " ")
        )),
        p(case_when(length(unique(recent_records$personId)) > 3 ~
                      paste0("Since the start of ", ref_yr, " there have been ",
                             nrow(recent_records),
                             " ", selected$region_name, " ", record_type, "s set. ",
                             "The largest number of recent records were set by ",
                             recent_records %>% group_by(personId, personName) %>% tally() %>% 
                               ungroup() %>% arrange(desc(n)) %>% filter(n > nth(n, 4)) %>% select(personName, n) %>% pmap_chr(~paste0(
                                 ..1, " (", ..2, " records)"
                               )) %>% text_list(),
                             "."),
                    length(unique(recent_records$personId)) > 1 ~
                      paste0("Since the start of ", ref_yr, " there have been ",
                             nrow(recent_records),
                             " ", selected$region_name, " ", record_type, "s set. ",
                             "These were set by ",
                             recent_records %>% group_by(personId, personName) %>% tally() %>% 
                               ungroup() %>% filter(n > nth(n, 4)) %>% select(personName, n) %>% pmap_chr(~paste0(
                                 ..1, " (", ..2, " records)"
                               )) %>% text_list(),
                             "."),
                    length(unique(recent_records$personId)) == 1 ~
                      paste0("Since the start of ", ref_yr, " there have been ",
                             nrow(recent_records),
                             " ", selected$region_name, " ", record_type, "s set. ",
                             "These were all set by ",
                             unique(recent_records$personName),
                             if_else(selected$record_type > 1,
                                     paste0(" (", sum(recent_records$type == "average"), " average records; ", sum(recent_records$type == "single"), " single records)"),
                                     ""),
                             ".", collapse = ""),
                    TRUE ~ 
                      paste0("The last ", selected$region_name, " ", record_type, " was set",
                             if_else(graphData1()$imputedTime[which.max(graphData1()$dateRecord)],
                                     " around ",
                                     " on "),
                             format(max(graphData1()$dateRecord), "%d %B %Y"),
                             " by ",
                             graphData1()$personName[which.max(graphData1()$dateRecord)],
                             " at the ",
                             graphData1()$competitionName[which.max(graphData1()$dateRecord)],
                             ".", collapse = ""
                      )
        ))
      )
      
      graph_text <- tagList(
        h3(paste0(map_chr(most_records,~.x$personName[1]) %>% text_list(), if_else(length(most_records) == 1, " has", " have"), " set the most ", selected$region_name, " ", record_type, "s")),
        h4(paste0("Cummulative number of ", selected$region_name, " ", record_type, "s by cuber"))
      )
      
    }
    
    title_text <- capwords(paste(selected$region_name, "Record Holders"))
    
    tagList(h2(title_text),
            p(strong(warning_text)),
            record_text,
            graph_text)
  })
  
  output$text2 <- renderUI({
    if(is.null(graphData2())) return(NULL)
    if(nrow(graphData2()) == 0) return(NULL)
    
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
    ref_yr <- year(Sys.Date()) - 2
    record_type <- case_when(length(selected$record_type) == 2 ~
                               "record",
                             "average" %in% selected$record_type ~ "average record",
                             "single" %in% selected$record_type ~ "single record")
    
    current_holder <- graphData2() %>%
      filter(key == "Current holder")
    concurrent_record <- graphData2() %>%
      group_by(personId) %>%
      arrange(dateRecord) %>%
      mutate(endRecord = lead(dateRecord, default = Sys.Date())) %>%
      ungroup() %>%
      filter(key != "Current holder", endRecord > dateRecord) %>%
      filter(numRecords == max(numRecords)) %>%
      arrange(dateRecord) %>%
      filter(row_number() == 1)
    
    tagList(
      p(paste0(
        "There are currently ", nrow(current_holder), " ", selected$region_name, " ", record_type, "s",
        " held by ", nrow(distinct(current_holder, personId)), " cubers.  ",
        current_holder %>% group_by(personId, personName) %>% tally() %>% ungroup() %>%
          filter(n == max(n)) %>% pull(personName) %>% text_list(),
        " currently hold the most ", selected$region_name, " records, with ",
        current_holder %>% group_by(personId, personName) %>% tally() %>% pull(n) %>% max(),
        " followed by ",
        current_holder %>% group_by(personId, personName) %>% tally() %>% ungroup() %>%
          filter(n == max(n[n != max(n)])) %>% pull(personName) %>% text_list(),
        " on ",
        current_holder %>% group_by(personId, personName) %>% tally() %>% ungroup() %>% filter(n != max(n)) %>% pull(n) %>% max(),
        " records.")),
      p(paste0(
        "The largest number of ", selected$region_name, " ", record_type, "s held by a cuber at any one time was ",
        concurrent_record$numRecords, " held by ",
        concurrent_record$personName, " from ", format(concurrent_record$dateRecord, "%d %B %Y"),
        " to ", format(concurrent_record$endRecord, "%d %B %Y"), "."
      )),
      h3(paste0(current_holder %>% group_by(personId, personName) %>% tally() %>% ungroup() %>%
                  filter(n == max(n)) %>% pull(personName) %>% text_list(),
                " currently hold ",
                current_holder %>% group_by(personId, personName) %>% tally() %>% pull(n) %>% max(),
                " ", selected$region_name, " ", record_type)),
      h4(paste0("Number of ", selected$region_name, " ", record_type, "s concurrently held by cubers"))
    )
  })
  
  
  graphData1 <- reactive({
    if(is.null(graphData())) return(NULL)
    
    message("Updating Record chart data")
    graphData() %>%
      group_by(personId) %>%
      arrange(recordStart) %>%
      mutate(numRecords = 1:n(), key = "recordStart") %>%
      rename(dateRecord = recordStart) %>%
      ungroup()
  })
  
  graphDataFinal1 <- reactive({
    if(is.null(graphData1())) return(NULL)
    graphData1() %>%
      left_join(events %>% select(eventId = id, eventName = name), by = "eventId") %>%
      mutate(dateRecord = as_datetime(dateRecord),
             numRecords = as.numeric(numRecords),
             personLabel = paste0(personName, " (", personId, ")"),
             label = case_when(key == "recordStart" ~paste("<strong>", eventName, type, "</strong><br>", competitionName, roundName),
                               key == "recordEnd" ~paste("<strong>", eventName, type, "</strong><br> record lost"),
                               TRUE ~ paste("<strong>Current records held</strong>")))%>%#<br> ",format(dateRecord, "%d %b %Y:")))) %>%
      group_by(dateRecord) %>%
      arrange(numRecords, label) %>%
      #mutate(adj = ifelse(key %in% c("recordStart", "recordEnd"), row_number(), numRecords)*10) %>%
      mutate(adj = ifelse(key %in% c("recordStart", "recordEnd"), cumsum(!duplicated(label)), numRecords)*10) %>%
      ungroup() %>%
      mutate(dateRecord = dateRecord + 12 * 3600 + adj) %>%
      distinct(dateRecord, label, personLabel, numRecords) %>%
      spread(personLabel, numRecords)
    
  })
  
  graphData2 <- reactive({
    if(is.null(graphData())) return(NULL)
    
    message("Updating Record chart data")
    graphData() %>%
      filter(is.na(recordEnd) | recordEnd > recordStart) %>%
      gather("key", "dateRecord", recordStart, recordEnd) %>%
      group_by(personId, eventId, type, dateRecord) %>%
      summarise(personName = last(personName),
                competitionName = last(competitionName),
                roundName = last(roundName),
                numRecords = sum(key == "recordStart") - sum(key == "recordEnd"),
                key = case_when(numRecords > 0 ~ "recordStart", numRecords < 0 ~ "recordEnd", TRUE ~ "")) %>%
      ungroup() %>%
      filter(numRecords != 0) %>%
      mutate(key = replace(key, dateRecord %in% NA, "Current holder"),
             numRecords = replace(numRecords, dateRecord %in% NA, 0),
             dateRecord = replace(dateRecord, dateRecord %in% NA, (metadata$max+1))) %>%
      group_by(personId) %>%
      arrange(dateRecord) %>%
      mutate(numRecords = cumsum(numRecords)) %>%
      ungroup()
  })
  
  graphDataFinal2 <- reactive({
    if(is.null(graphData2())) return(NULL)
    graphData2() %>%
      left_join(events %>% select(eventId = id, eventName = name), by = "eventId") %>%
      mutate(dateRecord = as_datetime(dateRecord),
             numRecords = as.numeric(numRecords),
             personLabel = paste0(personName, " (", personId, ")"),
             label = case_when(key == "recordStart" ~paste("<strong>", eventName, type, "</strong><br>", competitionName, roundName),
                               key == "recordEnd" ~paste("<strong>", eventName, type, "</strong><br> record lost"),
                               TRUE ~ paste("<strong>Current records held</strong>")))%>%#<br> ",format(dateRecord, "%d %b %Y:")))) %>%
      group_by(dateRecord) %>%
      arrange(label) %>%
      #mutate(adj = ifelse(key %in% c("recordStart", "recordEnd"), row_number(), numRecords)*10) %>%
      mutate(adj = ifelse(key %in% c("recordStart", "recordEnd"), cumsum(!duplicated(label)), numRecords)*10) %>%
      mutate(adj = ifelse(key %in% c("recordStart", "recordEnd"), cumsum(!duplicated(label)), rank(personLabel))*10) %>%
      group_by(dateRecord, numRecords) %>%
      mutate(adj2 = row_number()) %>%
      ungroup() %>%
      distinct(dateRecord, label, personLabel, numRecords, .keep_all = TRUE) %>%
      mutate(dateRecord = dateRecord + 12 * 3600 + adj,
             numRecords = numRecords + 0.01 * adj2) %>%
      select(dateRecord, label, personLabel, numRecords) %>%
      spread(personLabel, numRecords)
  })
  
  output$chart1 <- renderDygraph({
    if(is.null(graphDataFinal1())) return(NULL)
    if(is.null(refresh())) tmp <- 1
    message("Updating Record chart")
    
    key_all <- isolate(graphData1()) %>%
      distinct(personId, personName) %>%
      mutate(personLabel = paste0(personName, " (", personId, ")"))
    key <- key_all %>%
      filter(personId %in% select_cuber()) %>%
      arrange(personId) %>%
      mutate(r = match(personId, select_cuber()))
    
    plot <- xts(graphDataFinal1()[key_all$personLabel], graphDataFinal1()$dateRecord) %>%
      dygraph(group = "records") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindowAll())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(connectSeparatedPoints = TRUE, stepPlot = TRUE,
                titleHeight=40,drawGrid=F,rightGap = 25,strokeWidth=2,digitsAfterDecimal = 0, includeZero = TRUE,
                drawPoints = F, labelsKMB = TRUE, colors = rep(colourPal$background, nrow(key_all))) %>%
      dyAxis("x", rangePad = 20,valueFormatter = "function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'short', day: 'numeric' });}") %>%
      #dyLegend(show = "follow",labelsSeparateLines = T,width=150)#,labelsDiv="legenddiv")
      dyLegend(show = "always",labelsSeparateLines = TRUE,labelsDiv=ns("legenddiv1"),width=225) %>%
      dyCallbacks(
        #zoomCallback = paste0("function(minDate, maxDate, yRanges) {Shiny.onInputChange('",ns("zoom"),"',yRanges);}"),
        #underlayCallback = paste0("function(c,a,d) {Shiny.onInputChange('",ns("area"),"',a);}"),
        highlightCallback = paste0("function(event, x, points, row, seriesName) {$('#",ns("legend1"),"').css({'display':'block','top':event.clientY+10+'px','left':event.clientX+10+'px'});$('#",ns("legendtext1"),"').html([",paste0("\"",graphDataFinal1()$label,"\"",collapse=", "),"][row]);}"),
        unhighlightCallback = paste0("function(event) {$('#",ns("legend1"),"').css('display','none');}"),
        pointClickCallback = paste0("function(e,p) {Shiny.onInputChange('",ns("click"),"',p);}")
      )
    #highlightCallback = paste0("function(event, x, points, row, seriesName) {var i = points.findIndex(function(x){return x.name==seriesName;});$('#mylegend').css({'top':points[i].canvasy,'left':points[i].canvasx,'display':'block'}).text('background-color'); }"),
    #
    for(i in seq_len(nrow(key))) {
      plot <- plot %>% dySeries(key$personLabel[i],color=colourPal$colours[key$r[i]],strokeWidth=4,drawPoints=F)#,pointSize=5)
    }
    
    plot
  })
  
  output$chart2 <- renderDygraph({
    if(is.null(graphDataFinal2())) return(NULL)
    if(is.null(refresh())) tmp <- 1
    message("Updating Record chart")
    
    
    key_all <- isolate(graphData2()) %>%
      distinct(personId, personName) %>%
      mutate(personLabel = paste0(personName, " (", personId, ")"))
    key <- key_all %>%
      filter(personId %in% select_cuber()) %>%
      arrange(personId) %>%
      mutate(r = match(personId, select_cuber()))
    
    plot <- xts(graphDataFinal2()[key_all$personLabel], graphDataFinal2()$dateRecord) %>%
      dygraph(group = "records") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindowAll())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(connectSeparatedPoints = TRUE, stepPlot = TRUE,
                titleHeight=40,drawGrid=F,rightGap = 25,strokeWidth=2,digitsAfterDecimal = 2, includeZero = TRUE,
                drawPoints = F, labelsKMB = TRUE, colors = rep(colourPal$background, nrow(key_all))) %>%
      dyAxis("x", rangePad = 20,valueFormatter = "function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'short', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = "function(d) {return Math.floor(d);}") %>%
      #dyLegend(show = "follow",labelsSeparateLines = T,width=150)#,labelsDiv="legenddiv")
      dyLegend(show = "always",labelsSeparateLines = TRUE,labelsDiv=ns("legenddiv2"),width=225) %>%
      dyCallbacks(
        #zoomCallback = paste0("function(minDate, maxDate, yRanges) {Shiny.onInputChange('",ns("zoom"),"',yRanges);}"),
        #underlayCallback = paste0("function(c,a,d) {Shiny.onInputChange('",ns("area"),"',a);}"),
        highlightCallback = paste0("function(event, x, points, row, seriesName) {$('#",ns("legend2"),"').css({'display':'block','top':event.clientY+10+'px','left':event.clientX+10+'px'});$('#",ns("legendtext2"),"').html([",paste0("\"",graphDataFinal2()$label,"\"",collapse=", "),"][row]);}"),
        unhighlightCallback = paste0("function(event) {$('#",ns("legend2"),"').css('display','none');}"),
        pointClickCallback = paste0("function(e,p) {Shiny.onInputChange('",ns("click"),"',p);}")
      )
    #highlightCallback = paste0("function(event, x, points, row, seriesName) {var i = points.findIndex(function(x){return x.name==seriesName;});$('#mylegend').css({'top':points[i].canvasy,'left':points[i].canvasx,'display':'block'}).text('background-color'); }"),
    #
    for(i in seq_len(nrow(key))) {
      plot <- plot %>% dySeries(key$personLabel[i],color=colourPal$colours[key$r[i]],strokeWidth=4,drawPoints=F)#,pointSize=5)
    }
    
    plot
  })
  
  dateWindowRangeAll <- reactive({
    if(is.null(graphData1())) return(NULL)
    if(is.null(graphData2())) return(NULL)
    as_datetime(range(c(graphData1()$dateRecord, graphData2()$dateRecord)))
  })
  
  updateDateWindowAll <- reactiveVal(NULL)
  
  observeEvent(input$chart1_date_window, {
    if(is.null(input$chart1_date_window)) return(NULL)
    if(is.null(dateWindowRangeAll())) return(NULL)
    message("...updating window range")
    if(input$chart1_date_window[1] > dateWindowRangeAll()[2] | input$chart1_date_window[2] < dateWindowRangeAll()[1]) {
      message("...resetting window range")
      updateDateWindowAll(NULL)
    } else {
      updateDateWindowAll(input$chart1_date_window)
    }
  })
  
  observeEvent(input$chart2_date_window, {
    if(is.null(input$chart2_date_window)) return(NULL)
    if(is.null(dateWindowRangeAll())) return(NULL)
    message("...updating window range")
    if(input$chart2_date_window[1] > dateWindowRangeAll()[2] | input$chart2_date_window[2] < dateWindowRangeAll()[1]) {
      message("...resetting window range")
      updateDateWindowAll(NULL)
    } else { 
      updateDateWindowAll(input$chart2_date_window)
    }
  })
  
  observeEvent(input$click,{
    sel <- select_cuber()
    click <- sub("^.*\\(([0-9A-Z]+)\\)$", "\\1", input$click$name)
    match_sel <- sel %in% click
    if(any(match_sel)) {
      sel <- sel[!match_sel]
    } else if(length(sel) < 6) {
      sel <- c(sel,click)
    }
    select_cuber(sel)
    #updateSelectizeInput(session,"players",selected=sel)
  })
  
  return(select_cuber)
}