ui_champ <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("text")) %>% withSpinner(),
          uiOutput(ns("container"))
  )
}

server_champ <- function(input, output, session, graphData, select_cuber, appOptions, refresh) {
  ns <- session$ns
  
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
    
    comp_text <- NULL
    if(is.null(graphData())) {
      warning_text <- paste0("No results found for ",selected$event_name, " ", selected$region_name, " championships. Please select another ranking region or event from the menu")
    } else if(nrow(graphData()) == 0) {
      warning_text <- paste0("No results found for ",selected$event_name, " ", selected$region_name, " championships. Please select another ranking region or event from the menu")
    } else if(length(select_cuber()) == 0) {
      warning_text <- "No results found. Please select a cuber from the menu"
    } else if(!any(select_cuber() %in% graphData()$personId)) {
      warning_text <- "No results found. Please select a cuber from the menu"
    } else {
      cuber_data <- graphData() %>%
        filter(personId %in% select_cuber()) %>%
        rename(tourn_pos = champ_pos) %>%
        group_split(personId)
      
      warning_text <- "Additional cubers can be selected from the menu"
      comp_text <- tagList(
        cuber_data %>% map(~{
          best <- .x %>% arrange(tourn_pos, end_date) %>% filter(row_number() == 1)
          p(HTML(paste0(
            strong(.x$personName[1]), " has competed in ", nrow(.x), " ", selected$event_name, " ", selected$region_name, " championships. ",
            case_when(min(.x$tourn_pos) == 1 & sum(.x$tourn_pos == 1) > 1 ~
                        paste0(case_when(.x$gender[1] == "m" ~  "He has", .x$gender[1] == "f" ~ "She has", TRUE ~ "They have"),
                               strong("won", sum(.x$tourn_pos == 1), selected$region_name, "championships"), " and had ",
                               sum(.x$tourn_pos <= 3), " podium finishes in total. ",
                               case_when(.x$gender[1] == "m" ~ "His", .x$gender[1] == "f" ~ "Her", TRUE ~ "Their"),
                               " first win was at the ",
                               best$competitionName[1], " on ", format(best$end_date[1], "%d %B %Y"), "."),
                      min(.x$tourn_pos) == 1~
                        paste0(case_when(.x$gender[1] == "m" ~ "He", .x$gender[1] == "f" ~ "She", TRUE ~ "They"),
                               strong(" won the ",best$competitionName[1]), " on ", format(best$end_date[1], "%d %B %Y"),
                               " and has had a total of ", sum(.x$tourn_pos <= 3), " podium finishes."),
                      min(.x$tourn_pos) == 2 & sum(.x$tourn_pos == 2) > 1 ~
                        paste0(case_when(.x$gender[1] == "m" ~ "He has", .x$gender[1] == "f" ~ "She has", TRUE ~ "They have"),
                               strong("  been runner up in ", sum(.x$tourn_pos == 2), " championships"), " and had ",
                               sum(.x$tourn_pos <= 3), " podium finishes in total. ",
                               case_when(.x$gender[1] == "m" ~ "His", .x$gender[1] == "f" ~ "Her", TRUE ~ "Their"),
                               " first second place was at the ",
                               best$competitionName[1], " on ", format(best$end_date[1], "%d %B %Y"), "."),
                      min(.x$tourn_pos) == 2 ~
                        paste0(case_when(.x$gender[1] == "m" ~ "He was", .x$gender[1] == "f" ~ "She was", TRUE ~ "They were"),
                               strong("  runner up in the ",best$competitionName[1]), " on ", format(best$end_date[1], "%d %B %Y"),
                               " and has had a total of ", sum(.x$tourn_pos <= 3), " podium finishes."),
                      min(.x$tourn_pos) == 3 & sum(.x$tourn_pos == 3) > 1 ~
                        paste0(case_when(.x$gender[1] == "m" ~ "He has", .x$gender[1] == "f" ~ "She has", TRUE ~ "They have"),
                               strong("  come third ", sum(.x$tourn_pos == 1), " times in championships"), " with ",
                               case_when(.x$gender[1] == "m" ~ "his", .x$gender[1] == "f" ~ "her", TRUE ~ "their"),
                               " first podium finish at the ",
                               best$competitionName[1], " on ", format(best$end_date[1], "%d %B %Y"), "."),
                      sum(.x$tourn_pos == min(.x$tourn_pos)) > 1 ~
                        paste0(case_when(.x$gender[1] == "m" ~ "His ", .x$gender[1] == "f" ~ "Her", TRUE ~ "Their"),
                               " best result was finishing in ", st_nd_th(min(.x$tourn_pos)), 
                               " place, first doing so at the ",
                               best$competitionName[1], " on ", format(best$end_date[1], "%d %B %Y"), "."),
                      TRUE ~
                        paste0(case_when(.x$gender[1] == "m" ~ "His", .x$gender[1] == "f" ~ "Her", TRUE ~ "Their"),
                               " best result was finishing in ", st_nd_th(min(.x$tourn_pos)), 
                               " place, at the ",
                               best$competitionName[1], " on ", format(best$end_date[1], "%d %B %Y"), ".")
            ),
            case_when(all(.x$average %in% NA) & all(.x$best %in% NA) ~ "",
                      all(.x$average %in% NA) ~
                        paste0(
                          case_when(.x$gender[1] == "m" ~ " His", .x$gender[1] == "f" ~ " Her", TRUE ~ " Their"),
                          " best ",
                          case_when(selected$event %in% "333fm" ~ paste0("fewest moves (single) is ",comma(min(.x$best, Inf, na.rm = TRUE), 1)),
                                    selected$event %in% c("333mbf", "333mbo") ~ paste0("net solves (single) is ", format_mbd(min(.x$best, Inf, na.rm = TRUE))),
                                    TRUE ~ paste0("single time is ",(format_time(min(.x$best, Inf, na.rm = TRUE))))),
                          "."),
                      TRUE ~
                        paste0(
                          case_when(.x$gender[1] == "m" ~ " His", .x$gender[1] == "f" ~ " Her", TRUE ~ " Their"),
                          " best ",
                          case_when(selected$event %in% "333fm" ~ paste0("fewest moves average is ",comma(min(.x$average, Inf, na.rm = TRUE), 1)),
                                    selected$event %in% c("333mbf", "333mbo") ~ paste0("net solves average is ", format_mbd(min(.x$average, Inf, na.rm = TRUE))),
                                    TRUE ~ paste0("average time is ",(format_time(min(.x$average, Inf, na.rm = TRUE))))),
                          " and best ",
                          case_when(selected$event %in% "333fm" ~ paste0("fewest moves single is ",comma(min(.x$best, Inf, na.rm = TRUE), 1)),
                                    selected$event %in% c("333mbf", "333mbo") ~ paste0("net solves single is ", format_mbd(min(.x$best, Inf, na.rm = TRUE))),
                                    TRUE ~ paste0("single time is ",(format_time(min(.x$best, Inf, na.rm = TRUE))))),
                          ".")
            )
          )))
        })
      )
    }
    tagList(h2(capwords(selected$region_name), "Championship"),
            p(strong(warning_text)),
            comp_text
    )
  })
  
  graphDataBest <- reactive({
    if(is.null(graphData())) return(NULL)
    if(all(graphData()[["best"]] %in% NA)) return(NULL)  
    
    message("Updating Championship chart data")
    selected <- isolate(appOptions())
    
    if(selected$event %in% c("333mbf", "333mbo")) {
      graph_data <- graphData() %>%
        rename(tourn_pos = champ_pos) %>%
        select(end_date, personLabel, competitionName, value = best) %>%
        mutate(value = (99 - (value %/% 10000000))*10000000 + (value %% 10000000)) %>%
        spread(personLabel, value) %>%
        arrange(end_date)
    } else {
      graph_data <- graphData() %>%
        rename(tourn_pos = champ_pos) %>%
        select(end_date, personLabel, competitionName, value = best) %>%
        mutate(value = as.numeric(value)) %>%
        spread(personLabel, value) %>%
        arrange(end_date)
    }
    graph_data
  })
  
  output$chart_best <- renderDygraph({
    if(is.null(graphDataBest())) return(NULL)
    if(is.null(refresh())) tmp <- 1
    if(is.null(appOptions())) return(NULL)
    message("Updating Championship chart")
    
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
    
    key_all <- isolate(graphData()) %>%
      distinct(personId, personName) %>%
      mutate(personLabel = paste0(personName, " (", personId, ")"))
    key <- key_all %>%
      filter(personId %in% select_cuber()) %>%
      arrange(personId) %>%
      mutate(r = match(personId, select_cuber()))
    
    plot <- xts(graphDataBest() %>% select(key_all$personLabel), 
                as_datetime(graphDataBest()$end_date)) %>%
      dygraph(group = "champ_pos") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindow())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(titleHeight=40,drawGrid=F,rightGap = 25,strokeWidth=1,digitsAfterDecimal = 2, includeZero = TRUE,
                connectSeparatedPoints=T, drawPoints = F, labelsKMB = TRUE, 
                colors = rep(colourPal$background, nrow(key_all))) %>%
      dyAxis("x", rangePad = 20,valueFormatter = "function(d){return '';}") %>% #"function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'numeric', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = yValueFormatter,
             axisLabelFormatter=yAxisFormatter) %>%
      #dyLegend(show = "follow",labelsSeparateLines = T,width=150)#,labelsDiv="legenddiv")
      dyLegend(show = "always",labelsSeparateLines = FALSE,labelsDiv=ns("legenddivB"),width=225) %>%
      dyCallbacks(
        #zoomCallback = paste0("function(minDate, maxDate, yRanges) {Shiny.onInputChange('",ns("zoom"),"',yRanges);}"),
        #underlayCallback = paste0("function(c,a,d) {Shiny.onInputChange('",ns("area"),"',a);}"),
        highlightCallback = paste0("function(event, x, points, row, seriesName) {$('#",ns("legendB"),"').css({'display':'block','top':event.clientY+10+'px','left':event.clientX+10+'px'});$('#",ns("legendtextB"),"').html([",paste0("\"<strong>",graphDataBest()$competitionName,"</strong><br>(",format(graphDataBest()$end_date, "%d %b %Y"),")","\"",collapse=", "),"][row]);}"),
        unhighlightCallback = paste0("function(event) {$('#",ns("legendB"),"').css('display','none');}"),
        pointClickCallback = paste0("function(e,p) {Shiny.onInputChange('",ns("click"),"',p);}")
      )
    #highlightCallback = paste0("function(event, x, points, row, seriesName) {var i = points.findIndex(function(x){return x.name==seriesName;});$('#mylegend').css({'top':points[i].canvasy,'left':points[i].canvasx,'display':'block'}).text('background-color'); }"),
    #
    for(i in seq_len(nrow(key))) {
      plot <- plot %>% dySeries(key$personLabel[i],color=colourPal$colours[key$r[i]],strokeWidth=4,drawPoints=T,pointSize=5)
    }
    
    plot
  })
  
  graphDataAvg <- reactive({
    if(is.null(graphData())) return(NULL)
    if(all(graphData()[["average"]] %in% NA)) return(NULL)  
    if(is.null(appOptions())) return(NULL)
    message("Updating Championship chart data")
    
    selected <- isolate(appOptions())
    
    
    if(selected$event %in% c("333mbf", "333mbo")) {
      graph_data <- graphData() %>%
        rename(tourn_pos = champ_pos) %>%
        select(end_date, personLabel, competitionName, value = average) %>%
        mutate(value = (99 - (value %/% 10000000))*10000000 + (value %% 10000000)) %>%
        spread(personLabel, value) %>%
        arrange(end_date)
    } else {
      graph_data <- graphData() %>%
        rename(tourn_pos = champ_pos) %>%
        select(end_date, personLabel, competitionName, value = average) %>%
        mutate(value = as.numeric(value)) %>%
        spread(personLabel, value) %>%
        arrange(end_date)
    }
    graph_data
  })
  
  output$chart_avg <- renderDygraph({
    if(is.null(graphDataAvg())) return(NULL)
    if(is.null(refresh())) tmp <- 1
    message("Updating Championship chart")
    
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
    
    key_all <- isolate(graphData()) %>%
      distinct(personId, personName) %>%
      mutate(personLabel = paste0(personName, " (", personId, ")"))
    key <- key_all %>%
      filter(personId %in% select_cuber()) %>%
      arrange(personId) %>%
      mutate(r = match(personId, select_cuber()))
    
    plot <- xts(graphDataAvg() %>% select(key_all$personLabel), 
                as_datetime(graphDataAvg()$end_date)) %>%
      dygraph(group = "champ_pos") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindow())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(titleHeight=40,drawGrid=F,rightGap = 25,strokeWidth=1,digitsAfterDecimal = 2, includeZero = TRUE,
                connectSeparatedPoints=T, drawPoints = F, labelsKMB = TRUE, 
                colors = rep(colourPal$background, nrow(key_all))) %>%
      dyAxis("x", rangePad = 20,valueFormatter = "function(d){return '';}") %>% #"function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'numeric', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = yValueFormatter,
             axisLabelFormatter=yAxisFormatter) %>%
      #dyLegend(show = "follow",labelsSeparateLines = T,width=150)#,labelsDiv="legenddiv")
      dyLegend(show = "always",labelsSeparateLines = FALSE,labelsDiv=ns("legenddivA"),width=225) %>%
      dyCallbacks(
        #zoomCallback = paste0("function(minDate, maxDate, yRanges) {Shiny.onInputChange('",ns("zoom"),"',yRanges);}"),
        #underlayCallback = paste0("function(c,a,d) {Shiny.onInputChange('",ns("area"),"',a);}"),
        highlightCallback = paste0("function(event, x, points, row, seriesName) {$('#",ns("legendA"),"').css({'display':'block','top':event.clientY+10+'px','left':event.clientX+10+'px'});$('#",ns("legendtextA"),"').html([",paste0("\"<strong>",graphDataAvg()$competitionName,"</strong><br>(",format(graphDataAvg()$end_date, "%d %b %Y"),")","\"",collapse=", "),"][row]);}"),
        unhighlightCallback = paste0("function(event) {$('#",ns("legendA"),"').css('display','none');}"),
        pointClickCallback = paste0("function(e,p) {Shiny.onInputChange('",ns("click"),"',p);}")
      )
    #highlightCallback = paste0("function(event, x, points, row, seriesName) {var i = points.findIndex(function(x){return x.name==seriesName;});$('#mylegend').css({'top':points[i].canvasy,'left':points[i].canvasx,'display':'block'}).text('background-color'); }"),
    #
    for(i in seq_len(nrow(key))) {
      plot <- plot %>% dySeries(key$personLabel[i],color=colourPal$colours[key$r[i]],strokeWidth=4,drawPoints=T,pointSize=5)
    }
    
    plot
  })
  
  graphDataFin <- reactive({
    if(is.null(graphData())) return(NULL)
    if(all(graphData()[["final"]] %in% NA)) return(NULL)  
    message("Updating Championship chart data")
    
    selected <- isolate(appOptions())
    
    if(selected$event %in% c("333mbf", "333mbo")) {
      graph_data <- graphData() %>%
        rename(tourn_pos = champ_pos) %>%
        select(end_date, personLabel, competitionName, value = final) %>%
        mutate(value = (99 - (value %/% 10000000))*10000000 + (value %% 10000000)) %>%
        spread(personLabel, value) %>%
        arrange(end_date)
    } else {
      graph_data <- graphData() %>%
        rename(tourn_pos = champ_pos) %>%
        select(end_date, personLabel, competitionName, value = final) %>%
        mutate(value = as.numeric(value)) %>%
        spread(personLabel, value) %>%
        arrange(end_date)
    }
    graph_data
  })
  
  output$chart_final <- renderDygraph({
    if(is.null(graphDataFin())) return(NULL)
    if(is.null(refresh())) tmp <- 1
    message("Updating Championship chart")
    
    selected <- isolate(appOptions())
    if(selected$event %in% c("333mbf", "333mbo")) {
      yValueFormatter <- "function(d) {return '<br>'+formatWcaMbd(d);}"
      yAxisFormatter <- "function(d) {return d/10000000;}"
    } else if(selected$event %in% "333fm") {
      yValueFormatter <- "function(d) {return '<br>'+formatWcaMove(d);}"
      yAxisFormatter <- NULL
    } else  {
      yValueFormatter <- "function(d) {return '<br>'+formatWcaTime(d);}"
      yAxisFormatter <- NULL
    } 
    
    key_all <- isolate(graphData()) %>%
      distinct(personId, personName) %>%
      mutate(personLabel = paste0(personName, " (", personId, ")"))
    key <- key_all %>%
      filter(personId %in% select_cuber()) %>%
      arrange(personId) %>%
      mutate(r = match(personId, select_cuber()))
    
    plot <- xts(graphDataFin() %>% select(key_all$personLabel), 
                as_datetime(graphDataFin()$end_date)) %>%
      dygraph(group = "champ_pos") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindow())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(titleHeight=40,drawGrid=F,rightGap = 25,strokeWidth=1,digitsAfterDecimal = 2, includeZero = TRUE,
                connectSeparatedPoints=T, drawPoints = F, labelsKMB = TRUE, 
                colors = rep(colourPal$background, nrow(key_all))) %>%
      dyAxis("x", rangePad = 20,valueFormatter = "function(d){return '';}") %>% #"function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'numeric', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = yValueFormatter,
             axisLabelFormatter=yAxisFormatter) %>%
      #dyLegend(show = "follow",labelsSeparateLines = T,width=150)#,labelsDiv="legenddiv")
      dyLegend(show = "always",labelsSeparateLines = FALSE,labelsDiv=ns("legenddivF"),width=225) %>%
      dyCallbacks(
        #zoomCallback = paste0("function(minDate, maxDate, yRanges) {Shiny.onInputChange('",ns("zoom"),"',yRanges);}"),
        #underlayCallback = paste0("function(c,a,d) {Shiny.onInputChange('",ns("area"),"',a);}"),
        highlightCallback = paste0("function(event, x, points, row, seriesName) {$('#",ns("legendF"),"').css({'display':'block','top':event.clientY+10+'px','left':event.clientX+10+'px'});$('#",ns("legendtextF"),"').html([",paste0("\"<strong>",graphDataFin()$competitionName,"</strong><br>(",format(graphDataFin()$end_date, "%d %b %Y"),")","\"",collapse=", "),"][row]);}"),
        unhighlightCallback = paste0("function(event) {$('#",ns("legendF"),"').css('display','none');}"),
        pointClickCallback = paste0("function(e,p) {Shiny.onInputChange('",ns("click"),"',p);}")
      )
    #highlightCallback = paste0("function(event, x, points, row, seriesName) {var i = points.findIndex(function(x){return x.name==seriesName;});$('#mylegend').css({'top':points[i].canvasy,'left':points[i].canvasx,'display':'block'}).text('background-color'); }"),
    #
    for(i in seq_len(nrow(key))) {
      plot <- plot %>% dySeries(key$personLabel[i],color=colourPal$colours[key$r[i]],strokeWidth=4,drawPoints=T,pointSize=5)
    }
    
    plot
  })
  
  graphDataPos <- reactive({
    if(is.null(graphData())) return(NULL)
    if(all(graphData()[["champ_pos"]] %in% NA)) return(NULL)  
    message("Updating Championship chart data")
    
    selected <- isolate(appOptions())
    graphData() %>%
      rename(tourn_pos = champ_pos) %>%
      select(end_date, personLabel, competitionName, value = tourn_pos) %>%
      mutate(value = as.numeric(value)) %>%
      spread(personLabel, value) %>%
      arrange(end_date)
    
  })
  
  output$chart_pos <- renderDygraph({
    if(is.null(graphDataPos())) return(NULL)
    if(is.null(refresh())) tmp <- 1
    if(is.null(appOptions())) return(NULL)
    message("Updating Championship chart")
    
    selected <- isolate(appOptions())
    yValueFormatter <- "function(d) {return formatCommas(d,0);}"
    yAxisFormatter <- NULL
    
    
    key_all <- isolate(graphData()) %>%
      distinct(personId, personName) %>%
      mutate(personLabel = paste0(personName, " (", personId, ")"))
    key <- key_all %>%
      filter(personId %in% select_cuber()) %>%
      arrange(personId) %>%
      mutate(r = match(personId, select_cuber()))
    
    plot <- xts(graphDataPos() %>% select(key_all$personLabel), 
                as_datetime(graphDataPos()$end_date)) %>%
      dygraph(group = "champ_pos") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindow())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(titleHeight=40,drawGrid=F,rightGap = 25,strokeWidth=1,digitsAfterDecimal = 2, includeZero = TRUE,
                connectSeparatedPoints=T, drawPoints = F, labelsKMB = TRUE, 
                colors = rep(colourPal$background, nrow(key_all))) %>%
      dyAxis("x", rangePad = 20,valueFormatter = "function(d){return '';}") %>% #"function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'numeric', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = yValueFormatter,
             axisLabelFormatter=yAxisFormatter) %>%
      #dyLegend(show = "follow",labelsSeparateLines = T,width=150)#,labelsDiv="legenddiv")
      dyLegend(show = "always",labelsSeparateLines = FALSE,labelsDiv=ns("legenddivP"),width=225) %>%
      dyCallbacks(
        #zoomCallback = paste0("function(minDate, maxDate, yRanges) {Shiny.onInputChange('",ns("zoom"),"',yRanges);}"),
        #underlayCallback = paste0("function(c,a,d) {Shiny.onInputChange('",ns("area"),"',a);}"),
        highlightCallback = paste0("function(event, x, points, row, seriesName) {$('#",ns("legendP"),"').css({'display':'block','top':event.clientY+10+'px','left':event.clientX+10+'px'});$('#",ns("legendtextP"),"').html([",paste0("\"<strong>",graphDataPos()$competitionName,"</strong><br>(",format(graphDataPos()$end_date, "%d %b %Y"),")","\"",collapse=", "),"][row]);}"),
        unhighlightCallback = paste0("function(event) {$('#",ns("legendP"),"').css('display','none');}"),
        pointClickCallback = paste0("function(e,p) {Shiny.onInputChange('",ns("click"),"',p);}")
      )
    #highlightCallback = paste0("function(event, x, points, row, seriesName) {var i = points.findIndex(function(x){return x.name==seriesName;});$('#mylegend').css({'top':points[i].canvasy,'left':points[i].canvasx,'display':'block'}).text('background-color'); }"),
    #
    for(i in seq_len(nrow(key))) {
      plot <- plot %>% dySeries(key$personLabel[i],color=colourPal$colours[key$r[i]],strokeWidth=4,drawPoints=T,pointSize=5)
    }
    
    plot
  })
  
  output$container <- renderUI({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(!any(graphData()$personId %in% select_cuber())) return(NULL)
    selected <- isolate(appOptions())
    
    t_panel <- list()
    
    if(!is.null(graphDataPos())) {
      t_panel <- c(t_panel, list(
        tabPanel("Final Position",
                 h4("Final position in WCA competitions for selected cubers"),
                 div(
                   style="position:relative",
                   dygraphOutput(ns("chart_pos")),
                   div(id=ns("legendP"),class="legend0 legend-single",
                       span(id=ns("legendtextP")),
                       div(id=ns("legenddivP"),class="legenddiv"))))
      ))
    }
    if(!is.null(graphDataFin())) {
      t_panel <- c(t_panel, list(
        tabPanel("Final result",
                 h4("Final", case_when(selected$event %in% "333fm" ~ "fewest moves", selected$event %in% c("333mbf", "333mbo") ~ "net solves", TRUE ~ "time"), 
                    "in WCA competitions for selected cubers"),
                 div(
                   style="position:relative",
                   dygraphOutput(ns("chart_final")),
                   div(id=ns("legendF"),class="legend0 legend-single",
                       span(id=ns("legendtextF")),
                       div(id=ns("legenddivF"),class="legenddiv"))))
      ))
    }
    if(!is.null(graphDataAvg())) {
      t_panel <- c(t_panel, list(
        tabPanel("Best Average",
                 h4("Best average", case_when(selected$event %in% "333fm" ~ "fewest moves", selected$event %in% c("333mbf", "333mbo") ~ "net solves", TRUE ~ "time"), 
                    "in WCA competitions for selected cubers"),
                 div(
                   style="position:relative",
                   dygraphOutput(ns("chart_avg")),
                   div(id=ns("legendA"),class="legend0 legend-single",
                       span(id=ns("legendtextA")),
                       div(id=ns("legenddivA"),class="legenddiv"))))
      ))
    }
    if(!is.null(graphDataBest())) {
      t_panel <- c(t_panel, list(
        tabPanel("Best Solve",
                 h4("Best single", case_when(selected$event %in% "333fm" ~ "fewest moves", selected$event %in% c("333mbf", "333mbo") ~ "net solves", TRUE ~ "time"), 
                    "in WCA competitions for selected cubers"),
                 div(
                   style="position:relative",
                   dygraphOutput(ns("chart_best")),
                   div(id=ns("legendB"),class="legend0 legend-single",
                       span(id=ns("legendtextB")),
                       div(id=ns("legenddivB"),class="legenddiv"))))
      ))
    }
    do.call(tabsetPanel, c(id = ns("panels"),t_panel))
  })
  
  dateWindowRange <- reactive({
    if(is.null(graphDataPos())) return(NULL)
    as_datetime(range(graphDataPos()$end_date))
  })
  
  updateDateWindow <- reactiveVal(NULL)
  
  observeEvent(input$chart_best_date_window, {
    if(is.null(input$chart_best_date_window)) return(NULL)
    if(is.null(dateWindowRange())) return(NULL)
    message("...updating window range")
    if(input$chart_best_date_window[1] > dateWindowRange()[2] | input$chart_best_date_window[2] < dateWindowRange()[1]) {
      message("...resetting window range")
      updateDateWindow(NULL)
    } else {
      updateDateWindow(input$chart_best_date_window)
    }
  })
  observeEvent(input$chart_avg_date_window, {
    if(is.null(input$chart_avg_date_window)) return(NULL)
    if(is.null(dateWindowRange())) return(NULL)
    message("...updating window range")
    if(input$chart_avg_date_window[1] > dateWindowRange()[2] | input$chart_avg_date_window[2] < dateWindowRange()[1]) {
      message("...resetting window range")
      updateDateWindow(NULL)
    } else { 
      updateDateWindow(input$chart_avg_date_window)
    }
  })
  observeEvent(input$chart_final_date_window, {
    if(is.null(input$chart_final_date_window)) return(NULL)
    if(is.null(dateWindowRange())) return(NULL)
    message("...updating window range")
    if(input$chart_final_date_window[1] > dateWindowRange()[2] | input$chart_final_date_window[2] < dateWindowRange()[1]) {
      message("...resetting window range")
      updateDateWindow(NULL)
    } else { 
      updateDateWindow(input$chart_final_date_window)
    }
  })
  observeEvent(input$chart_final_pos_window, {
    if(is.null(input$chart_final_pos_window)) return(NULL)
    if(is.null(dateWindowRange())) return(NULL)
    message("...updating window range")
    if(input$chart_final_pos_window[1] > dateWindowRange()[2] | input$chart_final_pos_window[2] < dateWindowRange()[1]) {
      message("...resetting window range")
      updateDateWindow(NULL)
    } else { 
      updateDateWindow(input$chart_final_pos_window)
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