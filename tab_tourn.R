ui_tourn <- function(id) {
  ns <- NS(id)
  tagList(h2("Competition Results"),
          uiOutput(ns("text")),
          uiOutput(ns("container"))
  )
}

server_tourn <- function(input, output, session, graphData, appOptions, refresh) {
  ns <- session$ns
  
  output$text <- renderUI({
    selected <- isolate(appOptions())
    
    comp_text <- NULL
    if(is.null(graphData())) {
      warning_text <- "No results found. Please select a cuber from the menu"
    } else if(nrow(graphData()) == 0) {
      warning_text <- "No results found. Please select a cuber from the menu"
    } else {
      cuber_data <- graphData() %>%
        filter(key %in% c("best", "average", "final", "tourn_pos")) %>%
        spread(key, time) %>%
        filter(! is.na(tourn_pos)) %>%
        arrange(startTime) %>%
        group_split(personId)
      
      warning_text <- "Additional cubers can be selected from the menu"
      comp_text <- tagList(
        cuber_data %>% map(~{
          best <- .x %>% arrange(tourn_pos, startTime) %>% filter(row_number() == 1)
          p(HTML(paste0(
            strong(.x$personName[1]), " has competed in ", nrow(.x), " ", selected$event_name, " competitions. ",
            case_when(min(.x$tourn_pos) == 1 & sum(.x$tourn_pos == 1) > 1 ~
                        paste0(case_when(.x$gender[1] == "m" ~  "He has", .x$gender[1] == "f" ~ "She has", TRUE ~ "They have"),
                               strong("  won ", sum(.x$tourn_pos == 1), " competitions"), " and had ",
                               sum(.x$tourn_pos <= 3), " podium finishes in total. ",
                               case_when(.x$gender[1] == "m" ~ "His", .x$gender[1] == "f" ~ "Her", TRUE ~ "Their"),
                               " first win was at the ",
                               best$competitionName[1], " on ", format(best$startTime[1], "%d %B %Y"), "."),
                      min(.x$tourn_pos) == 1~
                        paste0(case_when(.x$gender[1] == "m" ~ "He", .x$gender[1] == "f" ~ "She", TRUE ~ "They"),
                               strong(" won the ",best$competitionName[1]), " on ", format(best$startTime[1], "%d %B %Y"),
                               " and has had a total of ", sum(.x$tourn_pos <= 3), " podium finishes."),
                      min(.x$tourn_pos) == 2 & sum(.x$tourn_pos == 2) > 1 ~
                        paste0(case_when(.x$gender[1] == "m" ~ "He has", .x$gender[1] == "f" ~ "She has", TRUE ~ "They have"),
                               strong("  been runner up in ", sum(.x$tourn_pos == 2), " competitions"), " and had ",
                               sum(.x$tourn_pos <= 3), " podium finishes in total. ",
                               case_when(.x$gender[1] == "m" ~ "His", .x$gender[1] == "f" ~ "Her", TRUE ~ "Their"),
                               " first second place was at the ",
                               best$competitionName[1], " on ", format(best$startTime[1], "%d %B %Y"), "."),
                      min(.x$tourn_pos) == 2 ~
                        paste0(case_when(.x$gender[1] == "m" ~ "He was", .x$gender[1] == "f" ~ "She was", TRUE ~ "They were"),
                               strong("  runner up in the ",best$competitionName[1]), " on ", format(best$startTime[1], "%d %B %Y"),
                               " and has had a total of ", sum(.x$tourn_pos <= 3), " podium finishes."),
                      min(.x$tourn_pos) == 3 & sum(.x$tourn_pos == 3) > 1 ~
                        paste0(case_when(.x$gender[1] == "m" ~ "He has", .x$gender[1] == "f" ~ "She has", TRUE ~ "They have"),
                               strong("  come third ", sum(.x$tourn_pos == 1), " times in competitions"), " with ",
                               case_when(.x$gender[1] == "m" ~ "his", .x$gender[1] == "f" ~ "her", TRUE ~ "their"),
                               " first podium finish at the ",
                               best$competitionName[1], " on ", format(best$startTime[1], "%d %B %Y"), "."),
                      sum(.x$tourn_pos == min(.x$tourn_pos)) > 1 ~
                        paste0(case_when(.x$gender[1] == "m" ~ "His ", .x$gender[1] == "f" ~ "Her", TRUE ~ "Their"),
                               " best result was finishing in ", st_nd_th(min(.x$tourn_pos)), 
                               " place, first doing so at the ",
                               best$competitionName[1], " on ", format(best$startTime[1], "%d %B %Y"), "."),
                      TRUE ~
                        paste0(case_when(.x$gender[1] == "m" ~ "His", .x$gender[1] == "f" ~ "Her", TRUE ~ "Their"),
                               " best result was finishing in ", st_nd_th(min(.x$tourn_pos)), 
                               " place, at the ",
                               best$competitionName[1], " on ", format(best$startTime[1], "%d %B %Y"), ".")
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
    tagList(p(strong(warning_text)),
            comp_text
    )
  })
  
  # Multiple charts ---------------------------------------------------------
  
  graphDataBest <- reactive({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(all(graphData()$time[graphData()$key == "best"] %in% NA)) return(NULL)
    if(is.null(appOptions())) return(NULL)
    selected <- isolate(appOptions())
    message("Updating Tournament chart data")
    
    key <- graphData() %>% distinct(personId, personName, r) %>% 
      arrange(r) %>%
      mutate(seriesName = paste0(personName, ": Best Single"))
    key <- bind_rows(key %>% mutate(personId = paste0("time_", personId), 
                                    seriesName = paste0(personName, ": Single")),
                     key)
    graph_data <- graphData() %>%
      filter(key %in% c("time1", "time2", "time3", "time4", "time5", "best")) %>% #TEMP
      mutate(label = if_else(key == "best", competitionName, roundLabel)) %>%
      group_by(label) %>%
      mutate(datetime = ifelse(key == "best", endTime, startTime + (endTime-startTime)*(row_number()-1)/n())) %>%
      mutate(key = if_else(key == "best", personId, paste0("time_", personId))) %>%
      select(datetime, label, key, time) %>%
      group_by(datetime) %>%
      arrange(label) %>%
      mutate(adj = 10*cumsum(duplicated(label)) - 10) %>%
      ungroup() %>%
      mutate(datetime = datetime + adj) %>%
      spread(key, time) %>%
      arrange(datetime)
    
    if(selected$event %in% c("333mbf", "333mbo")) {
      graph_data <- graph_data %>%
        mutate_at(key$personId, ~(99 - (. %/% 10000000))*10000000 + (. %% 10000000))
      yValueFormatter <- "function(d) {return '<br>'+formatWcaMbd(d);}"
      yAxisFormatter <- "function(d) {return d/10000000;}"
    } else if(selected$event %in% "333fm") {
      yValueFormatter <- "function(d) {return '<br>'+formatWcaMove(d);}"
      yAxisFormatter <- NULL
    } else {
      yValueFormatter <- "function(d) {return '<br>'+formatWcaTime(d);}"
      yAxisFormatter <- NULL
    } 
    
    list(data = graph_data, key = key, yValueFormatter = yValueFormatter, yAxisFormatter = yAxisFormatter)
  })
  
  output$chart_best <- renderDygraph({
    if(is.null(graphDataBest())) return(NULL)
    if(is.null(refresh())) tmp <- 1
    message("Updating Tournament chart")
    
    plot <- xts(graphDataBest()$data %>% select(graphDataBest()$key$personId), 
                as_datetime(graphDataBest()$data$datetime)) %>%
      dygraph(group = "tourn_pos") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindow())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(connectSeparatedPoints = TRUE, includeZero = TRUE, retainDateWindow = FALSE,
                titleHeight=40,drawGrid=F,rightGap = 25,digitsAfterDecimal = 2) %>%
      dyAxis("x", rangePad = 20,
             valueFormatter = "function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'short', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = graphDataBest()$yValueFormatter, 
             axisLabelFormatter=graphDataBest()$yAxisFormatter) %>%
      dyLegend(show = "always",labelsSeparateLines = FALSE,labelsDiv=ns("legenddivB"),width=225) %>%
      dyCallbacks(
        highlightCallback = paste0("function(event, x, points, row, seriesName) {$('#",ns("legendB"),"').css({'display':'block','top':event.clientY+10+'px','left':event.clientX+10+'px'});$('#",ns("legendtextB"),"').text([",paste0("\"",graphDataBest()$data$label,"\"",collapse=", "),"][row]);}"),
        unhighlightCallback = paste0("function(event) {$('#",ns("legendB"),"').css('display','none');}")
      )
    for(i in seq_len(nrow(graphDataBest()$key))) {
      if(grepl("time_", graphDataBest()$key$personId[i])) {
        plot <- plot %>% dySeries(graphDataBest()$key$personId[i], label = graphDataBest()$key$seriesName[i], strokeWidth = 0, drawPoints = TRUE, pointSize = 1.5, color = colourPal$light[graphDataBest()$key$r[i]])
      } else {
        plot <- plot %>% dySeries(graphDataBest()$key$personId[i], label = graphDataBest()$key$seriesName[i], strokeWidth = 3, drawPoints = FALSE, color = colourPal$colours[graphDataBest()$key$r[i]])
      }
    }
    plot
  })
  
  graphDataAvg <- reactive({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(all(graphData()$time[graphData()$key == "average"] %in% NA)) return(NULL)
    if(is.null(appOptions())) return(NULL)
    selected <- isolate(appOptions())
    message("Updating Tournament chart data")
    key <- graphData() %>% distinct(personId, personName, r) %>% 
      arrange(r) %>%
      mutate(seriesName = paste0(personName, ": Best Average"))
    key <- bind_rows(key %>% mutate(personId = paste0("time_", personId), 
                                    seriesName = paste0(personName, ": Average")),
                     key)
    graph_data <- graphData() %>%
      filter(key %in% c("time_average", "average")) %>% #TEMP
      mutate(label = if_else(key == "average", competitionName, roundLabel),
             datetime = if_else(key == "average", endTime, startTime),
             key = if_else(key == "average", personId, paste0("time_", personId))) %>%
      select(label,datetime,key,time) %>%
      group_by(datetime) %>%
      arrange(label) %>%
      mutate(adj = 10*cumsum(duplicated(label)) - 10) %>%
      ungroup() %>%
      mutate(datetime = datetime + adj) %>%
      spread(key, time) %>%
      arrange(datetime)
    if(selected$event %in% c("333mbf", "333mbo")) {
      graph_data <- graph_data %>%
        mutate_at(key$personId, ~(99 - (. %/% 10000000))*10000000 + (. %% 10000000))
      yValueFormatter <- "function(d) {return '<br>'+formatWcaMbd(d);}"
      yAxisFormatter <- "function(d) {return d/10000000;}"
    } else if(selected$event %in% "333fm") {
      yValueFormatter <- "function(d) {return '<br>'+formatWcaMove(d);}"
      yAxisFormatter <- NULL
    } else {
      yValueFormatter <- "function(d) {return '<br>'+formatWcaTime(d);}"
      yAxisFormatter <- NULL
    } 
    
    list(data = graph_data, key = key, yValueFormatter = yValueFormatter, yAxisFormatter = yAxisFormatter)
  })
  
  output$chart_avg <- renderDygraph({
    if(is.null(graphDataAvg())) return(NULL)
    if(is.null(refresh())) tmp <- 1
    message("Updating Tournament chart")
    
    plot <- xts(graphDataAvg()$data %>% select(graphDataAvg()$key$personId), 
                as_datetime(graphDataAvg()$data$datetime)) %>%
      dygraph(group = "tourn_pos") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindow())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(connectSeparatedPoints = TRUE, includeZero = TRUE, retainDateWindow = FALSE,
                titleHeight=40,drawGrid=F,rightGap = 25,digitsAfterDecimal = 2) %>%
      dyAxis("x", rangePad = 20,
             valueFormatter = "function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'short', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = graphDataAvg()$yValueFormatter, 
             axisLabelFormatter=graphDataAvg()$yAxisFormatter) %>%
      dyLegend(show = "always",labelsSeparateLines = FALSE,labelsDiv=ns("legenddivA"),width=225) %>%
      dyCallbacks(
        highlightCallback = paste0("function(event, x, points, row, seriesName) {$('#",ns("legendA"),"').css({'display':'block','top':event.clientY+10+'px','left':event.clientX+10+'px'});$('#",ns("legendtextA"),"').text([",paste0("\"",graphDataAvg()$data$label,"\"",collapse=", "),"][row]);}"),
        unhighlightCallback = paste0("function(event) {$('#",ns("legendA"),"').css('display','none');}")
      )
    for(i in seq_len(nrow(graphDataAvg()$key))) {
      if(grepl("time_", graphDataAvg()$key$personId[i])) {
        plot <- plot %>% dySeries(graphDataAvg()$key$personId[i], label = graphDataAvg()$key$seriesName[i], strokeWidth = 0, drawPoints = TRUE, pointSize = 1.5, color = colourPal$light[graphDataAvg()$key$r[i]])
      } else {
        plot <- plot %>% dySeries(graphDataAvg()$key$personId[i], label = graphDataAvg()$key$seriesName[i], strokeWidth = 3, drawPoints = FALSE, color = colourPal$colours[graphDataAvg()$key$r[i]])
      }
    }
    plot
  })
  
  graphDataFin <- reactive({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(all(graphData()$time[graphData()$key == "final"] %in% NA)) return(NULL)
    if(is.null(appOptions())) return(NULL)
    selected <- isolate(appOptions())
    message("Updating Tournament chart data")
    key <- graphData() %>% distinct(personId, personName, r) %>% 
      arrange(r) %>%
      mutate(seriesName = paste0(personName, ": Final Result"))
    
    graph_data <- graphData() %>%
      filter(key %in% c("final")) %>% #TEMP
      mutate(key = if_else(key == "final", personId, paste0("time_", personId))) %>%
      select(label = roundLabel,datetime = endTime,key,time) %>%
      group_by(datetime) %>%
      arrange(label) %>%
      mutate(adj = 10*cumsum(duplicated(label)) - 10) %>%
      ungroup() %>%
      mutate(datetime = datetime + adj) %>%
      spread(key, time) %>%
      arrange(datetime)
    
    if(selected$event %in% c("333mbf", "333mbo")) {
      graph_data <- graph_data %>%
        mutate_at(key$personId, ~(99 - (. %/% 10000000))*10000000 + (. %% 10000000))
      yValueFormatter <- "function(d) {return '<br>'+formatWcaMbd(d);}"
      yAxisFormatter <- "function(d) {return d/10000000;}"
    } else if(selected$event %in% "333fm") {
      yValueFormatter <- "function(d) {return '<br>'+formatWcaMove(d);}"
      yAxisFormatter <- NULL
    } else {
      yValueFormatter <- "function(d) {return '<br>'+formatWcaTime(d);}"
      yAxisFormatter <- NULL
    } 
    list(data = graph_data, key = key, yValueFormatter = yValueFormatter, yAxisFormatter = yAxisFormatter)
  })
  
  output$chart_final <- renderDygraph({
    if(is.null(graphDataFin())) return(NULL)
    if(is.null(refresh())) tmp <- 1
    message("Updating Tournament chart")
    
    plot <- xts(graphDataFin()$data %>% select(graphDataFin()$key$personId), 
                as_datetime(graphDataFin()$data$datetime)) %>%
      dygraph(group = "tourn_pos") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindow())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(connectSeparatedPoints = TRUE, includeZero = TRUE, retainDateWindow = FALSE,
                titleHeight=40,drawGrid=F,rightGap = 25,digitsAfterDecimal = 2) %>%
      dyAxis("x", rangePad = 20,
             valueFormatter = "function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'short', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = graphDataFin()$yValueFormatter, 
             axisLabelFormatter=graphDataFin()$yAxisFormatter) %>%
      # dyLegend(show = "follow",labelsSeparateLines = T,width=150)#,labelsDiv="legenddiv")%>%
      dyLegend(show = "always",labelsSeparateLines = FALSE,labelsDiv=ns("legenddivF"),width=225) %>%
      dyCallbacks(
        highlightCallback = paste0("function(event, x, points, row, seriesName) {$('#",ns("legendF"),"').css({'display':'block','top':event.clientY+10+'px','left':event.clientX+10+'px'});$('#",ns("legendtextF"),"').text([",paste0("\"",graphDataFin()$data$label,"\"",collapse=", "),"][row]);}"),
        unhighlightCallback = paste0("function(event) {$('#",ns("legendF"),"').css('display','none');}")
      )
    for(i in seq_len(nrow(graphDataFin()$key))) {
      if(grepl("time_", graphDataFin()$key$personId[i])) {
        plot <- plot %>% dySeries(graphDataFin()$key$personId[i], label = graphDataFin()$key$seriesName[i], strokeWidth = 0, drawPoints = TRUE, pointSize = 1.5, color = colourPal$light[graphDataFin()$key$r[i]])
      } else {
        plot <- plot %>% dySeries(graphDataFin()$key$personId[i], label = graphDataFin()$key$seriesName[i], strokeWidth = 3, drawPoints = FALSE, color = colourPal$colours[graphDataFin()$key$r[i]])
      }
    }
    plot
  })
  
  graphDataPos <- reactive({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(all(graphData()$time[graphData()$key == "tourn_pos"] %in% NA)) return(NULL)
    if(is.null(appOptions())) return(NULL)
    selected <- isolate(appOptions())
    message("Updating Tournament chart data")
    key <- graphData() %>% distinct(personId, personName, r) %>% 
      arrange(r) %>%
      mutate(seriesName = paste0(personName, ": Position"))
    graph_data <- graphData() %>%
      filter(key %in% c("tourn_pos")) %>% #TEMP
      select(label = competitionName,datetime = endTime,key = personId,time) %>%
      group_by(datetime) %>%
      arrange(label) %>%
      mutate(adj = 10*cumsum(duplicated(label)) - 10) %>%
      ungroup() %>%
      mutate(datetime = datetime + adj) %>%
      spread(key, time) %>%
      arrange(datetime)
    
    yValueFormatter <- NULL
    yAxisFormatter <- NULL
    
    list(data = graph_data, key = key, yValueFormatter = yValueFormatter, yAxisFormatter = yAxisFormatter)
  })
  
  output$chart_pos <- renderDygraph({
    if(is.null(graphDataPos())) return(NULL)
    if(is.null(refresh())) tmp <- 1
    message("Updating Tournament chart")
    
    plot <- xts(graphDataPos()$data %>% select(graphDataPos()$key$personId), 
                as_datetime(graphDataPos()$data$datetime)) %>%
      dygraph(group = "tourn_pos") %>%
      dyRangeSelector(height = 40, strokeColor = "", dateWindow = isolate(updateDateWindow())) %>%
      dyHighlight(highlightCircleSize = 0,
                  highlightSeriesBackgroundAlpha = 0.2,
                  highlightSeriesOpts = list(highlightCircleSize = 5),
                  hideOnMouseOut = TRUE) %>%
      dyOptions(connectSeparatedPoints = TRUE, includeZero = TRUE, retainDateWindow = FALSE,
                titleHeight=40,drawGrid=F,rightGap = 25,digitsAfterDecimal = 2) %>%
      dyAxis("x", rangePad = 20,
             valueFormatter = "function(d) {var date = new Date(d);return date.toLocaleDateString('en-GB',{ year: 'numeric', month: 'short', day: 'numeric' });}") %>%
      dyAxis("y", valueFormatter = graphDataPos()$yValueFormatter, 
             axisLabelFormatter=graphDataPos()$yAxisFormatter) %>%
      # dyLegend(show = "follow",labelsSeparateLines = T,width=150)#,labelsDiv="legenddiv")%>%
      dyLegend(show = "always",labelsSeparateLines = FALSE,labelsDiv=ns("legenddivP"),width=225) %>%
      dyCallbacks(
        highlightCallback = paste0("function(event, x, points, row, seriesName) {$('#",ns("legendP"),"').css({'display':'block','top':event.clientY+10+'px','left':event.clientX+10+'px'});$('#",ns("legendtextP"),"').text([",paste0("\"",graphDataPos()$data$label,"\"",collapse=", "),"][row]);}"),
        unhighlightCallback = paste0("function(event) {$('#",ns("legendP"),"').css('display','none');}")
      )
    for(i in seq_len(nrow(graphDataPos()$key))) {
      if(grepl("time_", graphDataPos()$key$personId[i])) {
        plot <- plot %>% dySeries(graphDataPos()$key$personId[i], label = graphDataPos()$key$seriesName[i], strokeWidth = 0, drawPoints = TRUE, pointSize = 1.5, color = colourPal$light[graphDataPos()$key$r[i]])
      } else {
        plot <- plot %>% dySeries(graphDataPos()$key$personId[i], label = graphDataPos()$key$seriesName[i], strokeWidth = 3, drawPoints = FALSE, color = colourPal$colours[graphDataPos()$key$r[i]])
      }
    }
    plot
  })
  
  output$container <- renderUI({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    selected <- isolate(appOptions())
    
    t_panel <- list()
    
    if(!is.null(graphDataPos())) {
      t_panel <- c(t_panel, list(
        tabPanel("Final Position",
                 h4("Final position in WCA competitions for selected cubers"),
                 div(
                   style="position:relative",
                   dygraphOutput(ns("chart_pos")),
                   div(id=ns("legendP"),class="legend0",
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
                   div(id=ns("legendF"),class="legend0",
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
                   div(id=ns("legendA"),class="legend0",
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
                   div(id=ns("legendB"),class="legend0",
                       span(id=ns("legendtextB")),
                       div(id=ns("legenddivB"),class="legenddiv"))))
      ))
    }
    do.call(tabsetPanel, c(id = ns("panels"),t_panel))
  })
  
  dateWindowRange <- reactive({
    if(is.null(graphDataPos())) return(NULL)
    as_datetime(range(graphDataPos()$datetime))
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
  
}