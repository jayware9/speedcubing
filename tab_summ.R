ui_summ <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("text")),
    plotlyOutput(ns("chart"))%>%withSpinner(),
    p("Events are ranked based on each cuber's best single time and best average time set during a competition. For most events, each cuber completes five solves each round, the best and worst results are dropped and the average is taken of the remaining three results. Note that many competitions operate 'cut-off' times where any solve above this is recorded as a DNF (did not finish). This chart only includes completed attempts and so may not be representative of all cubers entering competitions")
  )
}
format_percentile <- function(x, ...) {
  ifelse(x < 1, comma(x, 0.1, ...), comma(x, 1, ...))
}

server_summ <- function(input, output, session, graphData, cuberData, resultsData, select_avg, appOptions, refresh) {
  ns <- session$ns
  
  output$text <- renderUI({
    selected <- isolate(appOptions())
    selected$event_name <- events$name[match(selected$event, events$id)]
    selected$region_name <- paste0(case_when(selected$gender %in% "m" ~ "male cubers",
                                             selected$gender %in% "f" ~ "female cubers",
                                             selected$gender %in% "o" ~ "cubers of another gender",
                                             TRUE ~ "cubers"),
                                  ifelse(selected$region == "world", "",
                                   paste(" from",regions$region_name[match(selected$region, regions$championship_type)])))
    warning_text <- NULL
    results_text <- NULL
    cuber_text <- NULL
    percentile_text <- NULL
    graph_title <- NULL
      if(is.null(graphData())) {
      warning_text <- strong(
        paste0("No results found for ",selected$event_name, ifelse(selected$year[1] == selected$year[2], paste0(" in ", selected$year[1]), paste0(" between ", selected$year[1]," and ", selected$year[2])),". Please select another event or year from the menu"))
    } else {
      ds <- graphData() %>% mutate(d = abs(50 - percentile)) %>% filter(row_number() == which.min(d))
      graph_title <- tagList(
        h3(paste0(comma(ds$percentile, 1), "% of cubers had a personal best of ", select_avg(), " ",
                  case_when(selected$event %in% "333fm" ~ 
                              paste0(comma(ds$time, 1), " or lower"),
                            selected$event %in% c("333mbf", "333mbo") ~ 
                              paste0(99 - (ds$time %/% 10000000), " net solves or better"),
                            TRUE ~ 
                              paste0(format_time(ds$time), " or faster"),
                  )
        )),
        h4(paste0(selected$event_name," best ",select_avg()," times for ", selected$region_name, " in WCA competitions by percentile ", selected$year[1],"-", selected$year[2]))
      )
        
      percentile_text <- tags$ul(
        case_when(resultsData()$n >= 100 ~ list(c(0.5, 0.1, 0.01)),
                  resultsData()$n >= 50 ~ list(c(0.5, 0.25, 0.1)),
                  resultsData()$n >= 20 ~ list(c(0.5, 0.25)),
                  resultsData()$n >= 10 ~ list(0.5),
                  TRUE ~ list(NULL)) %>%
          unlist() %>%
          map(~{
            ds <- graphData() %>% mutate(d = abs(.x - percentile / 100)) %>% filter(row_number() == which.min(d))
            tags$li(
              paste0(comma(ds$percentile, 1), "% of cubers had a best ", select_avg(), " ",
                     case_when(selected$event %in% "333fm" ~ 
                                 paste0("fewest moves of ", comma(ds$time, 1), " or lower"),
                               selected$event %in% c("333mbf", "333mbo") ~ 
                                 paste0("result of ", 99 - (ds$time %/% 10000000), " net solves or better"),
                               TRUE ~ 
                                 paste0("time of less than or equal to ", format_time(ds$time)),
                     )
              )
            )}
          )
      )

      results_text <-p(HTML(
        paste0(ifelse(selected$year[1] == selected$year[2], paste0(" In ", selected$year[1]), paste0(" Between ", selected$year[1]," and ", selected$year[2])), ", ",
        resultsData()$n %>% comma(1), " ", selected$region_name, " completed a competition ",select_avg()," time for ", selected$event_name, ". ", 
        ifelse(selected$gender %in% "o", "", paste0("The ",
        case_when(selected$event %in% "333fm" ~ paste0("fewest moves (",select_avg(),") of ",comma(resultsData()$time, 1)),
                  selected$event %in% c("333mbf", "333mbo") ~ paste0("best result of ", format_mbd(resultsData()$time)),
                  TRUE ~ paste0("fastest ",select_avg()," time of ",strong(format_time(resultsData()$time)))),
        " was set by ",strong(resultsData()$personName)," at the ",resultsData()$competitionName, ", ", ifelse(all(resultsData()$imputedTime %in% TRUE), "around ", "on "), format(resultsData()$endTime, "%d %B %Y"), ".")))))
    }
    if(!is.null(cuberData())) if(nrow(cuberData()) > 0) {
      cuber_text <- paste("<ul>", paste0("<li>",cuberData()$personName, " (", cuberData()$personId, ") is ranked ",comma(cuberData()$rank, 1),
                                         " placing ", case_when(cuberData()$gender == "f" ~ "her",cuberData()$gender == "m" ~ "him", TRUE~"them"), " in the top ", 
                                         #ifelse(cuberData()$percentile < 1, ceiling(cuberData()$percentile*10)/10, ceiling(cuberData()$percentile)), "%. ",
                                         cuberData()$percentile %>% (function(x){case_when(x<0.1~comma(ceiling(x*100)/100,0.01),x<1~comma(ceiling(x*10)/10,0.1),TRUE~comma(ceiling(x),1))}), "%. ",
                                         case_when(cuberData()$gender == "f" ~ "Her",cuberData()$gender == "m" ~ "His", TRUE~"Their"),
                                         case_when(selected$event %in% "333fm" ~ paste0(" fewest moves (",select_avg(),") is ",comma(cuberData()$time, 1)),
                                                   selected$event %in% c("333mbf", "333mbo") ~ paste0(" best result is ", format_mbd(cuberData()$time)),
                                                   TRUE ~ paste0(" fastest ",select_avg()," time is ",format_time(cuberData()$time))),
                                         "</li>",collapse = "\n"), "</ul>", sep = "\n") %>%
        HTML()
    }
    
    tagList(
      h2(paste0(selected$event_name, " Competition Personal Bests for ", 
                replace(selected$region_name, selected$region_name == "cubers", "all cubers") %>% capwords)),
      p(paste0(
        "This chart shows the percentiles for each cuber's best ",selected$event_name, " ", select_avg()," time in a competition. ",
        "Only cubers that have set a time in competition are included and so may not be representative of all cubers. ",
        "Times slower than the 99th percentile are not shown.")),
      percentile_text,
      warning_text,
      results_text,
      cuber_text,
      graph_title
    )
               

    # Rmd_text %>%
    #   knit(text = ., quiet = TRUE, encoding = "UTF-8") %>%
    #   markdownToHTML(text = ., fragment.only = TRUE, encoding = "UTF-8") %>%
    #   HTML()
    
  })
  
  graphDataFinal <- reactive({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(is.null(appOptions())) return(NULL)
    selected <- isolate(appOptions())
    message("Updating Percentile chart data")

    graphData() %>%
      mutate(time = case_when(selected$event %in% c("333mbf") ~ 99 - (time %/% 10000000),
                              selected$event %in% c("333mbo") ~ 99 - (time %/% 10000000),
                              TRUE ~ time),
             text = case_when(selected$event %in% c("333fm") ~
                                paste0("percentile: ", format_percentile(percentile), "<br>",comma(time, 1), " moves", "<br>(rank: ", comma(rank, 1), ")"),
                              selected$event %in% c("333mbf", "333mbo") ~
                                paste0("percentile: ", format_percentile(percentile), "<br>",comma(time, 1), " net solves", "<br>(rank: ", comma(rank, 1), ")"),
                              TRUE ~
                                paste0("percentile: ", format_percentile(percentile), "<br>",format_time(time), "<br>(rank: ", comma(rank, 1), ")")))
    
  })
  
  cuberDataFinal <- reactive({
    if(is.null(cuberData())) return(NULL)
    if(nrow(cuberData()) == 0) return(NULL)
    if(is.null(appOptions())) return(NULL)
    selected <- isolate(appOptions())
    cuberData() %>%
      mutate(text = case_when(selected$event %in% c("333fm") ~
                                paste0("percentile: ", comma(percentile, 0.01), "<br>",comma(time, 1), " moves", "<br>(rank: ", comma(rank, 1), ")"),
                              selected$event %in% c("333mbf", "333mbo") ~
                                paste0("percentile: ", comma(percentile, 0.01), "<br>",format_mbd(time), "<br>(rank: ", comma(rank, 1), ")"),
                              TRUE ~
                                paste0("percentile: ", comma(percentile, 0.01), "<br>", format_time(time), "<br>(rank: ", comma(rank, 1), ")"))) %>%
      mutate(time = case_when(selected$event %in% c("333mbf") ~ 99 - (time %/% 10000000),
                              selected$event %in% c("333mbo") ~ 99 - (time %/% 10000000),
                              TRUE ~ time),
             text = paste0(personName, " (", personId, ")<br>", text))
    
  })
  
  output$chart <- renderPlotly({
    if(is.null(graphDataFinal())) return(NULL)
    if(is.null(appOptions())) return(NULL)
    if(is.null(refresh())) tmp <- 1
    selected <- isolate(appOptions())
    message("Updating Percentile chart")
   
    plot <- graphDataFinal() %>%
      plot_ly(x = ~percentile, y = ~ time, name = ~col, color =~ col,
              colors = colourPal$all, hoverinfo = "text", 
              text = ~text) %>%
      add_lines()
    if(!is.null(cuberDataFinal())) {
      plot <- plot %>% add_markers(data = cuberDataFinal(),
                                   marker = list(size = 10))
    }
    yaxis_text <- case_when(selected$event %in% "333fm" ~ "moves",
                            selected$event %in% "333mbf" ~ "net solved",
                            selected$event %in% "333mbo" ~ "solved",
                            TRUE ~ "seconds")
    
    plot %>%
      layout(showlegend=F, yaxis=list(title = yaxis_text, showgrid=F,rangemode = "tozero")) %>%
      config(displayModeBar = F)
  })
}