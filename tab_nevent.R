ui_nevent <- function(id) {
  ns <- NS(id)
  tagList(h2("Speedcubing Event Popularity"),
          uiOutput(ns("warning_text")),
          uiOutput(ns("results_text")),
          tabsetPanel(id = ns("panels"),
                      tabPanel("% of Cubers",
                               h4("% of active cubers that competed in an event at least once in the year, by event and gender"),
                               plotlyOutput(ns("chart1")) %>% withSpinner()),
                      tabPanel("% of competition entrants", 
                               h4("% of all cubers competing in a competition, that competed in a particular event, by event and gender"),
                               plotlyOutput(ns("chart3")) %>% withSpinner(),
                               p("Note: some cubers compete multiple times in a year and all their entries are included in this chart")),
                      tabPanel("No. of Competitions Entered", 
                               h4("Average number of competitions entered of those cubers who took part in an event at least once in the year, by event and gender"),
                               plotlyOutput(ns("chart2")) %>% withSpinner()))
          #          plotlyOutput(ns("chart"))%>% withSpinner(),#plotlyOutput(ns("chart2"))
  )
}

server_nevent <- function(input, output, session, graphData, select_nevent, appOptions, refresh) {
  ns <- session$ns
  
  output$warning_text <- renderUI({
    text <- ""
    if(is.null(graphDataFinal())) {
      text <- "No results found. Please select another ranking region from the menu"
    } else if(nrow(graphDataFinal()) == 0) {
      text <- "No results found. Please select another ranking region from the menu"
    } 
    p(strong(text))
  })
  
  output$results_text <- renderUI({
    if(is.null(graphDataFinal())) return(NULL)
    if(nrow(graphDataFinal()) == 0) return(NULL)
    selected <- isolate(appOptions())
    
    refYr <- graphDataFinal() %>%
      filter(nCuber >= 100) %>%
      pull(year) %>%
      c(metadata$refYr) %>%
      max()
    #refYr = metadata$refYr
    
    #View(graphDataFinal())
    
    summ <- graphDataFinal() %>%
      filter(year <= refYr, 
             gender %in% c("Women", "Men", "Total cubers")) %>%
      mutate(pctc = nCuber / nCuber_all, pcte = nTourn / nTourn_all,
             gender = replace(tolower(gender), gender == "Total cubers", "total")) %>%
      group_by(year, gender)%>%
      mutate(rnkc = rank(1 - pctc, ties = "min", na.last = "keep"),
             rnke = rank(1 - pcte, ties = "min", na.last = "keep")) %>%
      ungroup() %>%
      select(year, eventId, eventName, gender, pctc, pcte, rnkc, rnke, numc = nCuber) %>%
      pivot_wider(values_from = c(pctc, pcte, rnkc, rnke, numc), names_from = c(gender)) %>%
      filter(!pctc_total %in% NA) 
    
    rank1 <- summ %>% 
      pivot_longer(cols = starts_with(c("pct","rnk", "num")), 
                   names_to = c(".value", "key"), 
                   names_pattern = "(\\w{3})(.*)", 
                   values_to = "value") %>%
      filter(rnk <= 3) %>%
      group_by(key, rnk) %>%
      arrange(desc(year)) %>%
      mutate(grp = cumsum(eventId != lag(eventId, default =""))) %>%
      group_by(key, rnk, grp, eventId, eventName) %>%
      summarise(year1 = min(year), year2 = max(year), peak = max(pct), year_peak = year[which.max(pct)],
                num1 = num[which.min(year)], num2 = num[which.max(year)],
                pct1 = pct[which.min(year)], pct2 = pct[which.max(year)])
    
    
    
    
    tagList(
      p(case_when(
        rank1 %>% filter(rnk == 1) %>% pull(eventId) %>% unique() %>% length() == 1 ~
          paste0("The most popular speedcubing event for cubers ", selected$region_from, " is ", 
                 rank1 %>% filter(rnk == 1) %>% pull(eventName) %>% first(), ".",
                 " In ", refYr, " ", 
                 rank1 %>% filter(rnk == 1, key == "c_women", grp == 1) %>% pull(pct2) %>% percent(1),
                 " of female cubers and ",
                 rank1 %>% filter(rnk == 1, key == "c_men", grp == 1) %>% pull(pct2) %>% percent(1),
                 " of male cubers entering speedcubing competitions during the year took part in a ", rank1 %>% filter(rnk == 1) %>% pull(eventName) %>% first(),
                 " event."),
        TRUE ~
          paste0("Since ", 
                 rank1 %>% filter(rnk == 1, key == "c_total") %>% pull(year1) %>% first(), ", with ",
                 "the most popular speedcubing event for cubers ", selected$region_from, " is ", 
                 rank1 %>% filter(rnk == 1, key == "c_total") %>% pull(eventName) %>% first(), ".",
                 " In ", refYr, " ",
                 rank1 %>% filter(rnk == 1, key == "c_women", grp == 1) %>% pull(pct2) %>% percent(1),
                 " of female cubers and ",
                 rank1 %>% filter(rnk == 1, key == "c_men", grp == 1) %>% pull(pct2) %>% percent(1),
                 " of male cubers took part in a ", 
                 rank1 %>% filter(rnk == 1, key == "c_total") %>% pull(eventName) %>% first(),
                 " event."),
      )),
      p(paste0(
        if_else(rank1 %>% filter(rnk == 2, grp == 1, key == "c_total") %>% mutate(test = pct2 > pct1 + 0.1) %>% pull(test),
                "However, over recent years, other events have been increasing in popularity. ", ""),
        case_when(
          #2nd or 3rd most popular different for men and women
          (rank1 %>% filter(rnk == 2, key %in% c("c_women", "c_men"), grp == 1) %>% pull(eventId) %>% unique() %>% length() > 1) &
            (rank1 %>% filter(rnk == 3, key %in% c("c_women", "c_men"), grp == 1) %>% pull(eventId) %>% unique() %>% length() > 1) ~
            paste0("For women the second and third most popular events were ", 
                   rank1 %>% filter(rnk == 2, key %in% "c_women", grp == 1) %>% pull(eventName),
                   " and ",
                   rank1 %>% filter(rnk == 3, key %in% "c_women", grp == 1) %>% pull(eventName),
                   " respectively.",
                   " In ", refYr, " ",
                   rank1 %>% filter(rnk == 2, key %in% "c_women", grp == 1) %>% pull(pct2) %>% percent(1),
                   " of women took part in a ", 
                   rank1 %>% filter(rnk == 2, key %in% "c_women", grp == 1) %>% pull(eventName),
                   " event and ",
                   rank1 %>% filter(rnk == 3, key %in% "c_women", grp == 1) %>% pull(pct2) %>% percent(1),
                   " in a ",
                   rank1 %>% filter(rnk == 3, key %in% "c_women", grp == 1) %>% pull(eventName),
                   " event.",
                   " For men the second and third most popular events were ", 
                   rank1 %>% filter(rnk == 2, key %in% "c_men", grp == 1) %>% pull(eventName),
                   " and ",
                   rank1 %>% filter(rnk == 3, key %in% "c_men", grp == 1) %>% pull(eventName),
                   " respectively.",
                   " In ", refYr, " ",
                   rank1 %>% filter(rnk == 2, key %in% "c_men", grp == 1) %>% pull(pct2) %>% percent(1),
                   " of men took part in a ", 
                   rank1 %>% filter(rnk == 2, key %in% "c_men", grp == 1) %>% pull(eventName),
                   " event and ",
                   rank1 %>% filter(rnk == 3, key %in% "c_men", grp == 1) %>% pull(pct2) %>% percent(1),
                   " in a ",
                   rank1 %>% filter(rnk == 3, key %in% "c_men", grp == 1) %>% pull(eventName),
                   " event."),
          #same start dates for 2nd and 3rd, men and women
          rank1 %>% filter(rnk %in% 2:3, key %in% c("c_women", "c_men"), grp == 1) %>% pull(year1) %>% unique() %>% length() == 1 ~
            paste0("Since ",
                   rank1 %>% filter(rnk %in% 2:3, key %in% c("c_women", "c_men"), grp == 1) %>% pull(year1) %>% first(),
                   " the second and third most popular events were ",
                   rank1 %>% filter(rnk == 2, key %in% "c_total", grp == 1) %>% pull(eventName),
                   " and ",
                   rank1 %>% filter(rnk == 3, key %in% "c_total", grp == 1) %>% pull(eventName),
                   " respectively."),
          TRUE ~
            paste0(
              if_else(rank1 %>% filter(rnk %in% 2, key %in% c("c_women", "c_men"), grp == 1) %>% pull(year1) %>% unique() %>% length() == 1,
                      paste0("Since ",
                             rank1 %>% filter(rnk %in% 2, key %in% c("c_women", "c_men"), grp == 1) %>% pull(year1) %>% first(),
                             " the second most popular event has been ",
                             rank1 %>% filter(rnk == 2, key %in% "c_total", grp == 1) %>% pull(eventName),
                             " for both men and women."),
                      paste0("The second most popular event has been ",
                             rank1 %>% filter(rnk == 2, key %in% "c_total", grp == 1) %>% pull(eventName),
                             " for women since ",
                             rank1 %>% filter(rnk %in% 2, key %in% c("c_women"), grp == 1) %>% pull(year1) %>% unique(),
                             " and for men since ",
                             rank1 %>% filter(rnk %in% 2, key %in% c("c_men"), grp == 1) %>% pull(year1) %>% unique(),
                             ".")),
              if_else(rank1 %>% filter(rnk %in% 3, key %in% c("c_women", "c_men"), grp == 1) %>% pull(year1) %>% unique() %>% length() == 1,
                      paste0("Whilst since ",
                             rank1 %>% filter(rnk %in% 3, key %in% c("c_women", "c_men"), grp == 1) %>% pull(year1) %>% first(),
                             " the third most popular event has been ",
                             rank1 %>% filter(rnk == 3, key %in% "c_total", grp == 1) %>% pull(eventName),
                             " for both men and women."),
                      paste0("The third most popular event has been ",
                             rank1 %>% filter(rnk == 3, key %in% "c_total", grp == 1) %>% pull(eventName),
                             " for women since ",
                             rank1 %>% filter(rnk %in% 3, key %in% c("c_women"), grp == 1) %>% pull(year1) %>% unique(),
                             " and for men since ",
                             rank1 %>% filter(rnk %in% 3, key %in% c("c_men"), grp == 1) %>% pull(year1) %>% unique(),
                             "."))
            )))),
        p(case_when(
          (rank1 %>% filter(rnk == 2, key %in% c("c_women", "c_men"), grp == 1) %>% pull(eventId) %>% unique() %>% length() > 1) &
            (rank1 %>% filter(rnk == 3, key %in% c("c_women", "c_men"), grp == 1) %>% pull(eventId) %>% unique() %>% length() > 1) ~
            "",
          rank1 %>% filter(rnk == 2, grp == 1, key %in% "c_women") %>% mutate(test = year1 == year2) %>%pull(test) ~
            paste0("Amongst female cubers, the proportion each year who entered a ",
                   rank1 %>% filter(rnk == 2, key %in% "c_total", grp == 1) %>% pull(eventName),
                   " was ",
                   rank1 %>% filter(rnk == 2, key %in% "c_women", grp == 1) %>% pull(pct2) %>% percent(1),
                   " in ",
                   rank1 %>% filter(rnk == 2, key %in% "c_women", grp == 1) %>% pull(year2),
                   "."),
          rank1 %>% filter(rnk == 2, grp == 1, key %in% "c_women") %>% pull(year_peak) == refYr ~
            paste0("Amongst female cubers, the proportion each year who entered a ",
                   rank1 %>% filter(rnk == 2, key %in% "c_total", grp == 1) %>% pull(eventName),
                   " event increased from ",
                   rank1 %>% filter(rnk == 2, key %in% "c_women", grp == 1) %>% pull(pct1) %>% percent(1),
                   " in ",
                   rank1 %>% filter(rnk == 2, key %in% "c_women", grp == 1) %>% pull(year1),
                   " to ",
                   rank1 %>% filter(rnk == 2, key %in% "c_women", grp == 1) %>% pull(pct2) %>% percent(1),
                   " in ",
                   rank1 %>% filter(rnk == 2, key %in% "c_women", grp == 1) %>% pull(year2),
                   "."),
          rank1 %>% filter(rnk == 2, grp == 1, key %in% "c_women") %>% mutate(test = peak - pct2 >= 0.05) %>% pull(test) ~
            paste0("In ", refYr, " the proportion of female cubers who entered a ",
                   rank1 %>% filter(rnk == 2, key %in% "c_total", grp == 1) %>% pull(eventName),
                   " event was ",
                   rank1 %>% filter(rnk == 2, key %in% "c_women", grp == 1) %>% pull(pct2) %>% percent(1),
                   " down from the peak of ",
                   rank1 %>% filter(rnk == 2, key %in% "c_women", grp == 1) %>% pull(peak) %>% percent(1),
                   " in ",
                   rank1 %>% filter(rnk == 2, key %in% "c_women", grp == 1) %>% pull(year_peak),
                   "."),
          TRUE ~ 
            paste0("In ", refYr, " the proportion of female cubers who entered a ",
                   rank1 %>% filter(rnk == 2, key %in% "c_total", grp == 1) %>% pull(eventName),
                   " event was ",
                   rank1 %>% filter(rnk == 2, key %in% "c_women", grp == 1) %>% pull(pct2) %>% percent(1),
                   ".")
        ),
        case_when(
          (rank1 %>% filter(rnk == 2, key %in% c("c_women", "c_men"), grp == 1) %>% pull(eventId) %>% unique() %>% length() > 1) &
            (rank1 %>% filter(rnk == 3, key %in% c("c_women", "c_men"), grp == 1) %>% pull(eventId) %>% unique() %>% length() > 1) ~
            "",
          rank1 %>% filter(rnk == 2, grp == 1, key %in% "c_women") %>% mutate(test = year1 == year2) %>%pull(test) ~
            paste0("For male cubers, the proportion each year who entered a ",
                   rank1 %>% filter(rnk == 2, key %in% "c_total", grp == 1) %>% pull(eventName),
                   " was ",
                   rank1 %>% filter(rnk == 2, key %in% "c_men", grp == 1) %>% pull(pct2) %>% percent(1),
                   " in ",
                   rank1 %>% filter(rnk == 2, key %in% "c_men", grp == 1) %>% pull(year2),
                   "."),
          rank1 %>% filter(rnk == 2, grp == 1, key %in% "c_men") %>% pull(year_peak) == refYr ~
            paste0("For male cubers, the proportion each year who entered a ",
                   rank1 %>% filter(rnk == 2, key %in% "c_total", grp == 1) %>% pull(eventName),
                   " event increased from ",
                   rank1 %>% filter(rnk == 2, key %in% "c_men", grp == 1) %>% pull(pct1) %>% percent(1),
                   " in ",
                   rank1 %>% filter(rnk == 2, key %in% "c_men", grp == 1) %>% pull(year1),
                   " to ",
                   rank1 %>% filter(rnk == 2, key %in% "c_men", grp == 1) %>% pull(pct2) %>% percent(1),
                   " in ",
                   rank1 %>% filter(rnk == 2, key %in% "c_men", grp == 1) %>% pull(year2),
                   "."),
          rank1 %>% filter(rnk == 2, grp == 1, key %in% "c_men") %>% mutate(test = peak - pct2 >= 0.05) %>% pull(test) ~
            paste0("In ", refYr, " the proportion of male cubers who entered a ",
                   rank1 %>% filter(rnk == 2, key %in% "c_total", grp == 1) %>% pull(eventName),
                   " event was ",
                   rank1 %>% filter(rnk == 2, key %in% "c_men", grp == 1) %>% pull(pct2) %>% percent(1),
                   " down from the peak of ",
                   rank1 %>% filter(rnk == 2, key %in% "c_men", grp == 1) %>% pull(peak) %>% percent(1),
                   " in ",
                   rank1 %>% filter(rnk == 2, key %in% "c_men", grp == 1) %>% pull(year_peak),
                   "."),
          TRUE ~ 
            paste0("In ", refYr, " the proportion of male cubers who entered a ",
                   rank1 %>% filter(rnk == 2, key %in% "c_total", grp == 1) %>% pull(eventName),
                   " event was ",
                   rank1 %>% filter(rnk == 2, key %in% "c_men", grp == 1) %>% pull(pct2) %>% percent(1),
                   ".")
        )),
      p(paste0("When comparing the proportions of women and men who took part in different events in ", refYr,
               " the largest gap between women and men was for ",
               summ %>% filter(year == refYr) %>% filter(abs(replace_na(pctc_women, 0) - replace_na(pctc_men, 0)) == max(abs(replace_na(pctc_women, 0) - replace_na(pctc_men, 0)))) %>%
                 select(eventName, pctc_women, pctc_men) %>%
                 pmap_chr(~ paste0(..1, " (women: ", percent(replace_na(..2, 0), 0.1), "; men: ", percent(replace_na(..3, 0), 0.1), ")")) %>%
                 text_list(),
               " with a gap of ",
               summ %>% filter(year == refYr) %>% summarise(max = max(abs(replace_na(pctc_women, 0) - replace_na(pctc_men, 0)))) %>% pull(max) %>% percent(0.1, suffix = ""),
               " percentage points.",
               " The smallest gap was for ",
               summ %>% filter(year == refYr) %>% filter(abs(replace_na(pctc_women, 0) - replace_na(pctc_men, 0)) == min(abs(replace_na(pctc_women, 0) - replace_na(pctc_men, 0)))) %>%
                 select(eventName, pctc_women, pctc_men) %>%
                 pmap_chr(~ paste0(..1, " (women: ", percent(replace_na(..2, 0), 0.1), "; men: ", percent(replace_na(..3, 0), 0.1), ")")) %>%
                 text_list(),
               " with a gap of ",
               summ %>% filter(year == refYr) %>% summarise(min = min(abs(replace_na(pctc_women, 0) - replace_na(pctc_men, 0)))) %>% pull(min) %>% percent(0.1, suffix = ""),
               " points."
      ))
      )
    
    
  })
  
  graphDataFinal <- reactive({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    if(any(! c("m", "f") %in% graphData()$gender)) return(NULL)
    message("Updating Events chart data")
    
    graphData() %>%
      pivot_wider(values_from = c(n), names_from=c(type,eventId)) %>%
      #pivot_longer(cols = all_of(c(paste0("nEvent_",events$id),paste0("nCuber_",events$id))), names_to = c("type","eventId"), values_to = c("n"),names_sep="_") %>%
      pivot_longer(cols = c(contains(events$id)), names_to = c("type","eventId"), values_to = c("n"),names_sep="_") %>%
      pivot_wider(values_from = c(n), names_from = c(type)) %>%
      #filter(type == "nEvent") %>%
      mutate(year = year(end_date),
             gender = case_when(gender == "f" ~ "Women", 
                                gender == "m" ~ "Men", 
                                gender == "o" ~ "Other", 
                                gender == "t" ~ "Total cubers",
                                TRUE ~ "Unknown")) %>%
      complete(year, gender, eventId) %>%
      left_join(events %>% select(eventId = id, eventName = name), by = "eventId")
    
    
    
  })
  
  output$chart <- renderPlotly({
    if(is.null(graphDataFinal())) return(NULL)
    if(is.null(select_nevent())) return(NULL)
    if(is.null(refresh())) tmp <- 1
    message("Updating Events chart")
    
    if(select_nevent() == "pct") {
      graph_data <- graphDataFinal() %>%
        mutate(pct = replace_na(nCuber / nCuber_all, 0),
               info = paste0(eventName, ":<br>",gender,": ",comma(replace_na(nCuber, 0), 1) ," cubers (",percent(pct,0.1),")"))
      tickformat <- "%"
      tickvals <- c(0,0.25,0.5,0.75,1)
      title <- "% of cubers"
    } else if(select_nevent() == "all") {
      graph_data <- graphDataFinal() %>%
        mutate(pct = replace_na(nTourn / nCuber_all, 0),
               info = paste0(eventName, ":<br>",gender,": ",comma(pct, 0.1), " competitions"))
      tickformat <- ""
      tickvals <- pretty(range(graph_data$pct))
      title <- "Average number of competitions entered"
    } else if(select_nevent() == "pct_event") {
      graph_data <- graphDataFinal() %>%
        mutate(pct = replace_na(nTourn / nTourn_all, 0),
               info = paste0(eventName, ":<br>",gender,": ",comma(replace_na(nTourn, 0), 1) ," total competitions (",percent(pct,0.1),")"))
      tickformat <- "%"
      tickvals <- c(0,0.25,0.5,0.75,1)
      title <- "Proportion of all competitions participated in"
    }
    
    graph_data <- graph_data %>%
      #      filter(year == 2019) %>%
      filter(gender %in% c("Men", "Women")) %>%
      group_by(eventId, year) %>%
      mutate(pct2 = case_when(max(pct)-min(pct)>0.02 ~ pct, TRUE ~ pct + c(-0.01,0.01))) %>%
      ungroup() %>%
      left_join(graph_data %>% filter(gender == "Total cubers")%>% group_by(year) %>%
                  #group_by(year,eventName) %>% #filter(n>0) %>%
                  mutate(rank=rank(pct, ties.method = "last")) %>% select(year, eventId, rank),
                by = c("year", "eventId")) %>%
      ungroup()
    
    graph_data %>%
      group_by(eventName) %>%
      plot_ly(x= ~pct, y = ~rank,frame = ~year, hoverinfo = "none", height = "400px",# 
              colors=c("Women" = colourPal$colours[1], "Men" = colourPal$colours[2], "background" = "black")
              #marker = list(size = 15), color = ~gender, fill = ~gender),
      ) %>%
      add_lines(showlegend=F, x= ~pct2, color = "background") %>%
      add_markers(size = 5, color = ~gender, hoverinfo = "text", 
                  text = ~info) %>%
      add_text(showlegend=F, #data = graph_data %>% group_by(year, eventName,rank) %>% summarise(pos=ifelse(min(pct)>0.5,"middle right","middle left"),pct=ifelse(min(pct)>0.5,max(pct),min(pct))) %>%mutate(text=paste0(eventName,"   ")),
               data = graph_data %>% group_by(year, eventName,rank) %>% summarise(pct=max(pct)) %>%ungroup() %>% mutate(pct = pct+0.02*max(pct), text=paste0(eventName,"   ")),
               text = ~text, textposition = "middle right") %>%
      animation_slider() %>%
      animation_button() %>%
      animation_opts(60000, transition = 30000, redraw = FALSE) %>%
      layout(legend = list(orientation = "h", y = 1, itemclick = FALSE, itemdoubleclick = FALSE, traceorder = "reversed"), 
             yaxis=list(visible = FALSE, range = c(0,25)),
             xaxis = list(range = ~max(pct)*c(-0.1,1.3), zeroline=FALSE,
                          #tickvals = c(0,0.25,0.5,0.75,1), tickformat = "%",
                          tickvals=tickvals, tickformat=tickformat, showgrid = TRUE, title = title)) %>%
      config(displayModeBar = F)
    
  })
  
  chart1 <- reactive({
    if(is.null(graphDataFinal())) return(NULL)
    if(is.null(select_nevent())) return(NULL)
    message("Updating Events chart 1")
    
    graph_data <- graphDataFinal() %>%
      mutate(pct = replace_na(nCuber / nCuber_all, 0),
             info = paste0(eventName, ":<br>",gender,": ",comma(replace_na(nCuber, 0), 1) ," cubers (",percent(pct,0.1),")"))
    tickformat <- "%"
    tickvals <- c(0,0.25,0.5,0.75,1)
    title <- "% of cubers"
    
    
    graph_data <- graph_data %>%
      #      filter(year == 2019) %>%
      filter(gender %in% c("Men", "Women")) %>%
      group_by(eventId, year) %>%
      mutate(pct2 = case_when(max(pct)-min(pct)>0.02 ~ pct, TRUE ~ pct + c(-0.01,0.01))) %>%
      ungroup() %>%
      left_join(graph_data %>% filter(gender == "Total cubers")%>% group_by(year) %>%
                  #group_by(year,eventName) %>% #filter(n>0) %>%
                  mutate(rank=rank(pct, ties.method = "last")) %>% select(year, eventId, rank),
                by = c("year", "eventId")) %>%
      ungroup()
    
    graph_data %>%
      group_by(eventName) %>%
      plot_ly(x= ~pct, y = ~rank,frame = ~year, hoverinfo = "none", height = "400px",# 
              colors=c("Women" = colourPal$colours[1], "Men" = colourPal$colours[2], "background" = "black")
              #marker = list(size = 15), color = ~gender, fill = ~gender),
      ) %>%
      add_lines(showlegend=F, x= ~pct2, color = "background") %>%
      add_markers(size = 5, color = ~gender, hoverinfo = "text", 
                  text = ~info) %>%
      add_text(showlegend=F, #data = graph_data %>% group_by(year, eventName,rank) %>% summarise(pos=ifelse(min(pct)>0.5,"middle right","middle left"),pct=ifelse(min(pct)>0.5,max(pct),min(pct))) %>%mutate(text=paste0(eventName,"   ")),
               data = graph_data %>% group_by(year, eventName,rank) %>% summarise(pct=max(pct)) %>%ungroup() %>% mutate(pct = pct+0.02*max(pct), text=paste0(eventName,"   ")),
               text = ~text, textposition = "middle right") %>%
      animation_slider() %>%
      animation_button() %>%
      animation_opts(60000, transition = 30000, redraw = FALSE) %>%
      layout(legend = list(orientation = "h", y = 1, itemclick = FALSE, itemdoubleclick = FALSE, traceorder = "reversed"), 
             yaxis=list(visible = FALSE, range = c(0,25)),
             xaxis = list(range = ~max(pct)*c(-0.1,1.3), zeroline=FALSE,
                          #tickvals = c(0,0.25,0.5,0.75,1), tickformat = "%",
                          tickvals=tickvals, tickformat=tickformat, showgrid = TRUE, title = title)) %>%
      config(displayModeBar = F)
    
  })
  
  chart2 <- reactive({
    if(is.null(graphDataFinal())) return(NULL)
    if(is.null(select_nevent())) return(NULL)
    message("Updating Events chart 2")
    
    graph_data <- graphDataFinal() %>%
      mutate(pct = replace_na(nTourn / nCuber_all, 0),
             info = paste0(eventName, ":<br>",gender,": ",comma(pct, 0.1), " competitions"))
    tickformat <- ""
    tickvals <- pretty(range(graph_data$pct))
    title <- "Average number of competitions entered"
    
    graph_data <- graph_data %>%
      #      filter(year == 2019) %>%
      filter(gender %in% c("Men", "Women")) %>%
      group_by(eventId, year) %>%
      mutate(pct2 = case_when(max(pct)-min(pct)>0.02 ~ pct, TRUE ~ pct + c(-0.01,0.01))) %>%
      ungroup() %>%
      left_join(graph_data %>% filter(gender == "Total cubers")%>% group_by(year) %>%
                  #group_by(year,eventName) %>% #filter(n>0) %>%
                  mutate(rank=rank(pct, ties.method = "last")) %>% select(year, eventId, rank),
                by = c("year", "eventId")) %>%
      ungroup()
    
    graph_data %>%
      group_by(eventName) %>%
      plot_ly(x= ~pct, y = ~rank,frame = ~year, hoverinfo = "none", height = "400px",# 
              colors=c("Women" = colourPal$colours[1], "Men" = colourPal$colours[2], "background" = "black")
              #marker = list(size = 15), color = ~gender, fill = ~gender),
      ) %>%
      add_lines(showlegend=F, x= ~pct2, color = "background") %>%
      add_markers(size = 5, color = ~gender, hoverinfo = "text", 
                  text = ~info) %>%
      add_text(showlegend=F, #data = graph_data %>% group_by(year, eventName,rank) %>% summarise(pos=ifelse(min(pct)>0.5,"middle right","middle left"),pct=ifelse(min(pct)>0.5,max(pct),min(pct))) %>%mutate(text=paste0(eventName,"   ")),
               data = graph_data %>% group_by(year, eventName,rank) %>% summarise(pct=max(pct)) %>%ungroup() %>% mutate(pct = pct+0.02*max(pct), text=paste0(eventName,"   ")),
               text = ~text, textposition = "middle right") %>%
      animation_slider() %>%
      animation_button() %>%
      animation_opts(60000, transition = 30000, redraw = FALSE) %>%
      layout(legend = list(orientation = "h", y = 1, itemclick = FALSE, itemdoubleclick = FALSE, traceorder = "reversed"), 
             yaxis=list(visible = FALSE, range = c(0,25)),
             xaxis = list(range = ~max(pct)*c(-0.1,1.3), zeroline=FALSE,
                          #tickvals = c(0,0.25,0.5,0.75,1), tickformat = "%",
                          tickvals=tickvals, tickformat=tickformat, showgrid = TRUE, title = title)) %>%
      config(displayModeBar = F)
    
  })
  
  chart2b <- reactive({
    if(is.null(graphDataFinal())) return(NULL)
    if(is.null(select_nevent())) return(NULL)
    message("Updating Events chart 2")
    
    graph_data <- graphDataFinal() %>%
      mutate(pct = replace_na(nTourn / nCuber, 0),
             info = paste0(eventName, ":<br>",gender,": ",comma(pct, 0.1), " competitions"))
    tickformat <- ""
    tickvals <- pretty(range(graph_data$pct))
    title <- "Average number of competitions entered"
    
    graph_data <- graph_data %>%
      #      filter(year == 2019) %>%
      filter(gender %in% c("Men", "Women")) %>%
      group_by(eventId, year) %>%
      mutate(pct2 = case_when(max(pct)-min(pct)>0.1 ~ pct, pct[1] > pct[2] ~ pct + c(0.05,-0.05), TRUE ~ pct + c(-0.05,0.05))) %>%
      ungroup() %>%
      left_join(graph_data %>% filter(gender == "Total cubers")%>% group_by(year) %>%
                  #group_by(year,eventName) %>% #filter(n>0) %>%
                  mutate(rank=rank(pct, ties.method = "last")) %>% select(year, eventId, rank),
                by = c("year", "eventId")) %>%
      ungroup()
    
    graph_data %>%
      group_by(eventName) %>%
      plot_ly(x= ~pct, y = ~rank,frame = ~year, hoverinfo = "none", height = "400px",# 
              colors=c("Women" = colourPal$colours[1], "Men" = colourPal$colours[2], "background" = "black")
              #marker = list(size = 15), color = ~gender, fill = ~gender),
      ) %>%
      add_lines(showlegend=F, x= ~pct2, color = "background") %>%
      add_markers(size = 5, color = ~gender, hoverinfo = "text", 
                  text = ~info) %>%
      add_text(showlegend=F, #data = graph_data %>% group_by(year, eventName,rank) %>% summarise(pos=ifelse(min(pct)>0.5,"middle right","middle left"),pct=ifelse(min(pct)>0.5,max(pct),min(pct))) %>%mutate(text=paste0(eventName,"   ")),
               data = graph_data %>% group_by(year, eventName,rank) %>% summarise(pct=max(pct)) %>%ungroup() %>% mutate(pct = pct+0.02*max(pct), text=paste0(eventName,"   ")),
               text = ~text, textposition = "middle right") %>%
      animation_slider() %>%
      animation_button() %>%
      animation_opts(60000, transition = 30000, redraw = FALSE) %>%
      layout(legend = list(orientation = "h", y = 1, itemclick = FALSE, itemdoubleclick = FALSE, traceorder = "reversed"), 
             yaxis=list(visible = FALSE, range = c(0,25)),
             xaxis = list(range = ~max(pct)*c(-0.1,1.3), zeroline=FALSE,
                          #tickvals = c(0,0.25,0.5,0.75,1), tickformat = "%",
                          tickvals=tickvals, tickformat=tickformat, showgrid = TRUE, title = title)) %>%
      config(displayModeBar = F)
    
  })
  
  chart2c <- reactive({
    if(is.null(graphDataFinal())) return(NULL)
    if(is.null(select_nevent())) return(NULL)
    message("Updating Events chart 2")
    
    graph_data <- graphDataFinal() %>%
      mutate(pct = replace_na(nTourn, 0),
             info = paste0(eventName, ":<br>",gender,": ",comma(pct, 0.1), " total competitions"))
    tickformat <- ""
    tickvals <- pretty(range(graph_data$pct))
    title <- "Average number of competitions entered"
    
    graph_data <- graph_data %>%
      #      filter(year == 2019) %>%
      filter(gender %in% c("Men", "Women")) %>%
      group_by(eventId, year) %>%
      mutate(pct2 = case_when(max(pct)-min(pct)>0.02 ~ pct, TRUE ~ pct + c(-0.01,0.01))) %>%
      ungroup() %>%
      left_join(graph_data %>% filter(gender == "Total cubers")%>% group_by(year) %>%
                  #group_by(year,eventName) %>% #filter(n>0) %>%
                  mutate(rank=rank(pct, ties.method = "last")) %>% select(year, eventId, rank),
                by = c("year", "eventId")) %>%
      ungroup()
    
    graph_data %>%
      group_by(eventName) %>%
      plot_ly(x= ~pct, y = ~rank,frame = ~year, hoverinfo = "none", height = "400px",# 
              colors=c("Women" = colourPal$colours[1], "Men" = colourPal$colours[2], "background" = "black")
              #marker = list(size = 15), color = ~gender, fill = ~gender),
      ) %>%
      add_lines(showlegend=F, x= ~pct2, color = "background") %>%
      add_markers(size = 5, color = ~gender, hoverinfo = "text", 
                  text = ~info) %>%
      add_text(showlegend=F, #data = graph_data %>% group_by(year, eventName,rank) %>% summarise(pos=ifelse(min(pct)>0.5,"middle right","middle left"),pct=ifelse(min(pct)>0.5,max(pct),min(pct))) %>%mutate(text=paste0(eventName,"   ")),
               data = graph_data %>% group_by(year, eventName,rank) %>% summarise(pct=max(pct)) %>%ungroup() %>% mutate(pct = pct+0.02*max(pct), text=paste0(eventName,"   ")),
               text = ~text, textposition = "middle right") %>%
      animation_slider() %>%
      animation_button() %>%
      animation_opts(60000, transition = 30000, redraw = FALSE) %>%
      layout(legend = list(orientation = "h", y = 1, itemclick = FALSE, itemdoubleclick = FALSE, traceorder = "reversed"), 
             yaxis=list(visible = FALSE, range = c(0,25)),
             xaxis = list(range = ~max(pct)*c(-0.1,1.3), zeroline=FALSE,
                          #tickvals = c(0,0.25,0.5,0.75,1), tickformat = "%",
                          tickvals=tickvals, tickformat=tickformat, showgrid = TRUE, title = title)) %>%
      config(displayModeBar = F)
    
  })
  
  chart3 <- reactive({
    if(is.null(graphDataFinal())) return(NULL)
    if(is.null(select_nevent())) return(NULL)
    message("Updating Events chart 3")
    
    graph_data <- graphDataFinal() %>%
      mutate(pct = replace_na(nTourn / nTourn_all, 0),
             info = paste0(eventName, ":<br>",gender,": ",comma(replace_na(nTourn, 0), 1) ," total competitions (",percent(pct,0.1),")"))
    tickformat <- "%"
    tickvals <- c(0,0.25,0.5,0.75,1)
    title <- "Proportion of all competitors"
    
    graph_data <- graph_data %>%
      #      filter(year == 2019) %>%
      filter(gender %in% c("Men", "Women")) %>%
      group_by(eventId, year) %>%
      mutate(pct2 = case_when(max(pct)-min(pct)>0.02 ~ pct, TRUE ~ pct + c(-0.01,0.01))) %>%
      ungroup() %>%
      left_join(graph_data %>% filter(gender == "Total cubers")%>% group_by(year) %>%
                  #group_by(year,eventName) %>% #filter(n>0) %>%
                  mutate(rank=rank(pct, ties.method = "last")) %>% select(year, eventId, rank),
                by = c("year", "eventId")) %>%
      ungroup()
    
    graph_data %>%
      group_by(eventName) %>%
      plot_ly(x= ~pct, y = ~rank,frame = ~year, hoverinfo = "none", height = "400px",# 
              colors=c("Women" = colourPal$colours[1], "Men" = colourPal$colours[2], "background" = "black")
              #marker = list(size = 15), color = ~gender, fill = ~gender),
      ) %>%
      add_lines(showlegend=F, x= ~pct2, color = "background") %>%
      add_markers(size = 5, color = ~gender, hoverinfo = "text", 
                  text = ~info) %>%
      add_text(showlegend=F, #data = graph_data %>% group_by(year, eventName,rank) %>% summarise(pos=ifelse(min(pct)>0.5,"middle right","middle left"),pct=ifelse(min(pct)>0.5,max(pct),min(pct))) %>%mutate(text=paste0(eventName,"   ")),
               data = graph_data %>% group_by(year, eventName,rank) %>% summarise(pct=max(pct)) %>%ungroup() %>% mutate(pct = pct+0.02*max(pct), text=paste0(eventName,"   ")),
               text = ~text, textposition = "middle right") %>%
      animation_slider() %>%
      animation_button() %>%
      animation_opts(60000, transition = 30000, redraw = FALSE) %>%
      layout(legend = list(orientation = "h", y = 1, itemclick = FALSE, itemdoubleclick = FALSE, traceorder = "reversed"), 
             yaxis=list(visible = FALSE, range = c(0,25)),
             xaxis = list(range = ~max(pct)*c(-0.1,1.3), zeroline=FALSE,
                          #tickvals = c(0,0.25,0.5,0.75,1), tickformat = "%",
                          tickvals=tickvals, tickformat=tickformat, showgrid = TRUE, title = title)) %>%
      config(displayModeBar = F)
    
  })
  
  output$chart1 <- renderPlotly({
    if(is.null(input$panels)) tmp <- 1
    if(is.null(refresh())) tmp <- 1
    message("redraw")
    chart1()
  })
  
  output$chart2 <- renderPlotly({
    if(is.null(input$panels)) tmp <- 1
    if(is.null(refresh())) tmp <- 1
    message("redraw")
    chart2b()
  })
  
  output$chart3 <- renderPlotly({
    if(is.null(input$panels)) tmp <- 1
    if(is.null(refresh())) tmp <- 1
    message("redraw")
    chart3()
  })
  
  
}