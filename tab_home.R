server_home <- function(input, output, session, graphData, appOptions, refresh) {
  
  output$text <- renderUI({
    if(is.null(graphData())) return(NULL)
    if(nrow(graphData()) == 0) return(NULL)
    selected <- isolate(appOptions())
    
    selected$event_name <- "speedcubing"
    selected$event <- "all"
    selected$total_tourn <- no_events %>% 
      ungroup() %>%
      filter(championship_type == selected$region, eventId == selected$event) %>%
      summarise(n = sum(n)) %>%
      pull(n)
    
    text_data <- c(
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

    readLines("home.Rmd", encoding = "UTF-8") %>%
      knit(text = ., quiet = TRUE, encoding = "UTF-8") %>%
      markdownToHTML(text = ., fragment.only = TRUE, encoding = "UTF-8") %>%
      HTML()
  })
}
  