library(tidyverse)
library(shiny)
library(shinythemes)
library(lubridate)
library(dygraphs)
library(xts)
library(scales)
library(plotly)
library(RColorBrewer)
library(shinycssloaders)
library(knitr)
library(markdown)

load("data/wca.rData")

source("tab_summ.R", local = TRUE)
source("tab_rank.R", local = TRUE)
source("tab_tourn.R", local = TRUE)
source("tab_champ.R", local = TRUE)
source("tab_record.R", local = TRUE)
source("tab_progression.R", local = TRUE)
source("tab_home.R", local = TRUE)
source("tab_ncuber.R", local = TRUE)
source("tab_nevent.R", local = TRUE)
source("tab_event_vs2.R", local = TRUE)

colourPal <- list(background = brewer.pal(12, "Paired")[1],
                  colours = brewer.pal(12, "Paired")[c(4, 6, 8, 10, 12, 2)],
                  light = brewer.pal(12, "Paired")[c(4, 6, 8, 10, 12, 2) - 1],
                  all = brewer.pal(12, "Paired")[c(1, 4, 6, 8, 10, 12, 2)])
names(colourPal$all) = c("background", paste0("highlight", 1:6))
colourPal <- list(background = brewer.pal(12, "Paired")[1],
                  colours = brewer.pal(12, "Paired")[c(2, 8, 4, 10, 6, 12)],
                  light = brewer.pal(12, "Paired")[c(2, 8, 4, 10, 6, 12) - 1],
                  all = brewer.pal(12, "Paired")[c(1, 2, 8, 4, 10, 6, 12)])
names(colourPal$all) = c("background", paste0("highlight", 1:6))
# 
# colourPal <- list(background = brewer.pal(8, "Dark2")[6],
#                   colours = brewer.pal(8, "Dark2")[-6],
#                   light = brewer.pal(8, "Set2")[-6],
#                   all = brewer.pal(8, "Dark2")[c(6, 1:5, 7:8)])
# names(colourPal$all) = c("background", paste0("highlight", 1:7))


format_time <- function(x, accuracy = 0.01, suffix = " seconds") {
  case_when(x < 60 ~ comma(x, accuracy = accuracy, suffix = suffix),
            x %% 60 < 10 ~ paste0(x %/% 60, ":0", comma(x %% 60, accuracy = accuracy, suffix = "")),
            TRUE ~ paste0(x %/% 60, ":", comma(x %% 60, accuracy = accuracy, suffix = "")))
}

format_mbd <- function(x) {
  paste0(99 - (x %/% 10000000),
         " net solves (",
         99 - (x %/% 10000000) + (x %% 100),
         "/",
         99 - (x %/% 10000000) + 2*(x %% 100),
         "; ",
         ((x %/% 100) %% 100000) %/% 60, ":", ((x %/% 100) %% 100000) %% 60,
         ")")
}

text_list <- function(x, oxford.comma = FALSE) {
  if(length(x) == 1) return(x)
  paste0(paste(x[-length(x)], collapse = ", "), ifelse(oxford.comma, ", and ", " and "), x[length(x)])
}

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

st_nd_th <- function(x) {
  paste0(x, case_when(x %% 10 == 1 & x %% 100 != 11 ~ "st", 
                      x %% 10 == 2 & x %% 100 != 12 ~ "nd", 
                      x %% 10 == 3 & x %% 100 != 13 ~ "rd", 
                      TRUE~"th"))
}

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$head(#shinyjs::useShinyjs(),
    tags$script(src = "funcs.js"),
    includeCSS("styles.css")),
  #titlePanel("World Cubing"),
  sidebarLayout(
    mainPanel(
      #tabsetPanel(id = "tab",
      navbarPage(id = "tab", title = NULL,
                 tabPanel("Speedcubing", uiOutput(NS("home", "text"))),
                 navbarMenu(title = "Cubers",
                            tabPanel("Competitions Entered", ui_ncuber("ncuber")),
                            tabPanel("Events Entered", ui_nevent("nevent"))
                 ),
                 navbarMenu(title = "Fastest Times",
                            tabPanel("Record Progression", ui_progression("progression")),
                            tabPanel("Record Holders", ui_record("record"))
                 ),
                 #tabPanel("New", ui_event_vs("eventvs")),
                 navbarMenu(title = "Results",
                            tabPanel("Personal Bests", ui_summ("summ")),
                            tabPanel("Rankings", ui_rank("rank")),
                            tabPanel("Competitions", ui_tourn("tourn")),
                            tabPanel("Championships", ui_champ("champ"))
                 )
                 
      ),
      br(),
      br(),
      br(),
      br()
    ),
    
    sidebarPanel(
    #navlistPanel(widths = c(12, 12),
      selectizeInput("select_region", "Select ranking region:", {
        region_list <- regions %>% distinct(hierarchy, championship_type, region_name) %>% 
          arrange(hierarchy, region_name)
        region_vals <- region_list$championship_type
        names(region_vals) <- mutate(region_list, region_name = ifelse(floor(hierarchy) < hierarchy, 
                                                                       paste0("---", region_name),
                                                                       region_name))$region_name 
        region_vals
      }, "world"),
      conditionalPanel("(input.tab=='Competitions Entered' && input.select_num_event != true) || input.tab=='Record Progression' || input.tab=='Personal Bests' || input.tab=='Rankings' || input.tab=='Competitions' || input.tab=='Championships'", 
                       selectizeInput("select_event", "Select cubing event:", {
                         event_list <- arrange(events, rank)
                         event_vals <- event_list$id %>% as.character()
                         names(event_vals) <- event_list$name %>% as.character()
                         event_vals
                       })
      ),
      conditionalPanel("input.tab=='New'", 
                       selectizeInput("select_event2", "Select cubing event:", {
                         event_list <- arrange(events, rank)
                         event_vals <- event_list$id %>% as.character()
                         names(event_vals) <- event_list$name %>% as.character()
                         event_vals
                       })
      ),
      conditionalPanel("input.tab=='Competitions Entered'",
                       checkboxInput("select_num_event", "All events:", TRUE)),
                       #radioButtons("select_num_type", "Display:", c("Total cubers"="cum", "Cubers by year"="nCuber", "Competitions entered"="nTourn", "Events entered"="nEvent"), inline = FALSE)),
      #display options
      conditionalPanel("input.tab=='Temp'",
                       radioButtons("select_num_type", "Display:", c("Total cubers"="cum", "Cubers by year"="nCuber", "Competitions entered"="nTourn", "Events entered"="nEvent"), inline = FALSE)),
      conditionalPanel("input.tab=='Temp'",
                       radioButtons("select_nevent", "Display:", c("% of cubers"="pct", "Competitions entered"="all", "% of events"="pct_event"), inline = TRUE)),
      conditionalPanel("input.tab=='Personal Bests' || input.tab=='Rankings'", 
                       radioButtons("select_avg", "Display:", c("Average"="average", "Single"="best"), inline = TRUE)),
      # conditionalPanel("input.tab=='Rankings'",
      #                  radioButtons("select_rnk", "Display:", c("Rank"="rank", "Percentile"="pct", "Personal Best"="pb"), inline = TRUE)),
      # conditionalPanel("input.tab=='Competitions' || input.tab=='Championships'",
      #                  radioButtons("select_tourn", "Display:", c("Final Result"="final", "Final Position"="tourn_pos", "Best Average"="average", "Best Single"="best"), inline = FALSE)),
      conditionalPanel("input.tab=='Record Holders'",
                       checkboxGroupInput("select_record", "Record type:", c("Single"="single", "Average"="average"), c("single", "average"), inline = FALSE)),
      #filter options
      conditionalPanel("input.tab=='Personal Bests' || input.tab=='Rankings' || input.tab=='Competitions' || input.tab=='Championships'", 
                       selectInput("select_gender", "Filter cubers by gender:", 
                                   c("All", "Men"="m", "Women"="f", "Other"="o")),
                       sliderInput("select_year", "Include cubers active between:", 
                                   min = year(metadata$min), 
                                   max = year(metadata$max), 
                                   value = c(year(metadata$min), year(metadata$max)), step = 1, sep = "")
                       ),
      conditionalPanel("input.tab=='Record Holders' || input.tab=='Personal Bests' || input.tab=='Rankings' || input.tab=='Competitions' || input.tab=='Championships'", 
                       uiOutput("select_search_cuber"),
                       uiOutput("menu_cuber") %>% withSpinner(),
                       uiOutput("cubers")),
      p(strong("About:")),
      p("This dashboard is based on competition results owned and maintained by the World Cube Assocation, published at",
        a(href="https://www.worldcubeassociation.org/results","https://www.worldcubeassociation.org/results"),"as of",
        format(metadata$updated, "%d %B %Y"))
    )
    #)
    # selectizeInput("select_year", NULL,
    #                distinct(competitions, yr=lubridate::year(end_date))%>%arrange(desc(yr)) %>% pull(yr) %>%c("All years", .)),
    #selectizeInput("select_cuber", NULL, c("Select cuber" = "", distinct(results, personName)$personName %>%as.character), "")
  )
)

server <- function(input, output, session) {
  
  ####Filter data####
  
  filter_events <- reactive({
    if(length(input$select_event) == 0) return(tibble())
    message("Filtering events")
    #tourn_results[[input$select_event]]
    readRDS(paste0("data/tourn_results",input$select_event,".rds"))
  })
  
  filter_years <- reactive({
    if(is.null(input$select_year)) return(tibble())
    if(nrow(filter_events()) == 0) return(tibble())

    message("Filtering years")
    if(input$select_year[1] == year(metadata$min) & input$select_year[2] == year(metadata$max)) {
      return(filter_events())
    } else if(input$select_year[1] == year(metadata$min)) {
      return(filter_events() %>% filter(year <= input$select_year[2]))
    } else if(input$select_year[2] == year(metadata$max)) {
      return(filter_events() %>% filter(year >= input$select_year[1]))
    } else {
      return(filter_events() %>% filter(year >= input$select_year[1], year <= input$select_year[2]))
    }
  })
  
  filter_gender <- reactive({
    if(is.null(input$select_gender)) return(tibble())
    if(nrow(filter_years()) == 0) return(tibble())
    message("Filtering gender")
    
    if(input$select_gender == "All") return(filter_years())
    
    cubers %>%
      filter(gender %in% input$select_gender) %>%
      select(personId) %>%
      inner_join(filter_years(), by = "personId")
  })
  
  
  filter_countries <- reactive({
    if(is.null(input$select_region)) return(tibble())
    if(nrow(filter_gender()) == 0) return(tibble())
    message("Filtering countries")
    
    #reset cached datasets - overkill as may not need resetting? - ranking always needs resetting, tournaments resets on eventId change, tournaments might or might not need a reset on other changes
    memory$tournLoaded <- NULL
    memory$tournData <- NULL
    memory$rankingLoaded <- NULL
    memory$rankingData <- NULL
    
    if(input$select_region == "world") return(filter_gender())
    
    countries <- regions %>% filter(championship_type %in% input$select_region) %>% select(personCountryId = countryId)
    filter_gender() %>%
      inner_join(countries, by = "personCountryId") %>%
      mutate(championship_type = input$select_region)
  })
  
  output$select_search_cuber <- renderUI({
    if(input$select_gender %in% "o") return(NULL)
    if(input$tab == "Record Holders") {
      search_text <- p("Select cuber(s) below, to highlight on the charts, or click to show the ",
                       a("top ranked cubers.", src = "#", onclick = "Shiny.onInputChange('select_record_cubers', Math.random(100000));"),
                       "Alternatively cubers can be selected by clicking on the chart.")
    } else {
      search_text <- p("Rankings will be calculated for the selected region, event, year(s), and gender(s). Select cuber(s) below, to display statistics, or click to show the ",
                       a("top 5 ranked cubers", src = "#", onclick = "Shiny.onInputChange('select_top_cubers', Math.random(100000));"))
    }
    tagList(
      search_text,
      textInput("search_cuber", "Search cubers")
    )
  })
  
  output$menu_cuber <- renderUI({
    if(length(input$search_cuber) == 0) return(NULL)
    if(nchar(input$search_cuber) == 0) return(NULL)
    if(nrow(filter_countries()) == 0) return(NULL)
    if(input$select_gender %in% "o") return(NULL)
    if(nchar(input$search_cuber) < 3) return(span("Searching... (Too many matches)"))
    if(length(select_cuber()) == 6) return(span("Maximum number of cubers selected"))
    cuber_list <- cubers %>% inner_join(filter_countries() %>% distinct(personId))
    mat <- union(grep(input$search_cuber,cuber_list$personName,ignore.case = TRUE),
                 grep(input$search_cuber,cuber_list$personId,ignore.case = TRUE))
    mat <- mat[order(cuber_list$personName[mat])]
    if(length(mat) < 15) {
      amat <- agrep(input$search_cuber,cuber_list$personId,ignore.case = TRUE)
      amat <- amat[order(cuber_list$personName[amat])]
      if(length(amat)<30) mat <- c(mat, amat) 
    }
    if(length(mat) == 0) return(span("No cubers found"))
    if(length(mat) >= 50) return(span("Searching... (Too many matches)"))
    
    choices <- c("",cuber_list$personId[mat])
    names(choices) <- c("",paste0(cuber_list$personName[mat], " (",cuber_list$personId[mat],")"))
    selectInput("options_cuber", NULL, choices)
    
  })
  observeEvent(input$options_cuber, {
    if(is.null(input$options_cuber)) return(NULL)
    if(input$options_cuber == "") return(NULL)
    message("Updating cuber selection")
    if(!input$options_cuber %in% select_cuber()) {
      select_cuber() %>%
        c(input$options_cuber) %>%
        select_cuber()
      updateTextInput(session, "search_cuber", value = "")
    }
  })
  
  select_cuber = reactiveVal({character()})
  
  observeEvent(input$select_top_cubers, {
    if(is.null(personal_bests())) return(NULL)
    if(nrow(personal_bests()) == 0) return(NULL)
    if(input$select_gender %in% "o") return(NULL)
    message("Updating cuber selection")
    
    #more efficient than tidyverse equiv.as only does a partial sort
    if(length(personal_bests()$time) <=5) {
      select_cuber(personal_bests()$personId)
    } else {
      x <- which(personal_bests()$time <= sort(personal_bests()$time, partial = 5)[5])
      select_cuber(personal_bests()$personId[x][order(personal_bests()$personId[x])[1:5]])
    }
  })
  
  observeEvent(input$select_record_cubers, {
    if(is.null(wca_record())) return(NULL)
    if(nrow(wca_record()) == 0) return(NULL)
    if(input$select_gender %in% "o") return(NULL)
    message("Updating cuber selection")

    wca_record() %>%
      group_by(personId) %>%
      summarise(totRecords = n(),
                curRecords = sum(recordEnd %in% NA)) %>%
      filter(curRecords == max(curRecords) | totRecords == max(totRecords)) %>%
      arrange(desc(curRecords)) %>%
      filter(row_number() <=5) %>%
      pull(personId) %>%
      select_cuber()
  })
  
  output$cubers <- renderUI({
    cuber_name <- paste0(cubers$personName[match(select_cuber(), cubers$personId)], " (",select_cuber(),")")
    if(length(select_cuber()) == 0) {
      cuber_list = list(span("No cubers selected"))
    } else {
      cuber_list <- imap(select_cuber(), ~div(id = paste0("select_cuber",.y), 
                                              style = paste0("color:",colourPal$colours[.y]), 
                                              a(href = "#", onclick = paste0("Shiny.onInputChange('remove','",.x,"');$('#select_cuber",.y,"').css('display','none');"), 
                                                img(src="x-circle-fill.svg", height = "16px")), 
                                              strong(cuber_name[.y])))
    }
    do.call(div,c(
      list(strong("Cubers selected:")),
      cuber_list
    ))
  })
  observeEvent(input$remove,{
    select_cuber() %>%
      setdiff(input$remove) %>%
      select_cuber()
  })
  
  ##remove cubers from selection if not in filter countries  
  # observeEvent(filter_countries(), {
  #   filter_countries() %>%
  #     select(personId, competitionId, eventId) %>%
  #     filter(personId %in% select_cuber()$personId)
  #     inner_join(select_cuber(), by = c("personId", "competitionId", "eventId")) %>%
  #     select_cuber()
  #  })
  
  
  ####Graph data####
  #IMPORTANT: convention for data is return NULL if no results data; return empty tibble if no cuber data
  
  personal_bests <- reactive({
    if(nrow(filter_countries()) == 0) return(NULL)
    if(is.null(input$select_avg)) return(NULL)
    message("Calculating personal bests")
    filter_countries() %>%
      select(personId, time = all_of(input$select_avg)) %>%
      filter(! time %in% NA) %>%
      #group_by(personId) %>%
      #summarise(time = min(time)) %>%
      arrange(time) %>%
      distinct(personId, .keep_all = TRUE) %>%
      ungroup()
  })
  
  graph_quantiles <- reactive({
    if(is.null(personal_bests())) return(NULL)
    if(nrow(personal_bests()) == 0) return(NULL)
    message("Calculating percentiles")
    
    tibble(#percentile = c((0:9) / 10, 1:99),
      rank = c(1,round(c((1:9) / 10, 1:99) / 100 * nrow(personal_bests()))),
      col = "background") %>%
      distinct() %>%
      filter(rank > 0) %>%
      mutate(percentile = rank / nrow(personal_bests()) * 100,
             time = sort(personal_bests()$time, partial=rank)[rank])
  })
  
  highlight_quantiles <- reactive({
    if(is.null(personal_bests())) return(NULL)
    if(nrow(personal_bests()) == 0) return(NULL)
    if(length(select_cuber()) == 0) return(tibble())
    message("Calculating percentile highlights")
    
    
    highlight <- personal_bests() %>%
      filter(personId %in% select_cuber()) %>%
      mutate(rank = map_dbl(time, ~{sum(personal_bests()$time < .x) + 1}),
             percentile = rank / nrow(personal_bests()) * 100,
             col = paste0("highlight", match(personId, select_cuber())))%>%
      left_join(cubers, by = "personId")
  })
  
  results_quantiles <- eventReactive(personal_bests(), {
    if(is.null(personal_bests())) return(NULL)
    if(nrow(personal_bests()) == 0) return(NULL)
    unofficial_records %>%
      filter(championship_type %in% input$select_region, 
             eventId %in% input$select_event,
             gender %in% input$select_gender | input$select_gender %in% "All",
             year >= input$select_year[1],
             year <= input$select_year[2],
             type %in% input$select_avg) %>%
      arrange(time, endTime) %>%
      filter(row_number() == 1) %>%
      mutate(n = nrow(personal_bests()))
  })
  
  memory <- reactiveValues(tournData = NULL, tournLoaded = NULL, rankingData = NULL, rankingLoaded = NULL)
  
  update_avg <- eventReactive(input$select_avg, {
    memory$rankingLoaded <- NULL
    memory$rankingData <- NULL
    return(input$select_avg)
  })
  
  wca_ranking <- reactive({
    if(is.null(update_avg())) return(NULL)
    if(nrow(filter_countries()) == 0) return(NULL)
    if(length(select_cuber()) == 0) return(tibble())
    message("Calculating rankings")
    
      loaded <- isolate(memory$rankingLoaded)
      case <- case_when(is.null(isolate(memory$rankingData)) ~ "full",
                        length(intersect(loaded, select_cuber())) == 0 ~ "full",
                        TRUE ~ "partial")
      message("...", case, " update")
      if(case == "full") {
        add <- select_cuber()
        rem <- character(0)
        memory$rankingData <- tibble()
      } else {
        add <- setdiff(select_cuber(), loaded)
        rem <- setdiff(loaded, select_cuber())
      }
      if(length(rem) > 0) {
        memory$rankingData <- isolate(memory$rankingData) %>%
          filter(! personId %in% rem)
      }
      if(length(add) > 0 ) {
        pb <- isolate(filter_countries()) %>%
          filter(personId %in% add)  %>%
          rename(time = all_of(update_avg())) %>%
          filter(! time %in% NA) %>%
          group_by(personId) %>%
          arrange(end_date) %>%
          mutate(pb = time == cummin(time)) %>%
          ungroup() %>%
          filter(pb %in% TRUE) %>%
          group_by(personId) %>%
          arrange(desc(time)) %>%
          mutate(end_date1 = end_date,
                 end_date2 = lead(end_date, default = Sys.Date())) %>%
          ungroup() %>%
          filter(end_date1 != end_date2) %>%
          mutate(solve = row_number())
        
        if(nrow(pb) > 0) {
          x <- isolate(filter_countries()) %>% select(personId, end_date, time = all_of(update_avg())) %>%
            filter(time < max(pb$time)) %>%
            arrange(end_date)
          pb<- pb %>%
            select(time, solve, end_date2, end_date1) %>%
            pmap_dfr(~ {
              x %>%
                filter(end_date < ..3, time < ..1) %>%
                mutate(end_date = replace(end_date, end_date < ..4, ..4)) %>%
                distinct(personId, .keep_all = TRUE) %>%
                mutate(solve = ..2)
            }) %>%
            bind_rows(pb %>% select(personId, solve, end_date)) %>%
            group_by(solve, end_date) %>%
            tally() %>%
            arrange(end_date) %>%
            mutate(rank = cumsum(n)) %>%
            left_join(pb %>% select(solve, personId, pb = time), by = "solve")
          
          n_cubers <- isolate(filter_countries()) %>%
            rename(time = all_of(update_avg())) %>%
            filter(! time %in% NA) %>%
            arrange(end_date) %>%
            distinct(personId, .keep_all = TRUE) %>%
            mutate(end_date = replace(end_date, end_date < min(pb$end_date), min(pb$end_date))) %>%
            group_by(end_date) %>%
            tally() %>%
            arrange(end_date) %>%
            mutate(n_cubers = cumsum(n), personId = list(add)) %>%
            unnest(personId) %>%
            select(-n)
          
          memory$rankingData <- full_join(n_cubers, pb, by = c("personId", "end_date")) %>%
            group_by(personId) %>%
            arrange(end_date) %>%
            tidyr::fill(n_cubers, rank, pb) %>%
            ungroup() %>%
            filter(! rank %in% NA) %>%
            mutate(pct = rank / n_cubers * 100) %>%
            left_join(cubers, by = "personId")  %>%
            mutate(personLabel = paste0(personName, " (", personId, ")"),
                   r = match(personId, select_cuber())) %>%
            bind_rows(isolate(memory$rankingData))  
        }
      }
      memory$rankingLoaded <- select_cuber()
      
      return(memory$rankingData)
    })
    
    wca_ranking_old <- reactive({
      if(is.null(input$select_avg)) return(NULL)
      if(nrow(filter_countries()) == 0) return(NULL)
      if(length(select_cuber()) == 0) return(NULL)
      message("Calculating rankings Old")
      
      system.time({
        pb <- isolate(filter_countries()) %>%
          filter(personId %in% isolate(select_cuber()))  %>%
          rename(time = all_of(input$select_avg)) %>%
          filter(! time %in% NA) %>%
          group_by(personId) %>%
          arrange(end_date) %>%
          mutate(pb = time == cummin(time)) %>%
          ungroup() %>%
          filter(pb %in% TRUE) %>%
          group_by(personId) %>%
          arrange(desc(time)) %>%
          mutate(end_date1 = end_date,
                 end_date2 = lead(end_date, default = Sys.Date())) %>%
          ungroup() %>%
          filter(end_date1 != end_date2) %>%
          mutate(solve = row_number())
        
        if(nrow(pb) == 0) return(NULL)
        x <- isolate(filter_countries()) %>% select(personId, end_date, time = all_of(input$select_avg)) %>%
          filter(time < max(pb$time)) %>%
          arrange(end_date)
        pb<- pb %>%
          select(time, solve, end_date2, end_date1) %>%
          pmap_dfr(~ {
            x %>%
              filter(end_date < ..3, time < ..1) %>%
              mutate(end_date = replace(end_date, end_date < ..4, ..4)) %>%
              distinct(personId, .keep_all = TRUE) %>%
              mutate(solve = ..2)
          }) %>%
          bind_rows(pb %>% select(personId, solve, end_date)) %>%
          #left_join(pb %>% select(solve, end_date1), by = "solve") %>%
          #mutate(end_date = if_else(end_date < end_date1, end_date1, end_date)) %>%
          #group_by(solve, personId) %>%
          #summarise(end_date = min(end_date)) %>%
          
          #arrange(end_date) %>%
          #distinct(solve, personId, .keep_all = TRUE) %>%
          group_by(solve, end_date) %>%
          tally() %>%
          arrange(end_date) %>%
          mutate(rank = cumsum(n)) %>%
          left_join(pb %>% select(solve, personId, pb = time), by = "solve")
        
        n_cubers <- isolate(filter_countries()) %>%
          rename(time = all_of(input$select_avg)) %>%
          filter(! time %in% NA) %>%
          arrange(end_date) %>%
          distinct(personId, .keep_all = TRUE) %>%
          mutate(end_date = replace(end_date, end_date < min(pb$end_date), min(pb$end_date))) %>%
          group_by(end_date) %>%
          tally() %>%
          arrange(end_date) %>%
          mutate(n_cubers = cumsum(n), personId = list(isolate(select_cuber()))) %>%
          unnest(personId) %>%
          select(-n)
        
        x <- full_join(n_cubers, pb, by = c("personId", "end_date")) %>%
          group_by(personId) %>%
          arrange(end_date) %>%
          tidyr::fill(n_cubers, rank, pb) %>%
          ungroup() %>%
          mutate(pct = rank / n_cubers * 100) %>%
          left_join(cubers, by = "personId")  %>%
          mutate(personLabel = paste0(personName, " (", personId, ")"),
                 r = match(personId, select_cuber()))
      })%>%print
      return(x)
    })
    
    loadResults <- reactive({
      if(length(input$select_event) == 0) return(NULL)
      message("loading results")
      readRDS(paste0("data/results",input$select_event,".rds"))
    })
    
    
    wca_tourn <- reactive({
      if(nrow(filter_countries()) == 0) return(NULL)
      if(is.null(loadResults())) return(NULL)
      message("Calculating tournaments")
      
      if(length(select_cuber()) == 0) return(tibble())
      system.time({
        loaded <- isolate(memory$tournLoaded)
        case <- case_when(is.null(isolate(memory$tournData)) ~ "full",
                          length(intersect(loaded, select_cuber())) == 0 ~ "full",
                          TRUE ~ "partial")
        message("...", case, " update")
        if(case == "full") {
          add <- select_cuber()
          rem <- character(0)
          memory$tournData <- tibble()
        } else {
          add <- setdiff(select_cuber(), loaded)
          rem <- setdiff(loaded, select_cuber())
        }
        if(length(rem) > 0) {
          memory$tournData <- isolate(memory$tournData) %>%
            filter(! personId %in% rem)
        }
        if(length(add) > 0 ) {
          memory$tournData <- loadResults() %>%
            filter(personId %in% add) %>%
            mutate(eventId = isolate(input$select_event)) %>%
            left_join(round_types %>% select(roundTypeId = id, roundRank = rank, roundName = cellName), by = "roundTypeId") %>%
            left_join(rounds %>% select(competitionId, eventId, formatId, roundTypeId, startTime, endTime),
                      by = c("competitionId", "eventId", "roundTypeId")) %>%
            group_by(personId, competitionId) %>%
            mutate(final = ifelse(formatId %in% 1:5, best, average),final_round = roundRank == max(roundRank)) %>%
            ungroup() %>%
            select(personId, competitionId, time_average = average, time = starts_with("value"), final, 
                   roundName, formatId, startTime, endTime, final_round) %>%
            inner_join(filter_countries() %>% select(personId, competitionId, tourn_pos, best, average),
                       by = c("competitionId", "personId")) %>%
            left_join(competitions %>% select(competitionId, competitionName = name), by = "competitionId") %>%
            left_join(cubers %>% select(personId, personName, gender), by = "personId") %>%
            mutate(roundLabel = paste(roundName, competitionName),
                   r = match(personId, select_cuber())) %>%
            select(-roundName) %>%
            gather("key", "time", matches("time\\d"), best, time_average, average, final, tourn_pos)  %>%
            filter(final_round %in% TRUE | grepl("time", key)) %>%
            bind_rows(isolate(memory$tournData))
          # memory$tournData <- loadResults() %>%
          #   filter(personId %in% add) %>%
          #   select(personId, competitionId, time_average = average, time = starts_with("value"), roundTypeId) %>%
          #   inner_join(filter_countries() %>% select(personId, competitionId, tourn_pos, best, average, final),
          #              by = c("competitionId", "personId")) %>%
          #   mutate(eventId = isolate(input$select_event)) %>%
          #   left_join(round_types %>% select(roundTypeId = id, roundName = cellName), by = "roundTypeId") %>%
          #   left_join(competitions %>% select(competitionId, competitionName = name), by = "competitionId") %>%
          #   left_join(cubers %>% select(personId, personName), by = "personId") %>%
          #   left_join(rounds %>% select(competitionId, eventId, roundTypeId, startTime, endTime),
          #             by = c("competitionId", "eventId", "roundTypeId")) %>%
          #   mutate(roundLabel = paste(roundName, competitionName),
          #          r = match(personId, select_cuber())) %>%
          #   select(-roundTypeId, -roundName, -eventId) %>%
          #   gather("key", "time", matches("time\\d"), best, time_average, average, final, tourn_pos)  %>%
          #   bind_rows(isolate(memory$tournData))
        }
        memory$tournLoaded <- select_cuber()
        
      }) %>% print()
      
      return(memory$tournData)
    })
    
    wca_tourn_old <- reactive({
      if(nrow(filter_countries()) == 0) return(NULL)
      if(is.null(loadResults())) return(NULL)
      message("Calculating tournaments")
      
      system.time({
        graph_data <- loadResults() %>%
          filter(personId %in% select_cuber()) %>%
          select(personId, competitionId, time_average = average, time = starts_with("value"), roundTypeId) %>%
          inner_join(filter_countries() %>% select(personId, competitionId, eventId, tourn_pos, best, average, final),
                     by = c("competitionId", "personId")) %>%
          left_join(round_types %>% select(roundTypeId = id, roundName = cellName), by = "roundTypeId") %>%
          left_join(competitions %>% select(competitionId, competitionName = name), by = "competitionId") %>%
          left_join(cubers, by = "personId") %>%
          left_join(rounds %>% select(competitionId, eventId, roundTypeId, startTime, endTime),
                    by = c("competitionId", "eventId", "roundTypeId")) %>%
          mutate(roundLabel = paste(roundName, competitionName),
                 r = match(personId, select_cuber())) %>%
          select(-roundTypeId, -roundName)
      })%>%print()
      graph_data
    })
    
    wca_champ <- reactive({
      if(is.null(input$select_event)) return(NULL)
      if(is.null(input$select_year)) return(NULL)
      if(is.null(input$select_region)) return(NULL)
      message("Calculating championships")
      
      championship_results %>%
        filter(eventId %in% input$select_event,
               championship_type %in% input$select_region,
               year >= input$select_year[1], year <= input$select_year[2]) %>%
        left_join(cubers, by = "personId") %>%
        left_join(competitions %>% select(competitionId, competitionName = name), by = "competitionId") %>%
        mutate(personLabel = paste0(personName, " (", personId, ")"))
      
    })
    
    wca_record <- reactive({
      if(length(input$select_region)==0) return(NULL)
      if(length(input$select_record)==0) return(NULL)
      
      record_data <- records %>%
        filter(championship_type %in% input$select_region, 
               type %in% input$select_record) %>%
        left_join(cubers %>% select(personId, gender), by = "personId")
      if(nrow(record_data) == 0) return(NULL)
      
      return(record_data)
    })
    
    wca_progression <- reactive({
      if(length(input$select_region)==0) return(NULL)
      if(length(input$select_event)==0) return(NULL)
      
      record_data <- records %>%
        filter(championship_type %in% input$select_region, 
               eventId %in% input$select_event)
      if(nrow(record_data) == 0) return(NULL)
      
      record_data %>%
        gather("key", "dateRecord", recordStart, recordEnd) %>%
        filter(key == "recordStart" | dateRecord %in% NA) %>%
        mutate(key = replace(key, dateRecord %in% NA, "Current holder"),
               dateRecord = replace(dateRecord, dateRecord %in% NA, (metadata$max+1))) %>%
        group_by(type, dateRecord) %>%
        mutate(timeRecord = min(time)) %>%
        ungroup()
    })
    
    # wca_cubers <- reactive({
    #   if(is.null(input$select_num_by)) return(NULL)
    #   if(is.null(input$select_num_cum)) return(NULL)
    #   if(is.null(input$select_region)) return(NULL)
    #   
    #   if(input$select_num_cum %in% c("nTourn", "nEvent")) {
    #     graph_data <- no_cubers[[paste(input$select_num_by, "year", sep = "_")]] %>%
    #       filter(championship_type %in% input$select_region)
    #   } else {
    #     graph_data <- no_cubers[[paste(input$select_num_by, input$select_num_cum, sep = "_")]] %>%
    #       filter(championship_type %in% input$select_region)
    #   }
    #   graph_data %>%
    #     mutate(nCuber = as.numeric(nCuber), nTourn = as.numeric(nTourn), nEvent = as.numeric(nEvent))
    # })
    
    wca_cubers <- reactive({
        if(is.null(input$select_num_event)) return(NULL)
        if(is.null(input$select_num_type)) return(NULL)
        if(is.null(input$select_region)) return(NULL)
      
      if(input$select_num_event %in% TRUE) {
        filter_cubersY <- filter(no_cubers_year, championship_type == input$select_region, eventId == "all") %>%
          select(gender, type, end_date, n)
        filter_cubersC <- filter(no_cubers_cum, championship_type == input$select_region, eventId == "all") %>%
          select(gender, type, end_date, n)
      } else if(input$select_region == "world") {
        filter_cubersY <- filter(no_cubers_year, championship_type == input$select_region, eventId == input$select_event) %>%
          select(gender, type, end_date, n)
        filter_cubersC <- filter(no_cubers_cum, championship_type == input$select_region, eventId == input$select_event) %>%
          select(gender, type, end_date, n)
      } else {
        filter_cubersY <- filter(no_cubers_year, championship_type == input$select_region, eventId == input$select_event) %>%
          select(gender, type, end_date, n)
        
        countries <- regions %>% filter(championship_type %in% input$select_region) %>% select(personCountryId = countryId)
        filter_cubersC <- filter_events() %>%
          inner_join(countries, by = "personCountryId") %>%
          select(personId, competitionId, end_date) %>%
          arrange(end_date) %>%
          distinct(personId,.keep_all = TRUE) %>%
          left_join(cubers %>% select(personId, gender), by = "personId") %>%
          group_by(gender, end_date) %>%
          summarise(n = n()) %>%
          arrange(end_date) %>%
          mutate(n = cumsum(n)) %>%
          ungroup() %>%
          mutate(type = "cum")
      }
      
      filter_cubersC <- filter_cubersC %>%
        complete(type, gender, end_date) %>%
        group_by(type, gender) %>%
        arrange(end_date) %>%
        tidyr::fill(n) %>%
        group_by(type, end_date) %>%
        summarise(n = sum(n, na.rm = TRUE)) %>%
        mutate(gender = "t") %>%
        bind_rows(filter_cubersC)
      filter_cubersY <- filter_cubersY %>%
        complete(type, gender, end_date, fill = list(n = 0)) %>%
        group_by(type, end_date) %>%
        summarise(n = sum(n, na.rm = TRUE)) %>%
        mutate(gender = "t") %>%
        bind_rows(filter_cubersY)
      bind_rows(filter_cubersC, filter_cubersY)
    })
    
    wca_events <- reactive({
      if(is.null(input$select_region)) return(NULL)
      
      filter_cubers <- no_cubers_year %>%
        filter(championship_type == input$select_region) %>%
        select(-championship_type)
      filter_cubers %>%
        group_by(eventId, type, end_date) %>%
        summarise(n = sum(n)) %>%
        mutate(gender = "t") %>%
        bind_rows(filter_cubers)
    })
    
    
    ####Modules####
    
    app_options <- reactive({
      region_name <- regions$region_name[match(input$select_region, regions$championship_type)]
      if(length(region_name) == 0) return(NULL)
      region_name = ifelse(region_name == "World" | grepl("United", region_name) | grepl("Republic", region_name), 
                           paste("the", region_name),
                           region_name)
      
      list(event=input$select_event, 
           year = input$select_year, 
           region=input$select_region,
           gender = input$select_gender,
           record_type = input$select_record,
           event_name = events$name[match(input$select_event, events$id)],
           region_name = region_name,
           region_from = ifelse(input$select_region %in% "world", "", paste("from", region_name))
      )
    })
    
    callModule(server_summ, "summ", graph_quantiles, highlight_quantiles, results_quantiles, reactive(replace(input$select_avg, input$select_avg == "best", "single")), app_options, reactive(input$tab))
    #callModule(server_summ, "summ", graph_quantiles, highlight_quantiles, reactive(input$select_event), reactive(input$tab))
    callModule(server_rank, "rank", wca_ranking, reactive(replace(input$select_avg, input$select_avg == "best", "single")), app_options, reactive(input$tab))
    callModule(server_tourn, "tourn", wca_tourn, app_options, reactive(input$tab))
    select_cuber <- callModule(server_champ, "champ", wca_champ, select_cuber, app_options, reactive(input$tab))
    select_cuber <- callModule(server_record, "record", wca_record, select_cuber, app_options, reactive(input$tab))
    callModule(server_progression, "progression", wca_progression, app_options, reactive(input$tab))
    callModule(server_home, "home", wca_cubers, app_options, reactive(input$tab))
    callModule(server_ncuber, "ncuber", wca_cubers, reactive(input$select_num_event),reactive(input$select_num_type), app_options, reactive(input$tab))
    callModule(server_nevent, "nevent", wca_events, reactive(input$select_nevent), app_options, reactive(input$tab))
    callModule(server_event_vs, "eventvs", personal_bests, reactive(input$select_avg), reactive(input$select_event2), app_options, reactive(input$tab))
      
  }
  
  shinyApp(ui, server)