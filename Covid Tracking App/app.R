#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

library(shiny)
library(plotly)
library(tidyverse)
library(data.table)
library(lubridate)
library(shinyWidgets)
library(htmltools)
library(leaflet)
library(rgdal)

##################### Data ####################
##### Load Datasets #####
country_dim <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv", header = TRUE) %>%
  mutate(population = ifelse(Province_State %like% "Princess", 3770, 
                             ifelse(is.na(Population), 0, Population)))
country_dim$Province_State[country_dim$Province_State == "Virgin Islands"] <- "United States Virgin Islands"
country_dim$Province_State[country_dim$Province_State == "Northern Mariana Islands"] <- "Commonwealth of the Northern Mariana Islands"


# change name to join to the state shape file
country_dim$Province_State[country_dim$Province_State == "Virgin Islands"] <- "United States Virgin Islands"
country_dim$Province_State[country_dim$Province_State == "Northern Mariana Islands"] <- "Commonwealth of the Northern Mariana Islands"

covid_tm_US_dat <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv", header = TRUE)
covid_death_US_dat <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv", header = TRUE)

# Shape files
# US states shape files
state_shp <- readOGR(dsn= paste0(getwd(),"/tl_2019_us_state/"),  layer="tl_2019_us_state", verbose=FALSE)
state_shp$Lat <- as.numeric(state_shp$INTPTLAT)
state_shp$Long <- as.numeric(state_shp$INTPTLON)

# US county shape files
# county_shp <- readOGR(dsn= paste0(getwd(),"/tl_2019_us_county/"),  layer="tl_2019_us_county", verbose=FALSE)

### Confirmed Cases ###
covid_tm_US_dat_long <- pivot_longer(covid_tm_US_dat,
                                     cols = -c("UID", "iso2", "iso3", "code3", "FIPS", "Admin2", "Province_State", "Country_Region", "Lat", "Long_", "Combined_Key"),  
                                     names_to = "Day")  %>% 
  mutate(date = mdy(str_replace(Day, "X",""))) %>%
  left_join(country_dim[,c("UID", "population")], by="UID")
colnames(covid_tm_US_dat_long)[13] <- c("Confirmed")

### Daily Confirmed ###
idx <- match("X1.22.20", names(covid_tm_US_dat))
covid_tm_US_daily_diff <- covid_tm_US_dat[, "UID"]

for (i in idx:(length(covid_tm_US_dat)-1)) 
{
  covid_tm_US_daily_diff <- cbind(covid_tm_US_daily_diff, covid_tm_US_dat[i+1] - covid_tm_US_dat[i])
  
}

colnames(covid_tm_US_daily_diff)[1] <- "UID"

covid_tm_US_diff_long <- pivot_longer(covid_tm_US_daily_diff,
                                      cols = -c("UID"),  
                                      names_to = "Day")  %>% 
  mutate(date = mdy(str_replace(Day, "X","")))
colnames(covid_tm_US_diff_long)[3] <- c("Daily Confirmed")


covid_US_long <- covid_tm_US_dat_long %>% left_join(covid_tm_US_diff_long[, c("UID", "date", "Daily Confirmed")], by=c("date", "UID")) %>% 
  mutate(Daily_Confirmed = ifelse(is.na(`Daily Confirmed`), Confirmed, `Daily Confirmed`))


### Deaths ###
covid_death_US_dat_long <- pivot_longer(covid_death_US_dat[c("UID", grep("X", names(covid_death_US_dat), value = TRUE))],
                                        cols = -c("UID") ,  
                                        names_to = "Day")  %>% 
  mutate(date = mdy(str_replace(Day, "X","")))
colnames(covid_death_US_dat_long)[3] <- c("Death")


### Daily Death ###
idx <- match("X1.22.20", names(covid_death_US_dat))
covid_death_US_daily_diff <- covid_death_US_dat[, "UID"]

for (i in idx:(length(covid_death_US_dat)-1)) 
{
  covid_death_US_daily_diff <- cbind(covid_death_US_daily_diff, covid_death_US_dat[i+1] - covid_death_US_dat[i])
  
}
colnames(covid_death_US_daily_diff)[1] <- "UID"

covid_death_US_daily_diff_long <- pivot_longer(covid_death_US_daily_diff,
                                               cols = -c("UID"),  
                                               names_to = "Day") %>% 
  mutate(date = mdy(str_replace(Day, "X","")))
colnames(covid_death_US_daily_diff_long)[3] <- c("Daily Death")


covid_US_death_long <- covid_death_US_dat_long %>% 
  left_join(covid_death_US_daily_diff_long[, c("UID", "date", "Daily Death")], by=c("date", "UID")) %>% 
  mutate(Daily_Death = ifelse(is.na(`Daily Death`), Death, `Daily Death`))


# create one final dataset
covid_US_dat_long <- covid_US_long %>% 
  subset(select=-`Daily Confirmed`) %>% 
  left_join(covid_US_death_long[, c("UID", "date", "Daily_Death", "Death")], by=c("date", "UID")) %>% 
  subset(select=-c(iso2, iso3, code3, Day))
covid_US_dat_long $GEOID <- str_pad(covid_US_dat_long $FIPS, width=5, side="left", pad="0")

### state level summary, use only for mapping app
covid_US_dat_long_state <- covid_US_dat_long %>% 
  group_by(date, Province_State) %>% 
  summarise(Total_Confirmed = sum(Confirmed), 
            Total_Death = sum(Death),
            `Daily Confirmed` = sum(Daily_Confirmed),
            `Daily Death` = sum(Daily_Death), 
            prevalence_Rate = round((sum(Confirmed)/sum(population))* 10000, digits = 2),
            pop_death_rate = round((sum(Death)/sum(population))* 10000, digits = 2),
            death_Rate = round(sum(Death)/sum(Confirmed), digits = 2)
  ) %>%
  ungroup() %>%
  mutate(Death_Rate = ifelse(death_Rate<=1 & Total_Confirmed >=100, death_Rate, 0),
         Prevalence_Rate = ifelse(is.finite(prevalence_Rate), prevalence_Rate, 0))

## create base map

base_data <- merge(state_shp, covid_US_dat_long_state[covid_US_dat_long_state$date == (Sys.Date()-1), ], 
                   by.x = "NAME", by.y = "Province_State")

pal <- colorBin("YlOrRd", domain = base_data$Prevalence_Rate, bins = c(0, 20, 50, 100, 200, Inf))

base_map <- leaflet(base_data) %>% setView(-96, 37.8, 4) %>%
  addTiles() %>% 
  addLayersControl(
    position = "bottomright",
    overlayGroups = c("New Cases", "Death"),
    options = layersControlOptions(collapsed = FALSE)) %>%  
  hideGroup(c("New Cases", "Death"))  %>%
  addProviderTiles(providers$CartoDB.Positron)  %>%
  addLegend("bottomright", pal = pal, values = ~Prevalence_Rate,
            title = "<small>Confirmed Cases per 10,000</small>")


##################### UI ####################
ui <- fluidPage(
  
  tags$title("Covid-19 Tracking Plots"),
  
    wellPanel(
      fluidRow( sliderInput(inputId = "date", 
                              label = "Choose a date", 
                              value = max(covid_US_dat_long$date), 
                              min = min(covid_US_dat_long$date), max = max(covid_US_dat_long$date))
               )),
  tabsetPanel( 
   tabPanel("COVID-19 mapper", 
             leafletOutput("map_plot")
      ),
  
   tabPanel("COVID-19 tracker", 
       # select states and counties
     fluidRow(column(3, pickerInput(inputId="state",
                                    label="Choose State(s)",
                                    choices=unique(covid_US_dat_long$Province_State), 
                                    selected = unique(covid_US_dat_long$Province_State),
                                    options = list(`actions-box` = TRUE), multiple = T)),
     
             column(3, pickerInput(inputId="county",
                           label="Choose County",
                           choices=NULL, 
                           options = list(`actions-box` = TRUE), multiple = T)),
             
          # select top n state/ county
             column(1, numericInput(inputId = "n", label = "Top ", 
                                    value = 5, min = 1, max = 10, step = 1,
                                    width = NULL)),
          
             column(3, pickerInput(inputId="unit",
                                   label = "Ranking",
                                choices=c("Province_State", "Combined_Key"), 
                                selected = "Combined_Key",
                                options = list(`actions-box` = TRUE), multiple = F)),
             
             column(2, pickerInput(inputId="measure",
                                label="by",
                                choices=c("Total Confirmed", "Total Death", "Prevalence Rate", "Death Rate", "Fatality Rate"), 
                                selected = "Total Confirmed",
                                options = list(`actions-box` = TRUE), multiple = F))
             ),
     
     fluidRow( column(3, tags$text("*Combined_Key: State & County"), offset = 7) ),
     
     tags$h3("Confirmed Cases Tracking"),
     
     fluidRow(
       radioButtons(inputId = "conf_metric", label = "Display: ",
                    choices = c("Count", "Rate"),
                    selected = "Count", inline = TRUE)
     ),
     
     fluidRow(
       column(6, plotlyOutput("cum_conf")) ,
       column(6, plotlyOutput("daily_conf"))
     ),
     
     tags$hr(),
     
     tags$h3("Death Cases Tracking"),
     
     fluidRow(
       radioButtons(inputId = "death_metric", label = "Display: ",
                    choices = c("Count", "Rate", "Death Incident Rate"),
                    selected = "Count", inline = TRUE)
     ),
     
     fluidRow(
       column(6, plotlyOutput("cum_death")) ,
       column(6, plotlyOutput("daily_death"))
      )
  )
)
)



##################### Server ####################
server <- function(input, output, session) {
  # reactive dropdown menu
   state <- reactive({
     filter(covid_US_dat_long, Province_State %in% input$state)
   })
   observeEvent(state(), {
     choices <- unique(state()$Admin2)
     updatePickerInput(session=session, inputId="county", choices=choices, 
                       selected = choices)
   })
   
   ##### Data for output #####
   #### data for confirmed ####
   # summary subset confirmed dataset   
   
    track_data <- reactive({covid_US_dat_long[covid_US_dat_long$date <= input$date & 
                          covid_US_dat_long$Province_State %in% input$state &
                          covid_US_dat_long$Admin2 %in% input$county, ]%>% 
            group_by(date) %>% 
            summarise(`Total Confirmed` = sum(Confirmed), 
                      `Total Death` = sum(Death),
                      `Daily Confirmed` = sum(Daily_Confirmed),
                      `Daily Death` = sum(Daily_Death), 
                      prevalence_Rate = round((sum(Confirmed)/sum(population))* 10000, digits = 2),
                      pop_death_rate = round((sum(Death)/sum(population))* 10000, digits = 2),
                      fat_rate = round(sum(Death)/sum(Confirmed), digits = 2)
                      ) %>%
        ungroup() %>%
        mutate(`Fatality Rate` = ifelse(fat_rate<=1 & `Total Confirmed` >=100, fat_rate, 0),
               `Prevalence Rate` = ifelse(is.finite(prevalence_Rate), prevalence_Rate, 0),
               `Death Rate` = ifelse(is.finite(pop_death_rate), pop_death_rate, 0))
        })
   

    # rank by total confirmed 
   rank_data <- reactive({
     covid_US_dat_long[covid_US_dat_long$date <= input$date & 
                         covid_US_dat_long$Province_State %in% input$state &
                         covid_US_dat_long$Admin2 %in% input$county, ] %>%
       group_by(!!rlang::sym(input$unit), date) %>% 
       summarise(`Total Confirmed` = sum(Confirmed), 
                 `Total Death` = sum(Death),
                 `Daily Confirmed` = sum(Daily_Confirmed),
                 `Daily Death` = sum(Daily_Death), 
                 prevalence_Rate = round((sum(Confirmed)/sum(population))* 10000, digits = 2),
                 pop_death_rate = round((sum(Death)/sum(population))* 10000, digits = 2),
                 fat_rate = round(sum(Death)/sum(Confirmed), digits = 2)
       ) %>%
       ungroup() %>%
       mutate(`Fatality Rate` = ifelse(fat_rate<=1 & `Total Confirmed` >=100, fat_rate, 0),
              `Prevalence Rate` = ifelse(is.finite(prevalence_Rate), prevalence_Rate, 0),
              `Death Rate` = ifelse(is.finite(pop_death_rate), pop_death_rate, 0))
   })
   

    top_county <- reactive({
      arrange(rank_data()[rank_data()$date == input$date, ], desc(get(input$measure))) [1:min(input$n, nrow(unique(rank_data()[, input$unit]))), input$unit] %>% 
        unlist()
    })
    
    trace_data <- reactive({ 
      rank_data() %>% filter(date <= input$date & get(input$unit) %in% top_county() )
    })
    

    
    ## State level map dataset
    map_data <- reactive({
      merge(state_shp, covid_US_dat_long_state[covid_US_dat_long_state$date == input$date, ], by.x = "NAME", by.y = "Province_State")
    })
    
    ##### Output Plots #####
    # cumulative confirmed
    output$cum_conf <- renderPlotly({
      if (input$conf_metric=="Count")
      {
        cum_conf <- plot_ly(track_data(), x= ~date, y= ~`Total Confirmed`,
                type="scatter", mode = 'lines', name = 'Overall') %>%
          add_trace(data=trace_data(), 
                    x=~date, y=~`Total Confirmed`, 
                    mode = 'lines', color = ~get(input$unit), name=~get(input$unit)) %>%
          layout(title = 'Cumulative Confirmed Cases',
                 xaxis = list(title = 'Date'),
                 yaxis = list(title = 'Count'))
      }
      
      if (input$conf_metric=="Rate")
      {
        cum_conf <- plot_ly(track_data(), x= ~date, y= ~`Prevalence Rate`,
                type="scatter", mode = 'lines', name = 'Overall') %>%
          add_trace(data=trace_data(), 
                    x=~date, y=~`Prevalence Rate`,
                    mode = 'lines', color = ~get(input$unit), name=~get(input$unit)) %>%
          layout(title = 'Prevalence Rate',
                 xaxis = list(title = 'Date'),
                 yaxis = list(title = 'Prevalence Rate (per 10,000 persons)'))
      }
      cum_conf
    })
    
    # cumulative death
    output$cum_death <- renderPlotly({
      if (input$death_metric=="Count")
      {
        cum_death <- plot_ly(track_data(), x= ~date, y= ~`Total Death`,
                             type="scatter", mode = 'lines', name = 'Overall') %>%
          add_trace(data=trace_data(), 
                    x=~date, y=~`Total Death`,
                    mode = 'lines', color = ~get(input$unit), name=~get(input$unit)) %>%
          layout(title = 'Cumulative Death Cases',
                 xaxis = list(title = 'Date'),
                 yaxis = list(title = 'Count'))
      }
      
      if (input$death_metric=="Rate")
      {
        cum_death <- plot_ly(track_data(), x= ~date, y= ~`Death Rate`,
                             type="scatter", mode = 'lines', name = 'Overall') %>%
          add_trace(data=trace_data(), 
                    x=~date, y=~`Death Rate`,
                    mode = 'lines', color = ~get(input$unit), name=~get(input$unit)) %>%
          layout(title = 'Death Rate',
                 xaxis = list(title = 'Date'),
                 yaxis = list(title = 'Death per 10,000 persons'))
      }
      
      if (input$death_metric=="Death Incident Rate")
      {
        cum_death <- plot_ly(track_data(), x= ~date, y= ~`Fatality Rate`,
                             type="scatter", mode = 'lines', name = 'Overall') %>%
          add_trace(data=trace_data(), 
                    x=~date, y=~`Fatality Rate`,
                    mode = 'lines', color = ~get(input$unit), name=~get(input$unit)) %>%
          layout(title = 'Death Rate',
                 xaxis = list(title = 'Date'),
                 yaxis = list(title = 'Death/Confirmed'))
      }
      cum_death
    })
    
    
    # daily confirmed
    output$daily_conf <- renderPlotly({
      plot_ly(data=track_data(), x=~date, y=~`Daily Confirmed`, type = "bar", name = "Overall") %>%
        layout(title = 'Daily Confirmed Cases',
               xaxis = list(title = 'Date'),
               yaxis = list(title = 'Count'))
    })
    
    # daily death
    output$daily_death <- renderPlotly({
      plot_ly(data=track_data(), x=~date, y=~`Daily Death`, type = "bar", name = "Overall") %>%
        layout(title = 'Daily Death Cases',
               xaxis = list(title = 'Date'),
               yaxis = list(title = 'Count'))
    })
    
    
    
    # map plot
    output$map_plot <- renderLeaflet({ 
      base_map
    })
    
    observeEvent(input$date, {
      leafletProxy("map_plot") %>% 
        clearMarkers() %>%
        clearShapes() %>%
        addPolygons(data = map_data(),
                    stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.4,
                    fillColor = ~pal(map_data()$Prevalence_Rate )) %>% 
        
        addCircleMarkers(data = map_data(), lat = ~ Lat, lng = ~ Long, weight = 1, radius = ~(`Daily Confirmed`)^(1/4), 
                         fillOpacity = 0.1, color = "#cc4c02", 
                         group = "New Cases",
                         label = sprintf("<strong>%s (Daily)</strong><br/>Confirmed cases: %g<br/>Deaths: %d", map_data()$NAME, map_data()$`Daily Confirmed`, map_data()$`Daily Death`) %>% lapply(htmltools::HTML), 
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto")) %>%
        
        addCircleMarkers(data = map_data(), lat = ~ Lat, lng = ~ Long, weight = 1,
                         radius = ~(Total_Death)^(1/4), 
                         fillOpacity = 0.1, color = "#cc4c02", 
                         group = "Death",
                         label = sprintf("<strong>%s (cumulative)</strong><br/>Confirmed cases: %g<br/>Deaths: %d<br/>Cases per 10,000: %g<br/>Deaths per 10,000: %g<br/>Death per Confirmed Cases: %g", map_data()$NAME, map_data()$Total_Confirmed, map_data()$Total_Death, map_data()$Prevalence_Rate, map_data()$pop_death_rate, map_data()$Death_Rate) %>% lapply(htmltools::HTML), 
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto"))
    })
    
}

shinyApp(ui = ui, server = server)
