

library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyquant)
library(shiny)
library(zoo)
library(tidyr)
library(stringr)

covid_cases <- read.csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")

#National Data, but with cleaned dates
total_cases <- covid_cases %>%
  select(-(1:4)) %>% #General Cleaning
  gather(key = "days", value = "cases") %>% #Sets it to a format that is easy to graph
  group_by(days) %>% #This and the next line just sum the cases
  summarise(totals = sum(cases)) 

#Converts that char text into dates. 
total_cases$days <- str_remove_all(total_cases$days, "[X.]") 
total_cases$days <- ymd(total_cases$days)


###User Interface###
ui <- fluidPage(
  titlePanel("Covid Data"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId = "dates_displayed", label = "Dates Shown", 
                   choices = list(
                     "One Day" = "1 day",
                     "One Week" = "1 week",
                     "Two Weeks" = "2 week",
                     "One Month" = "1 month",
                     "Two Months" = "2 month"),
                   selected = "2 month"),
      
      radioButtons(inputId = "mean_number", label = "Cleaned Means", 
                   choices = list(
                     "1 Day Mean" = 1,
                     "3 Day Mean" = 3,
                     "7 Day Mean" = 7,
                     "15 Day Mean" = 15),
                   selected = 7),
      
      
      dateRangeInput("dates",
                     "Date range",
                     start = "2020-01-22",
                     end = Sys.Date())
      
    ),
    
    mainPanel(
      h1("Cases of Covid-19 in The United States"),
      plotOutput('cases_plot')
    )
  )
)


###Server###
server <- function(input, output){
  
  output$selected_var <- renderText({ 
    as.numeric(input$mean_number)
  })
  
  output$plz_god.work <- renderTable({
    total_cases %>%
      mutate(new_cases = totals - lag(totals)) %>%
      mutate(cleaned_mean = rollmean(new_cases, k = as.numeric(input$mean_number), fill = NA))
  })
  
  output$cases_plot <- renderPlot({
    
    total_cases %>%
      mutate(new_cases = totals - lag(totals)) %>%
      mutate(cleaned_mean = rollmean(new_cases, k = as.numeric(input$mean_number), fill = NA)) %>%
      ggplot() +
      geom_area(aes(x = days, y = cleaned_mean), fill = "Blue", alpha = .5) +
      coord_x_date(c(input$dates[1], input$dates[2])) +
      scale_x_date(date_breaks = input$dates_displayed) +
      ylab("Cases of Covid-19") +
      xlab("Dates")
    
  })
}


#Run The App
shinyApp(ui, server)
