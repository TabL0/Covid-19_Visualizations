

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
  
  sidebarLayout(
    sidebarPanel(
      h3("Data Control Inputs"),
      radioButtons(inputId = "dates_displayed", label = "Dates Shown", 
                   choices = list(
                     "One Day" = "1 day",
                     "One Week" = "1 week",
                     "Two Weeks" = "2 week",
                     "One Month" = "1 month",
                     "Two Months" = "2 month"),
                   selected = "2 month"),
      helpText("Sets the tick marks on the X-Axis. Increase number if illegible."),
      
      radioButtons(inputId = "mean_number", label = "Rolling Means For Decreasing Noise", 
                   choices = list(
                     "1 Day Mean" = 1,
                     "3 Day Mean" = 3,
                     "7 Day Mean" = 7,
                     "15 Day Mean" = 15),
                   selected = 7),
      helpText("Changing the rolling mean will decrease overall noise in the data, which may be caused to factors unrelated to the spread of Covid-19.
               For example: a large spike on one day caused by a large number of people being tested that day."),
      helpText("Caution: Higher averages may also erase relevant patterns in data."),
      
      
      dateRangeInput("dates",
                     "Date Range",
                     start = "2020-01-22",
                     end = Sys.Date()),
      helpText("Sets the range of dates visualized in the graph.")
      
    ),
    
    mainPanel(
      h3("New Cases of Covid-19 in The United States"),
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
      geom_area(aes(x = days, y = cleaned_mean), fill = "Black", alpha = .5) +
      coord_x_date(c(input$dates[1], input$dates[2])) +
      scale_x_date(date_breaks = input$dates_displayed) +
      ylab("Cases of Covid-19") +
      xlab("Dates")
    
  })
}


#Run The App
shinyApp(ui, server)
