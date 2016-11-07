#INSTALL SHINY IF REQUIRED

#install.packages('shiny')
library(shiny)

shinyUI(
  pageWithSidebar(
    
    headerPanel('US Election Trends'),
    
    sidebarPanel(
      selectInput('CandidateName', 'Select a Candidate',
                  choices = c('Select a candidate','Donald Trump', 'Hillary Clinton', 'Bernie Sanders','Ted Cruz')
                      ),
      radioButtons('timeSelect', 'Timeline',choices = c('Weekly Trend','Now')
      )
    ),
    
    mainPanel(
      plotOutput('myPlot')
    )
    
  )
)