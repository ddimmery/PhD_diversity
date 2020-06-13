library(shiny)
library(ggplot2)
library(readr)


data <- read_csv("data/aggregated_data.csv")



fluidPage(

  titlePanel("IPEDS PhD Completion Diversity Explorer, 2011 to 2018"),

  sidebarPanel(
      selectInput('schools', 'Schools', unique(data$INSTNM), multiple=TRUE, selected="Overall"),

      selectInput('fields', 'Field', unique(data$CIPTitle), multiple=TRUE, selected="Overall"),

      checkboxInput('percent', 'Percent', value=TRUE),

      p('Black counts exclude non-resident aliens (no race/ethnic info), but the denominator used in the percent includes them.'),
      p(HTML('Processing code forked from <a href="https://twitter.com/clara_bmc/status/1271556245267214336">Clara Bicalho</a>')),
      p(HTML('Data pulled from IPEDS: <a href="https://nces.ed.gov/ipeds/use-the-data">https://nces.ed.gov/ipeds/use-the-data</a>')),
      p(HTML('Pull requests welcome at <a href="https://github.com/ddimmery/PhD_diversity/pulls">the GitHub</a>!'))
    ),
  mainPanel(
      plotOutput('plot')
  ),
)
