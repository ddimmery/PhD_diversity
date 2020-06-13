library(shiny)
library(ggplot2)
library(readr)

function(input, output) {

    tab <- read_csv("data/aggregated_data.csv")
    dataset <- reactive({
        tab = tab[tab$CIPTitle %in% input$fields & tab$INSTNM %in% input$schools, ]
        if(input$percent) {
            tab$value = tab$Pct
        } else {
            tab$value = tab$Total
        }
        tab
    })

    output$plot <- renderPlot({

        value = ifelse(input$percent, "Percent", "Total")
        scale = ifelse(input$percent, scales::percent, scales::comma)

        print(dataset())

        p <- ggplot(dataset(), aes(x=INSTNM, y=value)) +
            geom_point() +
            scale_x_discrete("School") +
            scale_y_continuous(glue::glue("{value} identifying as 'Black' in IPEDS"), labels=scale, limits=c(0, NA)) +
            facet_wrap(~CIPTitle, scales='free_y') +
            coord_flip() +
            theme_minimal() +
            theme()

        print(p)

    }, height=700)

}
