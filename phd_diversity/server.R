library(shiny)
library(ggplot2)
library(readr)

function(input, output) {

    # wrapping function from:
    # https://stackoverflow.com/questions/2631780/r-ggplot2-can-i-set-the-plot-title-to-wrap-around-and-shrink-the-text-to-fit-t
    wrapper <- function(x, ...) {
        paste(strwrap(x, ...), collapse = "\n")
    }

    tab <- read_csv("data/aggregated_data.csv")
    tab$wrapped_name = sapply(tab$INSTNM, function(x) wrapper(x, 20))
    dataset <- reactive({
        tab = tab[tab$CIPTitle %in% input$fields & tab$INSTNM %in% input$schools, ]
        if(input$percent) {
            tab$value = tab$Pct
        } else {
            tab$value = tab$Total
        }
        tab
    })

    plotHeight <- reactive({
        dat = dataset()
        rows = ceiling(length(unique(dat$CIPTitle)) / 2)
        schools = length(unique(dat$INSTNM))
        return(100 * (1 + rows * schools))
    })

    observe(output$plot <- renderPlot({
        dat = dataset()
        value = ifelse(input$percent, "Percent", "Total")
        scale = ifelse(input$percent, scales::percent, scales::comma)

        if(nrow(dat) == 0) {
            p <- ggplot(dat, aes(x=wrapped_name, y=value)) +
                geom_blank() +
                annotation_custom(grid::textGrob("NO DATA", gp=grid::gpar(fontsize=48)),
                      xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
                scale_x_discrete("School") +
                scale_y_continuous(glue::glue("{value} identifying as 'Black' in IPEDS")) +
                coord_flip() +
                theme_minimal() +
                theme(
                    panel.border=element_rect(color='black', size=0.5, fill=NA),
                    axis.title.x=element_text(size=20),
                    axis.title.y=element_text(size=20),
                    axis.text.x=element_text(size=20),
                    axis.text.y=element_text(size=20),
                    strip.text.x=element_text(size=20)
                )
        } else {
            p <- ggplot(dataset(), aes(x=wrapped_name, y=value)) +
                geom_point(size=2) +
                scale_x_discrete("School") +
                scale_y_continuous(glue::glue("{value} identifying as 'Black' in IPEDS"), labels=scale, limits=c(0, NA)) +
                facet_wrap(~CIPTitle, ncol=2) +
                coord_flip() +
                theme_minimal() +
                theme(
                    panel.border=element_rect(color='black', size=0.5, fill=NA),
                    axis.title.x=element_text(size=20),
                    axis.title.y=element_text(size=20),
                    axis.text.x=element_text(size=20),
                    axis.text.y=element_text(size=20),
                    strip.text.x=element_text(size=20)
                )
        }
        print(p)

    }, height=plotHeight()))

}
