
# changes description:
#   data recording: add gender
# data table: last data is added in the first row
# only few rows are displayed
#
# plot: no colors, facet by gender
# last data added is the red point
# make the plot interactive with plotly

#==================================
require(shiny)
require(ggplot2)
require(dplyr)
require(plotly)

# Define UI for application that draws a scatterplot
shinyUI <- fluidPage(
  # Application title
  titlePanel("Can we guess your shoe size?"),
  sidebarLayout(
    sidebarPanel(width = 4,
                 numericInput("height",
                              label = h3("Height (in)"),
                              value = 1),
                 numericInput("shoe",
                              label = h3("Shoe Size (in)"),
                              value = 1),
                 selectInput("grade", label = h3("Grade"),
                             choices = list("Kindergarten",
                                            "1st",
                                            "2nd",
                                            "3rd",
                                            "4th",
                                            "5th",
                                            "6th",
                                            "Adult"),
                             selected = 'Kindergarten'),
                 selectInput("gender",
                             label = h3("Gender"),
                             choices = c('Boy',
                                         'Girl',
                                         'Other')),
                 actionButton("goButton",
                              "Plot!")
    ),

    # Show a plot of the generated distribution
    mainPanel(plotlyOutput("distPlot",
                           height='500px',
                           width = '1000px'),
              tableOutput("distTable"))
  )
)


# Define server logic required to draw a scatterplot
shinyServer <- function(input, output){
  require(ggplot2)
  require(dplyr)

  # Set up NULL device
  csv.dat <- NULL

  # try: 7.75,"Girl","1st",46.5

  # Only act when plot button is pressed
  observeEvent(input$goButton, {
    csv.dat <- read.csv("height_shoesize.csv", header=TRUE)
    csv.dat <- rbind(c(input$shoe, input$gender, input$grade, input$height), csv.dat)
    write.csv(x = csv.dat, file="height_shoesize.csv", row.names = FALSE)

    plot.dat <- csv.dat %>%
      mutate(Grade = factor(Grade,
                            levels = c("Kindergarten",
                                       "1st",
                                       # "Other"
                                       "2nd",
                                       "3rd",
                                       "4th",
                                       "5th",
                                       "6th",
                                       "Adult"
                            )),
             Gender = as.factor(Gender),
             ShoeSize = as.numeric(ShoeSize),
             Height = as.numeric(Height),
             inches = paste0(floor(Height/12), "'", Height %% 12)) %>%
      filter(Height > 1 & ShoeSize > 1)

    # Creates the output table
    output$distTable <- renderTable({
      csv.dat[1:1,]
    })

    boyPrediction <- plot.dat %>%
      filter(Gender == "Boy") %>%
      summarise(Gender = "Boy",
                label = lm(data = .,
                           formula = ShoeSize ~ Height) %>%
                  predict(newdata = plot.dat[1,]) %>%
                  as.numeric() %>%
                  round(1) %>%
                  paste0(.," ft"),
                x = plot.dat[1,"Height"],
                y = lm(data = .,
                       formula = ShoeSize ~ Height) %>%
                  predict(newdata = plot.dat[1,]) %>%
                  as.numeric())

    girlPrediction <- plot.dat %>%
      filter(Gender == "Girl") %>%
      summarise(Gender = "Girl",
                label = lm(data = .,
                           formula = ShoeSize ~ Height) %>%
                  predict(newdata = plot.dat[1,]) %>%
                  as.numeric() %>%
                  round(1) %>%
                  paste0(),
                x = plot.dat[1,"Height"],
                y = lm(data = .,
                       formula = ShoeSize ~ Height) %>%
                  predict(newdata = plot.dat[1,]) %>%
                  as.numeric())

    predictionLabels <- bind_rows(boyPrediction,
                                  girlPrediction)

    # Create the plotly plot
    output$distPlot <- renderPlotly({

      # draw the histogram with the specified number of bins
      plt <- ggplot(data=plot.dat,
                    aes(x = Height,
                        y = ShoeSize)) +
        geom_smooth(method = "lm",
                    se = FALSE) +
        geom_point(aes(colour = Grade),
                   alpha = .7) +
        geom_point(data = plot.dat[1,],
                   size = 3,
                   colour = "red") +
        facet_grid(~Gender) +
        labs(x = "Height",
             y = "Shoe Size") +
        theme_bw() +
        geom_text(data = predictionLabels,
                  aes(x = x,
                      y = y,
                      label = label),
                  size = 5,
                  colour = "darkgreen",
                  nudge_x = -3) +
        geom_point(data = predictionLabels,
                   aes(x = x,
                       y = y),
                   size = 2,
                   colour = "darkgreen")

      plotly::ggplotly(plt)
      # theme(text = element_text(size = 16),
      #       legend.position = "top")
      # theme(aspect.ratio = 1,
      #       axis.title.y = element_text(size = rel(1.5), angle = 90),
      #       axis.title.x = element_text(size = rel(1.5), angle = 00))
      # ggplotly(plt)# %>%
      # plotly::layout(legend = list(
      #   orientation = "h",
      #   y = 1.2
      # ))
    })

  })

}


shinyApp(ui = shinyUI, server = shinyServer)
