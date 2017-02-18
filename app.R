require(shiny)
require(ggplot2)

# Define UI for application that draws a histogram
shinyUI <- fluidPage(
  # Application title
  titlePanel("Hello Kids!"),
  sidebarLayout(
    sidebarPanel(
      numericInput("height", label = h3("Height (inches)"), value = 1),
      numericInput("shoe", label = h3("Shoe Size (inches"), value = 1),
      selectInput("age", label = h3("Grade"), 
                  choices = list("Kindergarten" = 0, "1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4,
                                 "5th" = 5, "6th" = 6, "Adult" = 7), selected = 0),
      actionButton("goButton", "Go!", class = "btn-primary")
    ),
    # Show a plot of the generated distribution
    mainPanel(plotOutput("distPlot"),
              tableOutput("distTable"))
  )
)


# Define server logic required to draw a histogram
shinyServer <- function(input, output){
  require(ggplot2)
  d <- data.frame(Height=c(71, 60),
                  ShoeSize=c(12, 7),
                  Grade=c(2, 3))
  write.csv(x = d, file="schooldata.csv", row.names = FALSE)
  
  observeEvent(input$goButton, {
    d <- read.csv("schooldata.csv", header=TRUE)
    d <- rbind(d,c(input$height, input$shoe, input$age))
    write.csv(x = d, file="schooldata.csv", row.names = FALSE)
  })
      
  output$distTable <- renderTable({
    input$goButton
    d <- read.csv("schooldata.csv", header=TRUE)
  })
    
  output$distPlot <- renderPlot({
    input$goButton
    d <- read.csv("schooldata.csv", header=TRUE)
    d$Grade <- as.factor(d$Grade)
    # draw the histogram with the specified number of bins
    ggplot(d, aes(x=Height, y=ShoeSize, color=Grade)) + geom_point() + geom_smooth(method = "lm") + 
      labs(x="Height", y="Shoe Size")
  })
  
}


shinyApp(ui = shinyUI, server = shinyServer)
