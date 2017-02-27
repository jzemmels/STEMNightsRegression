require(shiny)
require(ggplot2)
require(plyr)

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
  require(plyr)
  d <- data.frame(Height=  c(76, 69,  73, 60, 62, 68, 62, 60),
                  ShoeSize=c(11, 9.5, 12, 7,  8,  6,  5,  5),
                  Grade=   c(0,  1,   2,  3,  4,  5,  6,  7))
  write.csv(x = d, file="schooldata.csv", row.names = FALSE)
  
  observeEvent(input$goButton, {
    d <- read.csv("schooldata.csv", header=TRUE)
    d <- rbind(d,c(input$height, input$shoe, input$age))
    write.csv(x = d, file="schooldata.csv", row.names = FALSE)
  })
      
  output$distTable <- renderTable({
    input$goButton
    d <- read.csv("schooldata.csv", header=TRUE)
    d$Grade <- factor(d$Grade)
    d$Grade <- revalue(d$Grade, c("0"="Kindergarten","1"="1st","2"="2nd","3"="3rd","4"="4th","5"="5th","6"="6th","7"="Adult"))
    d
  })
    
  output$distPlot <- renderPlot({
    input$goButton
    d <- read.csv("schooldata.csv", header=TRUE)
    d$Grade <- factor(d$Grade)
    d$Grade <- revalue(d$Grade, c("0"="Kindergarten","1"="1st","2"="2nd","3"="3rd","4"="4th","5"="5th","6"="6th","7"="Adult"))
    # draw the histogram with the specified number of bins
    ggplot(d, aes(x=Height, y=ShoeSize)) + geom_point(aes(color=Grade)) + geom_smooth(method = "lm",se = FALSE) + 
      labs(x="Height", y="Shoe Size") + theme(axis.title.y = element_text(size = rel(1.8), angle = 90)) + 
      theme(axis.title.x = element_text(size = rel(1.8), angle = 00))
  })
  
}


shinyApp(ui = shinyUI, server = shinyServer)
