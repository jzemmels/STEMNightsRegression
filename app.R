
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
require(plyr)
require(plotly)

# Define UI for application that draws a histogram
shinyUI <- fluidPage(
  # Application title
  titlePanel("Hello Kids!"),
  sidebarLayout(
    sidebarPanel(width = 4,
      numericInput("height", label = h3("Height (inches)"), value = 1),
      numericInput("shoe", label = h3("Shoe Size (inches"), value = 1),
      selectInput("grade", label = h3("Grade"), 
                  choices = list("Kindergarten" = 'K', "1st" = 1, "2nd" = 2, "3rd" = 3, "4th" = 4,
                                 "5th" = 5, "6th" = 6, "Adult" = 7), selected = 'K'),
      selectInput("gender", label = h3("Gender"),  choices = c('F', 'M')),
      actionButton("goButton", "Go!", class = "btn-primary")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotlyOutput("distPlot", height='650px', width = '650px'),
              tableOutput("distTable"))
  )
)


# Define server logic required to draw a histogram
shinyServer <- function(input, output){
  require(ggplot2)
  require(plyr)
  # read current dataset
  d <- read.csv("height_shoesizeData.csv", header=TRUE)
  
  observeEvent(input$goButton, {
    d <- read.csv("height_shoesizeData.csv", header=TRUE)
    d <- rbind(c(input$shoe, input$gender, input$grade, input$height), d)
    write.csv(x = d, file="height_shoesizeData.csv", row.names = FALSE)
  })
      
  output$distTable <- renderTable({
    input$goButton
    d <- read.csv("height_shoesizeData.csv", header=TRUE)
    d$Grade <- factor(d$Grade)
    d$Grade <- revalue(d$Grade, c("0"="Kindergarten","1"="1st","2"="2nd","3"="3rd","4"="4th","5"="5th","6"="6th","7"="Adult"))
    d[1:5, ]
  })
    
  output$distPlot <- renderPlotly({
    input$goButton
    d <- read.csv("height_shoesizeData.csv", header=TRUE)
    d$Grade <- factor(d$Grade)
    d$Grade <- revalue(d$Grade, c("0"="Kindergarten","1"="1st","2"="2nd","3"="3rd","4"="4th","5"="5th","6"="6th","7"="Adult"))
    d$Gender <- factor(d$Gender)
    d$Gender <- revalue(d$Gender, c("F"="GIRL","M"="BOY"))
    
    # draw the histogram with the specified number of bins
   p<- ggplot(data=d, aes(x=Height, y=ShoeSize), size=I(1)) + 
     geom_point() +  geom_smooth(method = "lm",se = FALSE) + 
     geom_point(aes(colour=factor(Grade))) +
     #geom_point(data=d[1,], aes(x=Height, y=ShoeSize), size=I(2), color=I('red')) +
     facet_grid(Gender ~ .) + 
      labs(x="Height", y="Shoe Size") + 
      theme(aspect.ratio = 1, 
            axis.title.y = element_text(size = rel(1.8), angle = 90), 
            axis.title.x = element_text(size = rel(1.8), angle = 00))
  ggplotly(p)
   })
  
}


shinyApp(ui = shinyUI, server = shinyServer)
