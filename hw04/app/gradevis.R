library(shiny)
library(ggvis)  # for the diamonds dataset

clean_data <- read.csv("../data/cleandata/cleanscores.csv")

ui <- fluidPage(
  titlePanel("Grade Visualiser"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "barchart"',
        tableOutput("grade")
      ),
      conditionalPanel(
        'input.dataset === "histogram"',
        selectInput(inputId = "col_name", label = "X-axis variable", 
                    choices = c("Homework", "Quiz", "Test1", "Test2", "Overall")),
        sliderInput(inputId = "bwidth", label = "Bin Width", min = 1, max = 10, value = 10)
      ),
      conditionalPanel(
        'input.dataset === "scatterplot"',
        selectInput(inputId = "x", label = "X-axis variable", 
                    choices = c("Homework", "Quiz", "Test1", "Test2", "Overall")),
        selectInput(inputId = "y", label = "Y-axis variable", 
                    choices = c("Homework", "Quiz", "Test1", "Test2", "Overall")),
        sliderInput(inputId = "opacity", label = "Opacity", min = 0.0, max = 1.0, value = 1.0),
        radioButtons("line", "Show line",
                     c("none" = "none",
                       "lm" = "lm",
                       "loess" = "loess"
                       ))
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("barchart", plotOutput("barchart")),
        tabPanel("histogram", plotOutput("histogram"), headerPanel("Summary Statistics"), verbatimTextOutput("summary")),
        tabPanel("scatterplot", plotOutput("scatterplot"), headerPanel("Correlation"), verbatimTextOutput("cor"))
      )
    )
  )
)

server <- function(input, output) {
  
  # choose columns to display
  df <- data.frame(table(clean_data$Grade))
  colnames(df) <- c("grade", "freq")
  df$percentage <- df$freq / sum(df$freq) * 100
  output$barchart <- renderPlot(barplot(table(clean_data$Grade), xlab = "Grade", ylab = "frequency"))
  output$grade <- renderTable(
    df
    )
  # sorted columns are colored now because CSS are attached to them
  col_input <- reactive({
    switch(input$col_name,
           "Homework" = clean_data$Homework,
           "Quiz" = clean_data$Quiz,
           "Test1" = clean_data$Test1,
           "Test2" = clean_data$Test2,
           "Overall" = clean_data$Overall)
  })
  
  x_input <- reactive({
    switch(input$x,
           "Homework" = clean_data$Homework,
           "Quiz" = clean_data$Quiz,
           "Test1" = clean_data$Test1,
           "Test2" = clean_data$Test2,
           "Overall" = clean_data$Overall)
  })
  
  y_input <- reactive({
    switch(input$y,
           "Homework" = clean_data$Homework,
           "Quiz" = clean_data$Quiz,
           "Test1" = clean_data$Test1,
           "Test2" = clean_data$Test2,
           "Overall" = clean_data$Overall)
  })
  
  line_in <- reactive({
    switch(input$line,
           lm = abline,
           loess = loess
           )
  })
  
  output$summary <- renderPrint({col <- col_input()
  print_stats(summary_stats(col))})
  
  
  output$histogram <- renderPlot({col <- col_input()
  hist(col, main = "", input$bwidth, xlab = input$col_name, ylab = "Count")})
  
  # customize the length drop-down menu; display 5 rows per page by default
  output$scatterplot <- renderPlot({
    x <- x_input()
    y <- y_input()
    #l <- line_in()
    plot(x,y, col = rgb(red = 1, green = 0, blue = 0, alpha = input$opacity))
    if (input$line == "lm") {
      abline(lm(x~y))
    } else if(input$line == "loess") {
      lines(loess(x~y))
    }
  })
  
  output$cor <- renderText({
    x <- x_input()
    y <- y_input()
    cor(x,y)})
    
}

shinyApp(ui, server)


#Reflection Questions: 
#1. Yes
#2. I found it relatively confusing: 7/10
#3. Yes
#4. I found it okay, but still confusing 6/10
#5. Yes. It was my first time working in shiny
#6. I found it extremely confusing 10/10
#7. I enjoy working with ggplot because this paradigm exposes me to both the complexity of functions in R as well as its straight-forward operations. The functions are not too complex and you can create so much variety with this. 
#8. I spent a really long time, around 20 hours at least. 
#9. Figuring out shiny was really hard and time-consuming. 