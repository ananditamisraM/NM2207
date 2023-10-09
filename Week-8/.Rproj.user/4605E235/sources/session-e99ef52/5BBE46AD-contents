library(shiny)

# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Tabsets"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    
    # Sidebar panel for inputs ----
    sidebarPanel(
      a(href="https://shiny.posit.co/r/articles/build/layout-guide/", "Click on this link for more information on the application layout"),
      img(src = "week-8_shiny.png", height = 200, width = 200),
      div("this is another paragraph with only blue text using div function", style = "color:blue"),
      
      # Input: Select the random distribution type ----
      radioButtons("dist", "Distribution type:",
                   c("Normal" = "norm",
                     "Uniform" = "unif",
                     "Log-normal" = "lnorm",
                     "Exponential" = "exp")),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Slider for the number of observations to generate ----
      sliderInput("n",
                  "Number of observations:",
                  value = 500,
                  min = 1,
                  max = 1000)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      h1("Here are the different tabs for the data!"),
      h4("Choose one"),
      p("You have to choose plot tab to see the data visually. You have to choose the summary tab to get a five-number summary of the data and the mean. You have to choose the table tab to view the data in a table format."),
      p("How do you want to view the data depends on your needs", strong("What do you want to see depends on your needs to view the data")),
      p("Adjust the distribution type according to what you want", em("This is to show an italicised text") ),
      br(),
      p("here you see another paragraph",
        span("with words and characters", style = "color:blue"),
        "but not all characters are blue in colour"),
      img(src = "image2.png", height = 200, width = 550),
      p("this is the code that i added to add an image"),
      code("following is the code using the code command- img(src = image name in double quotes, height = 200, width = 550)"),
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  d <- reactive({
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
    dist <- input$dist
    n <- input$n
    
    hist(d(),
         main = paste("r", dist, "(", n, ")", sep = ""),
         col = "#75AADB", border = "white")
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(d())
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    d()
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)

