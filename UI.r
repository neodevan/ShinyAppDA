# Contributed by Shyam Krishnan(10533809) and Vipin Mavilapathayapurayil (10537195)

library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Descriptive statistics of x dataset with R and Shiny Apps"),
  
  # Sidebar with controls 
  sidebarLayout(
    sidebarPanel( 
      h3("Filtering data"),
      conditionalPanel(condition="input.tabselected==5 || input.tabselected==4",
      selectInput("Kvar", "K variable", 
                  choices = c("Ozone", "Solar.R", "Wind", "Temp"), selected = "Solar.R")),
      conditionalPanel(condition="input.tabselected==5 || input.tabselected==4 || input.tabselected==6" ,
      selectInput("Yvar", "Y variable", 
                  choices = c("Ozone", "Solar.R", "Wind", "Temp"), selected = "Solar.R")),
      conditionalPanel(condition="input.tabselected==1 || input.tabselected==2 || input.tabselected==3",selectInput("dataset", "Choose a dataset (or a subset) :", 
                  choices = c("all x data", "May", "June", "July","August", "September")),
      selectInput("var", label = "Select the quantitative Variable", 
                  choices = c("Ozone" = 1, "Solar.R" = 2, "Wind" = 3, "Temp"=4),
                  selected = 3), 
      
      sliderInput("bin", "Select the number of histogram BINs by using the slider below", min=5, max=25, value=15),
      
      radioButtons("colour", label = "Select the color of histogram",
                   choices = c("Green", "Red",
                               "Yellow"), selected = "Green"),
      numericInput("obs", "Number of observations to view on table:", 10),
      downloadButton("downloadData", "Download 'x.csv'", class = NULL))
      
    ),
    
    # MainPanel divided into many tabPanel
    mainPanel(
      tabsetPanel(tabPanel("Plot",value=1,plotOutput("myhist"),h1("Boxplot"), plotOutput("boxPlot")),
        tabPanel("Descriptive statistics",value=2, h1("Descriptive statistics"),verbatimTextOutput("summary")),
        tabPanel("Table",value=3, h1("Table"), textOutput("NbRows"), tableOutput("view")),
        tabPanel("Hypothesis Testing",value=4,h1("Correlation between Ozone and Temperature",plotOutput("hypt"),verbatimTextOutput("hyp"))),
        tabPanel("Linear Regression",value=5,h1("Linear Regression",plotOutput("linreg"))),
        tabPanel("Normal Distribution",value=6,h1("Probability Distribution",plotOutput("prob_dist"))),
        id = "tabselected"
        
      ) 
    )
  )
))