library(shiny)
library(here)

load(here("data","rawdata.rda"))

source(here("R","bootstrap_sampling.R"))
source(here("misc","histograms.R"))

#preparing age ranges for later
age_min <- min(rawdata$age_years, na.rm = TRUE)
age_max <- max(rawdata$age_years, na.rm = TRUE)

ui <- fluidPage(
  titlePanel("Bootstrap Reaction Time"),
  sidebarLayout(
    sidebarPanel(

      numericInput("seed","Random Seed",1),

      numericInput("resamples",
                   "Number of Bootstrap Samples",
                   5000,
                   min = 100),

      selectInput("gender",
                  "Gender",
                  choices = c("All","Male","Female")),

      sliderInput("age",
                  "Age Range",
                  min = age_min,
                  max = age_max,
                  value = c(age_min, age_max)),

      selectInput("dataset",
                  "Dataset",
                  choices = c("Dominant Hand",
                              "Non Dominant Hand",
                              "Dominant Foot",
                              "Non Dominant Foot")),

      radioButtons("statistic",
                   "Statistic",
                   choices = c("Mean","IQR")),

      selectInput(
        "colour",
        "Histogram Colour",
        choices = c(
          "Burlywood" = "burlywood",
          "Tomato" = "tomato",
          "Sienna" = "sienna",
          "Rosy Brown" = "rosybrown",
          "Dark Sea Green" = "darkseagreen",
          "Slate Gray" = "slategray",
          "Violet Red" = "violetred"
        ),
        selected = "burlywood"
      )
    ),
    mainPanel(
      plotOutput("bootstrap_plot")
    )
  )
)

server <- function(input, output) {

  filtered_data <- reactive({
    df <- rawdata
    if(input$gender != "All"){
      gender_code <- ifelse(input$gender == "Male",1,2)
      df <- df[df$gender == gender_code,]
    }
    df <- df[
      df$age_years >= input$age[1] &
        df$age_years <= input$age[2],
    ]
    df
  })

  selected_vector <- reactive({
    df <- filtered_data()
    vec <- switch(input$dataset,
                  "Dominant Hand" = df$RHD,
                  "Non Dominant Hand" = df$RHND,
                  "Dominant Foot" = df$RFD,
                  "Non Dominant Foot" = df$RFND
    )
    vec
  })

  output$bootstrap_plot <- renderPlot({
    vec <- selected_vector()
    if(length(vec) == 0){
      plot.new()
      text(0.5,0.5,"No data available for selected filters")
      return()
    }
    plot_bootstrap_hist(
      x = vec,
      statistic = input$statistic,
      num_samples = input$resamples,
      seed = input$seed,
      colour = input$colour
    )
  })
}

shinyApp(ui = ui, server = server)
