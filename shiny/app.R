library(shiny)

load("data/rawdata.rda")

# Prepare age ranges
age_min <- min(rawdata$age_years, na.rm = TRUE)
age_max <- max(rawdata$age_years, na.rm = TRUE)

ui <- fluidPage(

  titlePanel("Bootstrap Reaction Time"),

  sidebarLayout(
    sidebarPanel(

      h2("Data Selection"),

      selectInput("dataset",
                  "Dataset",
                  choices = c("Dominant Hand",
                              "Non Dominant Hand",
                              "Dominant Foot",
                              "Non Dominant Foot")),

      selectInput("gender",
                  "Gender",
                  choices = c("All", "Male", "Female")),

      sliderInput("age",
                  "Age Range",
                  min = age_min,
                  max = age_max,
                  value = c(age_min, age_max)),

      h2("Statistic"),

      radioButtons("statistic",
                   "Choose Statistic",
                   choices = c("Mean", "IQR")),

      h2("Bootstrap Parameters"),

      numericInput("resamples",
                   "Number of Bootstrap Samples",
                   value = 5000, min = 100),

      numericInput("seed",
                   "Random Seed",
                   value = 1),

      h2("Plot Appearance"),

      selectInput("colour",
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
      h2("Bootstrap Histogram"),
      plotOutput("bootstrap_plot", height = "450px"),
      fluidRow(
        column(
          width = 6,
          div(
            class = "card",
            style = "padding: 15px; margin-bottom: 20px; background-color: #e9ecef; border-radius: 8px;",
            h4("Histogram Description"),
            p(
              "This histogram shows the distribution of the selected statistic (Mean or IQR) for the filtered subset of participants. It is calculated by repeatedly resampling the filtered dataset with replacement, computing the statistic for each resample, and plotting the resulting distribution of these bootstrap estimates. The red vertical line indicates the observed value of the statistic calculated directly from the filtered dataset. The goal of this app is to allow users to explore how the mean or IQR varies across different groups defined by age, gender, and dataset type, providing a visual sense of the uncertainty and variability of the statistic."
            )
          ),
        ),
        column(
          width = 5,
          div(
            class = "card",
            style = "padding: 15px; margin-bottom: 20px; background-color: #e9ecef; border-radius: 8px;",
            h4("Summary of Filtered Data"),
            tableOutput("summary_table")
          )
        )
      )
    )
  ),

  tags$img(
    src = "dog.png",
    id = "dog",
    style="
    position: fixed;
    width: 200px;
    z-index: 9999;
    pointer-events: none;"
  ),
  tags$script(HTML("document.addEventListener('DOMContentLoaded', function() {
      const dog = document.getElementById('dog');
      const size = 200;
      let x = window.innerWidth/2 //Math.random() * (window.innerWidth - size);
      let y = window.innerHeight/2 //Math.random() * (window.innerHeight - size)
      let dx = 5;
      let dy = 5;
      function bounce() {
        const w = window.innerWidth - size;
        const h = window.innerHeight - size;
        x += dx;
        y += dy;

        if (x <= 0 || x >= w) dx = -dx;
        if (y <= 0 || y >= h) dy = -dy;
        dog.style.left = x + 'px';
        dog.style.top = y + 'px';
        requestAnimationFrame(bounce);
      }
      bounce();
    });"
  )
 )
)

server <- function(input, output) {
  source(file.path("server-plots.r"), local=TRUE)$value
}

shinyApp(ui = ui, server = server)
