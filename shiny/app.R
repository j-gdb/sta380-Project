library(shiny)

load("data/rawdata.rda")

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
  ))
)

server <- function(input, output) {
  source(file.path("server-plots.r"), local=TRUE)$value
}

shinyApp(ui = ui, server = server)
