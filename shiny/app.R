library(shiny)

load("data/rawdata.rda")

# Prepare age ranges
age_min <- min(rawdata$age_years, na.rm = TRUE)
age_max <- max(rawdata$age_years, na.rm = TRUE)

ui <- fluidPage(

  titlePanel("Bootstrap Reaction Time"),

  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("Data",
                 h3("Dataset"),
                 selectInput("dataset",
                             "Dataset",
                             choices = c(
                               "Dominant Hand",
                               "Non Dominant Hand",
                               "Dominant Foot",
                               "Non Dominant Foot"
                             )
                 ),
                 selectInput("gender",
                             "Gender",
                             choices = c("All", "Male", "Female")
                 ),
                 sliderInput("age",
                             "Age Range",
                             min = age_min,
                             max = age_max,
                             value = c(age_min, age_max)
                 )
        ),
        tabPanel("Statistic",
                 radioButtons("statistic",
                              "Statistic",
                              choices = c("Mean", "IQR")
                 )
        ),
        tabPanel("Bootstrap",
                 numericInput("resamples",
                              "Bootstrap Samples",
                              value = 5000,
                              min = 100
                 ),
                 numericInput("seed",
                              "Random Seed",
                              value = 1
                 )
        ),
        tabPanel("Appearance",
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
        tabPanel("Cat",
                 actionButton("toggle_dog", "Release the Cat"))
      ),
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
            p("This histogram shows the distribution of the selected statistic (Mean or IQR) for a filtered subset of participants from a reaction time study. In the original study, participants completed reaction time tasks using both their dominant and non-dominant hands and feet. Reaction times were measured using a Nintendo Wii Balance Board, which recorded response times as participants reacted to stimuli by applying pressure through their hands or feet. Additional participant information, including age and gender, was also collected."),
            p("The goal of this app is to allow users to explore how reaction time performance varies across different groups and conditions, and to visualize the variability and uncertainty of these estimates using bootstrap resampling."),
            p("The bootstrap distribution is calculated by repeatedly resampling the filtered dataset with replacement, computing the chosen statistic for each resample, and plotting the resulting values. The black vertical line represents the observed value of the statistic computed directly from the filtered data."),
            )
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
  tags$script(HTML("document.addEventListener('DOMContentLoaded', function() {
      let dog = null;
      const size = 220;

      function createDog(){
        dog = document.createElement('img');
        dog.src = 'catspin.gif' //I know it says dog
        dog.style.position = 'fixed'
        dog.style.width = size + 'px'
        dog.style.zIndex = 9999
        dog.style.pointerEvents = 'none'

        let x = Math.random() * (window.innerWidth - size);
        let y = Math.random() * (window.innerHeight - size)
        let dx = 5;
        let dy = 5

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
        document.body.appendChild(dog);
        bounce();
      }

      function removeDog(){
        if (dog){
          dog.remove();
          dog = null;
        }
      }

      Shiny.addCustomMessageHandler(\"toggleDog\", function(message) {
        if (dog){
          removeDog();
        } else {
          createDog();
        }
      })
    });"
  )
  )
)

server <- function(input, output, session) {
  observeEvent(input$toggle_dog,{
    session$sendCustomMessage("toggleDog", TRUE) # there has to be a better way
  })
  source(file.path("server-plots.r"), local=TRUE)$value
}

shinyApp(ui = ui, server = server)
