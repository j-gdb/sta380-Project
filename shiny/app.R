library(shiny)
library(shinyWidgets)

load("data/rawdata.rda")

# Prepare age ranges
age_min <- min(rawdata$age_years, na.rm = TRUE)
age_max <- max(rawdata$age_years, na.rm = TRUE)

ui <- fluidPage(

  titlePanel("How Fast are your Limbs? Measured by Bootstrapping"),

  fluidRow(
    column(4,
           div(
             class = "card",
             style = "padding: 12px; background-color: #e9ecef; border-radius: 8px; width: 100%;",
             tabsetPanel(
               type = "tabs",
               tabPanel("Data",
                        h3("Dataset", style = "text-align: center;"),
                        selectInput("dataset",
                                    "Dataset",
                                    choices = c(
                                      "Dominant Hand",
                                      "Non Dominant Hand",
                                      "Dominant Foot",
                                      "Non Dominant Foot")
                        ),
                        selectInput("dataset2",
                                    "Compare With",
                                    choices = c("None",
                                                "Dominant Hand",
                                                "Non Dominant Hand",
                                                "Dominant Foot",
                                                "Non Dominant Foot"),
                                    selected = "None"
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
                        h3("Statistic", style = "text-align: center;"),
                        radioButtons("statistic",
                                     "Statistic",
                                     choices = c("Mean", "IQR")
                        )
               ),
               tabPanel("Bootstrap",
                        h3("Bootstrap", style = "text-align: center;"),
                        numericInput("resamples",
                                     "Bootstrap Samples",
                                     value = 5000,
                                     min = 100),
                        numericInput("seed",
                                     "Random Seed",
                                     value = 1),
               ),
               tabPanel("Appearance",
                        h3("Cosmetics", style = "text-align: center;"),
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
             )
           ),

           div(
             class = "card",
             style = "padding: 12px; background-color: #e9ecef; border-radius: 8px; margin-top: 15px; width: 100%;",
             h4("Summary of Filtered Data", style = "text-align: center;"),
             tableOutput("summary_table")
           )

    ),

    column(8,
           div(
             class = "card",
             style = "padding: 15px; margin-bottom: 20px; background-color: #e9ecef; border-radius: 8px;",
             h2("Bootstrap Histogram"),
             plotOutput("bootstrap_plot", height = "450px"),
             div(
               class = "card",
               style = "padding: 15px; margin-bottom: 20px; background-color: #e9ecef; border-radius: 8px;",
               h4("Histogram Description"),
               p("This histogram shows the distribution of the selected statistic (Mean or IQR) for a filtered subset of participants from a reaction time study. In the original study, participants completed reaction time tasks using both their dominant and non-dominant hands and feet. Reaction times were measured using a Nintendo Wii Balance Board, which recorded response times as participants reacted to stimuli by applying pressure through their hands or feet. Additional participant information, including age and gender, was also collected."),
               p("The goal of this app is to allow users to explore how reaction time performance varies across different groups and conditions, and to visualize the variability and uncertainty of these estimates using bootstrap resampling."),
               p("The bootstrap distribution is calculated by repeatedly resampling the filtered dataset with replacement, computing the chosen statistic for each resample, and plotting the resulting values. The black vertical line represents the observed value of the statistic computed directly from the filtered data.")
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
