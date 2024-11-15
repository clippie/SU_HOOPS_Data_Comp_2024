library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)

# Published URL: https://cadenlippie.shinyapps.io/SUBBALL_Data_Comp_2024/

# Data
final_data <- read.csv("SUBBALL_Data_Comp_2024_final_data.csv")

# UI
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(
    title = "ORANGE HOOPS",
    titleWidth = 250,
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#",
        tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/4/49/Syracuse_Orange_logo.svg/1515px-Syracuse_Orange_logo.svg.png", height = "17px", width = "auto", style = "padding: 0px;")
      ),
      tags$head(
        tags$style(HTML('
          /* Custom CSS for logo in the main header */
          .skin-black .main-header .logo {
            font-family: "Georgia", Times, "Times New Roman", serif;
            font-weight: bold;
            font-size: 18px;
            color: #de472a;
          }
          /* Center the title of the "Recommended Player" box */
          .box-header .box-title {
            text-align: center;
            display: block;
            font-family: "Georgia", Times, "Times New Roman", serif;
            font-weight: bold;
            font-size: 18px;
            color: #ec9b00;
            padding-top: 5px;
          }
          /* Custom style for select inputs */
          .custom-select-label {
            font-weight: bold;
            font-size: 12px;
            color: #ec9b00;
          }
        '))

      )
    )
  ),
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      menuItem(" Home", tabName = "home", icon = icon("home"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$link(rel = "shortcut icon", href = "https://images.sidearmdev.com/fit?url=https%3a%2f%2fdxbhsrqyrr690.cloudfront.net%2fsidearm.nextgen.sites%2fsuathletics.com%2fimages%2flogos%2fsite%2fsite.png&height=120&width=150&type=png", type = "image/png", sizes = "20x20"),
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(
                  width = 3,
                  status = "warning",
                  height = "200px",
                  div(
                    selectInput("twos_threes", tags$span("Need 2 or 3 Points:", class = "custom-select-label"),
                                choices = c("2", "3"),
                                selected = "2"
                    ),
                    selectInput("team", tags$span("Select Team:", class = "custom-select-label"),
                                choices = final_data$team,
                                selected = "Syracuse"
                    )
                  )
                ),
                box(
                  title = tags$div("Recommended Player", class = "center-title"),
                  width = 3,
                  status = "warning",
                  tags$div(
                    uiOutput("player_image"),
                    uiOutput("player_caption"),
                    style = "text-align: center;"
                  )
                ),
                box(
                  title = tags$div("Recommended Play", class = "center-title"),
                  width = 6,
                  status = "warning",
                  tags$div(
                    uiOutput("play_design"),
                    uiOutput("play_description"),
                    style = "text-align: center;"
                  )
                )
              )
      )
    )
  ),
  title = "ORANGE HOOPS"
)

# Server
server <- function(input, output) {

    server_data <- reactive({
    if (input$twos_threes == "2") {
      final_data %>%
        filter(team == input$team)
    } else {
      final_data %>%
        filter(team == input$team)
    }
  })

  output$player_image <- renderUI({
    data <- server_data()
    img_src <- if (input$twos_threes == "2") {
      data$twos_headshots[1]
    } else {
      data$threes_headshots[1]
    }
    tags$img(src = img_src,
             height = "200px",
             width = "auto",
             style = "padding: 0px; border: 2px solid #ec9b00; border-radius: 5px;")
  })

  output$player_caption <- renderUI({
    if (input$twos_threes == "2") {
      tags$figcaption(
        HTML(paste(server_data()$twos_player, "<br>", server_data()$twos_cluster)),
        style = "text-align: center; margin-top: 10px; font-family: Georgia, Times, Times New Roman, serif;
        font-weight: bold;
        font-size: 12px;
        color: #ec9b00;
        padding-top: 5px;"
      )    } else {
        tags$figcaption(
          HTML(paste(server_data()$threes_player, "<br>", server_data()$threes_cluster)),
          style = "text-align: center; margin-top: 10px; font-family: Georgia, Times, Times New Roman, serif;
        font-weight: bold;
        font-size: 12px;
        color: #ec9b00;
        padding-top: 1px;"
        )    }
  })

  output$play_design <- renderUI({
    data <- server_data()
    img_src <- if (input$twos_threes == "2" && data$twos_cluster[1] == "Ball Handling Combo Guard") {
      "Iso.png"
    } else if (input$twos_threes == "2" && data$twos_cluster[1] == "Shooter") {
      "Flare.png"
    } else if (input$twos_threes == "2" && data$twos_cluster[1] == "Roll and Cut Big") {
      "Spain.png"
    } else if (input$twos_threes == "2" && data$twos_cluster[1] == "Three Level Scoring Forward") {
      "UCLA.png"
    } else if (input$twos_threes == "3" && data$threes_cluster[1] == "Ball Handling Combo Guard") {
      "Iso.png"
    } else if (input$twos_threes == "3" && data$threes_cluster[1] == "Shooter") {
      "Flare.png"
    } else if (input$twos_threes == "3" && data$threes_cluster[1] == "Roll and Cut Big") {
      "Spain.png"
    } else if (input$twos_threes == "3" && data$threes_cluster[1] == "Three Level Scoring Forward") {
      "UCLA.png"
    }

    tags$img(src = img_src, height = "300px", width = "auto", style = "padding: 0px; border: 2px solid #ec9b00; border-radius: 5px;")
  })



  output$play_description <- renderUI({
    data <- server_data()
    if (input$twos_threes == "2" && data$twos_cluster[1] == "Ball Handling Combo Guard"){
      tags$figcaption(
        HTML(paste("5-Out Isolation: where", data$twos_player, " is player 2.",
                   "<br>", "Goal: Get ", data$twos_player, "the ball with room to drive for a mid-range jumper or layup.")),
        style = "text-align: center; margin-top: 10px; font-family: Georgia, Times, Times New Roman, serif;
        font-weight: bold;
        font-size: 12px;
        color: #ec9b00;
        padding-top: 5px;"
      )} else if (input$twos_threes == "2" && data$twos_cluster[1] == "Shooter") {
        tags$figcaption(
          HTML(paste("Flare Screen: where ", data$twos_player[1], " is player 3.",
                     "<br>", "Goal: Get ", data$twos_player[1], "a mid-range jumper.")),
          style = "text-align: center; margin-top: 10px; font-family: Georgia, Times, Times New Roman, serif;
      font-weight: bold; font-size: 12px; color: #ec9b00; padding-top: 5px;"
        )
      }  else if (input$twos_threes == "2" && data$twos_cluster[1] == "Three Level Scoring Forward") {
        tags$figcaption(
          HTML(paste("UCLA: where ", data$twos_player[1], " is player 2.",
                     "<br>", "Goal: Get ", data$twos_player[1], " a driving opportunity.")),
          style = "text-align: center; margin-top: 10px; font-family: Georgia, Times, Times New Roman, serif;
      font-weight: bold; font-size: 12px; color: #ec9b00; padding-top: 5px;"
        )
      }  else if (input$twos_threes == "2" && data$twos_cluster[1] == "Roll and Cut Big") {
        tags$figcaption(
          HTML(paste("Spain Pick and Roll: where ", data$twos_player[1], " is player 2.",
                     "<br>", "Goal: Get ", data$twos_player[1], " the ball close to the basket.")),
          style = "text-align: center; margin-top: 10px; font-family: Georgia, Times, Times New Roman, serif;
      font-weight: bold; font-size: 12px; color: #ec9b00; padding-top: 5px;"
        )
      } else if (input$twos_threes == "3" && data$threes_cluster[1] == "Shooter") {
        tags$figcaption(
          HTML(paste("Flare Screen: where ", data$threes_player[1], " is player 2.",
                     "<br>", "Goal: Get ", data$threes_player[1], "a corner three.")),
          style = "text-align: center; margin-top: 10px; font-family: Georgia, Times, Times New Roman, serif;
      font-weight: bold; font-size: 12px; color: #ec9b00; padding-top: 5px;"
        )
      }  else if (input$twos_threes == "3" && data$threes_cluster[1] == "Three Level Scoring Forward") {
        tags$figcaption(
          HTML(paste("UCLA: where ", data$threes_player[1], " is player 2.",
                     "<br>", "Goal: Get ", data$threes_player[1], " a three point opportunity.")),
          style = "text-align: center; margin-top: 10px; font-family: Georgia, Times, Times New Roman, serif;
      font-weight: bold; font-size: 12px; color: #ec9b00; padding-top: 5px;"
        )
      }  else if (input$twos_threes == "3" && data$threes_cluster[1] == "Roll and Cut Big") {
        tags$figcaption(
          HTML(paste("Spain Pick and Roll: where ", data$threes_player[1], " is player 5.",
                     "<br>", "Goal: Get ", data$threes_player[1], " the ball on the wing for a three.")),
          style = "text-align: center; margin-top: 10px; font-family: Georgia, Times, Times New Roman, serif;
      font-weight: bold; font-size: 12px; color: #ec9b00; padding-top: 5px;"
        )
      } else if (input$twos_threes == "3" && data$threes_cluster[1] == "Ball Handling Combo Guard") {
        tags$figcaption(
          HTML(paste("5-Out Isolation: where ", data$threes_player[1], " is player 2.",
                     "<br>", "Goal: Get ", data$threes_player[1], " the ball on the wing to create a three for himself.")),
          style = "text-align: center; margin-top: 10px; font-family: Georgia, Times, Times New Roman, serif;
      font-weight: bold; font-size: 12px; color: #ec9b00; padding-top: 5px;"
        )
      }
  })


  }


shinyApp(ui, server)
