library(shiny)
library(shinydashboard)
library(tidyverse)

ui <- bootstrapPage(

  selectInput(inputId = "n_breaks",
      label = "Number of bins in histogram (approximate):",
      choices = c(10, 20, 35, 50),
      selected = 20),

  checkboxInput(inputId = "individual_obs",
      label = strong("Show individual observations"),
      value = FALSE),

  checkboxInput(inputId = "density",
      label = strong("Show density estimate"),
      value = FALSE),

  plotOutput(outputId = "eruption_dur_plot", height = "300px"),

  # Display this only if the density is shown
  conditionalPanel(condition = "input.density == true",
    sliderInput(inputId = "bw_adjust",
        label = "Bandwidth adjustment:",
        min = 0.2, max = 2, value = 1, step = 0.2)
  )

)

breaks_input <- selectInput(
    inputId = "n_breaks",
    label = "Number of bins in histogram (approximate):",
    choices = c(10, 20, 35, 50),
    selected = 20)

rugs_input <- checkboxInput(inputId = "individual_obs",
    label = strong("Show individual observations"),
    value = FALSE)

density_input <- checkboxInput(inputId = "density",
    label = strong("Show density estimate"),
    value = FALSE)

bw_input <- conditionalPanel(condition = "input.density == true",
    sliderInput(inputId = "bw_adjust",
        label = "Bandwidth adjustment:",
        min = 0.2, max = 2, value = 1, step = 0.2))

trend_input <- checkboxInput(inputId = "show_trend",
    label = strong("Show trend line"),
    value = FALSE)

alpha_input <- sliderInput(inputId = "point_alpha",
    label = "Alpha level:",
    min = 0.1, max = 1, value = 1, step = 0.1)

points_input <- checkboxInput(inputId = "show_points",
    label = strong("Show points"),
    value = FALSE)

auto_bw_input <- checkboxInput(inputId = "auto_bw",
    label = strong("Automatic Bandwidths"),
    value = TRUE)

bw_x_input <- conditionalPanel(condition = "input.auto_bw == false",
    sliderInput(inputId = "bw_x",
        label = "Waiting Time Bandwidth:",
        min = 0.1, max = 5, value = 2.5, step = 0.2))

bw_y_input <- conditionalPanel(condition = "input.auto_bw == false",
    sliderInput(inputId = "bw_y",
        label = "Eruption Duration Bandwidth:",
        min = 0.1, max = 5, value = 2.5, step = 0.2))

ui <- dashboardPage(
    dashboardHeader(title = "Eu Jing's Dashboard"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Part a)", tabName = "parta"),
            menuItem("Part b)", tabName = "partb")
        )),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "parta",
                fluidRow(
                    column(4, breaks_input),
                    column(4, rugs_input),
                    column(4, density_input)
                ),
                fluidRow(bw_input),
                fluidRow(plotOutput(outputId = "eruption_dur_plot",
                                    height = "300px"))),
            tabItem(tabName = "partb", tabsetPanel(type = "tabs",
                tabPanel("Scatter",
                    fluidRow(
                        column(6, trend_input),
                        column(6, alpha_input)
                    ),
                    fluidRow(plotOutput(outputId = "eruption_waiting_scatter",
                                        height = "300px"))
                ),
                tabPanel("Contour",
                    fluidRow(
                        column(6, points_input),
                        column(6, auto_bw_input)
                    ),
                    fluidRow(
                        column(6, bw_x_input),
                        column(6, bw_y_input)
                    ),
                    fluidRow(plotOutput(outputId = "eruption_waiting_contour",
                                        height = "300px"))
                )
            ))
        ))
)

server <- function(input, output) {

    output$eruption_dur_plot <- renderPlot({
        g <- ggplot(faithful) +
            geom_histogram(aes(x = eruptions, y = stat(density)),
                           bins = as.numeric(input$n_breaks)) +
            labs(x = "Duration (minutes)", y = "Density",
                 title = "Geyser Eruption Duration")

        if (input$individual_obs) {
            g <- g + geom_rug(aes(x = eruptions))
        }

        if (input$density) {
            g <- g + geom_density(aes(x = eruptions),
                                  adjust = input$bw_adjust,
                                  color = "blue")
        }

        g
    })

    output$eruption_waiting_scatter <- renderPlot({
        g <- ggplot(faithful, aes(x = waiting, y = eruptions)) +
            geom_point(alpha = as.numeric(input$point_alpha)) +
            labs(x = "Waiting Time (mins)", y = "Eruption Duration (mins)",
                 title = "Eruption Duration against Waiting Time")

        if (input$show_trend) {
            g <- g + geom_smooth()
        }

        g
    })

    output$eruption_waiting_contour <- renderPlot({
        bw <- c(input$bw_x, input$bw_y)
        g <- ggplot(faithful, aes(x = waiting, y = eruptions)) +
            labs(x = "Waiting Time (mins)", y = "Eruption Duration (mins)",
                 title = "Eruption Duration against Waiting Time")

        if (input$auto_bw) {
            g <- g + geom_density_2d()
        }
        else {
            g <- g + geom_density_2d(h = bw)
        }

        if (input$show_points) {
            g <- g + geom_point()
        }

        g
    })
}

shinyApp(ui = ui, server = server)
