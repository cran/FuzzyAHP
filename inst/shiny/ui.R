shinyUI(fluidPage(
  titlePanel("AHP"),
  fluidRow(

    column(3, wellPanel(
      sliderInput("input_number", "Number of elements",
                  min = 2, max = 15, value = 5)
    )),

    column(3, wellPanel(
      # This outputs the dynamic UI component
      uiOutput("ui")
    )),

    column(3, wellPanel(
      # This outputs the dynamic UI component
      uiOutput("ui2")
    ))

    # column(3,
    #   tags$p("Input type:"),
    #   verbatimTextOutput("input_type_text"),
    #   tags$p("Dynamic input value:"),
    #   verbatimTextOutput("dynamic_value")
    # )


  )
))
