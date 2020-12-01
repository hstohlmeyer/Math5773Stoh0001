
#' Shiny MLR
#'
#' Input your data and then using the Shiny server you can choose your dependent and
#' independent variables.
#'
#' @param data dataset
#'
#' @return summary for MLR, normality check, mean of errors
#' @export
#'
#' @examples
#' data(package="s20x")
#' data(camplake.df)
#' data(books.df)
#' ShinyMLR(data=camplake.df)
#' ShinyMLR(data=books.df)
#'
ShinyMLR<-function(data){



  q<-names(data)

  ui<-fluidPage(
    titlePanel("MLR"),
    sidebarPanel(
      p("Select the input for the Dependent Variable"),
      selectInput(inputId = "DepVar", label = "Dependent Variables", multiple = FALSE, choices = q),
      p("Select the inputs for the Independent Variable"),
      selectInput(inputId = "IndVar", label = "Independent Variables", multiple = TRUE, choices = q)
    ),
    mainPanel(
      verbatimTextOutput(outputId = "RegSum"),
      verbatimTextOutput(outputId = "IndPrint"),
      verbatimTextOutput(outputId = "DepPrint"),
      verbatimTextOutput(outputId = "Normality"),
      verbatimTextOutput(outputId = "MeanofError"),
      verbatimTextOutput(outputId = "CIs")

    )
  )

  server <- function(input, output) {

    lm1 <- reactive({lm(reformulate(input$IndVar, input$DepVar), data = data)})





    output$DepPrint <- renderPrint({input$DepVar})
    output$IndPrint <- renderPrint({input$IndVar})
    output$RegSum <- renderPrint({summary(lm1())})
    output$Normality <- renderPrint({normcheck(lm1(),shapiro.wilk=TRUE)})

    output$MeanofError <- renderPrint({
      model=lm(reformulate(input$IndVar, input$DepVar), data = data)
      res=residuals(model)
      mean(res)
    })

    output$CIs <- renderPrint({
      model=lm(reformulate(input$IndVar, input$DepVar), data = data)
      s20x::ciReg(model)

    })


  }

  shiny::shinyApp(ui=ui,server=server)

}
