
#' @title Shiny MLR
#'
#' @description Input your data and then using the Shiny server you can choose your dependent and
#' independent variables.
#'
#' @param data dataset
#'
#' @return summary for MLR, normality check, mean of errors
#' @export
#'
#' @examples
#' quasar<-readxl::read_excel("C:/Users/hstoh/OneDrive - University of Oklahoma/Fall 2020/MATH 5773/Data/QUASAR.xls")
#' bub<-readxl::read_excel("C:/Users/hstoh/OneDrive - University of Oklahoma/Fall 2020/MATH 5773/Data/BUBBLE2.xls")
#' \dontrun{ ShinyMLR(data=quasar)}
#' \dontrun{ ShinyMLR(data=bub)}
#'
ShinyMLR<-function(data){



  q<-names(data)

  ui<-shiny::fluidPage(
    shiny::titlePanel("MLR"),
    shiny::sidebarPanel(
      shiny::p("Select the input for the Dependent Variable"),
      shiny::selectInput(inputId = "DepVar", label = "Dependent Variables", multiple = FALSE, choices = q),
      shiny::p("Select the inputs for the Independent Variable"),
      shiny::selectInput(inputId = "IndVar", label = "Independent Variables", multiple = TRUE, choices = q)
    ),
    shiny::mainPanel(
      shiny::verbatimTextOutput(outputId = "RegSum"),
      shiny::verbatimTextOutput(outputId = "IndPrint"),
      shiny::verbatimTextOutput(outputId = "DepPrint"),
      shiny::verbatimTextOutput(outputId = "Normality"),
      shiny::verbatimTextOutput(outputId = "MeanofError"),
      shiny::verbatimTextOutput(outputId = "CIs")

    )
  )

  server <- function(input, output) {

    lm1 <- shiny::reactive({lm(reformulate(input$IndVar, input$DepVar), data = data)})





    output$DepPrint <- shiny::renderPrint({input$DepVar})
    output$IndPrint <- shiny::renderPrint({input$IndVar})
    output$RegSum <- shiny::renderPrint({summary(lm1())})
    output$Normality <- shiny::renderPrint({s20x::normcheck(lm1(),shapiro.wilk=TRUE)})

    output$MeanofError <- shiny::renderPrint({
      model=lm(reformulate(input$IndVar, input$DepVar), data = data)
      res=residuals(model)
      mean(res)
    })

    output$CIs <- shiny::renderPrint({
      model=lm(reformulate(input$IndVar, input$DepVar), data = data)
      s20x::ciReg(model)

    })


  }

  shiny::shinyApp(ui=ui,server=server)

}
