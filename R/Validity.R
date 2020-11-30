#' Check MLR assumptions
#'
#' This function can be used to check the assumptions of a multiple linear regression model.
#'
#' @param model1 MLR model
#'
#' @return mean of the residuals, residuals vs. fitted plot, normality check, outliers
#' @export
#'
#' @examples
#' library(s20x)
#' data(camplake.df)
#' camp.lm=lm(Length~Age+Scale.Radius,data=camplake.df)
#' validitycheck(model1=camp.lm)
#' data(books.df)
#' book.lm=lm(length~book,data=books.df)
#' validitycheck(model1=book.lm)
#'
validitycheck<-function(model1){
  library(s20x)
  #varis=colnames(df)
  #xs=varis[varis!=y]
  #model <- paste0(y,"~", paste0(xs, collapse = '+'))
  #ylm = lm(model,data=df)
  #res=residuals(ylm)
  res=residuals(model1)

  #constant variance: residuals vs. fitted
  eovcheck(model1,levene=TRUE)

  #mean of errors equal to 0
  print(c("Mean of errors",mean(res)))

  #check normality
  normcheck(model1, shapiro.wilk = TRUE)

  #outliers
  cooks20x(model1)
}
