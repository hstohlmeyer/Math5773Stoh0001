#' @title Check MLR assumptions
#'
#' @description This function can be used to check the assumptions of a multiple linear regression model.
#'
#' @param model1 MLR model
#'
#' @return mean of the residuals, residuals vs. fitted plot, normality check, outliers
#' @export
#'
#' @examples
#' quasar<-readxl::read_excel("C:/Users/hstoh/OneDrive - University of Oklahoma/Fall 2020/MATH 5773/Data/QUASAR.xls")
#' quasar.lm=lm(RFEWIDTH~REDSHIFT+LINEFLUX+LUMINOSITY+AB1450,data=quasar)
#' validitycheck(model1=quasar.lm)
#' bub<-readxl::read_excel("C:/Users/hstoh/OneDrive - University of Oklahoma/Fall 2020/MATH 5773/Data/BUBBLE2.xls")
#' bub.lm=lm(Density~Diameter+HeatFlux+MassFlux,data=bub)
#' validitycheck(model1=bub.lm)
validitycheck<-function(model1){
  #varis=colnames(df)
  #xs=varis[varis!=y]
  #model <- paste0(y,"~", paste0(xs, collapse = '+'))
  #ylm = lm(model,data=df)
  #res=residuals(ylm)
  res=residuals(model1)

  #constant variance: residuals vs. fitted
  s20x::eovcheck(model1,levene=TRUE)

  #mean of errors equal to 0
  print(c("Mean of errors",mean(res)))

  #check normality
  s20x::normcheck(model1, shapiro.wilk = TRUE)

  #outliers
  s20x::cooks20x(model1)
}
