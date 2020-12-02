#' @title Bootstrap MLR
#'
#' @description This function uses bootstrapping to create estimates and confidence intervals for the parameters of a multiple linear regression model.
#'
#' @param alpha confidence level
#' @param iter iterations
#' @param model MLR model
#' @param data dataset
#'
#' @return Bootstrap beta estimates, CIs, and plots of point estimates; MLR summary and CIs
#' @export
#'
#' @examples
#' quasar<-readxl::read_excel("C:/Users/hstoh/OneDrive - University of Oklahoma/Fall 2020/MATH 5773/Data/QUASAR.xls")
#' Bootstrap(alpha=.95,iter=10000,model=RFEWIDTH~REDSHIFT+LINEFLUX+LUMINOSITY+AB1450,data=quasar)
#' bub<-readxl::read_excel("C:/Users/hstoh/OneDrive - University of Oklahoma/Fall 2020/MATH 5773/Data/BUBBLE2.xls")
#' Bootstrap(alpha=.9,iter=1000,model=Density~Diameter+HeatFlux+MassFlux,data=bub)
#'
Bootstrap<-function(alpha,iter,model,data){
  myf <- function(formula, data, indices){
    L<-data[indices,]
    fit<-lm(formula, data=L)
    return(coef(fit))
  }

  obj <-boot::boot(data=data,statistic=myf,R=iter,formula=model)

  betas<-c(1:length(obj$t0))

  for (i in betas){
    print(boot::boot.ci(obj,type="bca", index=i, conf=alpha))
  }

  for (i in betas){
    print(plot(obj,index=i))
  }

  #MLR

  model1=lm(model,data=data)
  regression<-summary(model1)
  cis<-s20x::ciReg(model1)

  list("Beta estimates"=obj,"MLR output"=regression,"CIs for MLR"=cis)
}
