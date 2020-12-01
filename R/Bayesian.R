#' Comparison of Bayesian and classical MLR
#'
#' @param model model
#' @param data dataset
#' @param alpha confidence level
#' @param burn burnin
#' @param mc mcmc
#'
#' @return Bayesian estimates, classical estimates, posterior plots and diagnostics
#' @export
#'
#' @examples
#' quasar<-readxl::read_excel("C:/Users/hstoh/OneDrive - University of Oklahoma/Fall 2020/MATH 5773/Data/QUASAR.xls")
#' Bayesian(model=RFEWIDTH~REDSHIFT+LINEFLUX+LUMINOSITY+AB1450,data=quasar,alpha=.95)
#' bub<-readxl::read_excel("C:/Users/hstoh/OneDrive - University of Oklahoma/Fall 2020/MATH 5773/Data/BUBBLE2.xls")
#' Bayesian(model=Density~Diameter+HeatFlux+MassFlux,data=bub,alpha=.90)
#'
Bayesian<-function(model,data,alpha,burn=1000,mc=10000){
  model.lm=lm(model,data=data)
  bay=MCMCpack::MCMCregress(model,data,burnin=burn,mcmc=mc)
  q<-summary(bay)
  c<-summary(model.lm)
  par(mar=rep(2,4))
  plot(bay)
  cis<-s20x::ciReg(model.lm,conf.level=alpha)
  h<-coda::HPDinterval(bay)

  posterior <- as.array(bay)
  bayesplot::color_scheme_set("purple")
  par(mfrow=c(2, 2))
  p=bayesplot::mcmc_hist(posterior)
  p

  data.mcmc = coda::as.mcmc(bay)
  d1=coda::raftery.diag(data.mcmc)
  d2=coda::autocorr.diag(data.mcmc)

  list("Bayesian quantiles and estimates"=q,
       "Highest Posterior Density intervals"=h,"Raftery diagnostic"=d1,
       "Autocorrelation diagnostic"=d2,"MLR output"=c,"CIs for MLR"=cis,"Posterior plots
       of marginal distributions for the parameters"=p)
}
