#' @title question1_in_HW2
#' @description The function used of question1 in HW2
#' @examples 
#' \dontrun{
#' q1<-question1b(10000)
#' }
#' @export
question1b<-function(m){
  x<-runif(m,min=0,max=pi/3)
  theta.hat<-mean(sin(x))*pi/3
  thetas<-sin(x)*pi/3
  se<-sd(thetas)/sqrt(m)
  round(c(theta.hat-1.96*se,theta.hat+1.96*se),4)
}
#' @title question2_in_HW2
#' @description The function used of question2 in HW2
#' @examples 
#' \dontrun{
#' q2<-question2b(1000,1.95)
#' }
#' @export
MC.phi<-function(x,R=10000,antithetic=TRUE){
  u<-runif(R/2)
  if(!antithetic) v<-runif(R/2)
  else v<-1-u
  u<-c(u,v)
  cdf<-numeric(length(x))
  for(i in 1:length(x)){
    g<-exp(u*x[i])
    cdf[i]<-mean(g)
  }
  cdf
}
question2b<-function(m,x){
  MC1<-MC2<-numeric(m)
  for(i in 1:m){
    MC1[i]<-MC.phi(x,R=1000,antithetic=FALSE)
    MC2[i]<-MC.phi(x,R=1000)
  }
  print(sd(MC1))
  print(sd(MC2))
  print((var(MC1)-var(MC2))/var(MC1))
}