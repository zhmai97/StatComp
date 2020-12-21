#' @title question1_in_HW3
#' @description The function used of question1 in HW3
#' @examples 
#' \dontrun{
#' q1<-question1c(10000)
#' }
#' @export
question1c<-function(m){
  theta.hat<-se<-numeric(2)
  g<-function(x){
    x^2*exp(-x^2/2)*(x>1)/(sqrt(2*pi))
  }
  x<-abs(rnorm(m))
  fg<-g(x)/(2*exp(-x^2/2)/sqrt(2*pi))
  theta.hat[1]<-mean(fg)
  se[1]<-sd(fg)
  y<-rgamma(m,3,rate = 1)
  fg1<-g(y)/(y^2*exp(-y)/2)
  theta.hat[2]<-mean(fg1)
  se[2]<-sd(fg1)
  rbind(theta.hat,se)
}
#' @title question2.1_in_HW3
#' @description The first function used of question2 in HW3
#' @examples 
#' \dontrun{
#' q2_1<-question2_1c(10000,10,2)
#' }
#' @export
question2_1c<-function(m,n,k){
  r<-m/k
  T2<-numeric(k)
  est<-matrix(0,n,2)
  g<-function(x){
    x^2*exp(-x^2/2)*(x>1)/(sqrt(2*pi))
  }
  for (i in 1:n) {
    est[i,1]<mean(g(runif(m)))
    for (j in 1:k) {
      x<-rgamma(r,3,rate = 1)
      T2[j]<-mean(g(x)/(x^2*exp(-x)))
    }
    est[i,2]<-mean(T2)
  }
  apply(est,2,mean)
  apply(est,2,sd)
}
#' @title question2.2_in_HW3
#' @description The second function used of question2 in HW3
#' @examples 
#' \dontrun{
#' q2_2<-question2_2c(10000,1.95)
#' }
#' @export
question2_2c<-function(m,x){
  Anti<-function(x,R=10000,antithetic=TRUE){
    u<-runif(R/2)
    if(!antithetic)v<-runif(R/2)else
      v<-1-u
    u<-c(u,v)
    cdf<-numeric(length(x))
    for (i in 1:length(x)) {
      g<-exp(-u-log(1+u^2))
      cdf[i]<-mean(g)
    }
    cdf
  }
  MC<-numeric(m)
  for (i in 1:m) {
    MC[i]<-Anti(x,R=10000)
  }
  print(sd(MC))
}
#' @title question3_in_HW3
#' @description The function used of question3 in HW3
#' @examples 
#' \dontrun{
#' q3<-question3c(20)
#' }
#' @export
question3c<-function(n){
  alpha<-.05
  UCL<-replicate(500,expr = {
    x<-rlnorm(n,meanlog = 0,sdlog = 2)
    mean(log(x))-qnorm(alpha/2)*sd(log(x))/sqrt(n-1)
  })
  LCL<-replicate(500,expr = {
    x<-rlnorm(n,meanlog = 0,sdlog = 2)
    mean(log(x))+qnorm(alpha/2)*sd(log(x))/sqrt(n-1)
  })
  mean(UCL>0 & LCL<0)
}
#' @title question4_in_HW3
#' @description The function used of question4 in HW3
#' @examples 
#' \dontrun{
#' q4<-question4c(20)
#' }
#' @export
question4c<-function(n){
  alpha<-.05
  UCL<-replicate(500,expr = {
    x<-rchisq(n,df=2)
    mean(x)-qnorm(alpha/2)*sd(x)/sqrt(n-1)
  })
  LCL<-replicate(500,expr = {
    x<-rchisq(n,df=2)
    mean(x)+qnorm(alpha/2)*sd(x)/sqrt(n-1)
  })
  mean(UCL>2 & LCL<2)
}