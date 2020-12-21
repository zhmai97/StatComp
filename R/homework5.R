#' @title question1_in_HW4
#' @description The function used of question1 in HW4
#' @examples 
#' \dontrun{
#' q1<-question1d(10000)
#' }
#' @export
question1d<-function(m){
  n<-c(20,50,100,200,500)
  cv<-qnorm(.975,0,sqrt(6/n))
  sk<-function(x){
    xbar<-mean(x)
    m3<-mean((x-xbar)^3)
    m2<-mean((x-xbar)^2)
    return(m3/(m2^1.5))
  }
  p.reject<-numeric(length(n))
  for (i in 1:length(n)) {
    sktests<-numeric(m)
    for (j in 1:m) {
      x<-rbeta(n[i],2,2)
      sktests[j]<-as.integer(abs(sk(x))>=cv[i])
    }
  }
  p.reject[i]<-mean(sktests)
  p.reject
}
#' @title question2.1_in_HW4
#' @description The first function used of question2 in HW4
#' @examples 
#' \dontrun{
#' q2_1<-question2_1d(10000)
#' }
#' @export
question2_1d<-function(m){
  count5test<-function(x,y){
    X<-x-mean(x)
    Y<-y-mean(y)
    outx<-sum(X>max(Y))+sum(X<min(Y))
    outy<-sum(Y>max(X))+sum(Y<min(X))
    return(as.integer(max(c(outx,outy))>5))
  }
  n1<-n2<-c(20,500,10000)
  mu1<-mu2<-c(0,0,0)
  sigma1<-sigma2<-c(1,1,1)
  for (i in 1:length(n1)) {
    tests<-replicate(m,expr={
      x<-rnorm(n1[i],mu1[i],sigma1[i])
      y<-rnorm(n2[i],mu2[i],sigma2[i])
      x<-x - mean(x)
      y<-y - mean(y)
      count5test(x,y)
    })
    alphahat<-mean(tests)
    print(alphahat)
  }
}
#' @title question2.2_in_HW4
#' @description The second function used of question2 in HW4
#' @examples 
#' \dontrun{
#' q2_2<-question2_2d(10000)
#' }
#' @export
question2_2d<-function(m){
  Ftest<-function(x,y,n){
    X<-var(x)
    Y<-var(y)
    a<-qf(0.025,n-1,n-1)
    result<-sum(X/Y<a)+sum(X/Y>1/a)
    return(result)
  }
  n1<-n2<-c(20,500,10000)
  mu1<-mu2<-c(0,0,0)
  sigma1<-sigma2<-c(1,1,1)
  for (i in 1:length(n1)) {
    tests<-replicate(m,expr={
      x<-rnorm(n1[i],mu1[i],sigma1[i])
      y<-rnorm(n2[i],mu2[i],sigma2[i])
      Ftest(x,y,n1[i])
    })
    alphahat<-mean(tests)
    print(alphahat)
  }
}