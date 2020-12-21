#' @title question1_in_HW5
#' @description The function used of question1 in HW5
#' @examples 
#' \dontrun{
#' q1<-question1e(10000)
#' }
#' @export
question1e<-function(m){
  data("law",package = "bootstrap")
  n<-nrow(law)
  lsat<-law$LSAT
  gpa<-law$GPA
  theta.hat<-cor(lsat,gpa)
  theta.jack<-numeric(n)
  for (i in 1:n) {
    theta.jack[i]<-cor(lsat[-i],gpa[-i])
  }
  bias<-(n-1)*(mean(theta.jack)-theta.hat)# bias of jack-knife
  print(bias)
  se<-sqrt((n-1)*mean((theta.jack-mean(theta.jack))^2))#standard error of jack-knife
  print(se)
}
#' @title question2_in_HW5
#' @description The function used of question2 in HW5
#' @examples 
#' \dontrun{
#' q2<-question2e(2000)
#' }
#' @export
question2e<-function(B){
  data("aircondit",package = "boot")
  n<-nrow(aircondit)
  theta.boot<-function(dat,index){
    y<-dat[index,1]
    mean(y)
  }
  data1<-aircondit
  index<-numeric(n)
  for (i in 1:n) {
    index[i]<-i
  }
  dat<-cbind(data1,index)
  boot.obj<-boot(dat,statistic = theta.boot,R=2000)
  print(boot.obj)
  print(boot.ci(boot.obj,type = c("basic","norm","perc")))
  boot.BCa<-function(x,th0,th,stat,conf=.95){
    x<-as.matrix(x)
    n1<-nrow(x)
    N<-1:n1
    alpha<-(1+c(-conf,conf))/2
    zalpha<-qnorm(alpha)
    z0<-qnorm(sum(th<th0)/length(th))
    th.jack<-numeric(n1)
    for (i in 1:n1) {
      J<-N[1:(n-1)]
      th.jack[i]<-stat(x[-i,],J)
    }
    L<-mean(th.jack)-th.jack
    a<-sum(L^3)/(6*sum(L^2)^1.5)
    adj.alpha<-pnorm(z0+(z0+zalpha)/(1-a*(z0+zalpha)))
    limits<-quantile(th,adj.alpha,type = 6)
    return(list("est"=th0,"BCa"=limits))
  }
  data2<-aircondit$hours
  index1<-numeric(n)
  for (i in 1:n) {
    index1[i]<-i
  }
  x<-cbind(data2,index1)
  theta.b<-numeric(B)
  theta.hat1<-mean(data2)
  for (b in 1:B) {
    i<-sample(1:n,size = n,replace = TRUE)
    y<-aircondit$hours[i]
    theta.b[b]<-mean(y)
  }
  stat<-function(dat,index){
    mean(dat[index,1])
  }
  boot.BCa(dat,th0=theta.hat1,th=theta.b,stat = stat)
}
#' @title question3_in_HW5
#' @description The function used of question3 in HW5
#' @examples 
#' \dontrun{
#' q3<-question3e(1000)
#' }
#' @export
question3e<-function(m){
  data("scor",package = "bootstrap")
  n1<-nrow(scor)
  n2<-ncol(scor)
  x<-as.matrix(scor)
  covmatrix<-matrix(data = 0,nrow = 5,ncol = 5)
  for (i in 1:5) {
    for (j in 1:5) {
      covmatrix[i,j]<-cov(x[,i],x[,j]) 
    }
  }
  ev<-eigen(covmatrix)
  theta.hat2<-max(ev$values)/sum(ev$values)
  theta.jack2<-numeric(n1)
  for (i in 1:n1) {
    covmatrix.jack<-matrix(data = 0,nrow = 5,ncol = 5)
    for (j in 1:5) {
      for (k in 1:5) {
        covmatrix.jack[j,k]<-cov(x[-i,j],x[-i,k])
      }
    }
    ev.jack<-eigen(covmatrix.jack)
    theta.jack2[i]<-max(ev.jack$values)/sum(ev.jack$values)
  }
  bias<-(n1-1)*(mean(theta.jack2)-theta.hat2)
  print(bias)
  se<-sqrt((n1-1)*mean((theta.jack2-mean(theta.jack2))^2))
  print(se)
}
#' @title question4_in_HW5
#' @description The function used of question4 in HW5
#' @examples 
#' \dontrun{
#' q4<-question4e(1000)
#' }
#' @export
question4e<-function(m){
  data("ironslag",package = "DAAG")
  attach(ironslag)
  n<-length(ironslag$magnetic)
  e1<-e2<-e3<-e4<-numeric(n*(n-1))
  for (i in 2:n) {
    for (j in 1:(i-1)) {
      x<-chemical[-c(i,j)]
      y<-magnetic[-c(i,j)]
      J1<-lm(y~x)
      yhat1<-J1$coef[1]+J1$coef[2]*chemical[i]
      yhat2<-J1$coef[1]+J1$coef[2]*chemical[j]
      e1[(i-1)*(i-2)/2+j]<-(magnetic[i]-yhat1+magnetic[j]-yhat2)/2
      J2<-lm(y~x+I(x^2))
      yhat1<-J2$coef[1]+J2$coef[2]*chemical[i]+J2$coef[3]*chemical[i]^2
      yhat2<-J2$coef[1]+J2$coef[2]*chemical[j]+J2$coef[3]*chemical[j]^2
      e2[(i-1)*(i-2)/2+j]<-(magnetic[i]-yhat1+magnetic[j]-yhat2)/2
      J3<-lm(log(y)~x)
      logyhat1<-J3$coef[1]+J3$coef[2]*chemical[i]
      logyhat2<-J3$coef[1]+J3$coef[2]*chemical[j]
      yhat1<-exp(logyhat1)
      yhat2<-exp(logyhat2)
      e3[(i-1)*(i-2)/2+j]<-(magnetic[i]-yhat1+magnetic[j]-yhat2)/2
      J4<-lm(log(y)~log(x))
      logyhat1<-J4$coef[1]+J4$coef[2]*log(chemical[i])
      logyhat2<-J4$coef[1]+J4$coef[2]*log(chemical[j])
      yhat1<-exp(logyhat1)
      yhat2<-exp(logyhat2)
      e4[(i-1)*(i-2)/2+j]<-(magnetic[i]-yhat1+magnetic[j]-yhat2)/2
    }
  }
  c(mean(e1^2),mean(e2^2),mean(e3^2),mean(e4^2))
}