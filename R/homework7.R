#' @title question1_in_HW6
#' @description The function used of question1 in HW6
#' @examples 
#' \dontrun{
#' q1<-question1f(20,30)
#' }
#' @export
question1f<-function(n1,n2){
  x<-rnorm(n1,mean=0,sd=1)
  y<-rnorm(n2,mean = 0,sd=1)#different samples from the same distribution
  R<-2000# Replicate times
  xhat<-x-mean(x)
  yhat<-y-mean(y)#bias of each sample
  z<-c(xhat,yhat)
  K<-1:50
  reps<-numeric(R)
  outy<-numeric(R)
  for (i in 1:R) {
    k<-sample(K,size = 30,replace = FALSE)
    x1<-z[k]-mean(z)
    y1<-z[-k]-mean(z)
    outy[i]<-sum(y1<min(xhat))+sum(y1>max(xhat))
    reps[i]<-as.integer(outy[i]>=5)
  }
  alphahat<-mean(reps)
  print(alphahat)
}