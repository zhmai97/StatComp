#' @title question1_in_HW1
#' @description The function used of question1 in HW1
#' @examples 
#' \dontrun{
#' q1<-question1a(1000)
#' }
#' @export
question1a<-function(N){
  u<-runif(N)
  x<-2/((1-u)^(1/2)) #F(x)=1-(2/x)^2 x>=2
  hist(x,prob=TRUE, main=expression(f(x)==8*x^(-3)))
  y<-seq(0,20,.02)
  lines(y,8*y^(-3))
}

#' @title question2_in_HW1
#' @description The function used of question2 in HW1
#' @examples 
#' \dontrun{
#' q2<-question2a(1000)
#' }
#' @export
question2a<-function(N){
  ab<-replicate(N,expr = {
    U1<-runif(1,-1,1)
    U2<-runif(1,-1,1)
    U3<-runif(1,-1,1)
    if(abs(U3)>=abs(U2)&abs(U3)>=abs(U1)){
      x2<-U2
    }else{x2<-U3}
    x2})
  hist(ab,prob=TRUE)
}

#' @title question4_in_HW1
#' @description The function used of question4 in HW1
#' @examples 
#' \dontrun{
#' tm1<-microbenchmark::microbenchmark(
#'   q4<-question4a(1000)
#' )
#' }
#' @export
question4a<-function(N){
  u<-runif(N)
  x<-2/((1-u)^(1/4))-2 #F(x)=1-(2/(x+2))^4 x>=0
  hist(x,prob=TRUE, main=expression(f(x)==64*(x+2)^(-5)))
  y<-seq(0,10,.01)
  lines(y,8*(y+2)^(-5))
}