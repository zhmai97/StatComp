#' @title question1_in_HW8
#' @description The function used of question1 in HW8
#' @examples 
#' \dontrun{
#' q1<-question1h(1000)
#' }
#' @export
question1h<-function(N){
  lfdata<-numeric(N/100)
  tol<-.Machine$double.eps^0.5 #convergence limit
  ini.est<-c(0.4,0.4) # initial and the latest assumption of p and q
  est.lst<-ini.est+1 # mark the estimation one process before
  database<-c(444,132,361,63)#data from the exercise,which have a order of A dot,B dot, OO and AB
  for (j in 1:N) {
    numAA<-rbinom(1,size=database[1],prob = ini.est[1]/(2-ini.est[1]-2*ini.est[2])) # number of AA
    numBB<-rbinom(1,size=database[2],prob = ini.est[2]/(2-ini.est[2]-2*ini.est[1])) # number of BB
    lf<-2*database[3]*log(1-sum(ini.est))+database[4]*log(ini.est[1]*ini.est[2])+database[1]*log(ini.est[1]*(1-sum(ini.est)))+database[1]*(ini.est[1]/(2-ini.est[1]-2*ini.est[2]))*log(ini.est[1]/(1-sum(ini.est)))+database[2]*log(ini.est[1]*(1-sum(ini.est)))+database[2]*(ini.est[2]/(2-ini.est[2]-2*ini.est[1]))*log(ini.est[2]/(1-sum(ini.est))) # calculate the likelihood function
    mu1<-(database[4]+database[1]*(2-2*ini.est[2])/(2-ini.est[1]-2*ini.est[2]))/(2*sum(database)) # update p and q
    mu2<-(database[4]+database[2]*(2-2*ini.est[1])/(2-ini.est[2]-2*ini.est[1]))/(2*sum(database))
    lfdata[j]<-lf # store the lf data
    ini.est<-c(mu1,mu2)
    if(sum(abs(ini.est-est.lst)/est.lst)<tol) break
    est.lst<-ini.est
  }
  print(list(out1=ini.est,out2=j,out3=tol))
  plot(lfdata)
}
#' @title question2_1_in_HW8
#' @description The first function used of question2 in HW8
#' @examples 
#' \dontrun{
#' q2<-question2_1h(1000)
#' }
#' @export
question2_1h<-function(N){
  attach(mtcars)
  formulas <- list(
    mpg ~ disp,
    mpg ~ I(1 / disp),
    mpg ~ disp + wt,
    mpg ~ I(1 / disp) + wt)
  datastorage<-vector(mode = "list",length = 4)
  for (i in 1:4) {
    datastorage[[i]]<-lm(formula = formulas[[i]],mtcars)
  }
  print(datastorage)
}
#' @title question2_2_in_HW8
#' @description The second function used of question2 in HW8
#' @examples 
#' \dontrun{
#' q2<-question2_2h(1000)
#' }
#' @export
question2_2h<-function(N){
  attach(mtcars)
  formulas<-list(
    mpg ~ disp,
    mpg ~ I(1 / disp),
    mpg ~ disp + wt,
    mpg ~ I(1 / disp) + wt)
  dataout<-vector(mode = "list",length = 4)
  j<-1:4
  try(lapply(mtcars,function(mt)lm(formulas[[j]],mt)))
}
#' @title question3_in_HW8
#' @description The function used of question3 in HW8
#' @examples 
#' \dontrun{
#' q3<-question3h(1000)
#' }
#' @export