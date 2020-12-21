#' @title question1_in_HW7
#' @description The function used of question1 in HW7
#' @examples 
#' \dontrun{
#' q1<-question1g(10000,1000,20)
#' }
#' @export
question1g<-function(N,b,x0){
  f<-function(x){
    return((1/2)*exp(abs(x)*(-1)))
  }
  rw.Metropolis<-function(sigma,x0,N){
    x<-numeric(N)
    x[1]<-x0
    u<-runif(N)
    k<-0
    for (i in 2:N) {
      y<-rnorm(1,mean=x[i-1],sd=sigma)
      if(u[i]<=(f(y)/f(x[i-1]))) x[i]<-y
      else{
        x[i]<-x[i-1]
        k<-k+1
      }
    }
    return(list(x=x,k=k))
  }
  Gelman.Rubin<-function(psi){
    psi<-as.matrix(psi)
    n<-ncol(psi)
    k<-nrow(psi)
    psi.means<-rowMeans(psi)
    B<-n*var(psi.means)
    psi.w<-apply(psi,1,"var")
    W<-mean(psi.w)
    v.hat<-W*(n-1)/n+(B/n)
    r.hat<-v.hat/W
    return(r.hat)
  }
  sigma<-c(0.3,0.5,1)
  rw1<-rw.Metropolis(sigma[1],x0,N)
  rw2<-rw.Metropolis(sigma[2],x0,N)
  rw3<-rw.Metropolis(sigma[3],x0,N)
  print(c(rw1$k,rw2$k,rw3$k))
  plot(rw1$x)
  plot(rw2$x)
  plot(rw3$x)
  X<-matrix(0,nrow = 3,ncol = N)
  X[1,]<-rw1$x
  X[2,]<-rw2$x
  X[3,]<-rw3$x
  psi<-t(apply(X,1,cumsum))
  for (i in 1:nrow(psi)) {
    psi[i,]<-psi[i,]/(1:ncol(psi))
  }
  print(Gelman.Rubin(psi))
  par(mfrow=c(2,2))
  for (i in 1:3) {
    plot(psi[i,(b+1):N],xlab = i,ylab = bquote(psi))
  }
  par(mfrow=c(1,1))
  rhat<-rep(0,N)
  for (j in (b+1):N) {
    rhat[j]<-Gelman.Rubin(psi[,1:j])
  }
  plot(rhat[(b+1):N])
  abline(h=1.2,lty=2)
}
#' @title question2_in_HW7
#' @description The function used of question2 in HW7
#' @examples 
#' \dontrun{
#' q2<-question2g(1000)
#' }
#' @export
question2g<-function(n){
  f<-function(a,k){
    pt(sqrt(a^2*(k-1)/(k-a^2)),df=k-1)-pt(sqrt(a^2*k/(k+1-a^2)),df=k)
  }
  k<-c(4:25,100,500,n)
  resolve<-numeric(length(k))
  times<-numeric(length(k))
  b0<-numeric(length(k))
  b1<-sqrt(k)
  for (i in 1:length(k)) {
    it<-0
    eps<-.Machine$double.eps^0.25
    r<-seq(b0[i],b1[i],length=4)
    y<-c(f(r[1],k[i]),f(r[2],k[i]),f(r[3],k[i]))
    if(y[1]*y[3]>0)
      stop("f does not have opposite sign at endpoints")
    while (it<1000&&abs(y[2])>eps) {
      it<-it+1
      if(y[1]*y[2]<0){
        r[3]<-r[2]
        y[3]<-y[2]
      }else{
        r[1]<-r[2]
        y[1]<-y[2]
      }
      r[2]<-(r[1]+r[3])/2
      y[2]<-f(r[2],k=k[i])
    }
    resolve[i]<-r[2]
    times[i]<-it
  }
  print(resolve)
}