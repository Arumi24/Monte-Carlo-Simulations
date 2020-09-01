

SimulateBrownianMotion<-function(t,n)
{
	bm<-c(0,cumsum(rnorm(n,0,sqrt(t/n))))
	steps<-seq(0,t,length=n+1)
	plot(steps,bm,type="l")
}


SimulateBrownianMotionWithDrift<-function(t,n,mu,sigma)
{
	bm<-c(0,cumsum(rnorm(n,(mu*(t/n)),sqrt(sigma*(t/n)))))
	steps<-seq(0,t,length=n+1)
	plot(steps,bm,type="l")
}



GeometricBrownianMotion_Simulation<-function(nsim,t,mu,sigma,S0)
{

	gbm <- matrix(ncol = nsim, nrow = t)

  	for (simu in 1:nsim) {

    gbm[1, simu] <- S0
    for (day in 2:t) {

      epsilon <- rnorm(1)
      dt = 1 / 365
      gbm[day, simu] <- gbm[(day-1), simu] * exp((mu - sigma * sigma / 2) * dt + sigma * epsilon * sqrt(dt))
    
    	}
  	}

	gbm_df <- as.data.frame(gbm) %>%
  	mutate(ix = 1:nrow(gbm)) %>%
  	pivot_longer(-ix, names_to = 'sim', values_to = 'price')


  	ggplot(data=gbm_df,aes(x=ix, y=price, color=sim)) +
  	geom_line() +
  	theme(legend.position = 'none')


}

BrownianMotionWithDrift_Simulation<-function(nsim,t,n,mu,sigma)
{

	gbm <- matrix(ncol = nsim, nrow = n)

  	for (simu in 1:nsim) {

    	gbm[1, simu] <- 0

    	for (day in 2:n) {



      
      		gbm[day, simu] <- gbm[(day-1), simu] + rnorm(1,(mu*(t/n)),sqrt(sigma*(t/n)))
    
    	}
  	}


	gbm_df <- as.data.frame(gbm) %>%
  	mutate(ix = 1:nrow(gbm)) %>%
  	pivot_longer(-ix, names_to = 'sim', values_to = 'price')


  	ggplot(data=gbm_df,aes(x=ix, y=price, color=sim)) +
  	geom_line() +
  	theme(legend.position = 'none')

}


BrownianMotionWithDrift_Histogram<-function(nsim,t,n,mu,sigma)
{

	gbm <- matrix(ncol = nsim, nrow = n)

  	for (simu in 1:nsim) {

    	gbm[1, simu] <- 0

    	for (day in 2:n) {
      
      		gbm[day, simu] <- gbm[(day-1), simu] + rnorm(1,(mu*(t/n)),sqrt(sigma*(t/n)))
    
    	}
  	}


	gbm_df <- as.data.frame(gbm) %>%
  	mutate(ix = 1:nrow(gbm)) %>%
  	pivot_longer(-ix, names_to = 'sim', values_to = 'price')


  	ggplot(data=gbm_df,aes(x=price, color=sim)) +
  	geom_histogram(color="darkblue", fill="lightblue",bins=30)+
  	theme(legend.position = 'none')

}




BrownianMotion_Simulation<-function(nsim,t,n)
{

	gbm <- matrix(ncol = nsim, nrow = n)

  	for (simu in 1:nsim) {

    	gbm[1, simu] <- 0

    	for (day in 2:n) {

      
      		gbm[day, simu] <- gbm[(day-1), simu] + rnorm(1,0,sqrt(t/n))
    
    	}
  	}


	gbm_df <- as.data.frame(gbm) %>%
  	mutate(ix = 1:nrow(gbm)) %>%
  	pivot_longer(-ix, names_to = 'sim', values_to = 'price')


  	ggplot(data=gbm_df,aes(x=ix, y=price, color=sim)) +
  	geom_line() +
  	theme(legend.position = 'none')

}


BrownianMotion_Histogram<-function(nsim,t,n)
{

	gbm <- matrix(ncol = nsim, nrow = n)

  	for (simu in 1:nsim) {

    	gbm[1, simu] <- 0

    	for (day in 2:n) {

      
      		gbm[day, simu] <- gbm[(day-1), simu] + rnorm(1,0,sqrt(t/n))
    
    	}
  	}


	gbm_df <- as.data.frame(gbm) %>%
  	mutate(ix = 1:nrow(gbm)) %>%
  	pivot_longer(-ix, names_to = 'sim', values_to = 'price')


  	ggplot(gbm_df, aes(x=price))+
  	geom_histogram(color="darkblue", fill="lightblue",bins=30)+
  	theme(legend.position = 'none')

}






GeometricBrownianMotion_Histogram<-function(nsim,t,mu,sigma,S0)
{

	gbm <- matrix(ncol = nsim, nrow = t)

  	for (simu in 1:nsim) {

    gbm[1, simu] <- S0
    for (day in 2:t) {

      epsilon <- rnorm(1)
      dt = 1 / 365
      gbm[day, simu] <- gbm[(day-1), simu] * exp((mu - sigma * sigma / 2) * dt + sigma * epsilon * sqrt(dt))
    
    	}
  	}

	gbm_df <- as.data.frame(gbm) %>%
  	mutate(ix = 1:nrow(gbm)) %>%
  	pivot_longer(-ix, names_to = 'sim', values_to = 'price')


  	ggplot(gbm_df, aes(x=price))+
  	geom_histogram(color="darkblue", fill="lightblue",bins=30)+
  	theme(legend.position = 'none')


}


FinancialOptions<-function(initial_price,premium,expiration_date,strike_price,mu,sigma)
{

	nsim<-10000
	t<-expiration_date

	gbm <- matrix(ncol = nsim, nrow = t)

  	for (simu in 1:nsim) {

    gbm[1, simu] <- initial_price
    for (day in 2:t) {

      epsilon <- rnorm(1)
      dt = 1 / 365
      gbm[day, simu] <- gbm[(day-1), simu] * exp((mu - sigma * sigma / 2) * dt + sigma * epsilon * sqrt(dt))
    
    	}
  	}

	gbm_df <- as.data.frame(gbm) %>%
  	mutate(ix = 1:nrow(gbm)) %>%
  	pivot_longer(-ix, names_to = 'sim', values_to = 'price')


  	

  	X<-seq(0,length(gbm_df$price))

  	for(i in 1:length(gbm_df$price))
  	{
  		X[i]<-max(gbm_df$price[i]-strike_price,0)
  	}

  	return(mean(X)-premium)

}


BlackScholesOptionPricing<-function(initial_price,strike_price,expiration_date,interest_rate,volatility)
{
	t<-expiration_date/365

	alpha<-(log(strike_price/initial_price)-(interest_rate-volatility/2)*t)/sqrt(volatility)


	(initial_price*pnorm((alpha-sqrt(volatility)*t)/sqrt(t), mean = 0, sd = 1, lower.tail=FALSE))-
	(exp(-interest_rate*t)*strike_price*pnorm(alpha/sqrt(t),mean=0,sd=1,lower.tail=FALSE))


}



for(i in 2:(n+1)){
	x[i]<-x[i-1]+(x[i-1]^3+A*sin(w*T*(i-1)/n))*T/n+sigma*srt(T/n)*rnorm(1)
}


