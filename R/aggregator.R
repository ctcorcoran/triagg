# Empirical Bayes estimate of t and m
# Y_i ~ N(\theta_i,D_i) and \theta_i ~ N(\nu,A); i.e. D_i are the standard error of the estimates, Y_i ar
# see 2.15 2.16 and 2.17 of Fay, R. E. and Herriot, R. A. (1979). Estimates of income for small place James-Stein procedures to census data. J. Amer. S

empirical_bayes <- function(y, D){
  k <- length(y)
  f <- function(A){
    vstar <- sum(y / (A + D)) / sum(1 / (A + D))
    abs(sum( (y - vstar)^2 / (A + D)) - k + 1)
  }
  t <- sqrt(optimize(f, c(0,var(y)))$minimum)
  m <- sum(y / (t^2 + D)) / sum(1 / (t^2 + D))
  return(list('m'=m, 't'=t))
}

empirical_bayes2 <- function(y, D){
  k <- length(y)
  f <- function(A){
    vstar <- sum(y / (A + D)) / sum(1 / (A + D))
    m <- (A / (A + D)) * y + (D / (A + D)) * vstar
    -sum(dnorm(y, mean = m, sd = sqrt(D), log = TRUE)) - sum(dnorm(m, mean = vstar, sd = sqrt(A), log = TRUE))
  }
  t <- sqrt(optimize(f, c(0,100*var(y)))$minimum)
  #t <- sapply(optimize(f, c(0,100*var(y))),sqrt)
  m <- sum(y / (t^2 + D)) / sum(1 / (t^2 + D))
  return(list('m'=m,'t'= t))
}

aggregator_input_data <- function(tri_consensus_output,demo_df,parameter_priors){
  # ALPHA and GAMMMA: Controls prior on urban rural ratio (kappa)
  # LAMBDA: Controls prior on how different the omegas are across district

  # Number of SNUs in Country
  demo_df <- demo_df[which(demo_df$province != 'Total'),]
  #print(demo_df)
  N <- nrow(demo_df)

  #DATA

  # Note that we prepopulate lists with a value that will map to zero under the transformation,
  # Since STAN doesn't like NAs
  zero_under_trans <- 0.5

  ests <- rep(zero_under_trans,N)
  names(ests) <- demo_df$province
  ests[tri_consensus_output$province] <- tri_consensus_output$proportion_estimate

  lower <- rep(zero_under_trans,N)
  names(lower) <- demo_df$province
  lower[tri_consensus_output$province] <- tri_consensus_output$proportion_lower

  upper <- rep(zero_under_trans,N)
  names(upper) <- demo_df$province
  upper[tri_consensus_output$province] <- tri_consensus_output$proportion_upper

  ###
  p <- demo_df$prop_of_nat_pop
  pc <- demo_df$urban_proportion

  # Urban Estimates
  nc <- nrow(tri_consensus_output)
  cinds <- which(names(ests) %in% tri_consensus_output$province)
  cests <- as.numeric(logit(ests))
  cvars <- as.numeric(((logit(upper)-logit(lower))/(2*1.96))^2)

  # No Rural Estimates
  nr <- 0
  rinds <- as.integer(c())
  rests <- rep(0,N)
  rvars <- rep(0,N)

  # No District-Level Estimates
  nt <- 0
  tinds <- as.integer(c())
  tests <- rep(0,N)
  tvars <- rep(0,N)

  ###
  data <- list(
    N=N,
    p=p,
    pc=pc,
    nc=nc,
    cinds=cinds,
    cests=cests,
    cvars=cvars,
    nr=nr,
    rinds=rinds,
    rests=rests,
    rvars=rvars,
    nt=nt,
    tinds=tinds,
    tests=tests,
    tvars=tvars,
    alpha=parameter_priors[['alpha']],
    gamma=parameter_priors[['gamma']],
    t=parameter_priors[['t']]
  )
  return(data)
}

# Process Aggregator Results to a Nice Dataframe:

process_agg_results <- function(agg_fit,demo_df,tri_consensus_output){
  country <- unique(tri_consensus_output$country)
  kp <- unique(tri_consensus_output$kp)
  demo_df <- demo_df[which(demo_df$province != 'Total'),]
  N <- nrow(demo_df)
  #
  agg_output_list <- list()
  #
  for(i in 1:N){
    omega <- as.numeric(quantile(inv_trans(rstan::extract(agg_fit)$omega[,i]),probs=c(0.025,0.5,0.975)))
    eta <- as.numeric(quantile(inv_trans(rstan::extract(agg_fit)$eta[,i]),probs=c(0.025,0.5,0.975)))
    theta <- as.numeric(quantile(inv_trans(rstan::extract(agg_fit)$theta[,i]),probs=c(0.025,0.5,0.975)))
    if(demo_df$province[i] %in% tri_consensus_output$province){
      #Could set another level here for interpolated standard errors
      has_ests <- 1
    } else {
      has_ests <- 0
    }
    new_row_u = list(country,kp,demo_df$province[i],'Province','Urban',omega[2],omega[1],omega[3],has_ests)
    new_row_r = list(country,kp,demo_df$province[i],'Province','Rural',eta[2],eta[1],eta[3],has_ests)
    new_row_t = list(country,kp,demo_df$province[i],'Province','Total',theta[2],theta[1],theta[3],has_ests)
    #
    agg_output_list <- append(agg_output_list,list(new_row_u,new_row_r,new_row_t))
    #
  }
  agg_output_df <- do.call('rbind.data.frame',agg_output_list)
  colnames(agg_output_df) <- c('country','kp','province','level','urb','proportion_estimate','proportion_lower','proportion_upper','has_ests')
  #
  agg_output_df <- merge(agg_output_df,demo_df[,c('province','prop_of_nat_pop','urban_proportion')],by='province')
  agg_output_df <- agg_output_df[,c(2:3,1,4:ncol(agg_output_df))]
  #
  country_prop <- as.numeric(quantile(rstan::extract(agg_fit)$country_proportion,probs=c(0.025,0.5,0.975)))
  country_prop_urban <- as.numeric(quantile(rstan::extract(agg_fit)$country_proportion_urban,probs=c(0.025,0.5,0.975)))
  country_prop_rural <- as.numeric(quantile(rstan::extract(agg_fit)$country_proportion_rural,probs=c(0.025,0.5,0.975)))
  #
  country_row_u <- list(country,kp,'National','National','Urban',country_prop_urban[2],country_prop_urban[1],country_prop_urban[3],2,1,
                        sum(agg_output_df$prop_of_nat_pop[agg_output_df$urb=='Urban']*agg_output_df$urban_proportion[agg_output_df$urb=='Urban']))
  country_row_r <- list(country,kp,'National','National','Rural',country_prop_rural[2],country_prop_rural[1],country_prop_rural[3],2,1,
                        sum(agg_output_df$prop_of_nat_pop[agg_output_df$urb=='Rural']*agg_output_df$urban_proportion[agg_output_df$urb=='Rural']))
  country_row_t <- list(country,kp,'National','National','Total',country_prop[2],country_prop[1],country_prop[3],2,1,
                        sum(agg_output_df$prop_of_nat_pop[agg_output_df$urb=='Total']*agg_output_df$urban_proportion[agg_output_df$urb=='Total']))
  agg_output_df <- rbind.data.frame(agg_output_df,country_row_u,country_row_r,country_row_t)
  return(agg_output_df)
}

# Wrapper for pre-processing, fitting, and post-processing aggregator
run_aggregator <- function(tri_consensus_output,demo_df,parameter_priors){
  agg_input_data <- aggregator_input_data(tri_consensus_output,demo_df,parameter_priors)

  agg_fit <- rstan::sampling(
    stan_models$aggregator,
    data=agg_input_data,
    thin=20,
    iter=iter,
    control=control
  )

  agg_out <- process_agg_results(agg_fit,demo_df,tri_consensus_output)

  return(agg_out)
}
