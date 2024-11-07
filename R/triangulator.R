rsq_lambda <- function(y,sigma,theta,tau){
  #y and sigma are N-dimensional vectors
  #theta,tau are the n_samp-dimensional results from the sampling
  N <- length(y)
  n_samp <- length(theta)
  #
  y_mat <- matrix(rep(y,n_samp),ncol=N,byrow=TRUE)
  sigma_sq_mat <- matrix(rep(sigma^2,n_samp),ncol=N,byrow=TRUE)
  tau_sq_mat <- matrix(rep(tau^2,N),ncol=N,byrow=FALSE)
  theta_mat <- matrix(rep(theta,N),ncol=N,byrow=FALSE)
  #
  w_mat <- sigma_sq_mat/(tau_sq_mat + sigma_sq_mat)
  ##
  V_E <- var(colMeans(w_mat*(y_mat-theta_mat)))
  #
  E_V_2 <- mean(apply(w_mat*(y_mat-theta_mat),2,var))
  E_V_3 <- mean(colMeans((1-w_mat)*sigma_sq_mat))
  E_V <- V_E+E_V_2+E_V_3

  ##
  lambda <- 1-V_E/E_V
  Rsq <- 1-E_V/var(y)

  return_list <- list(lambda,Rsq)
  names(return_list) <- c("lambda","Rsq")
  return(return_list)
}

# Run triangulator, given a single province

run_triangulator <- function(area_ests,prior_list){
  ### Stan Controls:
  control = list(adapt_delta=0.95,max_treedepth=12)
  iter=8000

  #Data for Triangulator
  data <- list(
    N = nrow(area_ests),
    yhat = area_ests$logit_est,
    sigma = area_ests$SE_logit_est,
    conf = pmax(area_ests$confidence,1e-6)/100,
    prior_med = prior_list[['med']],
    prior_sd = prior_list[['SE']],
    var_yhat = var(area_ests$logit_est),
    multi=0.1
  )

  # If there is only one estimate, there can be some datatype discrepancies to fix:
  if(nrow(area_ests)==1){
    data$yhat <- array(data$yhat,dim=1)
    data$sigma <- array(data$sigma,dim=1)
    data$conf <- array(data$conf,dim=1)
    data$var_yhat <- data$prior_sd
  }

  #Fit Model
  browser()
  fit <- rstan::sampling(stanmodels$triangulator,
                  data=data,
                  iter=iter,
                  control=control)

  return(list(fit=fit,data=data))
}

# Run Triangulator for all provinces

triangulate <- function(kp_df,tri_priors_df){
  country <- unique(kp_df$country)
  kp <- unique(kp_df$kp)
  #
  out_rows <- list()
  prior_rows <- list()
  areas <- unique(kp_df$province)

  # Transform Estimates:
  kp_df$logit_est <- logit(kp_df$proportion_estimate)

  # Compute the SE based on the upper and lower bounds; use delta method if the interval is symmetric
  kp_df$CI_symmetric <- ifelse(abs((kp_df$proportion_upper-kp_df$proportion_estimate)/(kp_df$proportion_estimate-kp_df$proportion_lower)-1)<0.05,TRUE,FALSE)
  kp_df$SE_logit_est <- ifelse(kp_df$CI_symmetric,
                               (1/(kp_df$proportion_estimate*(1-kp_df$proportion_estimate)))*(kp_df$proportion_upper-kp_df$proportion_lower)/(2*1.96),
                               (logit(kp_df$proportion_upper)-logit(kp_df$proportion_lower))/(2*1.96))

  for(a in areas){
    area_ests <- kp_df[kp_df$province==a,]

    # Get prior info
    tr_prior_med = logit(tri_priors_df$prior_med[tri_priors_df$province==a])
    tr_prior_SE = (logit(tri_priors_df$prior_q75[tri_priors_df$province==a])-logit(tri_priors_df$prior_med[tri_priors_df$province==a]))/qnorm(0.75)

    # Run the model
    t_out <- run_triangulator(area_ests,list('med'=tr_prior_med,'SE'=tr_prior_SE))
    fit <- t_out$fit
    data <- t_out$data

    # Summarize
    out <- unname(quantile(inv_trans(rstan::extract(fit)$theta),probs=c(0.025,0.5,0.975)))
    var_unexp <- max(rsq_lambda(data$yhat,data$sigma/data$conf, rstan::extract(fit)$theta, rstan::extract(fit)$tau)$Rsq,0.0)

    # Make new row for consensus result
    new_row <- list(country,kp,NA,NA,'Consensus',NA,NA,a,out[2],out[1],out[3],var_unexp)
    out_rows[[length(out_rows)+1]] <- new_row

    # Add row for prior
    prior_row <- list(country,kp,NA,NA,'Prior',NA,NA,a,expit(tr_prior_med),expit(tr_prior_med-1.96*tr_prior_SE),expit(tr_prior_med+1.96*tr_prior_SE),NA,NA) #NA,NA,
    prior_rows[[length(prior_rows)+1]] <- prior_row
  }

  # Add prior rows to the kp_df, subsetted for output columns
  kp_out_cols <- c('country','kp','study_idx','observation_idx','method','year','area_name','province','proportion_estimate','proportion_lower','proportion_upper','confidence','SE_interpolated') #'conf_lower','conf_upper',
  prior_df <- do.call('rbind.data.frame',prior_rows)
  colnames(prior_df) <- kp_out_cols

  full_df <- bind_rows(kp_df[,kp_out_cols],prior_df)

  # Make df of consensus rows
  out_df <- do.call('rbind.data.frame',out_rows)
  colnames(out_df) <- c(colnames(full_df)[1:11],'var_unexp')

  full_df <- bind_rows(full_df,out_df[,colnames(out_df)!='var_unexp'])
  rownames(full_df) <- 1:nrow(full_df)

  # Make some display columns for consensus df
  out_df$display_proportion_estimate <- paste0(round(100*out_df$proportion_estimate,2),'%')
  out_df$display_CI <- paste0('(',round(100*out_df$proportion_lower,2),'%, ',round(100*out_df$proportion_upper,2),'%)')

  return(list(full_df = full_df,consensus_df = out_df))
}
