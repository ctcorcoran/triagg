
data {
  int<lower=0> N; // number of provinces
  vector[N] p; // prop of pop in each province
  vector[N] pc; // prop urban in each
  
  int<lower=0> nc; // number of urban estimates
  int cinds[nc]; // indices of them
  vector[N] cests; // estimates
  vector[N] cvars; // variances
  
  int<lower=0> nr;
  int rinds[nr];
  vector[N] rests;
  vector[N] rvars;
  
  int<lower=0> nt;
  int tinds[nt];
  vector[N] tests;
  vector[N] tvars;
  
  real alpha;
  real gamma;
 //  real lambda;
  real t;
}

parameters {
  real kappa;
  vector[N] omega_raw;
  
  real m;
  //real<lower=0> t;
}

transformed parameters{
  vector[N] omega = omega_raw * t + m;
  vector[N] eta = omega + kappa; //logit(kappa);
  vector[N] theta = logit(inv_logit(eta) .* (1-pc) + inv_logit(omega) .* pc);
}

model {
 kappa ~ normal(alpha, gamma);
 //omega ~ normal(m,t);
 omega_raw ~ std_normal();
 // t ~ normal(0, lambda);
 
 for(i in cinds){
   cests[i] ~ normal(omega[i], sqrt(cvars[i]));
 }
 
 for(i in rinds){
   rests[i] ~ normal(eta[i], sqrt(rvars[i]));
 }
 
 for(i in tinds){
   tests[i] ~ normal(theta[i], sqrt(tvars[i]));
 }
}
generated quantities{
  real country_proportion = 0;
  real country_proportion_urban = 0;
  real country_proportion_rural = 0;
  for(i in 1:N){
    country_proportion = country_proportion + inv_logit(theta[i]) * p[i];
    country_proportion_urban = country_proportion_urban + inv_logit(omega[i]) * p[i] * pc[i];
    country_proportion_rural = country_proportion_rural + inv_logit(eta[i]) * p[i] * (1-pc[i]);
  }
  country_proportion_urban = country_proportion_urban / sum(p .* pc);
  country_proportion_rural = country_proportion_rural / sum(p .* (1-pc));
}
