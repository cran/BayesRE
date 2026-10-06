data {
  int<lower=0> N;
  vector[N] yield;
  vector[N] temp;
  vector[N] precip;
}
parameters {
  real alpha;
  real beta_temp;
  real beta_precip;
  real<lower=0> sigma;
}
model {
  alpha ~ normal(0, 100);
  beta_temp ~ normal(0, 100);
  beta_precip ~ normal(0, 100);
  sigma ~ exponential(1);
  yield ~ normal(alpha + beta_temp * temp + beta_precip * precip, sigma);
}
generated quantities {
  vector[N] y_pred;
  for (i in 1:N)
    y_pred[i] = normal_rng(alpha + beta_temp * temp[i] + beta_precip * precip[i], sigma);
}
