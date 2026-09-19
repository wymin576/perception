rm(list = ls())
library(cmdstanr);library(tidyverse)
# data preparation
setwd("C:\\Users\\jj\\Downloads")
# importing the data from multiple sources
df <- read.csv("survey.csv") %>% 
  select(-X) 
psframe <- read.csv("psframe.csv") %>% rename(prov = unit)
province1 <- read.csv("province2.csv") %>% 
  select(prov,ratio_urban_rural,rate_coresid) 
province2 <- read.csv("province3.csv") %>% 
  select(-X) 

province <- merge(province1,province2,
                  by = 'prov') 

# 直接替换成数字,
# 数字化按照df$prov
library(labelled)
df$unit <- as.integer(as.factor(df$prov))
unique(df[,c('unit','prov')])

# 假设psframe是你的目标数据框，已有prov列
psframe$unit <- df$unit[match(psframe$prov, df$prov)]
province$unit <- df$unit[match(province$prov, df$prov)]
names(table(psframe$unit)) == names(table(province$unit))
unique(province[,c('unit','prov')])

psframe <- psframe  %>% na.omit()
province <- province %>% na.omit()


four_way_array <- array(data = psframe$freq,
                        dim = c(28, 4, 2, 2),
                        dimnames = list(
                          state = 1:28,age = 1:4,
                          gender = c("1", "2"),
                          urban = c("1", "2") ))

data_for_stan <- list(
  N = nrow(df),
  state = df$unit,
  age = df$age,
  gender = df$gender,
  urban = df$urban,
  y = df$up,
  
  ratio_urban_rural = province$ratio_urban_rural,
  per_capita = province$per_capita,
  college = province$college,
  migration = province$migration_mean,
  mot_il = province$mot_il_mean,
  fat_farmer = province$fat_rural_mean,
  status_yuv = province$status_yuv_mean,
  growth_of_gdp = province$ratio_of_gdp,
  rate_coresid = province$rate_coresid,
  area = province$area,
  
  P = four_way_array
)




# by province
apply(df %>% select(-unit),2,table)

mrp_post <- '
data {
  int<lower=0> N;
  array[N] int<lower=1, upper=28> state;
  array[N] int<lower=1, upper=4> age;
  array[N] int<lower=1, upper=2> gender;
  array[N] int<lower=1, upper=2> urban;
  array[N] int<lower=0, upper=1> y;
  
  // state-level covariates
  array[28] real ratio_urban_rural, per_capita, college, growth_of_gdp;
  array[28] real<lower=0, upper=1> rate_coresid, migration, mot_il,
  fat_farmer, status_yuv;
  
  // area mapping
  array[28] int<lower=1, upper=4> area;
  array[28, 4, 2, 2] int<lower=0> P;

}

parameters {
  // intercept
  real alpha;
  
  // fixed effects (state-level covariates)
  real beta_1, beta_2, beta_3, beta_4, beta_5,
  beta_6, beta_7, beta_8, beta_9;
  
  // random effect scales
  real<lower=0> sigma_gamma;
  real<lower=0> sigma_area;
  real<lower=0> sigma_state;
  
  // non-centered random effects
  vector[4] gamma_raw;
  vector[4] zeta_area_raw;
  vector[28] xi_state_raw;
  
  // gender / urban (sum-to-zero)
  real delta_raw;
  real epsilon_raw;
}

transformed parameters {
  vector[4] gamma;
  vector[4] zeta_area;
  vector[28] xi_state;

  vector[2] delta;
  vector[2] epsilon;

  vector[28] beta_state;

  // non-centered transform
  gamma = gamma_raw * sigma_gamma;
  gamma = gamma - mean(gamma);   // 👈 就放在这里

  zeta_area = zeta_area_raw * sigma_area;
  xi_state = xi_state_raw * sigma_state;

  // sum-to-zero coding
  delta[1] =  delta_raw;
  delta[2] = -delta_raw;

  epsilon[1] =  epsilon_raw;
  epsilon[2] = -epsilon_raw;

  // state-level predictor
  for (s in 1:28) {
    beta_state[s] =
      ratio_urban_rural[s]*beta_1 +
      per_capita[s]*beta_2 +
      college[s]*beta_3 +
      migration[s]*beta_4 +
      mot_il[s]*beta_5 +
      fat_farmer[s]*beta_6 +
      status_yuv[s]*beta_7 +
      growth_of_gdp[s]*beta_8 +
      rate_coresid[s]*beta_9 +
      zeta_area[area[s]] +
      xi_state[s];
  }
}

model {
  vector[N] lp;
  
  // ---- individual-level predictor ----
  lp = alpha
     + gamma[age]
     + delta[gender]
     + epsilon[urban]
     + beta_state[state];
  
  // likelihood
  y ~ bernoulli_logit(lp);
  
  // ---- priors ----
    alpha ~ normal(0, 2.5);
  
  beta_1 ~ normal(0, 2.5);
  beta_2 ~ normal(0, 2.5);
  beta_3 ~ normal(0, 2.5);
  beta_4 ~ normal(0, 2.5);
  beta_5 ~ normal(0, 2.5);
  beta_6 ~ normal(0, 2.5);
  beta_7 ~ normal(0, 2.5);
  beta_8 ~ normal(0, 2.5);
  beta_9 ~ normal(0, 2.5);
  
  // non-centered priors
  gamma_raw ~ normal(0, 1);
  zeta_area_raw ~ normal(0, 1);
  xi_state_raw ~ normal(0, 1);
  
  // scales
  sigma_gamma ~ exponential(1);
  sigma_area  ~ exponential(1);
  sigma_state ~ exponential(1);
  
  // fixed effects
  delta_raw ~ normal(0, 1);
  epsilon_raw ~ normal(0, 1);
}

generated quantities {

  // -------- posterior predictive（保留你已有的）--------
  vector[N] predictor_sim;
  array[N] int<lower=0, upper=1> y_sim;

  for (i in 1:N) {
    predictor_sim[i] =
      alpha +
      gamma[age[i]] +
      delta[gender[i]] +
      epsilon[urban[i]] +
      beta_state[state[i]];
  }

  y_sim = bernoulli_logit_rng(predictor_sim);

  // -------- MRP: state-level estimates --------
  vector[28] state_est;
  
  for (s in 1:28) {
    real weighted_sum = 0;
    real total_pop = 0;

    for (a in 1:4) {
      for (g in 1:2) {
        for (u in 1:2) {

          real eta =
            alpha +
            gamma[a] +
            delta[g] +
            epsilon[u] +
            beta_state[s];

          real p = inv_logit(eta);

          weighted_sum += P[s,a,g,u] * p;
          total_pop += P[s,a,g,u];
        }
      }
    }

    state_est[s] = total_pop > 0 ? weighted_sum / total_pop : 0;
  }

  // -------- MRP: national estimate --------
  real national_est;
  real total_pop_all = 0;
  real weighted_sum_all = 0;

  for (s in 1:28) {
    for (a in 1:4) {
      for (g in 1:2) {
        for (u in 1:2) {

          real eta =
            alpha +
            gamma[a] +
            delta[g] +
            epsilon[u] +
            beta_state[s];

          real p = inv_logit(eta);

          weighted_sum_all += P[s,a,g,u] * p;
          total_pop_all += P[s,a,g,u];
        }
      }
    }
  }

  national_est = total_pop_all > 0 ? weighted_sum_all / total_pop_all : 0;
}

'
write(mrp_post, file = 'mrp_post.stan')

mod <- cmdstan_model("mrp_post.stan")

fit.bb <- mod$sample(data = data_for_stan,   
                     seed = 123, chains = 4,
                     parallel_chains = 4,
                     iter_warmup = 1500,
                     iter_sampling = 1500)

# 保存R对象为RDS文件
# setwd("C:\\Users\\Administrator\\Downloads")
fit.bb$save_object("poststratification_norm_chat_28.rds")