# Asset Pricing tw-variable SVAR
# distinsguishing UK Demand and UK risk premium 

library(bsvarSIGNs)
library(bsvars)


mat <- df.wide |> 
  mutate(yieldgap10 = c(NA, diff(gtgbp10yr_corp - usgg10yr_index  )),
         gbpusd     = c(NA, diff(log(gbpusd_curncy)))) |> 
  select(gbpusd, yieldgap10) |> 
  as.matrix()
mat <- mat[complete.cases(mat), ]

# specify identifying restrictions: 2 shocks, Table 1
sign_irf       = matrix(
  c(1, 1,     # Positive UK Demand
    -1,  1), 2,2)   # Higher Sterling risk premium

# specify the model
specification  = specify_bsvarSIGN$new(mat,
                                       p        = 4,
                                       sign_irf = sign_irf,
                                       stationary = rep(TRUE, ncol(mat) )) # prior with zero mean for stationary data

# estimate hyper-parameters
final_no_draws = 1000 # final no of posterior draws [use 10,000 for final run]
specification$prior$estimate_hyper(S=1000, 
                                   burn_in = 250, # S/2
                                   mu = TRUE,
                                   delta = TRUE, lambda = TRUE, psi = TRUE)

# estimate the model
posterior      = estimate(specification, S = final_no_draws)

# compute and plot impulse responses
irf            = compute_impulse_responses(posterior, horizon = 40)
plot(irf, probability = 0.68)

# compute draws from in-sample predictive density
fitted = compute_fitted_values(posterior)

# compute historical decompositions
hd = compute_historical_decompositions(posterior)

# compute structural shocks
shocks = compute_structural_shocks(posterior)

# compute forecast error variance decomposition 2 years ahead
fevd = compute_variance_decompositions(posterior, horizon = 10)

# sample from predictive density 5d ahead
predictive = forecast(posterior, 5)

# HISTORUICAL DECOMPOSITION
# dim(hd) = variables x shocks x T x boostrap simulations (conf int)
# matrix[i, j, t, r]: contr of the jth shock to the ith variable, at time 't' in the 'rth' simulation

# Analysing the Historcial Decomposition
# decomposing EU10y: median simulation 

# GBPUSD, 10y Yield GAp
var1 <- hd[1, , , ] # k x T x r - GBPUSD: 5 x t x r
var2 <- hd[2, , , ] # k x T x r - 10y yeild gao


