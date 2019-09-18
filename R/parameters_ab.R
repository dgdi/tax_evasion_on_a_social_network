#display options
options(scipen = 999)

#foreach options
maxcombine = 10000L

#random seed setting
set.seed(25112017)

#sets parameters to produce noise in a,b
pct_noise <- 5
expected_a = 2
expected_b = 80
min_a <- expected_a - (expected_a * pct_noise/100)
max_a <- expected_a + (expected_a * pct_noise/100)
min_b <- expected_b - (expected_b * pct_noise/100)
max_b <- expected_b + (expected_b * pct_noise/100)

# model parameters settings ###
link_n <- 5
n <- 200
a <- 2
b <- 80
f <- 1.75
theta <- .3
maxW <- max(b / a)
lb <- ppareto_fun(maxW * (1 / 8), scale = maxW * (1 / 8), shape = 2L)
ub <- ppareto_fun(maxW, scale = maxW * (1 / 8), shape = 2L)
Ws  <- qpareto_fun(seq(from = ub,  to = lb, length.out = n), scale  = maxW * (1 / 8), shape  = 2L)
Xs <- (1 - theta) * Ws
Xs_sp <- as(Xs, "sparseVector")
tWs_sp <- Matrix(theta * Ws, sparse = T)


n_kappas <- 500
kappas <- seq(
  from = 0,
  to = 1,
  length.out = n_kappas
)[-n_kappas]

#sets bound for p optimization
bound_below <- .5620
bound_above <- .5780

#sets level of evasion
pct_evasion <- 10L

#sets replications parameters
max_iter_pa <- 1000L
n_reps_pa <-  10L
max_iter_ab <- 1000L
n_reps_ab <-  2L


#sets parameters for stopping criterion
threshold_pct_diff_pa <- .01
initial_pct_diff_pa <- threshold_pct_diff_pa + 5
len_pct_diff_pa_vec <- 5

threshold_pct_diff_ab <- .01
initial_pct_diff_ab <- threshold_pct_diff_ab + 5
len_pct_diff_ab_vec <- 5

#sets number of tries to be performed
max_tries_ab <- 1000L
max_tries_p <- 1000L





