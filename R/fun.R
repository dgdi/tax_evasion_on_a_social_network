#combine matrices into an array
acomb <- function(...) {
  return(abind(..., along = 3))
}

#pareto probability density function
ppareto_fun <- function(x, scale, shape)
{
  return(ifelse(x > scale, 1 - (scale / x) ^ shape, 0))
}

#pareto probability quantile function
qpareto_fun <- function(y, scale, shape)
{
  return(ifelse(y >= 0 & y <= 1,
         scale * ((1 - y) ^ (-1 / shape)),
         NaN))
}

#generate a Bianconi-Barabasi fitness network in data.table format
generate_BB_mod_dt_fun <- function(custom_pa, fitness, n, m) {
  net <- generate_BB_mod(
    N = n,
    num_seed = 2,
    multiple_node = 1,
    m = link_n,
    mode_f = "supplied",
    supplied_fitness = fitness,
    custom_PA = custom_pa
  )
  net_dt_mult <- data.table(i = net$graph[, 1], j = net$graph[, 2])
  net_dt_mult <- rbind(net_dt_mult, data.table("i" = 1, "j" = 2))
  setorder(net_dt_mult, i, j)
  setkey(net_dt_mult, i, j)
  return(net_dt_mult)
}

#convert a graph from multiple-links data.table format into sparse Matrix format
network_sp_from_dt_fun <-
  function(network_dt_mult, link_n) {
    network_dt_mult[, N := .N, by = i]
    network_dt_mult[, Nij := .N, by = .(i, j)]
    network_dt_mult <- unique(network_dt_mult)
    network_dt_mult[, gij := Nij / N]
    network_dt_w <- network_dt_mult[, .(i, j, gij)]
    setkey(network_dt_w, i, j)
    setorder(network_dt_w, i, j)
    #sparse adjacency matrix of the network from data.table form
    network_sp <- get.adjacency(graph.data.frame(network_dt_w),
                                sparse = T,
                                attr = 'gij')
    return(network_sp)
}

#computes evasion
evasion_fun <- function(network_sp,
                               ps,
                               tWs_sp,
                               Xs_sp,
                               Xs,
                               Ws,
                               n,
                               a,
                               b,
                               f,
                               theta) {
  #computing variables depending on p
  zeta <- (((1 - ps * f) ^ 2) + (ps * (1 - ps) * (f ^ 2)))
  coef_alpha <- (1 - (ps * f)) / (a * zeta)
  coeff_m <- (((1 - ps * f) %*% t(1 - ps * f))) / zeta
  coef_E <- (1 - (ps * f)) / zeta
  #defines M matrix from adjacency matrix G
  network_prime_sp <- network_sp * coeff_m
  #computes reference income
  R_0 <-  as.numeric(network_sp %*% Xs_sp)
  #evaluates alpha
  alpha <- coef_alpha * (b - a * (Xs - R_0))
  #evaluation of internal condition
  check_sp <-
    as.numeric((Diagonal(n) - network_prime_sp) %*% tWs_sp)
  #check if condition is met
  condition_met <- all(alpha < check_sp)
  if (!condition_met) {
    stop("not satisfying conditions")
  }
  return(as.numeric(Matrix::solve((
    Diagonal(n) - network_prime_sp
  ), alpha)))
}

#computes audit revenues given actual evasion, predicted evasion, audit probability and fine
revenues_fun <- function(evasion_true, evasion_predict, p, f) {
  evasion_predict_rank <- frank(-evasion_predict)
  audit_quantile <- quantile(1:n,
                             probs = p,
                             type = 4,
                             names = F)
  last_whole_audit <- floor(audit_quantile)
  whole_audits <- which(evasion_predict_rank <= last_whole_audit)
  revenues_whole_audits <- sum(evasion_true[whole_audits])
  partial_audit <- which(evasion_predict_rank == (last_whole_audit + 1))
  revenues_partial_audit <- (audit_quantile - last_whole_audit) *
    evasion_true[partial_audit]
  revenues_given_info <-
    (revenues_whole_audits + revenues_partial_audit) * (1 + f)
  names(revenues_given_info) <- "direct"
  return(revenues_given_info)
}

#computes the predicted pre-tax income
predict_income_fun <- function(less_info_network,
                                    evasion_true,
                                    theta,
                                    ps_temp,
                                    f,
                                    n,
                                    Ws,
                                    Xs,
                                    a,
                                    b) {
  xi <-
    ((1 - theta) * (1 - ps_temp * f)) + (theta * (1 + (f - 2) * ps_temp * f))
  decl <- Ws - (evasion_true / theta)
  nu <-
    ((((1 - ps_temp * f) %*% t(
      1 - theta * ps_temp * f
    ))) / xi) * less_info_network
  gamma <-
    (((1 + (f - 2) * ps_temp * f) * theta * a * decl + b * (1 - ps_temp * f)) /
       (a * xi)) -
    (1 - ps_temp * f) * (less_info_network %*% (theta * (1 - ps_temp * f) *
                                                  decl)) / xi
  Ws_inverse <-
    as.numeric(Matrix::solve((Diagonal(n) - nu), gamma))
  return("Ws" = Ws_inverse)
}

#checks if evasion is internal
check_internal_evasion_fun <- function(check_sp,
                                                      network_sp,
                                                      coef_alpha,
                                                      Xs,
                                                      n,
                                                      a,
                                                      b) {
  R_0 <-  as.numeric(((network_sp %*% Xs)))
  #evaluates alpha
  alpha <- coef_alpha * (b - a * (Xs - R_0))
  #checks if condition is met
  return(all(alpha < check_sp))
}

#computes probability ensuring a given level of evasion (pct_evasion)
prob_pct_evasion_fun <- function(pct_evasion,
                                         p_init,
                                         bound_below,
                                         bound_above,
                                         f,
                                         ps_network_sp,
                                         tWs_sp,
                                         Xs_sp,
                                         Xs,
                                         Ws,
                                         n,
                                         a,
                                         b,
                                         theta,
                                         inner.iter,
                                         outer.iter,
                                         eval.type,
                                         tol) {
  #defines parameters to be passed to the optimization routine
  param <- list(
    "pct_evasion" = pct_evasion,
    "f" = f,
    "ps_network_sp" = ps_network_sp,
    "tWs_sp" = tWs_sp,
    "Xs_sp" = Xs_sp,
    "Xs" = Xs,
    "Ws" = Ws,
    "n" = n,
    "a" = a,
    "b" = b,
    "theta" = theta
  )
  #defines box bounds for the probability values
  box_lb_ps <- bound_below
  box_ub_ps <- bound_above
  
  #uses the Brent method to compute probability leading to evasion level pct_evasion
  opt_sol_ps_zero <- optimize(
    f = p_loss_fun,
    lower = box_lb_ps,
    upper = box_ub_ps,
    param = param,
    tol = Tol
  )
  
  #retrieves probability
  ps_pres <- unname(opt_sol_ps_zero$minimum)
  ps_pres_vec <- rep(ps_pres, n)

  #computes true steady-state evasion induced by optimal probability
  true_E_ss <- evasion_fun(
    network_sp = ps_network_sp,
    ps = ps_pres_vec,
    tWs_sp = tWs_sp,
    Xs = Xs,
    Xs_sp = Xs_sp,
    Ws = Ws,
    n = n,
    a = a,
    b = b,
    f = f,
    theta = theta
  )
  
  #computes the relative percentage error of true evasion wrt the target evasion (espressed as a percentage of total liabilities)
  pct_err_on_true_E_ss <-
    abs(100 * ((sum(true_E_ss) - (
      sum(Ws) * theta * (pct_evasion / 100)
    )) /
      (sum(Ws) * theta * (pct_evasion / 100))))
  
  return(c("pct_err_on_true_E_ss" = pct_err_on_true_E_ss,
           "ps_pct_E" = ps_pres))
}

#computes the loss function of the optimization problem for the probability
p_loss_fun <- function(new_p, param){
  f <- param$f
  ps_network_sp <- param$ps_network_sp
  tWs_sp <- param$tWs_sp
  Xs <- param$Xs
  Xs_sp <- param$Xs_sp
  Ws <- param$Ws
  n <- param$n
  a <- param$a
  b <- param$b
  theta <- param$theta
  pct_evasion <- param$pct_evasion

  new_ps <- rep(new_p, n)
  
  #computes true evasion
  true_E_ss <- evasion_fun(
    network_sp = ps_network_sp,
    ps = new_ps,
    tWs_sp = tWs_sp,
    Xs_sp = Xs_sp,
    Xs = Xs,
    Ws = Ws,
    n = n,
    a = a,
    b = b,
    f = f,
    theta = theta
  )
  
  #computes the loss function of the optimization problem
  #squared difference between total evasion in the system and the objective evasion divided by the objective evasion, times 100
  return(((((sum(true_E_ss) - (sum(Ws) * theta * (pct_evasion / 100))))^2) / (sum(Ws) *theta * (pct_evasion / 100))) * 100)

}
