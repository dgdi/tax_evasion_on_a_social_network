#################################################################################################
#################################################################################################
########################          Tax Evasion on Social Network         #########################
#################################################################################################
#################################################################################################
########      Revenues computation in presence of imperfect preference observability     ########
#################################################################################################
#
# This script is distributed under BSD license. For more information, please check the license.txt file.
#
# This script has been tested on Win 10, R-3.5.1, RStudio-1.2.1335
#
# For any question, suggestion or comment, write to:
# 
# mail@dgdi.me

#loads libraries
library("here")
library("data.table")
library("igraph")
library("Matrix")
library("abind")
library("parallel")
library("doParallel")
library("snow")
library("foreach")
library("qdapRegex")

#sources scripts
source(here("R/fun.R"))
source(here("R/generate_BB_mod.R"))
source(here("R/parameters_ab.R"))
source(here("R/parameters_opt.R"))

#defines cores for parallel backend
cores <- 30L

#defines the number of replications for parallel backend
n_reps_pa_cores <-  n_reps_pa * cores

#sets the starting probability of audit for the optimization
p_init <- mean(c(bound_above, bound_below))

# #registers parallel backend
registerDoParallel(cores = cores)

#defines the sublinear preferential attachment exponent parameter phi
phi <- .43

#defines preferential attachment component of probabilities
pa <- (0:(n - 1)) ^ (phi)

#initializes the vector used to check for convergence in revenues for different draws of the network (pa loop)
max_pct_diff_pa_vec <-
  rep(initial_pct_diff_pa, len_pct_diff_pa_vec)

#every iteration of the loop saves a file with revenues data
for (loop_iter_pa in 1:max_iter_pa) {
  
  #every iteration of the loop computes revenues for a network drawn from the Bianconi-Barabasi generative fitness model
  revenues_pa_kappa_rep <- foreach(
    loop_reps = 1:n_reps_pa_cores,
    .combine = "acomb",
    .maxcombine = 99999,
    .multicombine = TRUE,
    .packages = c(
      "Matrix",
      "data.table",
      "foreach",
      "igraph",
      "abind"
      ),
    .export = "acomb"
  ) %dopar% {
    
    #draws a network from the Bianconi-Barabasi generative fitness model
    network_dt <- generate_BB_mod_dt_fun(
      custom_pa = pa,
      fitness = Ws,
      n = n,
      m = link_n
    )
    
    #defines the weighted network g_over starting from an undirected network
    #allowing for multiple links among nodes
    g_over <-
      network_sp_from_dt_fun(network_dt_mult = network_dt, link_n = link_n)
    
    #computes the probability s.t. evasion in the system is equal to percentage pct_evasion of total liabilities
    tryCatch(
      optimal_p_pct_evasion <- prob_pct_evasion_fun(
        pct_evasion,
        p_init,
        bound_below,
        bound_above,
        f,
        ps_network_sp = g_over,
        tWs_sp,
        Xs_sp,
        Xs,
        Ws,
        n,
        a = expected_a,
        b = expected_b,
        theta,
        inner_iter,
        outer_iter,
        eval_type,
        Tol
      ),
      
      #if there is an error in the optimization the returned p is flagged
      error = function(err) {
        optimal_p_pct_evasion <<-     c("pct_err_on_true_E_ss" = 999,
                                        "ps_pct_E" = 0)
      }
    )
    
    #sets the probability (scalar and vector)
    p_pct_evasion <- optimal_p_pct_evasion["ps_pct_E"]
    ps_pct_evasion <- rep(p_pct_evasion, n)
    
    #computes variables depending on p
    zeta <-
      (((1 - ps_pct_evasion * f) ^ 2) + (ps_pct_evasion * (1 - ps_pct_evasion) * (f ^ 2)))
    coeff_m <-
      (((1 - ps_pct_evasion * f) %*% t(1 - ps_pct_evasion * f))) / zeta
    g_over_prime <- g_over * coeff_m
    check_sp <-
      as.numeric((Diagonal(n) - g_over_prime) %*% tWs_sp)
    
    #define the data.frame to store mean revenues across iterations imperfectly observed a and b
    #(for a given drawn of the network)
    revenues_mean_ab_interations_df <-
      data.frame("revenues" = rep(0, n_kappas),
                 "kappa" = c(1, kappas))
    
    #initializes the vector used to check for convergence in revenues across multiple instances
    #of imperfectly observed a and b
    max_pct_diff_ab_vec <-
      rep(initial_pct_diff_ab, len_pct_diff_ab_vec)
    
    #every iteration of the loop checks for convergence in revenues across multiple instances
    #of imperfectly observed a and b
    for (loop_iter_ab in 1:max_iter_ab) {
      
      #every iteration of the loop computes revenues given that a noisy signal
      #of a and b (noisy_a, noisy_b) is observed by the tax authority
      revenues_reps_ab <- foreach(
        n_reps_ab_loop = 1:n_reps_ab,
        .combine = "acomb",
        .multicombine = T,
        .maxcombine = maxcombine,
        .export = "acomb"
      ) %do% {
        
        #initializes counter for the samples noisy_a and noisy_b drawn
        #(if evasion for given sample of noisy_a and noisy_b is not internal a new sample
        #will be drawn until max_tries_ab is reached)
        tries_ab <- 0
        internal <- FALSE
        
        while (!internal & tries_ab < max_tries_ab) {
          noisy_a <- runif(min =  min_a,
                           max = max_a,
                           n = n) #2
          noisy_b <- runif(min =  min_b,
                           max = max_b,
                           n = n) #80L
          coef_alpha_init <-
            (1 - (ps_pct_evasion * f)) / (noisy_a * zeta)
          
          #checks if noisy_a and noisy_b lead to internal predicted evasion
          internal <-
            check_internal_evasion_fun(
              check_sp = check_sp,
              network_sp = g_over,
              coef_alpha = coef_alpha_init,
              Xs,
              n,
              a = noisy_a,
              b = noisy_b
            )
          tries_ab <- tries_ab + 1
        }
        if (tries_ab == max_tries_ab) {
          stop("max_tries_ab has been reached")
        }
        
        #computes evasion being performed (observing expected_a, expected_b and full network information)
        evasion_true_mat_expected_ab <- evasion_fun(
          network_sp = g_over,
          ps = ps_pct_evasion,
          tWs_sp = tWs_sp,
          Xs_sp = Xs_sp,
          Xs = Xs,
          Ws = Ws,
          n = n,
          a = expected_a,
          b = expected_b,
          f = f,
          theta = theta
        )
        
        #computes predicted evasion (observing noisy_a, noisy_b and full network information)
        evasion_true_mat_noisy_ab <- evasion_fun(
          network_sp = g_over,
          ps = ps_pct_evasion,
          tWs_sp = tWs_sp,
          Xs_sp = Xs_sp,
          Xs = Xs,
          Ws = Ws,
          n = n,
          a = noisy_a,
          b = noisy_b,
          f = f,
          theta = theta
        )
        
        #computes maximum revenues (observing expected_a, expected_b and full network information)
        revenues_true_mat_expected_ab <-
          revenues_fun(
            evasion_true = evasion_true_mat_expected_ab,
            evasion_predict = evasion_true_mat_noisy_ab,
            p = p_pct_evasion,
            f = f
          )
        revenues_true_mat_expected_ab_df <- data.frame("revenues" = revenues_true_mat_expected_ab,
                                                       "kappa" = 1)
        
        #computes true declarations (observing expected_a, expected_b and full network information)
        decl <- Ws - (evasion_true_mat_expected_ab / theta)
        
        #initialize the vector to store the indexes of individuals whose info delivers progressively bigger revenue gains
        indexes_und <- c()
        
        #initialize the matrix containing the network info on individuals with progressively bigger revenue gains
        g_und_outer <-
          matrix(rep(0, n ^ 2), ncol = n, nrow = n)
        
        #starting with the information in g_und_outer it identifies and adds the row that provides the lowest revenues gain
        for (n_loop in 1:n) {
          
          #computes predicted pre-tax income based on noisy_a, noisy_b and the network information provided by g_und_outer
          pred_Ws_outer <-
            predict_income_fun(
              less_info_network = g_und_outer,
              evasion_true = evasion_true_mat_expected_ab,
              theta,
              ps_temp = ps_pct_evasion,
              f,
              n,
              Ws,
              Xs,
              a = noisy_a,
              b = noisy_b
            )
          
          #computes predicted evasion based on noisy_a, noisy_b and the network information provided by g_und_outer
          evasion_predict_outer <-
            theta * (pred_Ws_outer - decl)
          
          #computes revenues collected when auditing noisy_a, noisy_b and the network information provided by g_und_outer
          revenues_kappa_mean_outer = as.numeric(unname(
            revenues_fun(
              evasion_true = evasion_true_mat_expected_ab,
              evasion_predict = evasion_predict_outer,
              p = p_pct_evasion,
              f = f
            )
          ))
          
          #excludes from the rows to be added g_und_outer the ones already present
          if (is.null(indexes_und)) {
            row_loop_indexes_und <- 1:n
          } else{
            row_loop_indexes_und <- copy((1:n)[-indexes_und])
          }
          
          #computes revenue gains from adding to the network information any of the rows not yet in the information set
          revenues_row <- foreach(
            row_loop = row_loop_indexes_und,
            .combine = "c",
            .multicombine = T,
            .maxcombine = maxcombine
          ) %do% {
            
            #defines g_und_inner as a copy of the g_und_outer matrix at step n_loop
            g_und_inner <- copy(g_und_outer)
            
            #updates g_und_inner by adding the network information of taxpayer row_loop (from g_over)
            g_und_inner[row_loop, ] <-
              g_over[row_loop,]
            
            #computes predicted pre-tax income based on noisy_a, noisy_b and the network information of g_und_inner
            pred_Ws <-
              predict_income_fun(
                less_info_network = g_und_inner,
                evasion_true = evasion_true_mat_expected_ab,
                theta,
                ps_temp = ps_pct_evasion,
                f,
                n,
                Ws,
                Xs,
                a = noisy_a,
                b = noisy_b
              )
            
            #computes predicted evasion based on noisy_a, noisy_b and the network information of g_und_inner
            evasion_predict <-
              theta * (pred_Ws - decl)
            
            #computes revenues collected based on noisy_a, noisy_b and the network information of g_und_inner
            revenues_kappa_mean = as.numeric(unname(
              revenues_fun(
                evasion_true = evasion_true_mat_expected_ab,
                evasion_predict = evasion_predict,
                p = p_pct_evasion,
                f = f
              )
            ))
            
            #computes revenue gains attained by exploiting the network information of row row_loop
            revenues_kappa_mean - revenues_kappa_mean_outer
          }
          #stops when the revenue gain becomes positive
          if (min(revenues_row) > 0.00000000001) {
            break
          }
          
          #identifies the row providing the lower revenue gain and adds it to the adjacency matrix
          order_improv_index <- which.min(revenues_row)
          min_improv_index <-
            row_loop_indexes_und[order_improv_index]
          g_und_outer[min_improv_index, ] <-
            g_over[min_improv_index, ]
          
          #removes the current row from the ones to be added
          indexes_und <- c(indexes_und, min_improv_index)
        }
        
        #creates g_und matrix
        g_und <-
          matrix(rep(0, n ^ 2), ncol = n, nrow = n)
        g_und[indexes_und,] <-
          as.matrix(g_over)[indexes_und,]
        
        #computes revenues for different kappa of the convex optimization g_over and g_und
        revenues_kappa <-
          foreach(
            kappa_loop = kappas,
            .combine = "rbind",
            .init = revenues_true_mat_expected_ab_df,
            .multicombine = T,
            .maxcombine = maxcombine
          ) %do% {
            #convex combination of networks g_und and g_over with parameter kappa_loop
            g_k <-
              (1 - kappa_loop) * g_und + kappa_loop * g_over
            
            #computes the predicted pre-tax income based on noisy_a, noisy_b and the network information provided by g_k
            pred_Ws <-
              predict_income_fun(
                less_info_network = g_k,
                evasion_true = evasion_true_mat_expected_ab,
                theta,
                ps_temp = ps_pct_evasion,
                f,
                n,
                Ws,
                Xs,
                a = noisy_a,
                b = noisy_b
              )
            
            #computes the predicted evasion based on noisy_a, noisy_b and the network information provided by g_k
            evasion_predict <-
              theta * (pred_Ws - decl)
            
            #computes the revenues collected based on noisy_a, noisy_b and the network information provided by g_k
            revenues_kappa_loop = as.numeric(unname(
              revenues_fun(
                evasion_true = evasion_true_mat_expected_ab,
                evasion_predict = evasion_predict,
                p = p_pct_evasion,
                f = f
              )
            ))
            data.frame("revenues" = revenues_kappa_loop,
                       "kappa" = kappa_loop)
          }
        revenues_kappa
      }
      #averages the revenues across parallel replications of noisy_a, noisy_b
      revenues_mean_ab <-
        apply(revenues_reps_ab, 1:2, mean)
      
      #averages the revenues of the latest ab iteration with the previous ab iterations
      ab_convergence_vec_new <-
        revenues_mean_ab_interations_df[, 1] * (((loop_iter_ab - 1)) / loop_iter_ab) +
        revenues_mean_ab[, 1] * (1 / loop_iter_ab)
      
      #computes the maximum percentage difference in average revenues due to the latest ab iteration
      if (loop_iter_ab == 1) {
        max_pct_diff_ab <- initial_pct_diff_ab
      } else {
        max_pct_diff_ab <- max(abs((
          ab_convergence_vec_new - revenues_mean_ab_interations_df[, 1]
        ) / revenues_mean_ab_interations_df[, 1]
        )
        , na.rm = T)
      }
      
      #updates the vector of maximum percentage differences of ab revenues by
      #removing the oldest entry and adding the latest one
      max_pct_diff_ab_vec[((loop_iter_ab - 1) %% len_pct_diff_ab_vec) + 1] <-
        max_pct_diff_ab
      
      #computes the mean of the vector of maximum percentage differences of ab revenues and prints it
      max_pct_diff_ab_avg <- mean(max_pct_diff_ab_vec)
      # print(
      #   paste(
      #     "the maximum difference in the ab loop at iteration",
      #     loop_iter_ab,
      #     "is",
      #     max_pct_diff_ab_avg
      #   )
      # )
      
      #updates the data.frame storing mean revenues across iterations of ab
      revenues_mean_ab_interations_df[, 1] <-
        ab_convergence_vec_new
      
      #if the mean of the vector of maximum percentage differences is below the threshold the simulation
      #has converged and the next random network is considered
      if (max_pct_diff_ab_avg < threshold_pct_diff_ab) {
        break
      }
      
      #if the revenues for ab loop has not converged in the maximum number of iterations a warning is thrown
      if (loop_iter_ab == max_iter_ab) {
        warning(paste(
          "The maximum amount of iterations in the ab loop has been reached "
        ))
      }
    }
    revenues_mean_ab_interations_df
  }
  
  #averages the revenues across different drawns from the Bianconi-Barabasi generative fitness model
  revenues_pa_kappa_mean <-
    data.frame(apply(revenues_pa_kappa_rep, 1:2, mean))
  
  #saves ravenues data
  saveRDS(revenues_pa_kappa_mean, file = here(
    "data",
    paste0(
      "revenues_ab",
      "_phi_",
      phi,
      "_n_reps_pa_",
      n_reps_pa_cores,
      "_iteration_",
      formatC(
        loop_iter_pa,
        width = 3,
        format = "d",
        flag = "0"
      ),
      "_n_",
      n,
      "_pct_evasion_",
      pct_evasion,
      ".rds"
    )
  ))
  
  if (loop_iter_pa == 1) {
    
    #set the initial maximum percentage difference in average revenues from replication
    max_pct_diff_pa <- initial_pct_diff_pa
  
    }else{
    
    #find names of revenues data for 
    revenues_rep_pa_kappa_rds_names <-
      list.files(
        path = here("data"),
        pattern = paste0("^", "revenues_ab"),
        full.names = T
      )
    
    #loads all revenues data 
    revenues_rep_pa_kappa_tot <-
      rbindlist(lapply(revenues_rep_pa_kappa_rds_names, function(d) {
        data_temp = readRDS(d)
        data_temp$rep = as.numeric(rm_between(d, "n_reps_pa_", "_", extract =
                                                TRUE)[[1]])
        data_temp
      }))
    
    #averages all revenues data 
    revenues_rep_pa_kappa_tot_mean <-
      revenues_rep_pa_kappa_tot[, .("revenues" = weighted.mean(x = revenues, w = rep)), by = .(kappa)]$revenues
    
    #loads all revenues data excluding the latest replication
    revenues_rep_pa_kappa_notlast <-
      rbindlist(lapply(head(revenues_rep_pa_kappa_rds_names, -1), function(d) {
        data_temp = readRDS(d)
        data_temp$rep = as.numeric(rm_between(d, "n_reps_pa_", "_", extract =
                                                TRUE)[[1]])
        data_temp
      }))
    
    #averages all revenues data for excluding the latest replication
    revenues_rep_pa_kappa_notlast_mean <-
      revenues_rep_pa_kappa_notlast[, .("revenues" = weighted.mean(x = revenues, w = rep)), by = .(kappa)]$revenues
    
    #computes the maximum percentage difference in average revenues due to the latest pa iteration
    max_pct_diff_pa <- max(abs(((
      revenues_rep_pa_kappa_tot_mean - revenues_rep_pa_kappa_notlast_mean
    ) / revenues_rep_pa_kappa_tot_mean
    )), na.rm = T) * 100
  }
  
  #updates the vector of maximum percentage differences of pa revenues by
  #removing the oldest entry and adding the latest one
  max_pct_diff_pa_vec[((loop_iter_pa - 1) %% len_pct_diff_pa_vec) + 1] <-
    max_pct_diff_pa
  
  #computes the mean of the  vector of maximum percentage differences and prints it
  max_pct_diff_pa_avg <- mean(max_pct_diff_pa_vec)
  print(
    paste(
      "the maximum difference in the pa loop at iteration",
      loop_iter_pa,
      "is",
      max_pct_diff_pa_avg
    )
  )
  
  #if the mean of the vector of maximum percentage differences of pa revenues is 
  #below the threshold the simulation has converged 
  if (max_pct_diff_pa_avg < threshold_pct_diff_pa) {

    stop("Simulation revenues_ab is completed :-)")
    
  }
  
  #if the revenues for pa loop has not converged in the maximum number of iterations a warning is thrown
  if (loop_iter_pa == max_iter_pa) {
    warning("The maximum amount of iterations has been reached for pa loop")
  }
  
}

stopImplicitCluster()
