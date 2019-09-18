#################################################################################################
#################################################################################################
########################          Tax Evasion on Social Network         #########################
#################################################################################################
#################################################################################################
########       Revenues computation for different preferential attachment exponent       ########
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
source(here("R/parameters_pa.R"))
source(here("R/parameters_opt.R"))

#defines cores for parallel backend
cores <- 30L

#defines the number of replications for parallel backend
n_reps_pa_cores <-  n_reps_pa * cores

#defines the vector of sublinear preferential attachment exponent parameters phi
phis <- c(0.00, .43, 1.00)

#sets the starting probability of audit for the optimization
p_init <- mean(c(bound_above, bound_below))

#registers parallel backend
registerDoParallel(cores = cores)

#every iteration of the loop uses a different preferential attachment exponent parameter phi
for (phi_loop in phis) {
  print(paste("phi =", phi_loop))
  
  #initializes the vector used to check for convergence in revenues across multiple network draws
  max_pct_diff_vec <- rep(initial_pct_diff, len_pct_diff_vec)
  
  #defines preferential attachment component of probabilities
  pa <- (0:(n - 1)) ^ (phi_loop)
  
  #every iteration of the loop saves a file with revenues data and checks for convergence
  for (loop_iter_pa in 1:max_iter_pa) {

    #every iteration of the (parallel) loop computes revenues for a network drawn from the Bianconi-Barabasi generative fitness model
    revenues_pa_kappa_rep <- foreach(
      loop_reps = 1:n_reps_pa_cores,
      .combine = "acomb",
      .maxcombine = maxcombine,
      .multicombine = TRUE,
      .packages = c("Matrix", "data.table", "foreach", "igraph")
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
          a,
          b,
          theta,
          inner_iter,
          outer_iter,
          eval_type,
          Tol
        ),
        
        #if there is an error in the optimization the returned p is flagged
        error = function(err) {
          optimal_p_pct_evasion <<- c("pct_err_on_true_E_ss" = 999,
                                      "ps_pct_E" = 0)
          
        }
      )
      
      #sets the probability (scalar and vector)
      p_pct_evasion <- optimal_p_pct_evasion["ps_pct_E"]
      ps_pct_evasion <- rep(p_pct_evasion, n)
      
      #computes evasion being performed
      evasion_true <- evasion_fun(
        network_sp = g_over,
        ps = ps_pct_evasion,
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
      
      #computes maximum revenues
      revenues_full <-
        revenues_fun(
          evasion_true = evasion_true,
          evasion_predict = evasion_true,
          p = p_pct_evasion,
          f = f
        )
      revenues_full_df <- data.frame("revenues" = revenues_full,
                                     "kappa" = 1,
                                     "phi" = phi_loop)
      #computes true declarations
      decl <- Ws - (evasion_true / theta)
      
      #initialize the vector to store the indexes of individuals whose info delivers progressively bigger revenue gains
      indexes_und <- c()
      
      #initialize the g_und_outer matrix that at every outer iteration adds network information of individuals
      #delivering progressively bigger revenue gains
      g_und_outer <-
        matrix(rep(0, n ^ 2), ncol = n, nrow = n)
      
      #starting with the information in g_und_outer it identifies and adds the row that provides the lowest revenues gain
      for (n_loop in 1:n) {
        
        #computes predicted pre-tax income based on the network information provided by g_und_outer
        pred_Ws_outer <-
          predict_income_fun(
            less_info_network = g_und_outer,
            evasion_true = evasion_true,
            theta,
            ps_temp = ps_pct_evasion,
            f,
            n,
            Ws,
            Xs,
            a,
            b
          )
        
        #computes predicted evasion based on the network information
        evasion_predict_outer <-
          theta * (pred_Ws_outer - decl)
        
        #computes revenues collected when auditing based on the network information
        revenues_kappa_loop_outer = as.numeric(unname(
          revenues_fun(
            evasion_true = evasion_true,
            evasion_predict = evasion_predict_outer,
            p = p_pct_evasion,
            f = f
          )
        ))
        
        #excludes from the rows to be added to g_und_outer the ones already present
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
          
          #computes predicted pre-tax income based on the network information of g_und_inner
          pred_Ws <-
            predict_income_fun(
              less_info_network = g_und_inner,
              evasion_true = evasion_true,
              theta,
              ps_temp = ps_pct_evasion,
              f,
              n,
              Ws,
              Xs,
              a,
              b
            )
          
          #computes predicted evasion based on the network information of g_und_inner
          evasion_predict <-
            theta * (pred_Ws - decl)
          
          #computes revenues collected when auditing based on the network information of g_und_inner
          revenues_kappa_loop = as.numeric(unname(
            revenues_fun(
              evasion_true = evasion_true,
              evasion_predict = evasion_predict,
              p = p_pct_evasion,
              f = f
            )
          ))
          
          #computes revenue gains attained by exploiting the network information of row row_loop
          revenues_kappa_loop - revenues_kappa_loop_outer
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
      revenues_pa_kappa <-
        foreach(
          kappa_loop = kappas,
          .combine = "rbind",
          .init = revenues_full_df,
          .multicombine = T,
          .maxcombine = maxcombine
        ) %do% {
          
          #convex combination of networks g_und and g_over with parameter kappa_loop
          g_k <-
            (1 - kappa_loop) * g_und + kappa_loop * g_over
          
          #computes the predicted pre-tax income based on the network information provided by g_k
          pred_Ws <-
            predict_income_fun(
              less_info_network = g_k,
              evasion_true = evasion_true,
              theta,
              ps_temp = ps_pct_evasion,
              f,
              n,
              Ws,
              Xs,
              a,
              b
            )
          
          #computes the predicted evasion based on the network information provided by g_k
          evasion_predict <-
            theta * (pred_Ws - decl)
          
          #computes the revenues collected based on the network information provided by g_k
          revenues_kappa_loop = as.numeric(unname(
            revenues_fun(
              evasion_true = evasion_true,
              evasion_predict = evasion_predict,
              p = p_pct_evasion,
              f = f
            )
          ))
          data.frame("revenues" = revenues_kappa_loop,
                     "kappa" = kappa_loop,
                     "phi" = phi_loop)
        }
      #saves revenues data
      revenues_pa_kappa
    }
    
    pbPost(
      type = c("note"),
      title = "Revenues_pa has completed one cluster loop :-)",
      body = "",
      recipients = 1,
      apikey = "o.ZMEJHUcAxqXDMNkQGl44N6MhjdK4geu0",
      devices = "uju7zIvMdIysjCdUBei0Z2",
      verbose = FALSE,
      debug = FALSE
    )
    
    #averages the revenues across different drawns from the Bianconi-Barabasi generative fitness model
    revenues_pa_kappa_mean <-
      data.frame(apply(revenues_pa_kappa_rep, 1:2, mean))
    
    #saves ravenues data
    saveRDS(revenues_pa_kappa_mean, file = here(
      "data",
      paste0(
        "revenues_pa",
        "_phi_",
        phi_loop,
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
    
    if(loop_iter_pa == 1){
      #set the initial maximum percentage difference in average revenues from replication
      max_pct_diff <- initial_pct_diff
    }else{ 
      #find names of revenues data for the phi_loop considered
      revenues_rep_pa_kappa_rds_names <-
        list.files(
          path = here("data"),
          pattern = paste0("^", "revenues_pa_phi_", phi_loop),
          full.names = T
        )
      
      #loads all revenues data for the phi_loop considered
      revenues_rep_pa_kappa_tot <-
        rbindlist(lapply(revenues_rep_pa_kappa_rds_names, function(d) {
          data_temp = readRDS(d)
          data_temp$rep = as.numeric(rm_between(d, "n_reps_pa_", "_", extract =
                                                  TRUE)[[1]])
          data_temp
        }))
      
      #averages all revenues data for the phi_loop considered
      revenues_rep_pa_kappa_tot_mean <-
        revenues_rep_pa_kappa_tot[, .("revenues" = weighted.mean(x = revenues, w = rep)), by = .(kappa)]$revenues
      
      #loads all revenues data for the phi_loop considered excluding the latest replication
      revenues_rep_pa_kappa_notlast <-
        rbindlist(lapply(head(revenues_rep_pa_kappa_rds_names, -1), function(d) {
          data_temp = readRDS(d)
          data_temp$rep = as.numeric(rm_between(d, "n_reps_pa_", "_", extract =
                                                  TRUE)[[1]])
          data_temp
        }))
      
      #averages all revenues data for the phi_loop considered excluding the latest replication
      revenues_rep_pa_kappa_notlast_mean <-
        revenues_rep_pa_kappa_notlast[, .("revenues" = weighted.mean(x = revenues, w = rep)), by = .(kappa)]$revenues
      
      #computes the maximum percentage difference in average revenues due to the latest replication
      max_pct_diff <- max(abs(((
        revenues_rep_pa_kappa_tot_mean - revenues_rep_pa_kappa_notlast_mean
      ) / revenues_rep_pa_kappa_tot_mean
      )), na.rm = T) * 100
    }
    
    #updates the vector of maximum percentage differences removing the oldest entry and adding the latest one
    max_pct_diff_vec[((loop_iter_pa - 1) %% len_pct_diff_vec) + 1] <-
      max_pct_diff
    
    #computes the mean of the vector of maximum percentage differences and prints it
    max_pct_diff_avg <- mean(max_pct_diff_vec)
    print(paste("the maximum difference at iteration", loop_iter_pa, "is", max_pct_diff_avg))
    
    #if the mean of the vector of maximum percentage differences is below the threshold the simulation
    #has converged and the next phi is considered
    if (max_pct_diff_avg < threshold_pct_diff) {
      break
    }
    
    #if the simulation have not converged before the maximum number of simulation has been performed
    #a warning is thrown
    if (loop_iter_pa == max_iter_pa) {
      warning(paste("The maximum amount of iterations has been reached for", phi_loop))
    }
  }
}



stopImplicitCluster()