#modified version og the PAFit package generate_net function that allow for supplied fitness
generate_BB_mod<-function(N = N, num_seed = 2, multiple_node = 1, specific_start = NULL, 
                             m = 1, prob_m = FALSE, increase = FALSE, log = FALSE, no_new_node_step = NULL, 
                             m_no_new_node_step = m, custom_PA = NULL, mode = 1, alpha = 1, 
                             beta = 2, sat_at = 100, offset = 1, mode_f = mode_f, rate = 10, 
                             shape = 10, meanlog = 0, sdlog = 1, scale_pareto = 2, shape_pareto = 2,
                             lb_truncpareto = scale_pareto, ub_truncpareto = Inf, supplied_fitness= rep(1, N)){

###########################################  
  if (num_seed >= N) 
    stop("num_seed too large")
  if (num_seed < 2) 
    stop("num_seed too small")
  if (multiple_node > N - num_seed) 
    stop("Multiple node and/or num_seed are too large")
  if ((alpha < 0 && mode != 1) || (beta < 0) || (sat_at < 
                                                 0) || (rate < 0) || (shape < 0) || (m <= 0)) 
    stop("The parameters must be non-negative")
  if ((mode[1] != 1) && (mode[1] != 3) && (mode[1] != 2)) 
    stop("Mode must be 1, 2 or 3 ")
  if (!is.null(no_new_node_step)) {
    if (no_new_node_step <= 0) 
      stop("If no_new_node_step is specified, it should be a positive integer.")
  }
  num_of_edge <- num_seed - 1 + m * (N - num_seed) + ifelse(is.null(no_new_node_step), 
                                                            0, no_new_node_step) * m_no_new_node_step
  edge_list <- matrix(nrow = num_of_edge, ncol = 3, 0)
  switch(mode_f[1], gamma = {
    if (shape * rate > 0) fitness <- rgamma(N, rate = rate, 
                                            shape = shape) else fitness <- rep(1, N)
  }, log_normal = {
    fitness <- rlnorm(N, meanlog = meanlog, sdlog = sdlog)
  }, power_law = {
    fitness <- rpareto(N, scale = scale_pareto, shape = shape_pareto)
  }, truncpareto = {
    fitness <- rtruncpareto(N, lower = lb_truncpareto, upper = ub_truncpareto, shape = shape_pareto)
  }, supplied = {
    fitness <- supplied_fitness
  },  {
    stop("mode_f must be either \"gamma\", \"log_normal\", \"power_law\", \"truncpareto\" or \"supplied\" ")
  })
  names(fitness) <- as.integer(1:N)
  edge_list_index <- 1
  for (n in 2:num_seed) {
    edge_list[edge_list_index, ] <- c(n, n - 1, 0)
    edge_list_index <- edge_list_index + 1
  }
  degree <- rep(0, num_seed)
  names(degree) <- as.integer(1:num_seed)
  u <- table(edge_list[1:(edge_list_index - 1), 2])
  degree[as.character(as.integer(labels(u)[[1]]))] <- degree[as.character(as.integer(labels(u)[[1]]))] + 
    u
  P <- degree
  P[P == 0] <- offset
  seed.graph.size <- length(P)
  count <- 0
  n <- seed.graph.size
  sum_m <- 0
  current_time_step <- 0
  current_length_edge <- dim(edge_list)[1]
  i <- 1
  break_flag <- FALSE
  PA_vector <- vector()
  while (!break_flag) {
    n_old <- n
    if (!is.null(custom_PA)) {
      A <- custom_PA
      final_A <- A[length(A)]
      temp <- A[P + 1]
      temp[is.na(temp)] <- final_A
      PA_vector <- temp
      P.sum <- sum(temp * fitness[1:n_old])
      node.weights <- temp * fitness[1:n_old]/P.sum
    }
    else if (mode[1] == 1) {
      P.sum <- sum(P^alpha * fitness[1:n_old])
      node.weights <- P^alpha * fitness[1:n_old]/P.sum
    }
    else if (mode[1] == 2) {
      temp <- pmin(P, sat_at)^alpha
      P.sum <- sum(temp * fitness[1:n_old])
      node.weights <- temp * fitness[1:n_old]/P.sum
    }
    else {
      temp <- alpha * (log(P))^beta + 1
      P.sum <- sum(temp * fitness[1:n_old])
      node.weights <- temp * fitness[1:n_old]/P.sum
    }
    current_time_step <- current_time_step + 1
    for (i in 1:multiple_node) {
      if (TRUE == increase) {
        count <- count + 1
        nodes <- sort(sample(1:n_old, size = ifelse(log, 
                                                    max(round(log(count)), 1), count), prob = node.weights, 
                             replace = TRUE))
      }
      else {
        if (prob_m == TRUE) {
          num_edge_temp <- rpois(1, lambda = m)
          if (num_edge_temp > 0) 
            nodes <- sort(sample(1:n_old, num_edge_temp, 
                                 prob = node.weights, replace = TRUE))
          else nodes <- NULL
        }
        else nodes <- sort(sample(1:n_old, size = m, 
                                  prob = node.weights, replace = TRUE))
      }
      degree <- c(degree, 0)
      if (0 != length(nodes)) {
        temp <- table(nodes)
        for (i in 1:length(temp)) {
          num_edge <- as.numeric(temp[i])
          node_name <- as.integer(labels(temp[i]))
          degree[node_name] <- degree[node_name] + num_edge
        }
      }
      if (0 != length(nodes)) {
        if (edge_list_index + length(nodes) - 1 > current_length_edge) {
          edge_list <- rbind(edge_list, matrix(0, nrow = edge_list_index + 
                                                 length(nodes) - current_length_edge, ncol = 3))
          current_length_edge <- dim(edge_list)[1]
        }
      }
      if (is.null(specific_start)) {
        final_time_step <- current_time_step
      }
      else final_time_step <- max(current_time_step - 
                                    specific_start, 0)
      if (0 != length(nodes)) {
        edge_list[edge_list_index:(edge_list_index + 
                                     length(nodes) - 1), ] <- cbind(rep(n + 1, 
                                                                        length(nodes)), nodes, rep(final_time_step, 
                                                                                                   length(nodes)))
      }
      edge_list_index <- edge_list_index + length(nodes)
      n <- n + 1
      if (n == (N)) {
        break_flag <- TRUE
        break
      }
    }
    P <- degree
    P[degree == 0] <- offset
    if (!is.null(no_new_node_step)) {
      for (jjj in 1:no_new_node_step) {
        n_old <- n
        if (!is.null(custom_PA)) {
          A <- custom_PA
          final_A <- A[length(A)]
          temp <- A[P + 1]
          temp[is.na(temp)] <- final_A
          P.sum <- sum(temp * fitness[1:n_old])
          node.weights <- temp * fitness[1:n_old]/P.sum
        }
        else if (mode[1] == 1) {
          P.sum <- sum(P^alpha * fitness[1:n_old])
          node.weights <- P^alpha * fitness[1:n_old]/P.sum
        }
        else if (mode[1] == 2) {
          temp <- pmin(P, sat_at)^alpha
          P.sum <- sum(temp * fitness[1:n_old])
          node.weights <- temp * fitness[1:n_old]/P.sum
        }
        else {
          temp <- alpha * (log(P))^beta + 1
          P.sum <- sum(temp * fitness[1:n_old])
          node.weights <- temp * fitness[1:n_old]/P.sum
        }
        current_time_step <- current_time_step + 1
        if (TRUE == increase) {
          count <- count + 1
          nodes <- sort(sample(1:n_old, size = ifelse(log, 
                                                      max(round(log(count)), 1), count), prob = node.weights, 
                               replace = TRUE))
        }
        else {
          if (prob_m == TRUE) {
            num_edge_temp <- rpois(1, lambda = m)
            if (num_edge_temp > 0) 
              nodes <- sort(sample(1:n_old, num_edge_temp, 
                                   prob = node.weights, replace = TRUE))
            else nodes <- NULL
          }
          else nodes <- sort(sample(1:n_old, size = m, 
                                    prob = node.weights, replace = TRUE))
        }
        if (0 != length(nodes)) {
          temp <- table(nodes)
          for (i in 1:length(temp)) {
            num_edge <- as.numeric(temp[i])
            node_name <- as.integer(labels(temp[i]))
            degree[node_name] <- degree[node_name] + 
              num_edge
          }
        }
        if (edge_list_index + length(nodes) - 1 > current_length_edge) {
          edge_list <- rbind(edge_list, matrix(0, nrow = edge_list_index + 
                                                 length(nodes) - current_length_edge, ncol = 3))
          current_length_edge <- dim(edge_list)[1]
        }
        if (is.null(specific_start)) 
          final_time_step <- current_time_step
        else final_time_step <- max(current_time_step - 
                                      specific_start, 0)
        if (length(nodes) >= 1) 
          edge_list[edge_list_index:(edge_list_index + 
                                       length(nodes) - 1), ] <- cbind(rep(n, length(nodes)), 
                                                                      nodes, rep(final_time_step, length(nodes)))
        edge_list_index <- edge_list_index + length(nodes)
        P <- degree
        P[degree == 0] <- offset
      }
    }
  }
  if (is.null(custom_PA)) {
    if (mode[1] == 1) {
      PA_vector <- c(1, 1:max(degree))^alpha
    }
    else if (mode[1] == 2) {
      PA_vector <- pmin(c(1, 1:max(degree)), sat_at)^alpha
    }
    else {
      PA_vector <- alpha * (log(c(1, 1:max(degree))))^beta + 
        1
    }
  }
 # edge_list <- edge_list[-(edge_list_index:dim(edge_list)[1]), 
  #                       ]
  result <- list(graph = edge_list, fitness = fitness, PA = PA_vector, 
                 type = "directed")
  class(result) <- "PAFit_net"
  return(result)
}