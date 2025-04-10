#' @title Hungarian Algorithm - Total Cost Calculation
#' @description
#' Computes the total minimum cost of an assignment using the Hungarian algorithm.
#' This function applies row and column reductions to estimate the optimal assignment cost.
#'
#' @param matrix A square numeric matrix representing the cost of assigning row \code{i} to column \code{j}.
#'
#' @return A numeric value: the minimal total assignment cost.
#'
#' @examples
#' mat <- matrix(c(4, 2, 8, 7, 6, 3, 1, 9, 5), nrow = 3)
#' hungarian_cost(mat)
#'
#' @export
hungarian_cost <- function(matrix) {
  if (nrow(matrix) != ncol(matrix)) {
    stop("Cost matrix must be square.");
  }
  
  n <- nrow(matrix)
  u <- rep(0, n)  # Labels for rows
  v <- rep(0, n)  # Labels for columns
  mat <- matrix
  
  # Step 1: Subtract the minimum from each row
  for (i in 1:n) {
    min_row <- min(mat[i, ])
    mat[i, ] <- mat[i, ] - min_row
    u[i] <- min_row
  }
  
  # Step 2: Subtract the minimum from each column
  for (j in 1:n) {
    min_col <- min(mat[, j])
    mat[, j] <- mat[, j] - min_col
    v[j] <- min_col
  }
  
  # Total cost is the sum of all labels
  total_cost <- sum(u) + sum(v)
  return(total_cost)
}

#' @title Hungarian Algorithm - Optimal Assignment
#' @description
#' Solves the assignment problem using the Hungarian algorithm and returns
#' the optimal assignment vector that minimizes total cost.
#'
#' This function implements a matrix-based version of the algorithm that finds
#' a minimum-cost perfect matching.
#'
#' @param cost A square numeric matrix representing the cost of assigning row \code{i} to column \code{j}.
#'
#' @return An integer vector indicating the column assigned to each row.
#'
#' @examples
#' mat <- matrix(c(4, 2, 8, 7, 6, 3, 1, 9, 5), nrow = 3)
#' hungarian_assignment(mat)
#'
#' @export
hungarian_assignment <- function(cost) {
  if (nrow(cost) != ncol(cost)) {
    stop("Cost matrix must be square.");
  }
  n <- nrow(cost)
  u <- numeric(n + 1)      # Row labels (including 0-indexed position)
  v <- numeric(n + 1)      # Column labels
  p <- integer(n + 1)      # Matching path
  way <- integer(n + 1)    # Path tracking
  assignment <- integer(n) # Final assignment
  
  for (i in 1:n) {
    p[1] <- i  # Corresponds to p[0] = i in C++
    j0 <- 0
    minv <- rep(Inf, n + 1)
    used <- rep(FALSE, n + 1)
    
    repeat {
      used[j0 + 1] <- TRUE  # +1 because R indexing starts at 1
      i0 <- p[j0 + 1]
      delta <- Inf
      j1 <- 0
      
      for (j in 1:n) {
        if (!used[j + 1]) {
          cur <- cost[i0, j] - u[i0 + 1] - v[j + 1]
          if (cur < minv[j + 1]) {
            minv[j + 1] <- cur
            way[j + 1] <- j0
          }
          if (minv[j + 1] < delta) {
            delta <- minv[j + 1]
            j1 <- j
          }
        }
      }
      
      # Update labels
      for (j in 0:n) {
        if (used[j + 1]) {
          u[p[j + 1] + 1] <- u[p[j + 1] + 1] + delta
          v[j + 1] <- v[j + 1] - delta
        } else {
          minv[j + 1] <- minv[j + 1] - delta
        }
      }
      j0 <- j1
      
      if (p[j0 + 1] == 0) break
    }
    
    # Update the path
    repeat {
      j1 <- way[j0 + 1]
      p[j0 + 1] <- p[j1 + 1]
      j0 <- j1
      if (j0 == 0) break
    }
  }
  
  # Build assignment vector
  for (j in 1:n) {
    assignment[p[j + 1]] <- j - 1  # -1 for 0-based adjustment
  }
  
  return(assignment + 1)  # +1 to return 1-based indexing for R
}
