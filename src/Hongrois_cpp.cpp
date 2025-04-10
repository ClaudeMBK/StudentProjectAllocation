#include <iostream>
#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <numeric>
#include <random>

using namespace Rcpp;

// Hungarian Algorithm Implementation - Returns total cost
//' @title Hungarian Algorithm - Total Cost Calculation
//' @description
//' Computes the total minimum cost of an assignment using the Hungarian algorithm
//' (also known as the Kuhn-Munkres algorithm). This version returns only the cost,
//' not the specific assignment.
//'
//' @param matrix A square numeric matrix representing the cost of assigning row \code{i} to column \code{j}.
//'
//' @return A single numeric value representing the total minimal assignment cost.
//'
//' @examples
//' cost_matrix <- matrix(c(5, 9, 1, 3, 2, 4, 8, 7, 6), nrow = 3)
//' hungarian_cost_cpp(cost_matrix)
//'
//' @export
// [[Rcpp::export]]
double hungarian_cost_cpp(NumericMatrix matrix) {
  
  if (matrix.nrow() != matrix.ncol()) {
    stop("Cost matrix must be square.");
  }
  
  int n = matrix.nrow();
  NumericVector u(n);  // Labels for rows
  NumericVector v(n);  // Labels for columns
  
  // Initialize to 0 (NumericVector is already initialized to 0 by default)
  for (int i = 0; i < n; i++) {
    u[i] = 0.0;
    v[i] = 0.0;
  }
  
  // Copy the matrix to avoid modifying the original
  NumericMatrix mat = clone(matrix);
  
  // Step 1: Subtract the row minimum from each row
  for (int i = 0; i < n; i++) {
    double min_row = mat(i, 0);
    for (int j = 1; j < n; j++) {
      min_row = std::min(min_row, mat(i, j));
    }
    for (int j = 0; j < n; j++) {
      mat(i, j) -= min_row;
    }
    u[i] = min_row;
  }
  
  // Step 2: Subtract the column minimum from each column
  for (int j = 0; j < n; j++) {
    double min_col = mat(0, j);
    for (int i = 1; i < n; i++) {
      min_col = std::min(min_col, mat(i, j));
    }
    for (int i = 0; i < n; i++) {
      mat(i, j) -= min_col;
    }
    v[j] = min_col;
  }
  
  // Compute the total cost based on labels
  double total_cost = sum(u) + sum(v);
  
  return total_cost;
}

//' @title Hungarian Algorithm - Optimal Assignment
//' @description
//' Solves the assignment problem using the Hungarian algorithm and returns
//' the optimal assignment vector that minimizes the total cost.
//'
//' The algorithm finds a perfect matching in a bipartite graph with minimum total weight,
//' based on the input cost matrix.
//'
//' @param cost A square numeric matrix representing the cost of assigning row \code{i} to column \code{j}.
//'
//' @return An integer vector of length \code{n}, where the \code{i}-th element indicates
//' the column assigned to row \code{i}.
//'
//' @examples
//' cost_matrix <- matrix(c(5, 9, 1, 3, 2, 4, 8, 7, 6), nrow = 3)
//' hungarian_assignment_cpp(cost_matrix)
//'
//' @export
// [[Rcpp::export]]
IntegerVector hungarian_assignment_cpp(NumericMatrix cost) {
  
  if (cost.nrow() != cost.ncol()) {
    stop("Cost matrix must be square.");
  }
  int n = cost.nrow();
  std::vector<double> u(n + 1, 0), v(n + 1, 0);
  std::vector<int> p(n + 1, 0), way(n + 1, 0);
  IntegerVector assignment(n);
  
  for (int i = 1; i <= n; i++) {
    p[0] = i;
    int j0 = 0;
    std::vector<double> minv(n + 1, INFINITY);
    std::vector<char> used(n + 1, false);
    
    do {
      used[j0] = true;
      int i0 = p[j0], j1 = 0;
      double delta = INFINITY;
      
      for (int j = 1; j <= n; j++) {
        if (!used[j]) {
          double cur = cost(i0 - 1, j - 1) - u[i0] - v[j];
          if (cur < minv[j]) {
            minv[j] = cur;
            way[j] = j0;
          }
          if (minv[j] < delta) {
            delta = minv[j];
            j1 = j;
          }
        }
      }
      
      for (int j = 0; j <= n; j++) {
        if (used[j]) {
          u[p[j]] += delta;
          v[j] -= delta;
        } else {
          minv[j] -= delta;
        }
      }
      j0 = j1;
    } while (p[j0] != 0);
    
    // Update the matching
    do {
      int j1 = way[j0];
      p[j0] = p[j1];
      j0 = j1;
    } while (j0);
  }
  
  // Build the final assignment vector
  for (int j = 1; j <= n; j++) {
    assignment[p[j] - 1] = j;
  }
  
  return assignment;  // Return with R-style 1-based indexing
}
