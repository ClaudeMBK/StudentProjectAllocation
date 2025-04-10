#' Genetic Algorithm for Student-Project Assignment
#'
#' Solves the student-project allocation problem using a genetic algorithm.
#' The goal is to find an assignment of students to projects that minimizes 
#' the total cost.
#'
#' @param cost_matrix A square numeric matrix where entry \code{[i, j]} 
#' represents the cost of assigning student \code{i} to project \code{j}.
#' @param pop_size An integer specifying the number of individuals (assignments) 
#' in each generation. Default is 100.
#' @param generations An integer giving the number of generations to run the 
#' genetic algorithm. Default is 500.
#' @param mutation_rate A numeric value (between 0 and 1) giving the probability 
#' of mutation. Default is 0.1.
#'
#' @return A list with two elements:
#' \describe{
#'   \item{cost}{The total cost of the best assignment found.}
#'   \item{assignment}{An integer vector giving the assigned project for 
#'   each student. Each position i in the vector corresponds to student i, 
#'   and the value at that position indicates the assigned project. }
#'   }
#' 
#'
#' @examples
#' generate_matrix <- function(n) {
#' matrix(apply(matrix(1:n, n, n, byrow = TRUE), 1, sample), 
#' nrow = n, byrow = TRUE)}
#' cost <- generate_matrix(4)
#' result <- genetic_algorithm(cost)
#' print(result$cost)
#' print(result$assignment)
#'
#' @export


genetic_algorithm <- function(cost_matrix, pop_size=100, generations=500,
                              mutation_rate = 0.1) {
  
  if (!is.matrix(cost_matrix)) {
    stop("Must be a matrix")
  }
  
  if (nrow(cost_matrix) != ncol(cost_matrix)) {
    stop("Cost matrix must be square.");
  }
  
  
  n <- nrow(cost_matrix)
  
  generate_individual <- function() {
    sample(1:n)
  }
  
  fitness <- function(individual) {
    sum(sapply(1:n, function(i) cost_matrix[i, individual[i]]))
  }
  
  crossover <- function(parent1, parent2) {
    point <- sample(2:(n - 1), 1)
    child <- c(parent1[1:point], setdiff(parent2, parent1[1:point]))
    return(child)
  }
  
  mutate <- function(individual) {
    if (runif(1) < mutation_rate) {
      swap <- sample(1:n, 2)
      individual[swap] <- individual[rev(swap)]
    }
    return(individual)
  }
  
  population <- replicate(pop_size, generate_individual(), simplify = FALSE)
  
  for (gen in 1:generations) {
    scores <- sapply(population, fitness)
    ranked_population <- population[order(scores)]
    new_population <- ranked_population[1:(pop_size / 2)]
    
    while (length(new_population) < pop_size) {
      parents <- sample(ranked_population[1:(pop_size / 2)], 2)
      child <- mutate(crossover(parents[[1]], parents[[2]]))
      new_population <- c(new_population, list(child))
    }
    population <- new_population
  }
  
  best_individual <- population[[which.min(sapply(population, fitness))]]
  best_cost <- fitness(best_individual)
  
  return(list(cost = best_cost, assignment = best_individual ))
}

