\name{StudentProjectAllocation-package}
\alias{StudentProjectAllocation-package}
\alias{StudentProjectAllocation}
\docType{package}
\title{
  Student Project Allocation using Optimization Algorithms
}
\description{
  This package provides methods for allocating students to projects with 
  the Hungarian algorithm and genetic algorithms.
}
\details{
  The package implements two primary algorithms for project allocation:

  - **Hungarian Algorithm**: This classical algorithm solves the assignment 
  problem by finding the optimal way to assign students to projects, 
  minimizing total cost.
  - **Genetic Algorithm**: This heuristic algorithm uses evolutionary principles 
  to find a good allocation by simulating natural selection, mutation, and 
  crossover processes.

  The package allows users to input a matrix of costs (e.g., distance or 
  preference scores) and returns an optimal or near-optimal solution for 
  allocating students to projects.
}
\author{
  Laetitia Ah-Mouck

  Maintainer: Laetitia Ah-mouock <laetitia.ahmouck@gmail.com>
}
\references{
  Kuhn, H. W. (1955). The Hungarian method for the assignment problem. *Naval Research Logistics Quarterly, 2*(1), 83-97.
  Holland, J. H. (1975). *Adaptation in Natural and Artificial Systems*. University of Michigan Press.
}
\keyword{ package }

\seealso{
  \code{\link{genetic_algorithm}},
  \code{\link{hungarian_assignment}}
}

\examples{
  \dontrun{
     # Example of using the Hungarian algorithm
     cost_matrix <- matrix(c(10, 15, 20, 25, 30, 35), nrow = 3, byrow = TRUE)
     result_hungarian <- hungarian_algorithm(cost_matrix)
     print(result_hungarian)

     # Example of using the Genetic algorithm for allocation
     population_size <- 50
     generations <- 100
     mutation_rate <- 0.1
     allocation_result <- genetic_algorithm(cost_matrix, population_size, 
     generations, mutation_rate)
     print(allocation_result)
  }
}
