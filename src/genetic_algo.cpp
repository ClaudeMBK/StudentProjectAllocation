#include <Rcpp.h>
using namespace Rcpp;

#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <random>

using namespace std;

// Crossover function
IntegerVector crossover(const IntegerVector& parent1, const IntegerVector& parent2) {
  int n = parent1.size();
  
  // Choose random crossover point between 2 and n-1
  int point = R::runif(2, n - 1); 
  
  IntegerVector child(n, -1);
  
  // Copy the first elements of parent1 until crossover point
  for (int i = 0; i < point; i++) {
    child[i] = parent1[i];
  }
  
  // Add elements of parent2 which are not in child yet
  int pos = point;
  for (int i = 0; i < n; i++) {
    if (std::find(child.begin(), child.end(), parent2[i]) == child.end()) {
      child[pos++] = parent2[i];
    }
  }
  
  return child;
}

//' @title Genetic Algorithm for Assignment Problem
//' @description
//' This function implements a genetic algorithm to solve an assignment problem
//' using a cost matrix. It returns the optimal assignment found (minimizing the
//' total cost) and its corresponding cost.
//'
//' The algorithm works by generating an initial population of random permutations,
//' selecting the best individuals each generation, and producing new ones via
//' crossover and mutation.
//'
//' @param costMatrix A square numeric matrix \code{n x n}, representing the cost
//' of assigning item \code{i} to position \code{j}.
//' @param popSize An integer specifying the number of individuals (assignments) in each generation. Default is 100.
//' @param generations An integer giving the number of generations to run the genetic algorithm. Default is 500.
//' @param mutationRate A numeric value (between 0 and 1) giving the probability of mutation. Default is 0.1.
//'
//' @return A \code{list} with the following elements:
//' \itemize{
//'   \item \code{cost}: The total cost (fitness) of the best assignment found.
//'   \item \code{assignment}: An integer vector representing the assignment.
//'   Each position i in the vector corresponds to student i, 
//'   and the value at that position indicates the assigned project. 
//' }
//'
//' @examples
//' set.seed(123)
//' cost_matrix <- matrix(sample(1:100, 36, replace = TRUE), nrow = 6)
//' result <- genetic_algorithm_cpp(cost_matrix)
//' result$cost
//' result$assignment
//'
//' @export
// [[Rcpp::export]]
List genetic_algorithm_cpp(SEXP costMatrix, 
                           int popSize = 100, 
                           int generations = 500,
                           double mutationRate = 0.1) {
  
  if (!Rf_isMatrix(costMatrix)) {
    stop("Input must be a matrix.");
  }
  
  NumericMatrix mat(costMatrix);
  
  if (mat.nrow() != mat.ncol()) {
    stop("Cost matrix must be square.");
  }
  int n = mat.nrow();  // matrix size (nb of individual)
  std::mt19937 rng(std::random_device{}());
  
  // Generate random individual (permutation of {0, 1, ..., n-1})
  auto generateIndividual = [&]() {
    IntegerVector individual(n);
    std::iota(individual.begin(), individual.end(), 0);
    std::shuffle(individual.begin(), individual.end(), rng);
    return individual;
  };
  
  // Fitness function (cost of assignment)
  auto fitness = [&](const IntegerVector& individual) {
    int totalCost = 0;
    for (int i = 0; i < n; i++) {
      totalCost += mat(i, individual[i]);
    }
    return totalCost;
  };
  
  // Mutation : random exchange of two positions
  auto mutate = [&](IntegerVector& individual) {
    std::uniform_real_distribution<double> dist(0.0, 1.0);
    if (dist(rng) < mutationRate) {
      std::uniform_int_distribution<int> indexDist(0, n - 1);
      int idx1 = indexDist(rng);
      int idx2 = indexDist(rng);
      std::swap(individual[idx1], individual[idx2]);
    }
  };
  
  // Population initialization
  std::vector<IntegerVector> population;
  for (int i = 0; i < popSize; i++) {
    population.push_back(generateIndividual());
  }
  
  // Population evolution
  for (int gen = 0; gen < generations; gen++) {
    
    // Compute population's scores
    std::vector<double> scores;
    for (const auto& individual : population) {
      scores.push_back(fitness(individual));
    }
    
    // Order population by ascendant score
    std::vector<int> ranked_population(population.size());
    std::iota(ranked_population.begin(), ranked_population.end(), 0);
    std::sort(ranked_population.begin(), ranked_population.end(), 
              [&](int a, int b) {
                return scores[a] < scores[b];
              });

    // Selection of the n/2 best individuals
    std::vector<IntegerVector> newPopulation;
    for (int i = 0; i < popSize / 2; i++) {
      newPopulation.push_back(population[ranked_population[i]]);
    }
    
    // Generation of child via crossover
    while (newPopulation.size() < popSize) {
      std::uniform_int_distribution<int> dist(0, popSize / 2 - 1);
      int p1 = dist(rng);
      int p2 = dist(rng);
      IntegerVector child = crossover(population[ranked_population[p1]], population[ranked_population[p2]]);
      mutate(child);
      newPopulation.push_back(child);
    }
    
    population = newPopulation;
  }
  
  // Find best individual
  IntegerVector bestIndividual = *std::min_element(
    population.begin(), 
    population.end(), 
    [&](const IntegerVector& a, const IntegerVector& b) {
      return fitness(a) < fitness(b);
    });
  
  return List::create(Named("cost") = fitness(bestIndividual), 
                      Named("assignment") = bestIndividual + 1);  // On ajoute 1 pour l'indexation en R
}
