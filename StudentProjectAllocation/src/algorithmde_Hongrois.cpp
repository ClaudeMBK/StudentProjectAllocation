#include <Rcpp.h>
#include <vector>
#include <algorithm>
#include <numeric>
#include <random>

using namespace Rcpp;

// Hungarian Algorithm Implementation
// [[Rcpp::export]]
IntegerVector hungarian_algorithm_cpp(NumericMatrix cost) {
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
                    double cur = cost(i0-1, j-1) - u[i0] - v[j];
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
        
        do {
            int j1 = way[j0];
            p[j0] = p[j1];
            j0 = j1;
        } while (j0);
    }
    
    for (int j = 1; j <= n; j++) {
        assignment[p[j]-1] = j-1;
    }
    
    return 1 + assignment;
}

// Genetic Algorithm Implementation
// [[Rcpp::export]]
IntegerVector genetic_algorithm_cpp(NumericMatrix cost, 
                              int population_size = 100, 
                              int generations = 500,
                              double mutation_rate = 0.05) {
    int n = cost.nrow();
    std::random_device rd;
    std::mt19937 gen(rd());
    
    // Create initial population
    std::vector<std::vector<int>> population(population_size, std::vector<int>(n));
    for (int i = 0; i < population_size; i++) {
        std::iota(population[i].begin(), population[i].end(), 0);
        std::shuffle(population[i].begin(), population[i].end(), gen);
    }
    
    // Main evolution loop
    for (int g = 0; g < generations; g++) {
        // Calculate fitness
        std::vector<double> fitness(population_size);
        for (int i = 0; i < population_size; i++) {
            double score = 0;
            for (int j = 0; j < n; j++) {
                score += cost(j, population[i][j]);
            }
            fitness[i] = -score; // Negative because we want to minimize
        }
        
        // Selection (tournament selection)
        std::vector<std::vector<int>> new_population(population_size);
        for (int i = 0; i < population_size; i++) {
            int best_idx = std::uniform_int_distribution<>(0, population_size-1)(gen);
            for (int k = 0; k < 2; k++) {
                int idx = std::uniform_int_distribution<>(0, population_size-1)(gen);
                if (fitness[idx] > fitness[best_idx]) best_idx = idx;
            }
            new_population[i] = population[best_idx];
        }
        
        // Crossover
        for (int i = 0; i < population_size; i += 2) {
            if (i + 1 < population_size && std::uniform_real_distribution<>(0,1)(gen) < 0.8) {
                int point = std::uniform_int_distribution<>(1, n-2)(gen);
                std::vector<int> parent1 = new_population[i];
                std::vector<int> parent2 = new_population[i+1];
                
                std::vector<int> child1(n, -1), child2(n, -1);
                for (int j = 0; j < point; j++) {
                    child1[j] = parent1[j];
                    child2[j] = parent2[j];
                }
                // Complete with remaining numbers maintaining validity
                int k1 = point, k2 = point;
                for (int j = 0; j < n; j++) {
                    if (std::find(child1.begin(), child1.begin() + point, parent2[j]) == child1.begin() + point) {
                        child1[k1++] = parent2[j];
                    }
                    if (std::find(child2.begin(), child2.begin() + point, parent1[j]) == child2.begin() + point) {
                        child2[k2++] = parent1[j];
                    }
                }
                new_population[i] = child1;
                new_population[i+1] = child2;
            }
        }
        
        // Mutation
        for (int i = 0; i < population_size; i++) {
            if (std::uniform_real_distribution<>(0,1)(gen) < mutation_rate) {
                int idx1 = std::uniform_int_distribution<>(0, n-1)(gen);
                int idx2 = std::uniform_int_distribution<>(0, n-1)(gen);
                std::swap(new_population[i][idx1], new_population[i][idx2]);
            }
        }
        
        population = new_population;
    }
    
    // Find best solution
    int best_idx = 0;
    double best_score = INFINITY;
    for (int i = 0; i < population_size; i++) {
        double score = 0;
        for (int j = 0; j < n; j++) {
            score += cost(j, population[i][j]);
        }
        if (score < best_score) {
            best_score = score;
            best_idx = i;
        }
    }
    
    return  wrap(population[best_idx]);
}

