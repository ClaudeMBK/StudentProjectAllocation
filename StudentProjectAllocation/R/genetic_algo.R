 genetic_algorithm_assignment <- function(cost_matrix, 
    pop_size = 100, 
    generations = 500, 
    mutation_rate = 0.05) {
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

return(list(cost = best_cost,assignment = best_individual ))
}
