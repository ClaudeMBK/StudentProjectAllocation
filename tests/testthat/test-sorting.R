library(testthat)
library(StudentProjectAllocation)

set.seed(123)
cost_matrix <- matrix(sample(1:100, 100, replace = TRUE), nrow = 10)

### RETURNS ###

test_that("Return is a list with expected elements", {
  result <- genetic_algorithm(cost_matrix, pop_size = 20, generations = 10)
  expect_type(result, "list")
  expect_named(result, c("cost", "assignment"))
  expect_type(result$cost, "integer")
  expect_type(result$assignment, "integer")
})

test_that("Return is a list with expected components", {
  result <- genetic_algorithm_cpp(cost_matrix, popSize = 20, generations = 10)
  expect_type(result, "list")
  expect_named(result, c("cost", "assignment"))
  expect_type(result$cost, "integer")
  expect_type(result$assignment, "integer")
})


### ERRORS ###

test_that("Throws error with non-square matrix", {
  bad_matrix <- matrix(1:12, nrow = 3, ncol = 4)
  expect_error(genetic_algorithm(bad_matrix), regexp = 
                 "Cost matrix must be square.")
})

test_that("Throws error when cost_matrix is not a matrix", {
  expect_error(genetic_algorithm(list(1, 2, 3)), regexp = "Must be a matrix")
})

test_that("Error when input is not a matrix", {
  expect_error(
    genetic_algorithm_cpp(as.list(1:6)),
    regexp = "Must be a matrix",
    ignore.case = TRUE
  )
})

test_that("Error when matrix is not square", {
  bad_matrix <- matrix(1:20, nrow = 4, ncol = 5)
  expect_error(genetic_algorithm_cpp(bad_matrix), regexp = 
                 "Cost matrix must be square.")
})


### VALID OUTPUT ###

test_that("Assignment is a valid permutation", {
  result <- genetic_algorithm(cost_matrix, pop_size = 20, generations = 10)
  expect_true(setequal(result$assignment, 1:nrow(cost_matrix)))
})

test_that("Cost corresponds to assignment", {
  result <- genetic_algorithm(cost_matrix, pop_size = 20, generations = 10)
  computed_cost <- sum(sapply(1:nrow(cost_matrix), 
                              function(i) cost_matrix[i, result$assignment[i]]))
  expect_equal(result$cost, computed_cost)
})

test_that("Assignment is a valid permutation", {
  result <- genetic_algorithm_cpp(cost_matrix, popSize = 20, generations = 10)
  expect_true(setequal(result$assignment, 1:nrow(cost_matrix)))
})

test_that("Returned cost matches assignment cost", {
  result <- genetic_algorithm_cpp(cost_matrix, popSize = 20, generations = 10)
  assignment <- result$assignment
  cost <- sum(sapply(1:length(assignment), 
                     function(i) cost_matrix[i, assignment[i]]))
  expect_equal(result$cost, cost)
})


mat <- matrix(c(4, 2, 8,
                7, 6, 3,
                1, 9, 5), nrow = 3)

### --- TESTS POUR hungarian_cost() ---

test_that("hungarian_cost returns a single numeric value", {
  cost <- hungarian_cost(mat)
  expect_type(cost, "double")
  expect_length(cost, 1)
})

test_that("hungarian_cost estimates cost correctly (basic test)", {
  expect_equal(hungarian_cost(mat), 6)
})

test_that("hungarian_cost throws error on non-square matrix", {
  nonsquare <- matrix(1:6, nrow = 2, ncol = 3)
  expect_error(hungarian_cost(nonsquare), regexp = "Cost matrix must be square.")
})

### --- TESTS POUR hungarian_assignment() ---

test_that("hungarian_assignment returns an integer vector of right length", {
  assign <- hungarian_assignment(mat)
  expect_type(assign, "double")
  expect_length(assign, nrow(mat))
})

test_that("hungarian_assignment returns a valid permutation", {
  assign <- hungarian_assignment(mat)
  expect_true(setequal(assign, 1:nrow(mat)))  # Should be a valid assignment
})

test_that("Assignment matches correct cost", {
  assign <- hungarian_assignment(mat)
  total_cost <- sum(sapply(1:nrow(mat), function(i) mat[i, assign[i]]))
  expect_equal(total_cost, 6)  # Manually computed optimal cost
})

test_that("hungarian_assignment throws error on non-square matrix", {
  bad <- matrix(1:9, nrow = 3, ncol = 3)
  bad <- bad[-1, ]
  expect_error(hungarian_assignment(bad), regexp = "Cost matrix must be square.")
})

# ----- Tests pour hungarian_cost_cpp() -----

test_that("hungarian_cost_cpp retourne un nombre", {
  cost <- hungarian_cost_cpp(mat)
  expect_type(cost, "double")
  expect_length(cost, 1)
})

test_that("hungarian_cost_cpp retourne la somme des étiquettes", {
  expect_equal(hungarian_cost_cpp(mat), 6)
})

test_that("hungarian_cost_cpp plante si matrice non carrée", {
  bad_mat <- matrix(1:6, nrow = 2)
  expect_error(hungarian_cost_cpp(bad_mat))
})


# ----- Tests pour hungarian_assignment_cpp() -----

test_that("hungarian_assignment_cpp retourne un vecteur d'entiers", {
  res <- hungarian_assignment_cpp(mat)
  expect_type(res, "integer")
  expect_length(res, 3)
})

test_that("hungarian_assignment_cpp retourne une permutation valide", {
  res <- hungarian_assignment_cpp(mat)
  expect_true(setequal(res, 1:3))
})

test_that("Coût de l'affectation retournée est optimal", {
  res <- hungarian_assignment_cpp(mat)
  total <- sum(mat[cbind(1:3, res)])
  expect_equal(total, 6)  # Affectation optimale = (1,2), (2,3), (3,1)
})

test_that("hungarian_assignment_cpp plante si matrice non carrée", {
  bad_mat <- matrix(1:12, nrow = 3)
  expect_error(hungarian_assignment_cpp(bad_mat))
})