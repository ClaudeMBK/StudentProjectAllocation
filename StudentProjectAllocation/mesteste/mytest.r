################################################
############# A simple test ####################
################################################

n <- 100 #data size 
generate_matrix <- function(n) {
  matrix(apply(matrix(1:n, n, n, byrow = TRUE), 1, sample), 
         nrow = n, byrow = TRUE)} #generate random cost matrix

# the 4 algorithms
hungarian_r(cost_matrix)
genetic_r(cost_matrix)
hungarian_cpp(cost_matrix)
genetic_cpp(cost_matrix)

################################################################################################
# We define the function one.simu which returns the execution time of a given algorithm
one.simu <- function(n, type = "random", func = "hungarian_r")
{
  if(type == "random"){
    v <- generate_matrix(n)
  } else {
    v <- matrix(1:(n*n), n, n)  # worst case scenario
  }
  if(func == "hungarian_r"){t <- system.time(hungarian_r(v))[[1]]}
  if(func == "genetic_r"){t <- system.time(genetic_r(v))[[1]]} 
  if(func == "hungarian_cpp"){t <- system.time(hungarian_cpp(v))[[1]]}
  if(func == "genetic_cpp"){t <- system.time(genetic_cpp(v))[[1]]}
  return(t)
}
################################################################################################

###########################################################
############# One time complexity test ####################
###########################################################
#we evaluate the time with a given n for the 4 algorithms
n <- 100
one.simu(n, func = "hungarian_r")
one.simu(n, func = "genetic_r")
one.simu(n, func = "hungarian_cpp")
one.simu(n, func = "genetic_cpp")

######################################################################### 
############# A short simulation study at fixed vector size ############# 
######################################################################### 

#we compare the running time at a given length n with repeated executions (nbSimus times)
nbSimus <- 10
time1 <- 0
time2 <- 0
time3 <- 0
time4 <- 0
for(i in 1:nbSimus){time1 <- time1 + one.simu(n, func = "hungarian_r")}
for(i in 1:nbSimus){time2 <- time2 + one.simu(n, func = "genetic_r")}
for(i in 1:nbSimus){time3 <- time3 + one.simu(n, func = "hungarian_cpp")}
for(i in 1:nbSimus){time4 <- time4 + one.simu(n, func = "genetic_cpp")}

#gain R -> Rcpp
time1/time3
time2/time4

#gain hungarian -> genetic
time1/time2
time3/time4

#max gain
time1/time4

##########################################
############# microbenchmark ############# 
##########################################

library(microbenchmark)
library("ggplot2")
n <- 100
res <- microbenchmark(
  one.simu(n, func = "hungarian_cpp"), 
  one.simu(n, func = "genetic_cpp"), 
  times = 50
)
autoplot(res)
res

##########################################
############# time complexity ############ 
##########################################

# Genetic algorithm complexity
nbSimus <- 10
vector_n <- seq(from = 50, to = 500, length.out = nbSimus)
nbRep <- 10
res_Genetic <- data.frame(matrix(0, nbSimus, nbRep + 1))
colnames(res_Genetic) <- c("n", paste0("Rep",1:nbRep))

j <- 1
for(i in vector_n)
{
  res_Genetic[j,] <- c(i, replicate(nbRep, one.simu(i, func = "genetic_cpp")))  
  print(j)
  j <- j + 1
}

res <- rowMeans(res_Genetic[,-1])
plot(vector_n, res, xlab = "matrix size (n x n)", ylab = "time in seconds")

# Hungarian algorithm complexity
nbSimus <- 40
vector_n <- seq(from = 20, to = 200, length.out = nbSimus)  # smaller range due to O(n³)
nbRep <- 10
res_Hungarian <- data.frame(matrix(0, nbSimus, nbRep + 1))
colnames(res_Hungarian) <- c("n", paste0("Rep",1:nbRep))

j <- 1
for(i in vector_n)
{
  res_Hungarian[j,] <- c(i, replicate(nbRep, one.simu(i, func = "hungarian_cpp")))  
  print(j)
  j <- j + 1
}

res <- rowMeans(res_Hungarian[,-1])
plot(vector_n, res, xlab = "matrix size (n x n)", ylab = "time in seconds")
lm(log(res) ~ log(vector_n))  # to estimate complexity