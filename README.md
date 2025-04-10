# StudentProjectAllocation: Student-Project Allocation with Rcpp

**StudentProjectAllocation** est un package R développé avec Rcpp pour résoudre le problème d'allocation étudiant-projet, une version spécifique du problème d'affectation bipartite en optimisation combinatoire. Ce package vise à assigner de manière optimale $n$ étudiants à $n$ projets en minimisant une fonction de coût basée sur des préférences ou des compatibilités. Il implémente deux approches principales : l'algorithme hongrois (Hungarian Algorithm), qui fournit une solution optimale exacte, et un algorithme génétique, qui offre une solution approchée pour explorer des alternatives heuristiques.

## Contexte

Le problème d'affectation étudiant-projet consiste à assigner un ensemble d'étudiants $S = \{s_1, s_2, \ldots, s_n\}$ à un ensemble de projets $P = \{p_1, p_2, \ldots, p_n\}$ de manière optimale. Chaque étudiant $i$ a une préférence ou une compatibilité avec un projet $j$, représentée par une matrice de coût $C = \{c_{ij}\}$, où $c_{ij}$ est la "distance" (ou le coût) entre l'étudiant $i$ et le projet $j$. L'objectif est de minimiser la fonction suivante :

$$
\sum_{i=1}^n c_{i, \sigma(i)}
$$

où $\sigma(i)$ est le projet assigné à l'étudiant $i$. Ce problème est une variante du problème du mariage stable ou de l'affectation bipartite.


## Objectifs

1. **Algorithme Hongrois** : Implémenter une solution exacte avec une complexité temporelle de $O(n^3)$ pour trouver l'affectation optimale.
2. **Algorithme Génétique** : Développer une approche heuristique pour obtenir une solution approchée rapidement, et évaluer si elle peut se rapprocher de l'optimum.
3. **Comparaison** : Analyser si l'algorithme génétique peut être une alternative fiable pour des tailles de problèmes trop grandes pour l'algorithme hongrois.


## Fonctionnalités

- **Hungarian Algorithm** :
  - `hungarian_cost` et `hungarian_cost_cpp` : Calcule le coût total minimal.
  - `hungarian_assignment` et `hungarian_assignment_cpp` : Retourne le vecteur d'affectation optimale.
  - Implémentations en R et C++ pour comparer les performances.

- **Genetic Algorithm** :
  - `genetic_algorithm` et `genetic_algorithm_cpp` : Fournit une solution approchée avec des paramètres ajustables (taille de la population, générations, taux de mutation).
  - Implémentations en R et C++.

- **Support des matrices de coût** : Les fonctions prennent en entrée une matrice carrée $C$ où $c_{ij}$ représente le coût d'assigner l'étudiant $i$ au projet $j$.


## Prérequis

- **R** (version 4.0.0 ou supérieure recommandée).
- **Rcpp** (installé automatiquement comme dépendance).
- Un compilateur C++ (ex. `g++` sur Linux, `Xcode` sur macOS, `Rtools` sur Windows).

## Installation

Installez **StudentProjectAllocation** depuis GitHub avec `devtools` :

```R
# Installer devtools si nécessaire
if (!require(devtools)) install.packages("devtools")

# Installer StudentProjectAllocation
devtools::install_github("ClaudeMBK/StudentProjectAllocation")
```

Chargez le package dans R :

```R
library(StudentProjectAllocation)
```

---

## Exemples d'utilisation

### 1. Allocation optimale avec l'algorithme hongrois (C++)

```R
# Matrice de coût (exemple de préférences)
cost_matrix <- matrix(c(5, 9, 1, 3, 2, 4, 8, 7, 6), nrow = 3)

# Coût total minimal
cost <- hungarian_cost_cpp(cost_matrix)
print(cost)  # Résultat attendu : 9

# Affectation optimale
assignment <- hungarian_assignment_cpp(cost_matrix)
print(assignment)  # Exemple : [3, 1, 2]
```

### 2. Solution approchée avec l'algorithme génétique (C++)

```R
set.seed(123)

# Matrice de coût aléatoire
cost_matrix <- matrix(sample(1:100, 16), nrow = 4)

# Exécuter l'algorithme génétique
result <- genetic_algorithm_cpp(cost_matrix, popSize = 100, generations = 500, mutationRate = 0.1)
print(result$cost)        # Coût total
print(result$assignment)  # Affectation
```

### 3. Comparaison des performances

```R
cost_matrix <- matrix(c(4, 2, 8, 7, 6, 3, 1, 9, 5), nrow = 3)

# Algorithme hongrois en R
system.time(assignment_r <- hungarian_assignment(cost_matrix))

# Algorithme hongrois en C++
system.time(assignment_cpp <- hungarian_assignment_cpp(cost_matrix))

print(assignment_r)
print(assignment_cpp)
```

---

## Structure du projet

- **`R/`** : Fonctions R et wrappers pour le code C++.
- **`src/`** : Code C++ pour les algorithmes hongrois et génétiques.
- **`man/`** : Documentation des fonctions exportées.
- **`NAMESPACE`** : Exports du package.
- **`DESCRIPTION`** : Métadonnées (nom, version, dépendances).

---

## Contact

Pour toute question, contactez-nous:  athoumani.ibroihima@gmail.com, laetitia.ahmouck@gmail.com .




