hungarian_algorithm <- function(cost_matrix) {
    original_cost <- cost_matrix       # Sauvegarde de la matrice originale pour le calcul du co�t final
    n <- nrow(cost_matrix)
    
    # �tape 1 : R�duction de la matrice
    # Soustraire le minimum de chaque ligne
    for (i in 1:n) {
      cost_matrix[i, ] <- cost_matrix[i, ] - min(cost_matrix[i, ])
    }
    # Soustraire le minimum de chaque colonne
    for (j in 1:n) {
      cost_matrix[, j] <- cost_matrix[, j] - min(cost_matrix[, j])
    }
    
    # Initialisation
    mask <- matrix(0, n, n)    # 0 : aucune marque, 1 : z�ro �toil�, 2 : z�ro prim�
    row_cover <- rep(0, n)      # 0 : non couvert, 1 : couvert
    col_cover <- rep(0, n)      # 0 : non couvert, 1 : couvert
    
    # �tape 2 : Marquage initial (starring) des z�ros
    for (i in 1:n) {
      for (j in 1:n) {
        if (cost_matrix[i, j] == 0 && row_cover[i] == 0 && col_cover[j] == 0) {
          mask[i, j] <- 1
          row_cover[i] <- 1
          col_cover[j] <- 1
        }
      }
    }
    # D�cocher toutes les lignes et colonnes
    row_cover[] <- 0
    col_cover[] <- 0
    
    # �tape 3 : Couvrir les colonnes contenant un z�ro �toil�
    for (j in 1:n) {
      if (any(mask[, j] == 1)) {
        col_cover[j] <- 1
      }
    }
    
    # Fonction auxiliaire pour trouver un z�ro non couvert
    find_zero <- function(mat, row_cov, col_cov) {
      for (i in 1:n) {
        if (row_cov[i] == 0) {
          for (j in 1:n) {
            if (col_cov[j] == 0 && mat[i, j] == 0) {
              return(c(i, j))
            }
          }
        }
      }
      return(NULL)
    }
    
    # Boucle principale de l'algorithme
    while (sum(col_cover) < n) {
      z <- find_zero(cost_matrix, row_cover, col_cover)
      if (!is.null(z)) {
        i <- z[1]
        j <- z[2]
        mask[i, j] <- 2  # Marquer ce z�ro comme prim�
        
        # Si la ligne i ne contient pas de z�ro �toil�, on passe � la phase d'augmentation
        if (!any(mask[i, ] == 1)) {
          # Construire le chemin d'augmentation
          path <- matrix(c(i, j), ncol = 2)
          done <- FALSE
          while (!done) {
            # Chercher un z�ro �toil� dans la colonne actuelle
            row_star <- which(mask[, path[nrow(path), 2]] == 1)
            if (length(row_star) == 0) {
              done <- TRUE
            } else {
              i_star <- row_star[1]
              path <- rbind(path, c(i_star, path[nrow(path), 2]))
              # Chercher le z�ro prim� dans la ligne nouvellement trouv�e
              j_prime <- which(mask[i_star, ] == 2)
              path <- rbind(path, c(i_star, j_prime[1]))
            }
          }
          # Inverser les marques sur le chemin :
          # - Tous les z�ros �toil�s du chemin deviennent non marqu�s.
          # - Tous les z�ros prim�s du chemin deviennent �toil�s.
          for (k in 1:nrow(path)) {
            if (mask[path[k, 1], path[k, 2]] == 1) {
              mask[path[k, 1], path[k, 2]] <- 0
            } else if (mask[path[k, 1], path[k, 2]] == 2) {
              mask[path[k, 1], path[k, 2]] <- 1
            }
          }
          # R�initialiser les couvertures et effacer tous les z�ros prim�s
          row_cover[] <- 0
          col_cover[] <- 0
          mask[mask == 2] <- 0
          
          # Re-couvrir les colonnes contenant un z�ro �toil�
          for (j in 1:n) {
            if (any(mask[, j] == 1)) {
              col_cover[j] <- 1
            }
          }
        } else {
          # S'il y a un z�ro �toil� dans la ligne i, couvrir cette ligne
          # et d�couvrir la colonne contenant le z�ro �toil�
          row_cover[i] <- 1
          star_col <- which(mask[i, ] == 1)[1]
          col_cover[star_col] <- 0
        }
      } else {
        # Aucun z�ro non couvert trouv�, ajuster la matrice
        # Trouver la plus petite valeur non couverte
        minval <- min(cost_matrix[row_cover == 0, col_cover == 0])
        # Ajouter minval aux �l�ments couverts par une ligne
        for (i in 1:n) {
          if (row_cover[i] == 1) {
            cost_matrix[i, ] <- cost_matrix[i, ] + minval
          }
        }
        # Soustraire minval aux �l�ments non couverts par une colonne
        for (j in 1:n) {
          if (col_cover[j] == 0) {
            cost_matrix[, j] <- cost_matrix[, j] - minval
          }
        }
      }
    }
    
    # Extraction de l'affectation optimale � partir des z�ros �toil�s
    assignment <- which(mask == 1, arr.ind = TRUE)
    colnames(assignment) <- c("etudiant", "projet")
    
    # Calcul du co�t total en utilisant la matrice de co�t originale
    total_cost <- sum(original_cost[cbind(assignment[, 1], assignment[, 2])])
    
    return(list(cost = total_cost, assignment = assignment))
  }