nodesModel <- function(data,
                       Y,
                       iT,
                       Z,
                       node_seed = NULL) {

  Z <- intersect(Z, names(data))

  if (length(Z) == 0L) {
    return(NULL)
  }

  fits <- setNames(vector("list", length(Z)), Z)
  scores <- setNames(rep(NA_real_, length(Z)), Z)

  for (z in Z) {
    fit <- multiLCA(data = data,
                    Y = Y,
                    iT = iT,
                    Z = z)

    fits[z] <- list(fit)
    scores[[z]] <- fit$R2entr
  }

  best_variable <- names(which.max(scores))

  list(
    variable = best_variable,
    score = scores[[best_variable]],
    scores = scores,
    fit = fits[[best_variable]]
  )
}
