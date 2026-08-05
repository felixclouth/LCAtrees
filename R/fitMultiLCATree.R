fitMultiLCATree <- function(data,
                            Y,
                            iT,
                            Z,
                            seed = NULL,
                            max_depth = Inf,
                            keep_fits = FALSE) {

  node_counter <- 0L

  grow_node <- function(rows, available_Z, depth) {

    node_counter <<- node_counter + 1L
    node_id <- node_counter

    node_data <- data[rows, , drop = FALSE]

    node <- list(
      id = node_id,
      depth = depth,
      n = length(rows),
      rows = rows,
      terminal = TRUE,
      reason = NULL,
      split_variable = NULL,
      split_score = NA_real_,
      candidate_scores = NULL,
      fit = NULL,
      children = NULL
    )

    if (nrow(node_data) <= nrow(data)*.1) {
      node$reason <- "node sample size is 10% or less of full dataset"
      return(node)
    }

        if (length(available_Z) == 0L) {
      node$reason <- "no covariates left"
      return(node)
    }

    if (depth >= max_depth) {
      node$reason <- "maximum depth reached"
      return(node)
    }

    node_complete <- node_data[
      complete.cases(node_data[, Y, drop = FALSE]),
      ,
      drop = FALSE
    ]

    z_tables <- lapply(
      available_Z,
      function(z) table(node_data[[z]], useNA = "no")
    )

    nonconstant <- vapply(
      z_tables,
      function(tab) sum(tab > 0L) >= 2L,
      logical(1)
    )

    available_Z <- available_Z[nonconstant]

    if (length(available_Z) == 0L) {
      node$reason <- "all remaining covariates are constant"
      return(node)
    }

    model_error <- NULL

    best_split <- tryCatch(
      nodesModel(
        data = node_data,
        Y = Y,
        iT = iT,
        Z = available_Z,
        node_seed = node_seed
      ),
      error = function(e) {
        model_error <<- conditionMessage(e)
        NULL
      }
    )

    if (is.null(best_split)) {
      node$reason <- paste("multiLCA error:", model_error)
      return(node)
    }

    z_star <- best_split$variable
    split_values <- node_data[[z_star]]

    rows_one <- rows[split_values == 1L]
    rows_zero <- rows[split_values == 0L]

    remaining_Z <- setdiff(available_Z, z_star)

    node$terminal <- FALSE
    node$split_variable <- z_star
    node$split_score <- best_split$score
    node$candidate_scores <- best_split$scores

    if (keep_fits) {
      node$fit <- best_split$fit
    }

    node$children <- list(
      `1` = grow_node(
        rows = rows_one,
        available_Z = remaining_Z,
        depth = depth + 1L
      ),
      `0` = grow_node(
        rows = rows_zero,
        available_Z = remaining_Z,
        depth = depth + 1L
      )
    )

    node
  }

  root <- grow_node(
    rows = seq_len(nrow(data)),
    available_Z = Z,
    depth = 0L
  )

  structure(
    list(
      root = root,
      data = data,
      Y = Y,
      iT = iT,
      Z = Z,
      seed = seed
    ),
    class = "multiLCA_tree"
  )
}
