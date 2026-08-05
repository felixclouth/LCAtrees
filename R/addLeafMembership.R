addLeafMembership <- function(tree,
                              variable_name = "leaf",
                              as_factor = FALSE) {

  result <- tree$data

  leaf_membership <- rep(NA_character_, nrow(result))

  assign_leaf <- function(node, path = "root") {

    if (node$terminal) {
      leaf_membership[node$rows] <<- path
      return(invisible(NULL))
    }

    z <- node$split_variable

    assign_leaf(
      node = node$children[["1"]],
      path = paste0(path, "/", z, "=1")
    )

    assign_leaf(
      node = node$children[["0"]],
      path = paste0(path, "/", z, "=0")
    )

    invisible(NULL)
  }

  assign_leaf(tree$root)

  if (as_factor) {
    leaf_membership <- factor(leaf_membership)
  }

  result[[variable_name]] <- leaf_membership

  result
}
