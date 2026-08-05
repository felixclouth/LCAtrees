leafData <- function(tree) {

  leaves <- list()

  collect_leaves <- function(node, path = "root") {

    if (node$terminal) {
      leaves[[path]] <<-
        tree$data[node$rows, , drop = FALSE]

      return(invisible(NULL))
    }

    z <- node$split_variable

    collect_leaves(
      node$children[["1"]],
      path = paste0(path, "/", z, "=1")
    )

    collect_leaves(
      node$children[["0"]],
      path = paste0(path, "/", z, "=0")
    )

    invisible(NULL)
  }

  collect_leaves(tree$root)
  leaves
}
