update_function_status <- function(
    function_name,
    status,
    notes = NULL,
    changes = character(),
    limitations = character()) {

  inventory_file <- "dev/function_inventory.csv"
  changelog_file <- "CHANGELOG.md"

  if (!file.exists(inventory_file)) {
    stop(
      "Function inventory not found: ",
      inventory_file
    )
  }

  inventory <- read.csv(
    inventory_file,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  if (!function_name %in% inventory$function_name) {
    stop(
      "Function not found in inventory: ",
      function_name
    )
  }

  row <- inventory$function_name == function_name

  inventory$status[row] <- status

  if (!is.null(notes)) {
    inventory$notes[row] <- notes
  }

  write.csv(
    inventory,
    inventory_file,
    row.names = FALSE
  )

  entry <- c(
    "",
    paste0("## ", Sys.Date()),
    "",
    paste0("### ", function_name),
    "",
    paste0("Status: ", status),
    ""
  )

  if (length(changes) > 0) {
    entry <- c(
      entry,
      "Changes:",
      paste0("- ", changes),
      ""
    )
  }

  if (length(limitations) > 0) {
    entry <- c(
      entry,
      "Notes / limitations:",
      paste0("- ", limitations),
      ""
    )
  }

  cat(
    paste(entry, collapse = "\n"),
    file = changelog_file,
    append = TRUE
  )

  message(
    function_name,
    " updated successfully."
  )

  invisible(inventory[row, ])
}


check_function_structure <- function(function_name) {

  checks <- data.frame(
    item = c(
      "R source",
      "testthat tests",
      "roxygen documentation"
    ),
    exists = c(
      file.exists(
        file.path(
          "R",
          paste0(function_name, ".R")
        )
      ),
      file.exists(
        file.path(
          "tests",
          "testthat",
          paste0("test-", function_name, ".R")
        )
      ),
      file.exists(
        file.path(
          "man",
          paste0(function_name, ".Rd")
        )
      )
    )
  )

  checks
}