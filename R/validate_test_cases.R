#' Validate multiple test cases for floristic data
#'
#' This is a wrapper function for \code{\link{validate_floristic_occdata}} that
#' validates a named list of test data frames (test cases) generated using
#' \code{\link{generate_test_cases}}. It returns a named list of validation
#' results including messages, warnings, and errors for each case, along with
#' a summarized data frame of the JSON validation status.
#'
#' @param test_cases_list A named list of data frames representing different
#'   floristic test cases to validate.
#'
#' @return A list with:
#' \describe{
#'   \item{results}{A named list of validation outputs for each test case}
#'   \item{summary}{A data frame summarizing JSON validity per test case}
#' }
#'
#' @examples
#' \dontrun{
#' test_cases <- generate_test_cases()
#' validation <- validate_test_cases(test_cases)
#' print(validation$summary)
#' print(validation$results$case1$errors)
#' }
#'
#' @seealso \code{\link{validate_floristic_occdata}}, \code{\link{generate_test_cases}}
#' @export
validate_test_cases <- function(test_cases_list) {
  result_list <- list()

  for (name in names(test_cases_list)) {
    df <- test_cases_list[[name]]
    message("Testing case: ", name)

    captured_messages <- character()
    captured_warnings <- character()
    captured_errors <- character()

    validation_output <- withCallingHandlers(
      tryCatch(
        {
          validate_floristic_occdata(
            df,
            schema = system.file("schemas", "structure.schema.json", package = "dufloR"),
            stop_on_error = FALSE,
            verbose = FALSE
          )
        },
        error = function(e) {
          captured_errors <<- c(captured_errors, conditionMessage(e))
          NULL
        }
      ),
      warning = function(w) {
        captured_warnings <<- c(captured_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      },
      message = function(m) {
        captured_messages <<- c(captured_messages, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )

    result_list[[name]] <- list(
      validation_output = validation_output,
      errors = c(captured_errors, validation_output$validation_errors),
      warnings = captured_warnings,
      messages = captured_messages
    )
  }

  # --- Build validation summary ---
  flat_items <- purrr::list_flatten(result_list)

  all_items_with_json_valid <- purrr::keep(
    flat_items,
    ~ is.list(.) && !is.null(.x$json_valid)
  )

  all_names <- purrr::map_lgl(all_items_with_json_valid, "json_valid")

  validation_summary <- data.frame(
    test_case = names(all_names),
    json_valid = unname(all_names),
    stringsAsFactors = FALSE
  ) %>%
    tidyr::separate(
      test_case,
      into = c("type", "index", "columns"),
      sep = "_",
      extra = "merge"
    )

  return(list(
    results = result_list,
    summary = validation_summary
  ))
}
