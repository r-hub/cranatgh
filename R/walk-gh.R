
#' @importFrom cli cli_status cli_alert_warning cli_status_clear
#'   cli_status_update cli_alert_danger

walk_gh_versions <- function(package_callback) {

  status <- cli_status("{.alert-info Running GitHub query}")

  # This will walk until an error happens or the user interrupts,
  # and then return partial results
  tryCatch(
    walk_gh_versions_worker(package_callback, status),
    interrupt = function(e) {
      cli_alert_danger("GH repo walk interrupted, results are incomplete")
    }
  )

  cli_status_clear(status, result = "clear")
}

walk_gh_versions_worker <- function(package_callback, status) {

  q <- '
  {
    organization(login: "<org>") {
      repositories(first: 50,
                   orderBy: { field: NAME, direction: ASC },
                   after: <after>) {
        nodes {
          name
          hasIssuesEnabled
          hasProjectsEnabled
          hasWikiEnabled
          homepageUrl
          description
          refs(refPrefix: "refs/tags/", last: 100,
               orderBy: { field: TAG_COMMIT_DATE, direction: ASC }) {
            nodes {
              name
            }
          }
        }
        pageInfo {
          endCursor
          hasNextPage
        }
      }
    }
  }'

  after <- "null"
  has_next <- TRUE
  done <- 0
  while (has_next) {
    qx <- sub("<after>", after, q)
    qx <- sub("<org>", default_cranatgh_org(), qx)

    cli_status_update(
      status,
      "{.alert-info Running GitHub query, {done} package{?s} done.}"
    )
    res <- gh("POST /graphql", query = qx, .token = get_gh_token())

    data <- res$data$organization$repositories
    has_next <- data$pageInfo$hasNextPage
    if (has_next) after <- sprintf('"%s"', data$pageInfo$endCursor)
    for (node in data$nodes) {
      pkg_data <- list(
        name = node$name,
        versions = vapply(node$refs$nodes, "[[", character(1), "name"),
        issues = node$hasIssuesEnabled,
        projects = node$hasProjectsEnabled,
        wiki = node$hasWikiEnabled,
        url = node$homepageUrl,
        description = node$description
      )
      pkg_data$versions <-
        grep("^R", pkg_data$versions, value = TRUE, invert = TRUE)
      package_callback(pkg_data)
    }
    done <- done + length(data$nodes)
  }

  invisible()
}
