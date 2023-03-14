#' Refresh cranatgh-status.yml
#'
#' Do a full refresh of `cranatgh-status.yml` by scraping all github
#' repositories from the CRAN organization.
#' @export
refresh_status_yml <- function(){
  repos <- gh::gh("/orgs/cran/repos", .limit = 1e5)
  packages <- vapply(repos, function(x){x$name}, character(1))
  versions <- rep(NA, length(packages))

  # Scrape versions from Metacran
  pool <- curl::new_pool(multiplex = FALSE)
  completed <- 0
  fetch_version <- function(i){
    url <- sprintf('https://raw.githubusercontent.com/cran/%s/master/DESCRIPTION', packages[i])
    curl::curl_fetch_multi(url, done = function(res){
      if(res$status_code == 200){
        try({
          versions[i] <<- parse_description_version(res$content)
        })
        completed <<- completed + 1
        if(completed %% 100 == 0)
          message(sprintf("Completed %d of %d", completed, length(packages)))
      } else if(res$status_code %in% c(403, 429)){
        message("Possibly hitting GH limit, waiting for a few seconds before retrying...")
        Sys.sleep(30)
        fetch_version(i)
      } else {
        message(sprintf("HTTP %d: %s", res$status_code, res$url))
      }
    }, fail = stop, pool = pool)
  }
  lapply(seq_along(packages), fetch_version)
  curl::multi_run(pool = pool)
  packages <- packages[!is.na(versions)]
  versions <- versions[!is.na(versions)]
  ord <- order(packages, method = 'radix')
  statusdata <- structure(as.list(versions[ord]), names = packages[ord])
  yaml::write_yaml(statusdata, 'cranatgh-status.yml')
}

# Get 'Version' field from DESCRIPTION
parse_description_version <- function(buf){
  con <- rawConnection(buf)
  on.exit(close(con))
  desc <- read.dcf(con)
  trimws(unname(desc[,'Version']))
}
