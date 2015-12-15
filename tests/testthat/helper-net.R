
is_online <- function(host = "github.com", port = 443) {

  res <- tryCatch(
    pingr::ping_port(host, count = 1L, port = port),
    error = function(e) NA
  )

  !is.na(res)
}
