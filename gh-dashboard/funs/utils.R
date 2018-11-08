

#' Retrieve infos for a GitHub account
#' 
#' @param user GitHub username
#' 
#' @examples 
#' gh_infos("dreamRs")
gh_infos <- function(user) {
  
  # repo infos
  user_repo <- gh("/users/:user/repos", user = user, .limit = Inf)
  if (length(user_repo) == 1 && user_repo == "")
    return(NULL)
  user_repo <- lapply(user_repo, `[`, c("name", "stargazers_count", "forks", "open_issues", "watchers"))
  user_repo <- rbindlist(user_repo)
  user_repo <- user_repo[order(stargazers_count, decreasing = FALSE)]
  user_repo[, name := factor(name, levels = name)]
  # user_repo <- tail(user_repo, 20)
  
  # repos visitors
  user_views <- try(lapply(
    X = user_repo$name,
    FUN = function(x) {
      dat <- gh("/repos/:owner/:repo/traffic/views", owner = user, repo = x, .limit = Inf)
      if (length(dat$views) == 0) {
        return(NULL)
      } else {
        dat <- rbindlist(dat$views)
        dat$repo <- x
        dat
      }
    }
  ), silent = TRUE)
  if ("try-error" %in% class(user_views)) {
    user_views <- data.table(repo = character(0), date = character(0), count = numeric(0))
  } else {
    user_views <- rbindlist(user_views)
    user_views[, date := as.Date(substr(timestamp, 1, 10))]
    setorder(user_views, repo, date)
    user_views <- user_views[CJ(repo = user_repo$name, date = unique(date)), on = c("repo", "date")]
    user_views[is.na(count), count := 0]
    user_views <- user_views[, list(repo, date, count)]
  }
  return(list(user_repo = user_repo, user_views = user_views))
}

