#' Player Performance Rating
#'
#' This function calculates a player's match rating (1-10) based on key performance metrics.
#'
#'
#' @param goals Number of goals scored.
#' @param assists Number of assists.
#' @param passes Total completed passes.
#' @param tackles Number of tackles.
#' @param shots Total shots taken.
#'
#' @return A numeric match rating between 1 and 10.
#' @examples
#' player_performance(goals = 2, assists = 1, passes = 45, tackles = 3, shots = 4)
#' @export
player_performance <- function(goals, assists, passes, tackles, shots) {


  weighted_score <- (goals * 35) +
    (assists * 25) +
    (passes * 0.2) +
    (tackles * 15) +
    (shots * 10)


  max_score <- 250


  rating <- (weighted_score / max_score) * 9 + 1  # Scale to fit between 1-10


  rating <- max(1, min(rating, 10))

  return(round(rating, 1))
}

