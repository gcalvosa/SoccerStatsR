#' Player Performance Score
#'
#' This function calculates a player's performance score based on goals, assists, passes, tackles, and shots.
#'
#' @param goals Number of goals scored.
#' @param assists Number of assists.
#' @param passes Total completed passes.
#' @param tackles Number of tackles.
#' @param shots Total shots taken.
#'
#' @return A numeric performance score.
#' @examples
#' player_performance(goals = 3, assists = 2, passes = 50, tackles = 4, shots = 6)
#' @export
player_performance <- function(goals, assists, passes, tackles, shots) {
  score <- (goals * 10) + (assists * 7) + (passes * 0.2) + (tackles * 3) + (shots * 2)
  return(score)
}

