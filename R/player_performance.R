#' Player Performance Score (Original + Advanced Metrics)
#'
#' This function calculates a player's performance score based on:
#' Goals, assists, passes, tackles, shots, and expected goals (xG),
#' expected assists (xA), defensive contributions, and passing accuracy.
#'
#' @param goals Number of goals scored.
#' @param assists Number of assists.
#' @param passes Total completed passes.
#' @param key_passes Passes leading to a shot.
#' @param tackles Number of tackles won.
#' @param interceptions Number of interceptions.
#' @param clearances Number of clearances.
#' @param shots Total shots taken.
#' @param xG Expected Goals from shots taken.
#' @param xA Expected Assists from key passes.
#' @param passing_accuracy Percentage of accurate passes (0-100).
#' @param minutes_played Total minutes played.
#' @param saves Number of goalkeeper saves.
#' @param goals_conceded Goals conceded by the goalkeeper.
#' @param position Player position ("Forward", "Midfielder", "Defender", "Goalkeeper").
#'
#' @return A numeric performance score from 1-10.
#' @export
player_performance <- function(goals, assists, passes, key_passes, tackles, interceptions, clearances,
                               shots, xG, xA, passing_accuracy, minutes_played, saves, goals_conceded, position) {

  # --- Original Stats ---
  goal_score <- (goals * 4.5) + (xG * 3.5) + (shots * 0.3)
  assist_score <- (assists * 3) + (xA * 2.5) + (key_passes * 0.5)
  defense_score <- (tackles * 1.2) + (interceptions * 1.5) + (clearances * 0.8)
  pass_score <- (passing_accuracy / 10) + (passes * 0.05)

  # --- New Goalkeeper Score ---
  if (position == "Goalkeeper") {
    gk_score <- (saves * 2) - (goals_conceded * 2)
  } else {
    gk_score <- 0  # Not applicable for outfield players
  }

  # --- Position-Specific Weights ---
  if (position == "Forward") {
    final_score <- goal_score + (assist_score * 1.2) + (pass_score * 0.5)
  } else if (position == "Midfielder") {
    final_score <- (goal_score * 0.8) + (assist_score * 1.5) + (defense_score * 1.1) + pass_score
  } else if (position == "Defender") {
    final_score <- (defense_score * 2) + (pass_score * 0.7) + (goal_score * 0.5)
  } else if (position == "Goalkeeper") {
    final_score <- gk_score + (defense_score * 1.5) + (pass_score * 0.7)
  } else {
    final_score <- goal_score + assist_score + defense_score + pass_score
  }

  # --- Normalize to 1-10 Scale ---
  rating <- pmin(pmax(final_score / 10, 1), 10)  # Ensure rating is within 1-10 range

  return(round(rating, 1))
}

# --- Example Usage ---
# player_performance(goals = 2, assists = 1, passes = 60, key_passes = 5, tackles = 3,
#                    interceptions = 2, clearances = 1, shots = 4, xG = 1.2, xA = 0.7,
#                    passing_accuracy = 85, minutes_played = 90, saves = 3,
#                    goals_conceded = 1, position = "Midfielder")

