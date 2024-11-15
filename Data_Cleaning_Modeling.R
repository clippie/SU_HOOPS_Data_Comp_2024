library(rvest)
library(rPref)
library(tidyverse)
library(factoextra)

# Load Data and set up schedule
#-------------------------------------------------------------------------------
bball <- read.csv("PBP2324.csv")

opps <- c("Syracuse", "Le Moyne", "Colgate", "Youngstown St", "Texas", "Cornell", "Tennessee", "Notre Dame",
          "UAlbany", "Georgetown", "Maryland", "Bucknell", "Wake Forest", "Florida St",
          "Georgia Tech", "Boston College", "Louisville", "Notre Dame", "Clemson",
          "Pitt", "Stanford", "California", "Duke", "Boston College", "Miami",
          "North Carolina", "Pitt", "NC State", "Virginia Tech",
          "SMU","Virginia")

uopps <- unique(opps)

uopps2 <- tolower(uopps) %>%
  str_replace_all(" ", "-") %>%
  str_replace_all("-st", "-state") %>%
  str_replace("ualbany", "albany-ny") %>%
  str_replace("miami", "miami-fl") %>%
  str_replace("pitt", "pittsburgh") %>%
  str_replace("nc-stateate", "north-carolina-state") %>%
  str_replace("smu", "southern-methodist")

# Scrape Sports Reference for other stats
#------------------------------------------------------------------------------

combined_df <- data.frame()

for (i in uopps2) {
  url <- paste0("https://www.sports-reference.com/cbb/schools/", i, "/men/2024.html")

  page <- tryCatch({
    read_html(url)
  }, error = function(e) {
    message("Error with ", i, ": ", e$message)
    return(NULL)
  })

  if (!is.null(page)) {
    table <- page %>%
      html_element("#advanced") %>%
      html_table()

    table$team <- i

    combined_df <- bind_rows(combined_df, table)

    Sys.sleep(runif(1, min = 1, max = 5))
  }
}

positions <- data.frame()

for (i in uopps2) {
  url <- paste0("https://www.sports-reference.com/cbb/schools/", i, "/men/2024.html")

  page <- tryCatch({
    read_html(url)
  }, error = function(e) {
    message("Error with ", i, ": ", e$message)
    return(NULL)
  })

  if (!is.null(page)) {
    table <- page %>%
      html_element("table") %>%
      html_table()

    table$team <- i

    positions <- bind_rows(positions, table)

    Sys.sleep(runif(1, min = 1, max = 5))
  }
}

# Combine Position and USG%
#------------------------------------------------------------------------------
pos_usg <- left_join(positions, combined_df, by = 'Player')

pos_usg <- pos_usg %>%
  mutate(
    Height_Inches = str_split(Height, "-", simplify = TRUE) %>%
      apply(1, function(x) as.numeric(x[1]) * 12 + as.numeric(x[2]))
  ) %>%
  select(Player, Pos, Height_Inches, `USG%`)

pos_usg <- pos_usg[-c(43, 347), ]

pos_usg <- pos_usg %>%
  mutate(Player = if_else(Player == "D.J. Burns" & Height_Inches == 81,
                          "D.J. Burns Jr.", Player))

# Add other stats to a play-by-play subset
#------------------------------------------------------------------------------
tot_pbp <- bball %>% filter(home %in% uopps | away %in% uopps)%>%
  mutate(
    shooter = str_replace(shooter, "Cam Corhen", "Cameron Corhen"),
    shooter = str_replace(shooter, "DJ Burns Jr.", "D.J. BurnsJr."),
    shooter = str_replace(shooter, "DJ Burns", "D.J. Burns"),
    shooter = str_replace(shooter, "E.J. Farmer", "EJ Farmer"),
    shooter = str_replace(shooter, "Elvin Edmonds IV", "Elvin Edmonds"),
    shooter = str_replace(shooter, "Jae'Lyn Withers", "Jae'lyn Withers"),
    shooter = str_replace(shooter, "Josh Beadle", "Joshua Beadle"),
    shooter = str_replace(shooter, "Julian Roper II", "Julian Roper"),
    shooter = str_replace(shooter, "Mike James", "Michael James")
  )

tot_pbp$shooter <- gsub(" III", "", tot_pbp$shooter)

tot_pbp$shooter <- gsub(" Jr\\.", "", tot_pbp$shooter)

tot_pbp <- tot_pbp %>%
  mutate(shooter = str_replace(shooter, "D.J. BurnsJr.", "D.J. Burns Jr."))

tot_pbp$two_pt <- ifelse(tot_pbp$three_pt == "FALSE" & tot_pbp$free_throw == "FALSE", "TRUE", "FALSE")

tot_pbp$two_pt <- as.integer(as.logical(tot_pbp$two_pt))

tot_pbp$three_pt <-  as.integer(as.logical(tot_pbp$three_pt))

tot_pbp$made_two <- ifelse(tot_pbp$two_pt == "1" & tot_pbp$shot_outcome == "made", 1, 0)

tot_pbp$made_three <- ifelse(tot_pbp$three_pt == "1" & tot_pbp$shot_outcome == "made", 1, 0)

tot_pbp$half_end_shot <- ifelse((tot_pbp$secs_remaining < 1205 & tot_pbp$secs_remaining > 1199) & tot_pbp$shot_outcome == "made" & tot_pbp$free_throw != 1, 1, 0)

tot_pbp$tot_half_end_shot <- ifelse((tot_pbp$secs_remaining < 1205 & tot_pbp$secs_remaining > 1199) & tot_pbp$free_throw != 1, 1, 0)

tot_pbp$made_game_clutch_shot <- ifelse(tot_pbp$secs_remaining < 5 & tot_pbp$shot_outcome == "made" & tot_pbp$score_diff <= 5 & tot_pbp$free_throw != 1, 1, 0)

tot_pbp$tot_game_clutch_shot <- ifelse(tot_pbp$secs_remaining < 5 & tot_pbp$score_diff <= 5 & tot_pbp$free_throw != 1, 1, 0)

tot_pbp$tot_clutch_scoring <- ifelse(tot_pbp$secs_remaining <= 120 & tot_pbp$score_diff <= 5 & tot_pbp$free_throw != 1, 1, 0)

tot_pbp$tot_made_clutch_scoring <- ifelse(tot_pbp$secs_remaining <= 120 & tot_pbp$shot_outcome == "made" & tot_pbp$score_diff <= 5 & tot_pbp$free_throw != 1, 1, 0)

tot_pbp$tot_pts_clutch_scoring <- ifelse(tot_pbp$secs_remaining <= 120 &
                                      tot_pbp$shot_outcome == "made" &
                                      tot_pbp$score_diff <= 5 &
                                      tot_pbp$three_pt == 1, 3, ifelse(tot_pbp$secs_remaining <= 120 &
                                                                    tot_pbp$shot_outcome == "made" &
                                                                    tot_pbp$score_diff <= 5 &
                                                                    tot_pbp$two_pt == 1, 2, 0))

tot_pbp$two_pt_Jumper <- ifelse(grepl("Jumper", tot_pbp$description, ignore.case = TRUE) & tot_pbp$two_pt == 1, 1, 0)
tot_pbp$Dunk <- ifelse(grepl("Dunk", tot_pbp$description, ignore.case = TRUE), 1, 0)
tot_pbp$Layup <- ifelse(grepl("Layup", tot_pbp$description, ignore.case = TRUE), 1, 0)
tot_pbp$Assisted <- ifelse(grepl("Assisted", tot_pbp$description, ignore.case = TRUE), 1, 0)

tot_pbp_cluster_prof <- tot_pbp %>%
  filter(shot_team %in% uopps) %>%
  group_by(shooter) %>%
  summarise(tot_shots = (sum(three_pt) + sum(two_pt_Jumper) + sum(Dunk) + sum(Layup)),
            three_pt_jumper_pct = sum(three_pt)/tot_shots,
            two_pt_Jumper_pct = sum(two_pt_Jumper)/tot_shots,
            Dunk_pct = sum(Dunk)/tot_shots,
            Layup_pct = sum(Layup)/tot_shots,
            Assisted_pct = sum(Assisted)/tot_shots) %>%
  filter(tot_shots >= 50)

tot_cluster_prof <- left_join(tot_pbp_cluster_prof, pos_usg, by = c("shooter" = "Player"))

# Add shot clock variable
#-------------------------------------------------------------------------------

# Adjusting for Fouls and Offensive Rebounds
# finding the time left of shot clock when shot was taken but need to
# account for when the shot clock doesn't reset to 30 but instead 20

adjust_for_foul <- function(x) {
  x$time_left_on_clock <- rep(NA, nrow(x))
  time_diff <- dplyr::lag(x$secs_remaining) - x$secs_remaining
  time_diff <- 30 - time_diff
  for (i in 2:nrow(x)) {
    if (grepl("Offensive Rebound|Foul", x$description[i - 1])) {
      time_diff[i] <- 20 - (dplyr::lag(x$secs_remaining)[i] - x$secs_remaining[i])
    } else {
      time_diff[i] <- 30 - time_diff[i]
    }
    x$time_left_on_clock[i] <- time_diff[i]
  }
  return(x)
}


# Adjusting for Blocks
# finding the time left of shot clock when shot was taken but need to
# account for when the shot was block because data set doesn't reset shot
# clock on the rebound from the block but instead the block itself

adjust_for_rebound_and_block <- function(x) {
  for (i in 2:nrow(x)) {
    time_diff <- x$time_left_on_clock[i]

    if (grepl("Rebound", x$description[i - 1])) {
      if (i > 2 && grepl("Block", x$description[i - 2])) {
        time_diff <- x$secs_remaining[i - 2] - x$secs_remaining[i]
      }
    }
    x$time_left_on_clock[i] <- time_diff
  }
  return(x)
}


# Creating Data Frames for All Players "Clutch" Points and Percentages
# Clutch Shots are defined by us as shots taken with equal to or less than 2
# seconds on the shot clock when the shot hit the rim or the surrounding area

player_shot_stats_list <- list()

for (i in uopps) {
  updated_data <- tot_pbp %>%
    filter(home == i | away == i)

  updated_team_data <- adjust_for_foul(updated_data)
  updated_team_data2 <- adjust_for_rebound_and_block(updated_team_data)

  shot_data <- updated_team_data2 %>%
    filter(time_left_on_clock <= 2 & !free_throw & shot_team == i)  # Ensure this line has the correct closing parenthesis

  shot_stats <- shot_data %>%
    group_by(shooter) %>%
    summarise(
      total_shots_taken = n(),
      total_shots_made = sum(grepl("made", description)),
      total_points = sum(
        ifelse(three_pt == TRUE & grepl("made", description), 3, 0) +
          ifelse(three_pt == FALSE & grepl("made", description), 2, 0)
      ),
      shooting_percentage = total_shots_made / total_shots_taken
    ) %>%
    mutate(team = i)

  player_shot_stats_list[[i]] <- shot_stats
}


# Creating Final Data Frame for Player Clutch Stats

all_player_shot_stats <- bind_rows(player_shot_stats_list)


# Set up player profiles for analysis
#-------------------------------------------------------------------------------
player_prof <- tot_pbp %>%
  filter(shot_team %in% uopps) %>%
  group_by(shooter) %>%
  summarise(tot_threes = sum(three_pt),
            tot_twos = sum(two_pt),
            three_pt_pct = (sum(made_three)/sum(three_pt)),
            two_pt_pct = (sum(made_two)/sum(two_pt)),
            tot_half_end_shot = sum(tot_half_end_shot),
            half_end_shot_pct = (sum(half_end_shot)/sum(tot_half_end_shot)),
            tot_game_clutch_shot = sum(tot_game_clutch_shot),
            game_clutch_shot_pct = (sum(made_game_clutch_shot)/sum(tot_game_clutch_shot)),
            clutch_scoring_pct = (sum(tot_made_clutch_scoring)/sum(tot_clutch_scoring)),
            clutch_scoring_pts = sum(tot_pts_clutch_scoring)
            ) %>%
  filter((tot_twos + tot_threes) >= 50)

player_prof$weighted_three_pointers <- player_prof$three_pt_pct * sqrt(player_prof$tot_threes)
player_prof$weighted_two_pointers <- player_prof$two_pt_pct * sqrt(player_prof$tot_twos)
player_prof$weighted_half_end_shot <- player_prof$half_end_shot_pct * sqrt(player_prof$tot_half_end_shot)
player_prof$weighted_game_end_shot <- player_prof$game_clutch_shot_pct * sqrt(player_prof$tot_game_clutch_shot)
player_prof$weighted_clutch_scoring <- player_prof$clutch_scoring_pct * sqrt(player_prof$clutch_scoring_pts)

combined_player_prof <- left_join(player_prof, pos_usg, by = c("shooter" = "Player"))

combined_player_prof <- combined_player_prof %>%
  select(shooter, `USG%`, weighted_two_pointers, weighted_three_pointers, weighted_half_end_shot, weighted_game_end_shot, weighted_clutch_scoring) %>%
  mutate(across(everything(), ~ replace_na(.x, 0)))

combined_player_prof <- left_join(combined_player_prof, all_player_shot_stats, by = "shooter") %>%
  drop_na()

combined_player_prof$weighted_shot_clock_scoring <- combined_player_prof$shooting_percentage * sqrt(combined_player_prof$total_shots_taken)

combined_player_prof <- combined_player_prof %>%
  select(shooter, team, `USG%`, weighted_two_pointers, weighted_three_pointers, weighted_half_end_shot, weighted_game_end_shot, weighted_clutch_scoring, weighted_shot_clock_scoring)

# Use Multi-Objective Optimization (Pareto Front)
# to get best responses for 2 and 3 point players
#------------------------------------------------------------------------------------------------------------------------------

player_recommendations <- list()

for (i in uopps) {
  team_data <- combined_player_prof %>% filter(team == i)

  combined_standardized <- team_data %>%
    select(where(is.numeric)) %>%
    scale() %>%
    as.data.frame()

  combined_standardized$shooter_name <- team_data$shooter
  combined_standardized$team <- team_data$team

  p <- high(combined_standardized$`USG%`) * high(combined_standardized$weighted_two_pointers) *
    high(combined_standardized$weighted_three_pointers) * high(combined_standardized$weighted_half_end_shot) *
    high(combined_standardized$weighted_clutch_scoring) * high(combined_standardized$weighted_shot_clock_scoring)

  pareto_optimal <- psel(combined_standardized, p)

  pareto_optimal$score_twos <- 0.20 * pareto_optimal$`USG%` +
    0.40 * pareto_optimal$weighted_two_pointers +
    0.10 * pareto_optimal$weighted_three_pointers +
    0.10 * pareto_optimal$weighted_game_end_shot +
    0.05 * pareto_optimal$weighted_half_end_shot +
    0.10 * pareto_optimal$weighted_clutch_scoring +
    0.05 * pareto_optimal$weighted_shot_clock_scoring

  best_twos_observation <- pareto_optimal[order(-pareto_optimal$score_twos), ][1, ]

  pareto_optimal$score_threes <- 0.20 * pareto_optimal$`USG%` +
    0.10 * pareto_optimal$weighted_two_pointers +
    0.40 * pareto_optimal$weighted_three_pointers +
    0.10 * pareto_optimal$weighted_game_end_shot +
    0.05 * pareto_optimal$weighted_half_end_shot +
    0.10 * pareto_optimal$weighted_clutch_scoring +
    0.05 * pareto_optimal$weighted_shot_clock_scoring

  best_threes_observation <- pareto_optimal[order(-pareto_optimal$score_threes), ][1, ]

  player_recommendations[[i]] <- data.frame(
    team = i,
    twos_player = best_twos_observation$shooter_name,
    threes_player = best_threes_observation$shooter_name
  )
}

final_recommendations <- do.call(rbind, player_recommendations)

# K-means clustering
#------------------------------------------------------------------------------

tot_cluster_prof <- tot_cluster_prof %>%
  select(-tot_shots)

standardized_data <- scale(tot_cluster_prof[ , c(2,3,4,5,6,8,9)])

fviz_nbclust(standardized_data, kmeans, method = "wss")

k <- 4
set.seed(123)
kmeans_result <- kmeans(standardized_data, centers = k, nstart = 25)

cluster_summary <- aggregate(tot_cluster_prof, by = list(cluster = kmeans_result$cluster), FUN = mean)

cluster_labels <- c("Three Level Scoring Forward", "Roll and Cut Big", "Shooter", "Ball Handling Combo Guard")

tot_cluster_prof$cluster <- kmeans_result$cluster

tot_cluster_prof$cluster_named <- factor(kmeans_result$cluster, levels = 1:4, labels = cluster_labels)


# Visualization

pca_result <- prcomp(standardized_data)
pca_data <- as.data.frame(pca_result$x[, 1:2])

pca_data$cluster <- as.factor(kmeans_result$cluster)

pca_data$Clusters <- factor(kmeans_result$cluster, levels = 1:4, labels = cluster_labels)

ggplot(pca_data, aes(x = PC1, y = PC2, color = Clusters)) +
  geom_point() +
  stat_ellipse(type = "norm", level = 0.95, alpha = 0.2) +
  labs(title = "K-means Clustering with Ellipses") +
  theme_minimal()


# Final Data merging
#------------------------------------------------------------------------------
final_data <- left_join(final_recommendations, tot_cluster_prof, by = c("twos_player" = "shooter")) %>%
  select(team, twos_player, threes_player, cluster_named) %>%
  rename(twos_cluster = cluster_named)

final_data <- left_join(final_data, tot_cluster_prof, by = c("threes_player" = "shooter")) %>%
  select(team, twos_player, threes_player, twos_cluster, cluster_named) %>%
  rename(threes_cluster = cluster_named)

# Creating headshots columns
twos_headshots <- c("https://www.sports-reference.com/req/202302071/cbb/images/players/judah-mintz-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/luke-sutherland-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/keegan-records-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/brandon-rush-2.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/max-abmas-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/chris-manon-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/dalton-knecht-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/markus-burton-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/amare-marshall-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/jayden-epps-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/jahmir-young-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/noah-williamson-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/hunter-sallis-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/cameron-corhen-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/baye-ndongo-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/quinten-post-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/skyy-clark-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/chase-hunter-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/blake-hinson-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/andrej-stojakovic-2.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/jaylon-tyson-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/jared-mccain-2.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/kyshawn-george-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/rj-davis-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/dj-burns-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/hunter-cattoor-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/zhuric-phelps-1.jpg",
                    "https://www.sports-reference.com/req/202302071/cbb/images/players/blake-buchanan-3.jpg"
                    )
threes_headshots <- c("https://www.sports-reference.com/req/202302071/cbb/images/players/chris-bell-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/luke-sutherland-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/braeden-smith-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/brandon-rush-2.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/max-abmas-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/cooper-noard-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/dalton-knecht-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/braeden-shrewsberry-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/amare-marshall-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/jayden-epps-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/jahmir-young-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/jack-forrest-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/hunter-sallis-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/cameron-corhen-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/miles-kelly-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/quinten-post-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/skyy-clark-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/chase-hunter-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/blake-hinson-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/andrej-stojakovic-2.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/jalen-cone-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/jared-mccain-2.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/kyshawn-george-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/rj-davis-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/dj-horne-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/hunter-cattoor-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/zhuric-phelps-1.jpg",
                      "https://www.sports-reference.com/req/202302071/cbb/images/players/blake-buchanan-3.jpg")


final_data$twos_headshots <- twos_headshots
final_data$threes_headshots <- threes_headshots



# Save as csv
write.csv(final_data, "SUBBALL_Data_Comp_2024_final_data.csv", row.names = FALSE)
