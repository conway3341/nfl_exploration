# NFL data #

# install code
## devtools::install_github(repo = "maksimhorowitz/nflscrapR")

# load required packages
library(tidyverse)
library(na.tools)
library(ggimage)
library(teamcolors)
library(nflscrapR)
library(ggbeeswarm)
library(ggrepel)
library(mgcv)

## pbp_2019 <- read_csv(url("https://github.com/ryurko/nflscrapR-data/raw/master/play_by_play_data/regular_season/reg_pbp_2019.csv"))
## 
## kc_2019 <- pbp_2019 %>%
##   filter(home_team == "KC" | away_team == "KC")

# explore teamcolors

kc_colors <- teamcolors %>%
  filter(name == "Kansas City Chiefs") %>%
  select(name, primary, secondary, quaternary)

kc_primary <- pull(kc_colors, primary)
kc_secondary <- pull(kc_colors, secondary)
kc_quaternary <- pull(kc_colors, quaternary)

# compare KC offense 2018 vs. 2019
## 

# scrape 2018 + 2019 KC pbp data

kc_pbp_2018 <- scrape_season_play_by_play(season = 2018, type = "reg", teams = c("KC"))
kc_pbp_2019 <- scrape_season_play_by_play(season = 2019, type = "reg", teams = c("KC"))

# select variables from data then rbind, filter on only run and passes

clean_bind <- function(x, y, keep_pen = FALSE) {
  
  var_selector <- c("game_date", "play_id", "game_id", "home_team", "away_team", "posteam", "down", "ydstogo", "qtr",
                    "drive", "yrdln", "yardline_100", "desc", "play_type", "ep", "epa", "yards_gained", "air_yards",
                    "incomplete_pass", "wp")
  
  x <- x %>% 
    select(var_selector) %>%
    mutate(season = as.factor(as.character(min(lubridate::year(game_date)))))
  y <- y %>% 
    select(var_selector) %>%
    mutate(season = as.factor(as.character(min(lubridate::year(game_date)))))
  
  xy_bind <- rbind(x, y)
  xy_bind <- xy_bind %>%
    mutate(pass = ifelse(str_detect(desc, "( pass)|(sack)|(scramble)"), 1, 0),
           rush = ifelse(str_detect(desc, "(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)"), 1, 0),
           success = ifelse(epa > 0, 1, 0))
  
  # any further cleaning here
  
  # filter on only run and pass plays w/ epa
  if (keep_pen == FALSE) {
    
  xy_bind <- xy_bind %>% 
    dplyr::filter(!is.na(epa), play_type %in% c("run", "pass"))
  
  } else if (keep_pen == TRUE) {
    
  xy_bind <- xy_bind %>% 
    dplyr::filter(!is.na(epa), play_type %in% c("run", "pass", "no_play")) %>%
    dplyr::filter(pass == 1 | rush == 1)
    
  }
  
  # play_type to factor
  xy_bind$play_type <- as.factor(as.character(xy_bind$play_type))
  xy_bind <- xy_bind %>% 
  # extract passer, rusher, and receiver from desc
  mutate(passer = as.factor(ifelse(play_type == "pass",
                         str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                         NA)),
         rusher = as.factor(ifelse(play_type == "run",
                         str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((scramble)|(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))"),
                         NA)),
         receiver = as.factor(ifelse(play_type == "pass",
                           str_extract(desc, "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                           NA)),
      KC_pos = ifelse(posteam == "KC", "Offense", "Defense"),
      play = 1) %>%
    mutate(completed_pass = ifelse(incomplete_pass == 0 & pass == 1, 1, 0))
  
  return(xy_bind)
  
}

# build theme

theme_main <- function() {
  theme(
    # titles
    plot.title = element_text(hjust = 0.50, face = "bold"),
    plot.subtitle = element_text(hjust = 0.50, face = "bold"),
    
    # panel
    panel.background = element_blank(),
    panel.grid.major = element_line(color = "light grey"),
    panel.border = element_rect(color = "black", fill = NA),
    
    # axes
    axis.text.x = element_text(face = "bold"),
    
    # facet
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(color = "black", fill = "white"),
    
    # legend
    legend.key = element_rect(fill = "white"),
    legend.title = element_text(hjust = 0.5)
  )
    
}

# apply clean_bind function

kc_pbp_1819_c <- clean_bind(kc_pbp_2018, kc_pbp_2019, keep_pen = FALSE)
kc_pbp_1819_c_2 <- clean_bind(kc_pbp_2018, kc_pbp_2019, keep_pen = TRUE)

# compare KC pass vs. rush epa 2018 vs. 2019

# beeswarm

kc_pbp_1819_c %>%
  filter(posteam == "KC") %>%
  ggplot(aes(x = play_type, y = epa, color = play_type)) +
  geom_quasirandom(alpha = 0.10, show.legend = FALSE) +
  geom_boxplot(alpha = 0, show.legend = FALSE) + 
  facet_grid(~season) +
  labs(title = "KC EPA: 2018 vs. 2019",
       x = "Play Type",
       y = "EPA") +
  scale_color_manual(values = c(kc_primary, kc_secondary)) +
  theme_main()

# violin/box plots

kc_pbp_1819_c %>%
  filter(posteam == "KC") %>%
  ggplot(aes(x = play_type, y = epa, fill = play_type)) +
  geom_violin(alpha = 0.5, show.legend = FALSE) +
  geom_boxplot(width = 0.15, show.legend = FALSE) +
  facet_grid(~season) +
  labs(title = "KC EPA: 2018 vs. 2019",
       x = "Play Type",
       y = "EPA") +
  scale_fill_manual(values = c(kc_primary, kc_secondary)) +
  theme_main()
# insignificant, too many average plays to compare


# segment text
bound.label <- 0.38
df.text <- data.frame(lab.text = c("Run +, Pass -", "Run +, Pass +", "Run -, Pass +", "Run -, Pass -"), 
                      x = c(bound.label, bound.label, -1*bound.label, -1*bound.label), 
                      y = c(-1*bound.label, bound.label, bound.label, -1*bound.label))

# plot
## EPA/Play

## Chiefs off epa/play vs def epa/play: 2018 vs. 2019
# EPA 
## Avg. EPA
## need to figure out what filters analysts are using here

kc_pbp_1819_c %>%
  filter(wp > 0.20 & wp < 0.80) %>% # filtering on competitive plays only
  group_by(season, play_type) %>%
  summarize(epa_play = mean(epa)) %>%
  spread(play_type, epa_play) %>%
  ggplot(aes(x = run, y = pass, label = season)) +
  geom_point() +
  geom_text_repel() +
  geom_hline(aes(yintercept = 0), lty = 2, col = "red", alpha = 0.5) + 
  geom_vline(aes(xintercept = 0), lty = 2, col = "red", alpha = 0.5) +
  xlim(c(-0.45, 0.45)) +
  ylim(c(-0.45, 0.45)) +
  labs(x = "Rush EPA/Play",
       y = "Pass EPA/Play",
       title= "KC Pass vs. Rush EPA",
       subtitle = "Through week 16 of 2019",
       caption = "Data from nflscrapR; Filtered on meaningful plays") +
  geom_text(data = df.text, aes(x, y, label = lab.text), colour = "red") +
  theme_main()


# EPA ~ air yards

kc_pbp_1819_c %>%
  filter((passer == "P.Mahomes" & pass == 1) & !is.na(air_yards)) %>%
  group_by(season, passer, air_yards) %>%
  summarize(epa_play = mean(epa),
            comp_perc = (sum(completed_pass)/sum(pass)),
            n = n()) %>%
  ungroup() %>%
  ggplot(aes(x = air_yards, y = epa_play, col = season)) +
  geom_smooth(se = FALSE) +
  geom_point() +
  labs(x = "Air Yards",
       y = "EPA/Play",
       title = "Patrick Mahomes: EPA/Play by Air Yards Attempted",
       color = "Season") +
  scale_color_manual(values = c(kc_primary, kc_secondary)) +
  scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40, 50)) +
  scale_y_continuous(breaks = c(-2, 0, 2, 4, 6, 8)) +
  theme_main()
# seems as if Mahomes had slightly more impactful short/intermediate throws in 2018 and df throws in 2019
# interesting plot, but flawed (no sample size at each air yard)

# summary df: Mahomes pass attempts, air yards

mahomes_db <- kc_pbp_1819_c %>%
  filter((passer == "P.Mahomes" & pass == 1) & !is.na(air_yards)) %>%
  group_by(season, passer, air_yards) %>%
  summarize(epa_play = mean(epa),
            comp_perc = (sum(completed_pass)/sum(pass)),
            pass_comps = sum(completed_pass),
            pass_attempts = n()) %>%
  ungroup()

# plot EPA/Play by Air Yards Attempted
mahomes_db %>%
  ggplot(aes(x = air_yards, y = epa_play, col = season)) +
  geom_point() +
  stat_smooth(method = "loess", span = 0.75, se = FALSE) +
  labs(x = "Air Yards",
       y = "EPA/Play",
       title = "Patrick Mahomes: EPA/Play by Air Yards Attempted",
       color = "Season") +
  scale_color_manual(values = c(kc_primary, kc_secondary)) +
  scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40, 50)) +
  scale_y_continuous(breaks = c(-2, 0, 2, 4, 6, 8)) +
  theme_main()

## facet by position 

# Mahomes completion % by air yards: "traditional"

mahomes_db %>%
  ggplot(aes(x = air_yards, y = comp_perc, color = season)) +
  geom_point() + 
  stat_smooth(method = "loess", span= 0.75, se = FALSE) +
  labs(x = "Air Yards",
       y = "Completion %",
       title = "Patrick Mahomes: Completion % by Air Yards Attempted",
       color = "Season") +
  scale_color_manual(values = c(kc_primary, kc_secondary)) +
  scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40, 50, 60)) +
  scale_y_continuous(labels = scales::percent) +
  theme_main()

# color by pass attempts to show flaw of this method

mahomes_db %>%
  ggplot(aes(x = air_yards, y = comp_perc, color = pass_attempts)) +
  geom_point() + 
  # stat_smooth(method = "loess", span= 0.75, se = FALSE) +
  labs(x = "Air Yards",
       y = "Completion %",
       title = "Patrick Mahomes: Completion % by Air Yards Attempted [2018 - 2019 Week 16]",
       color = "Pass Attempts") +
  scale_x_continuous(breaks = c(-10, 0, 10, 20, 30, 40, 50, 60)) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_continuous(type = "viridis") +
  theme_main()


# Mahomes completion % by air yards: GAM
## Mahomes 2018 vs. 2019




