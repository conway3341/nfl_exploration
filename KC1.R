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
  mutate(passer = ifelse(play_type == "pass",
                         str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((pass)|(sack)|(scramble)))"),
                         NA),
         rusher = ifelse(play_type == "run",
                         str_extract(desc, "(?<=\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?(?=\\s((scramble)|(left end)|(left tackle)|(left guard)|(up the middle)|(right guard)|(right tackle)|(right end)))"),
                         NA),
         receiver = ifelse(play_type == "pass",
                           str_extract(desc, "(?<=to\\s)[A-Z][a-z]*\\.\\s?[A-Z][A-z]+(\\s(I{2,3})|(IV))?"),
                           NA),
      KC_pos = ifelse(posteam == "KC", "Offense", "Defense"),
      play = 1)
  
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
    strip.background = element_rect(color = "black", fill = "white")
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
       caption = "Data: nflscrapR") +
  geom_text(data = df.text, aes(x, y, label = lab.text), colour = "red") +
  theme_main()


# EPA ~ air yards

## facet by position 

# Mahomes completion % by air yards: "traditional"


# Mahomes completion % by air yards: GAM
## Mahomes 2018 vs. 2019

 
