library(tidyverse)
library(ggimage)
library(gt)
library(gtExtras)
library(ggthemes)
library(ggrepel)


rm(list = ls())

pbp_2011_2016 <- read.csv("pbp_2011-2016.csv")
pbp_2017_2022 <- read.csv("pbp_2017-2022.csv")

player_stats_by_week <- read.csv("player_stats_by_week.csv")
nexgenstats_receiving <- read.csv("nexgenstats_pass.csv")

pbp_all_years1 <- rbind(pbp_2011_2016, pbp_2017_2022)

##### FIlter skill positions ####

skill_players <- c("RB", "FB", "WR", "TE")
default_color <- "gray"



##### here we create the datasets for skill offensive players #####

all_skill_players_regular_runners_season_99 <- player_stats_by_week %>% filter(season_type=="REG") %>% 
  filter(position%in% skill_players) %>% 
  select(player_id, player_display_name, position, recent_team, season,
         carries, rushing_yards, rushing_tds, rushing_fumbles, rushing_fumbles_lost,
         rushing_first_downs,rushing_epa,
         receptions, targets, receiving_yards,receiving_tds, receiving_fumbles,
         receiving_fumbles_lost, receiving_air_yards, receiving_yards_after_catch,
         receiving_first_downs, receiving_epa, target_share, air_yards_share, wopr,
         fantasy_points, fantasy_points_ppr, headshot_url) %>% 
  group_by(player_id, season, position,headshot_url) %>% 
  reframe(name=first(player_display_name),
          team=first(recent_team),
          total_rushing_yds= round(sum(rushing_yards)),
          total_carries_per_game=round(mean(carries),0),
          total_carries= sum(carries),
          rushing_yards_per_run=round(total_rushing_yds/total_carries,0),
          rushing_yards_game=round(mean(rushing_yards),0),
          total_rushing_tds=round(sum(rushing_tds),0),
          rushing_tds_per_game=round(mean(rushing_tds),2),
          total_rushing_first_downs=sum(rushing_first_downs),
          avg_rushing_epa=round(mean(rushing_epa, na.rm=TRUE),2),
          total_rushing_fumbles=sum(rushing_fumbles),
          total_rushing_fumbles_lost=sum(rushing_fumbles_lost),
          total_receptions=sum(receptions),
          total_receptions_per_game=round(mean(receptions),0),
          touches_per_game=sum(total_receptions_per_game+total_carries_per_game),
          total_targets=sum(targets),
          total_attempts_touches=sum(total_targets+total_carries_per_game) , 
          reception_rate_of_all_possibles=round((total_receptions/total_targets),2),
          total_receiving_yards= sum(receiving_yards),
          total_receiving_yards_per_game = mean(receiving_yards),
          recieving_yards_per_reception=round((total_receiving_yards/total_receptions),0),
          total_receiving_tds=sum(receiving_tds),
          total_receiving_fumbles= sum(receiving_fumbles),
          total_receiving_fumbles_lost= sum(receiving_fumbles_lost),
          total_receiving_first_downs=sum(receiving_first_downs),
          avg_receiving_epa=round(mean(receiving_epa, na.rm=TRUE),2),
          total_receiving_air_yards=sum(receiving_air_yards),
          percentage_air_yards=total_receiving_air_yards/total_receiving_yards,
          total_receiving_yards_after_catch=sum(receiving_yards_after_catch),
          percentage_after_yards=total_receiving_yards_after_catch/total_receiving_yards,
          total_first_downs=total_receiving_first_downs+total_rushing_first_downs,
          target_share_per_game=round(mean(target_share),2),
          air_yards_share_per_game=round(mean(air_yards_share),2),
          yards_after_catch_per_game= round(mean(receiving_yards_after_catch)),
          total_tds=total_receiving_tds+total_rushing_tds,
          total_fantasy_points=sum(fantasy_points),
          total_fantasy_points_ppr=sum(fantasy_points_ppr),
          total_yards=total_receiving_yards+total_rushing_yds
  ) %>% 
  mutate(rank = rank(desc(total_receiving_yards)),
         rank_total_carries_per_game= rank(desc(total_carries_per_game)),
         rank_total_rushing_tds= rank(desc(total_rushing_tds)),
         rank_avg_rushing_epa= rank(desc(avg_rushing_epa)),
         rank_touches_per_game= rank(desc(touches_per_game)),
         rank_total_receptions= rank(desc(total_receptions)),
         rank_total_targets= rank(desc(total_targets)),
         rank_yards_per_catch=rank(desc(recieving_yards_per_reception)),
         rank_total_receiving_yards_per_game= rank(desc(total_receiving_yards_per_game)),
         rank_total_receiving_first_downs= rank(desc(total_receiving_first_downs)),
         rank_reception_rate= rank(desc(reception_rate_of_all_possibles)),
         rank_rushing_yards_per_run= rank(desc(rushing_yards_per_run)),
         rank_rushing_yards_game= rank(desc(rushing_yards_game)),
         rank_total_tds= rank(desc(total_tds)),
         rank_total_receiving_tds= rank(desc(total_receiving_tds)),
         rank_total_receiving_yards_after_catch= rank(desc(total_receiving_yards_after_catch)),
         rank_total_receiving_air_yards= rank(desc(total_receiving_air_yards)),
         rank_catching_yards_season = rank(desc(total_receiving_yards))) %>% 
  ungroup() %>%
  group_by(season) %>%
  mutate(rank_total_receiving_yards_s= rank(desc(total_receiving_yards)),
         rank_total_rushing_tds_s= rank(desc(total_rushing_tds)),
         rank_avg_rushing_epa_s= rank(desc(avg_rushing_epa)),
         rank_touches_per_game_s= rank(desc(touches_per_game)),
         rank_total_receptions_s= rank(desc(total_receptions)),
         rank_total_targets_s= rank(desc(total_targets)),
         rank_yards_per_catch_s=rank(desc(recieving_yards_per_reception)),
         rank_total_receiving_yards_per_game_s= rank(desc(total_receiving_yards_per_game)),
         rank_total_receiving_first_downs_s= rank(desc(total_receiving_first_downs)),
         rank_reception_rate_s= rank(desc(reception_rate_of_all_possibles)),
         rank_rushing_yards_per_run_s= rank(desc(rushing_yards_per_run)),
         rank_rushing_yards_game_s= rank(desc(rushing_yards_game)),
         rank_total_tds_s= rank(desc(total_tds)),
         rank_total_receiving_tds_s= rank(desc(total_receiving_tds)),
         rank_total_receiving_yards_after_catch_s= rank(desc(total_receiving_yards_after_catch)),
         rank_total_receiving_air_yards_s= rank(desc(total_receiving_air_yards))
           ) %>% 
  unite(col = "name_year", c("name", "season"),
        sep = " ", remove = FALSE) %>%
  unite(col = "team_year", c("team", "season"),
        sep = " ", remove = FALSE)

##### filter by Cooks and teams of interest #####

teams <- all_skill_players_regular_runners_season_99 %>% filter(name=="Brandin Cooks" | team=="DAL") %>% 
  distinct(team_year)

##### get the data necesary for the images and team colors ####

all_skill_players_regular_runners_season_99 <- all_skill_players_regular_runners_season_99 %>% 
  ungroup() %>%
  left_join(teams_colors_logos, by=c("team"="team_abbr")) %>% #here we analyze the players ranking and teams
  mutate(color_override = ifelse(name=="Brandin Cooks", team_color2, default_color),
         color_override2= ifelse(team_year=="NO 2016" | team_year=="NO 2015", "black",ifelse (team_year %in% teams$team_year| name=="Brandin Cooks"|team=="DAL",
                                                                                              team_color, default_color)),
         image_override= ifelse(name=="Brandin Cooks"|team_year %in% teams$team_year, team_logo_espn, NA))

##### create the data sets to filter top 25 receivers ######

df_filtered <-  all_skill_players_regular_runners_season_99 %>% 
   filter(name == "Brandin Cooks", total_receiving_yards > 1000) %>% # filter for Brandin Cooks and total yards > 1000
   ungroup() %>% # remove grouping
   select(season, name, total_receiving_yards) # select the original columns


years <- df_filtered$season


top_wr_team_of_player <- all_skill_players_regular_runners_season_99 %>% 
  filter(rank_total_receiving_yards_s<25) %>% 
  filter(season %in% years) %>% 
  select(season,name_year,team_year, name, rank_total_receiving_yards_s, total_receiving_yards,
         color_override, color_override2,image_override,
         team_color, team_color2,team_logo_espn)

#####Top 25 data receivers #####


top_wr_team_of_player %>% 
  ggplot(aes(y = fct_reorder(name_year, total_receiving_yards), x = total_receiving_yards)) +
  geom_bar(aes(fill = color_override, color = color_override2), stat = "identity", size=.7) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  facet_wrap(~reorder(season, -season), ncol = 3, scales = "free_y") +
  geom_image(aes(image = image_override, x = total_receiving_yards + 100), asp=16/9, size = 0.05) +
  geom_text(data = filter(top_wr_team_of_player, name == "Brandin Cooks"), 
            aes(x = total_receiving_yards + 490, y = name_year, label = paste0(total_receiving_yards, " Yards")), 
            color = "black", size = 7, fontface = "bold") +
  theme_classic() +
  labs(x = "Receiving yards by season",
       y=NULL,
       title = "Thriving Among the Elite: Cooks has Dominated as a Top 25 WR in 6 out of 9 Seasons",
       subtitle = "Only receiving yards in regular season",
       caption = "By Helio Garcia 
       Source: NFLFastR") +
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(size = 25, hjust = 0.5, face = "bold", margin=margin(b=10)),
        plot.subtitle = element_text(size = 25, hjust = 0.5, margin=margin(b=20)),
        axis.text = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t=20)),
        strip.text = element_text(size = 25, face='bold'),
        plot.caption = element_text(size = 20)) +
  scale_y_discrete(labels = function(x) str_sub(x, end = -5))


ggsave("1Brandin_Cooks1.png", width = 25, height = 20, dpi = "retina") # saved look good

# top_wr_team_of_player %>% 
#   ggplot(aes(y = fct_reorder(name_year, total_receiving_yards), x = total_receiving_yards)) +
#   geom_bar(aes(fill = color_override, color = color_override2), stat = "identity", size=.7) +
#   scale_color_identity(aesthetics = c("fill", "color")) +
#   facet_wrap(~reorder(season, -season), ncol = 3, scales = "free_y") +
#   geom_image(aes(image = image_override, x = total_receiving_yards + 100), asp=16/9, size = 0.043) +
#   geom_text(data = filter(top_wr_team_of_player, name == "Brandin Cooks"), 
#             aes(x = total_receiving_yards + 350, y = name_year, label = paste0(total_receiving_yards, " Yards")), 
#             color = "black", size = 3, fontface = "bold") +
#   theme_classic() +
#   labs(x = "Receiving yards by season",
#        y=NULL,
#        title = "Thriving Among the Elite: Cooks has Dominated as a Top 25 WR in 6 out of 9 Seasons",
#        subtitle = "Only receiving yards in regular season",
#        caption = "By Helio Garcia 
#        Source: NFLFastR") +
#   theme(panel.grid.major = element_blank(),
#         plot.title = element_text(size = 15, hjust = 0.5, face = "bold", margin=margin(b=10)),
#         plot.subtitle = element_text(size = 15, hjust = 0.5, margin=margin(b=20)),
#         axis.text = element_text(size = 10, face = "bold"),
#         axis.title.x = element_text(size = 10, face = "bold", margin = margin(t=20)),
#         strip.text = element_text(size = 15, face='bold'),
#         plot.caption = element_text(size = 10)) +
#   scale_y_discrete(labels = function(x) str_sub(x, end = -5))

# ggsave("Brandin_Cooks3.png", width = 15, height = 10) for the website

###### get the data for offense week #####


total_offense_reg_1999_week <- pbp_all_years1 %>%
  filter( season_type=="REG") %>% #filter (penalty!=1) %>% 
  select(week, season,game_id, posteam, 
         yards_gained, air_yards, yards_after_catch, epa, first_down_rush,
         first_down_pass, first_down_penalty, first_down,
         third_down_converted, third_down_failed, fourth_down_converted, fourth_down_failed, interception,
         incomplete_pass, rush_attempt, pass_attempt, sack,
         pass_touchdown, rush_touchdown, two_point_attempt,
         passing_yards, rushing_yards, complete_pass, shotgun, no_huddle, qb_dropback, qb_scramble,
         pass_length, pass_location, run_location, run_gap, kick_distance, extra_point_result, total_home_epa, total_away_epa,
         total_home_rush_epa, total_away_rush_epa, total_home_pass_epa, total_away_pass_epa, air_epa, yac_epa, fumble_forced, fumble_not_forced,
         qb_hit, sack, field_goal_attempt, field_goal_result, fumble_lost, return_yards) %>% 
  filter(!is.na(posteam)) %>% 
  group_by(game_id, posteam, week) %>% 
  reframe(
    total_yards=sum(yards_gained, na.rm=TRUE),
    total_air_yards_Complete_pass=sum(ifelse(complete_pass==1,air_yards,0),na.rm=TRUE),
    total_air_yards_Incomplete_pass=sum(ifelse(incomplete_pass==1,air_yards,0),na.rm=TRUE),
    mean_epa=mean(round(epa,0)),
    total_first_down_rush=sum(first_down_rush, na.rm=TRUE),
    total_first_down_pass=sum(first_down_pass, na.rm = TRUE),
    total_first_down_penalty=sum(first_down_penalty, na.rm=TRUE),
    reported_total_first_down=sum(first_down, na.rm=TRUE),
    total_first_down=total_first_down_pass+total_first_down_rush+total_first_down_penalty,
    total_third_down_converted=sum(third_down_converted, na.rm=TRUE),
    total_third_down_failed=sum(third_down_failed, na.rm=TRUE),
    third_down_convertion=round((total_third_down_converted/(total_third_down_converted+total_third_down_failed)*100),0),
    total_complete_pass=sum(complete_pass, na.rm=TRUE),
    total_incomplete_pass=sum(incomplete_pass, na.rm=TRUE),
    total_passes=total_complete_pass+total_incomplete_pass,
    total_pass_attempt=sum(pass_attempt, na.rm = TRUE), #tengo que hacerlo para toda las jugadas válidas
    complete_pass_convertion=round((total_complete_pass/(total_complete_pass+total_incomplete_pass)*100),0),
    complete_pass_convertion1=round((total_complete_pass/(sum(pass_attempt, na.rm = TRUE))*100),0),
    total_intereceptions=sum(interception, na.rm = TRUE),
    total_pass_touchdown=sum(pass_touchdown,na.rm = TRUE),
    total_passing_yards=sum(passing_yards, na.rm=TRUE),
    total_rush_attempt=sum(rush_attempt, na.rm = TRUE),
    total_rushing_yards=sum(rushing_yards, na.rm = TRUE),
    total_pass_attempt=sum(pass_attempt,na.rm = TRUE),
    total_rush_touchdown=sum(rush_touchdown,na.rm = TRUE),
    total_two_pt_conv=sum(two_point_attempt, na.rm = TRUE)
  ) %>% 
  mutate(season=substr(game_id, 0,4)) %>% 
  filter(posteam!="")

###### get the data for offense season #####


total_offense_reg_1999_season <- total_offense_reg_1999_week %>%
  group_by( posteam, season) %>% 
  reframe(
    total_yards=sum(total_yards, na.rm=TRUE),
    total_air_yards_Complete_pass=sum(total_air_yards_Complete_pass,na.rm=TRUE),
    total_air_yards_Incomplete_pass=sum(total_air_yards_Incomplete_pass,na.rm=TRUE),
    mean_epa=mean(mean_epa),
    total_first_down_rush=sum(total_first_down_rush, na.rm=TRUE),
    total_first_down_pass=sum(total_first_down_pass, na.rm = TRUE),
    total_first_down_penalty=sum(total_first_down_penalty, na.rm=TRUE),
    reported_total_first_down=sum(reported_total_first_down, na.rm=TRUE),
    total_first_down=sum(total_first_down),
    total_third_down_converted=sum(total_third_down_converted, na.rm=TRUE),
    total_third_down_failed=sum(total_third_down_failed, na.rm=TRUE),
    third_down_convertion=round(mean(third_down_convertion),0),
    total_complete_pass=sum(total_complete_pass, na.rm=TRUE),
    total_incomplete_pass=sum(total_incomplete_pass, na.rm=TRUE),
    total_passes=sum(total_passes),
    total_pass_attempt=sum(total_pass_attempt, na.rm = TRUE), #tengo que hacerlo para toda las jugadas válidas
    complete_pass_convertion=round(mean(complete_pass_convertion),0),
    complete_pass_convertion1=round(mean(complete_pass_convertion1),0),
    total_intereceptions=sum(total_intereceptions, na.rm = TRUE),
    total_pass_touchdown=sum(total_pass_touchdown,na.rm = TRUE),
    total_passing_yards=sum(total_passing_yards, na.rm=TRUE),
    total_rush_attempt=sum(total_rush_attempt, na.rm = TRUE),
    total_rushing_yards=sum(total_rushing_yards, na.rm = TRUE),
    total_pass_attempt=sum(total_pass_attempt,na.rm = TRUE),
    total_rush_touchdown=sum(total_rush_touchdown,na.rm = TRUE),
    total_two_pt_conv=sum(total_two_pt_conv, na.rm = TRUE),
  )


##### analyze the contribuition of the players of interest ####

players_contribuition <- merge( all_skill_players_regular_runners_season_99, total_offense_reg_1999_season, by.x = c('team', 'season'), 
                                by.y = c('posteam', 'season'), all=TRUE) %>% filter(team_year %in% teams$team_year) %>% 
  mutate(recieving_contribuition=total_receiving_yards/total_passing_yards*100,
         air_target_contribuition=total_receptions/total_complete_pass*100,
         last_name = sub(".*\\s", "", name),
         last_name_year= paste(last_name, season, sep=' ' )) 

##### make the graph of interest ####


players_contribuition %>% filter(team_year %in% teams$team_year, season>=2014,
                                  air_target_contribuition>1,
                                  recieving_contribuition>1) %>% 
  ggplot(aes(x=(recieving_contribuition), y=(air_target_contribuition))) + 
  geom_point(aes(fill = team_color2, color = team_color, size=total_receiving_yards), 
             shape = 21, alpha = 1.1, stroke=1.)+
  scale_color_identity(aesthetics = c("fill", "color"))+
  theme_classic()+ 
  geom_label_repel(data = filter(players_contribuition,  name=="Brandin Cooks" | name=="CeeDee Lamb", season>=2014),
                   aes(y=air_target_contribuition, x= recieving_contribuition,label = paste0(last_name_year)), 
                   fontface = 'bold', color = 'black',
                   box.padding = unit(.9, "lines"),
                   point.padding = unit(0, "lines"),
                   segment.color = 'grey50',
                   size=4)+
  labs(x = "Air yards share of team's 
       total air yards",
       y="Reception share of team's
       total receptions",
       title = "Cooks: Has contributed with more than 1 of every 5 Yards in 6 Seasons",
       subtitle = "The graph only shows Dallas receivers and Brandin Cooks teamates during 2014-2022",
       caption = "By Helio Garcia. 
       Source: NFLFastR",
       size="Total receiving yards")+
  theme(plot.title = element_text(size = 25, hjust = 0.5, face = "bold", margin=margin(b=20)),
        plot.subtitle = element_text(size = 16, hjust = 0.5),
        axis.text = element_text(size = 20, face = "bold"),
        axis.title.x = element_text(size = 20, face = "bold", margin = margin(t=20)),
        axis.title.y = element_text(size = 20, face = "bold", margin = margin(r=15)),
        legend.position = "right", legend.direction = "vertical",
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face="bold"),
        plot.caption = element_text(size = 12))

ggsave("1sCorntribuition Brandin_Cooks Ce.png", width = 15, height = 10, dpi = "retina") 


######  data for the carrerr of players #####


all_skill_players_careers_99 <- player_stats_by_week  %>% 
  filter(position%in% skill_players) %>% 
  select(player_id, player_display_name, position, recent_team, season,
         carries, rushing_yards, rushing_tds, rushing_fumbles, rushing_fumbles_lost,
         rushing_first_downs,rushing_epa,
         receptions, targets, receiving_yards,receiving_tds, receiving_fumbles,
         receiving_fumbles_lost, receiving_air_yards, receiving_yards_after_catch,
         receiving_first_downs, receiving_epa, target_share, air_yards_share, wopr,
         fantasy_points, fantasy_points_ppr, headshot_url) %>% 
  group_by(player_id, player_display_name, headshot_url) %>% 
  reframe(name=first(player_display_name),
          total_rushing_yds= round(sum(rushing_yards)),
          total_carries_per_game=round(mean(carries),0),
          total_carries= sum(carries),
          rushing_yards_per_run=round(total_rushing_yds/total_carries,0),
          rushing_yards_game=round(mean(rushing_yards),0),
          total_rushing_tds=round(sum(rushing_tds),0),
          rushing_tds_per_game=round(mean(rushing_tds),2),
          total_rushing_first_downs=sum(rushing_first_downs),
          avg_rushing_epa=round(mean(rushing_epa, na.rm=TRUE),2),
          total_rushing_fumbles=sum(rushing_fumbles),
          total_rushing_fumbles_lost=sum(rushing_fumbles_lost),
          total_receptions=sum(receptions),
          total_receptions_per_game=round(mean(receptions),0),
          touches_per_game=sum(total_receptions_per_game+total_carries_per_game),
          total_targets=sum(targets),
          total_attempts_touches=sum(total_targets+total_carries_per_game) , 
          reception_rate_of_all_possibles=round((total_receptions/total_targets),2),
          total_receiving_yards= sum(receiving_yards),
          total_receiving_yards_per_game = mean(receiving_yards),
          recieving_yards_per_reception=round((total_receiving_yards/total_receptions),0),
          total_receiving_tds=sum(receiving_tds),
          total_receiving_fumbles= sum(receiving_fumbles),
          total_receiving_fumbles_lost= sum(receiving_fumbles_lost),
          total_receiving_first_downs=sum(receiving_first_downs),
          first_downs_per_game=mean(receiving_first_downs),
          avg_receiving_epa=round(mean(receiving_epa, na.rm=TRUE),2),
          total_receiving_air_yards=sum(receiving_air_yards),
          percentage_air_yards=total_receiving_air_yards/total_receiving_yards,
          total_receiving_yards_after_catch=sum(receiving_yards_after_catch),
          percentage_after_yards=total_receiving_yards_after_catch/total_receiving_yards,
          total_first_downs=total_receiving_first_downs+total_rushing_first_downs,
          target_share_per_game=round(mean(target_share),2),
          air_yards_share_per_game=round(mean(air_yards_share),2),
          yards_after_catch_per_game= round(mean(receiving_yards_after_catch)),
          total_tds=total_receiving_tds+total_rushing_tds,
          total_fantasy_points=sum(fantasy_points),
          total_fantasy_points_ppr=sum(fantasy_points_ppr),
          total_yards=total_receiving_yards+total_rushing_yds
  ) %>% 
  mutate(rank = rank(desc(total_receiving_yards)),
         rank_total_carries_per_game= rank(desc(total_carries_per_game)),
         rank_total_rushing_tds= rank(desc(total_rushing_tds)),
         rank_avg_rushing_epa= rank(desc(avg_rushing_epa)),
         rank_touches_per_game= rank(desc(touches_per_game)),
         rank_total_receptions= rank(desc(total_receptions)),
         rank_total_targets= rank(desc(total_targets)),
         rank_yards_per_catch=rank(desc(recieving_yards_per_reception)),
         rank_total_receiving_yards_per_game= rank(desc(total_receiving_yards_per_game)),
         rank_total_receiving_first_downs= rank(desc(total_receiving_first_downs)),
         rank_reception_rate= rank(desc(reception_rate_of_all_possibles)),
         rank_rushing_yards_per_run= rank(desc(rushing_yards_per_run)),
         rank_rushing_yards_game= rank(desc(rushing_yards_game)),
         rank_total_tds= rank(desc(total_tds)),
         rank_total_receiving_tds= rank(desc(total_receiving_tds)),
         rank_total_receiving_yards_after_catch= rank(desc(total_receiving_yards_after_catch)),
         rank_total_receiving_air_yards= rank(desc(total_receiving_air_yards)),
         rank_catching_yards_season = rank(desc(total_receiving_yards)))

##### filter the player of interest for the new viz #####

filter_players_of_interest1 <- players_contribuition %>% 
  filter(team_year %in% teams$team_year, season>=2014, team=="DAL"| name=="Brandin Cooks",
         air_target_contribuition>10,
         recieving_contribuition>10) %>% 
  select(headshot_url,name) %>% distinct()

data_base1 <- merge(filter_players_of_interest1, all_skill_players_careers_99, by.x = 'name',
                    by.y = 'name', all.x = TRUE) %>% 
  reframe(name,
          total_receiving_yards_per_game=round((total_receiving_yards_per_game),0),
          recieving_yards_per_reception,
          percentage_after_yards=round((total_receiving_yards_after_catch/total_receiving_yards*100),0),
          percentage_air_yards=100-percentage_after_yards,
          percentage_after_yards
  )

####genstats data acquired #####

gen_stats_for_db <- nexgenstats_receiving %>% 
  filter(player_display_name %in% data_base1$name, week==0, season_type=="REG") %>% 
  group_by(player_display_name) %>% 
  reframe(avg_cushion=round(mean(avg_cushion),2),
          avg_separation=round(mean(avg_separation),2),
          avg_separation=round(mean(avg_separation),2),
          avg_intended_air_yards=round(mean(avg_intended_air_yards),2),
          percent_share_of_intended_air_yards=round(mean(percent_share_of_intended_air_yards),2),
          targets=sum(targets),
          receptions=sum(receptions),
          true_catch=round((receptions/targets*100),2))

#now we merge both and try to add the headshot_url


both_db_h <-  merge( filter_players_of_interest1,both_db, by.x = "name",
                     by.y = "name", all.x = TRUE)

gtable <- both_db_h %>% filter(!name%in%c("Cedrick Wilson", "Ezekiel Elliott", "Tony Pollard", "DeMarco Murray")) %>% 
  arrange(-total_receiving_yards_per_game) %>% 
  select(headshot_url, name,total_receiving_yards_per_game,percentage_air_yards, avg_intended_air_yards, percent_share_of_intended_air_yards
  ) %>% 
  gt() %>% 
  cols_align(align= "center") %>% 
  gtExtras::gt_img_rows(headshot_url, height = 40) %>% 
  cols_label(headshot_url="", name="",
             total_receiving_yards_per_game="Yards per game", 
             percentage_air_yards="% of all total yards gained 
             thru air ",
             avg_intended_air_yards="Avg. intended
             air yards", percent_share_of_intended_air_yards="% share of intended
             air yards"
             
  ) %>% 
  tab_header(
    title = "Brandin Cooks vs Dallas's WRs Career Stats
    (2014-2022)") %>% #gt_color_rows(
  #total_receiving_yards_per_game:true_catch, palette = c("red", "green")) %>% 
  gtExtras::gt_hulk_col_numeric(total_receiving_yards_per_game:percent_share_of_intended_air_yards) %>% 
  gtExtras::gt_theme_espn() %>% opt_align_table_header(align = "center") 


gtsave(gtable,filename = "tab_2.html", inline_css = TRUE)

