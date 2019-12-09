## This script is meant to read in team mlb data then manipulate it into graphable dataframes
##Filtering out years prior to 1985 due to mlb salary data

library(data.table)
library(tidyverse)

#reading in team yearly stats and salaries
mlb_team_yearly_data <- fread("mlb_data/Teams.csv")
mlb_player_yearly_salary <- fread("mlb_data/Salaries.csv")
#reading in 2019 team stats
mlb_2019_team_data <- fread("mlb_data/2019TeamData.csv")
#reading in 2017 to 2019 team salary data
mlb_missing_salary_data <- fread("mlb_data/2017to2019SALARY.csv")

#mainoulating team yearly salray to get a rough estiamte of 
#teams yearly salary
#combing team salary data and missing yearly data
mlb_team_yearly_salary <- mlb_player_yearly_salary %>%
  filter(yearID >= 1985) %>%
  group_by(teamID, yearID, lgID) %>%
  summarise(salaryTeam = sum(salary)) %>%
  full_join(mlb_missing_salary_data) %>%
  select(teamID,yearID,salaryTeam)



#filtering team yearly stat to include 1985 to 2018
#joing salary data to team stat data
#Adding 2019 team data to datafram
mlb_team_yearly_stat <- mlb_team_yearly_data %>%
  filter(yearID >= 1985) %>%
  full_join(mlb_2019_team_data)

#joining team stats and salary data
#also adding some basic stats such as batting average, obp, slg, etc
mlb_yearly_data <- mlb_team_yearly_stat %>%
  left_join(mlb_team_yearly_salary) %>%
  mutate(
    BA = H/AB,
    OBP = (H+BB+HBP)/(AB+BB+HBP+SF),
    SLG = ((H-`2B`-`3B`-HR)+(2*`2B`)+(3*`3B`)+(4*HR))/AB,
    OPS = OBP+SLG,
    TB = (H-`2B`-`3B`-HR)+(2*`2B`)+(3*`3B`)+(4*HR),
    WHIP = (HA/BBA)/IPouts,
    HNINE = 9*HA/IPouts,
    HRNINE = 9*HRA/IPouts,
    BBNINE = 9*BBA/IPouts,
    SONINE= 9*SOA/IPouts,
    SOBB = SOA/BBA)


#MLB win correlation dataset
mlb_yearly_cor_df <- mlb_yearly_data %>%
  select(teamID, franchID, yearID, salaryTeam, W
         ,BA, OPS, R, H, HR
         ,ERA, ER, WHIP,SO, SOBB)

mlb_yearly_corplot <- mlb_yearly_cor_df %>%
  select( W,salaryTeam,BA, OPS, R, H, HR
          ,ERA, ER, WHIP,SO, SOBB)

mlb_hitting_yearly_cor_df <- mlb_yearly_cor_df %>%
  select(teamID, franchID, yearID, W
         ,BA, OPS, R, H, HR)
mlb_hitting_yearly_splom_df <- mlb_hitting_yearly_cor_df %>%
  select(yearID, W ,BA, OPS, R, H, HR)

mlb_pitching_yearly_cor_df <- mlb_yearly_cor_df %>%
  select(teamID, franchID, yearID, W
         ,ERA, ER, WHIP,SO, SOBB)

mlb_pitching_yearly_splom_df <- mlb_pitching_yearly_cor_df %>%
  select(yearID, W
         ,ERA, ER, WHIP,SO, SOBB)
