#Libraries needed for project

library(doBy)
library(dplyr)
library(ggplot2)
library(tidyr)


coaches <-read.csv("coaches.csv")


# In the past 20 years several NFL Teams have moved or updated their name
# this just updates them to their current names in the NFL
coaches <- coaches %>%
  mutate(Tm = case_when(
    Tm == "SDG" ~ "LAC",
    Tm == "STL" ~ "LAR",
    Tm == "OAK" ~ "LVR",
    Tm == "JAC" ~ "JAX",
    TRUE ~ Tm  # Keep other teams unchanged
  ))

head(coaches)

# The data frame currently does not have a variable someone is a new head coach, for the season
# We will begin by finding any head coaches who did not appear the previous season and mark them
# as new coach

coaches <- coaches %>%
  arrange(Season) %>%
  mutate(NewCoach = ifelse(duplicated(paste(Coach, Tm)), 2, 1)) 

#This is all new cochaes for each season, some coachs have multiple tenures wit different
#So they may appear mulitple times such as Mike Mccarthy former Packers and Cowboys Coach

# However onoe issue that appeared is that all coaches in the 2005 season will appear as
# first time hires, when only 3 new coaches were hired in the beginning of the 2005 season 
# Nick Saban 	Romeo Crennel Mike Nolan

coaches <- coaches %>%
  arrange(Season) %>%  # Ensure order by season
  mutate(NewCoach = case_when(
    Season == 2005 & (Coach %in% c("Nick Saban", "Romeo Crennel", "Mike Nolan") | Interim == 1) ~ 1,
    duplicated(paste(Coach, Tm)) ~ 2,  # Returning coach
    TRUE ~ 1  # Default for actual new coaches
  ))

coaches_2005 <- coaches %>%
  filter(Season == 2005 & (Coach %in% c("Nick Saban", "Romeo Crennel", "Mike Nolan") | Interim == 1))
fix05 <-coaches %>%
  filter(!(Season == 2005 & (Coach %in% c("Nick Saban", "Romeo Crennel", "Mike Nolan") | Interim == 1)))
fix05 <- subset(fix05, Season==2005)
fix05$NewCoach <- 2

coaches <- coaches %>%
  filter(Season != 2005)

# Add back only the selected 2005 coaches

coaches <- bind_rows(coaches, coaches_2005)
coaches <- bind_rows(coaches, fix05)
new_coaches_summary <- coaches %>%
  filter(NewCoach == 1, Interim == 0) %>%  # Exclude interim coaches
  group_by(Minority) %>%  # Group by minority status (1 = minority, 0 = white)
  summarize(Count = n())


# Now we need to see the length of tenure of these coaches, so we will group each coachs tenure
# with a team, the amount of seasons they have been there, if the go to another these are seperate
# events 
coach_tenure <- coaches %>%
  group_by(Coach, Tm) %>%
  summarize(
    Start_Season = min(Season),
    End_Season = max(Season),
    Tenure = End_Season - Start_Season + 1,
    .groups = "drop"
  ) %>%
  arrange(Tm, Start_Season)


#Now we get the total games coached, won, loss or tied in these past 20 years for each coaches tenure,
coach_summary <- coaches %>%
  group_by(Coach, Tm) %>%
  summarize(
    GAMES = sum(G, na.rm = TRUE),
    WINS = sum(W, na.rm = TRUE),
    LOSSES = sum(L, na.rm = TRUE),
    TIE = sum(T, na.rm = TRUE),
    Minority = first(Minority),   # Assuming it's consistent over tenure
    NewCoach = min(NewCoach,  na.rm = TRUE),
    Interim = max(Interim, na.rm = TRUE),
    .groups = "drop"
  )

coach_tenure_summary <- coach_tenure %>%
  left_join(coach_summary, by = c("Coach", "Tm"))

# Updated to have newly hired coaches and coaches retained from the 2024 season are given a 0
# Since they will be the coaches this upcoming season
coach_tenure_summary <- coach_tenure_summary %>%
  mutate(Fired = if_else(End_Season != 2025, 1, 0))

#Adding in wining percentage for all coaches during their tenure
coach_tenure_summary <- coach_tenure_summary %>%
  mutate(WinPer = round((WINS + 0.5 * TIE) / GAMES, 3))
#Fix N/A coaches who haven't coached yet
coach_tenure_summary$WinPer[is.na(coach_tenure_summary$WinPer)] <- 0  

#Now that the data frame is made, i want to observe only coaches who have at minimum coached 16 games
#New coaches hired this year will not count as they have not coached any games with their new team
#also we exclude coaches hired prior to 2005 since we do not have their total win percentage it would 
#be unfair to include them What we are looking for is the the average win percentage of minority and 
#white coaches significantly different, is this a reason for low amount of minorities

coach_tenure_summary_min <- coach_tenure_summary %>% filter(GAMES > 15 & NewCoach==1)

#Summarize the difference in win percentage between white and minority coaches
summarize_winper <- coach_tenure_summary_min %>%
  group_by(Minority) %>%
  summarize(
    Mean_WinPer = mean(WinPer, na.rm = TRUE),
    Count = n()
  )
#There are 104 white coaches compared to 30 minority coaches, in the past 20 years
# white coaches who have meet the minimum standard for this analysis

#White coaches have a win percentage of 44%, while minority coaches have a win percentage of 38%
#Next a t-test will be performed to see if this significantly different between the two
t.test(WinPer ~ Minority, data = coach_tenure_summary_min)

# The t-test shows that minority coaches do have significantly different win percentage 
# from white coaches according to the data we collected

#Plotting this with a boxplot we can see that minority coaches and white coaches do have a different
#wining percentage distribution

ggplot(coach_tenure_summary_min, aes(x = factor(Minority), y = WinPer)) +
  geom_boxplot(fill = c("forestgreen", "orange")) +
  stat_summary(fun = min, geom = "point", shape = 18, size = 3, color = "black") +
  stat_summary(fun = max, geom = "point", shape = 18, size = 3, color = "black") +
  labs(
    x = "White(Green) Minority(Orange)",
    y = "Win Percentage",
    title = "Win % by Coach Racial Status"
  ) +
  theme(plot.title = element_text(hjust = 0.5))

# Next we will observe the tenure that each group has while their are in their position, for this
# Only  coaches that are not in interim position unless they have more than 1 year of tenure, helps
# keeps those coaches who were retained vs filling a position, again any head coach who was hired
# prior to 2005 are kept out

filtered_coach_tenure <- coach_tenure_summary %>% filter(Interim==0 | 
                                                                (Interim==1 & Tenure>1))
filtered_coach_tenure <- filtered_coach_tenure %>% filter(GAMES>0 & NewCoach==1)

# Here is a brief summary overview of the coaches we have in this data set, for coaches we have 107
# white coaches vs 24 minority coaches, some coaches were fired prior to completing a season so this
# is why some numbers have changed
summarize_tenure <- filtered_coach_tenure %>%
  group_by(Minority) %>%
  summarize(
    Mean_Tenure = mean(Tenure, na.rm = TRUE),
    Count = n()
  )

# We will perform a t-test again to see if there is a significant difference in the length of tenure
# between white and minority coaches. I also will perform a ChiSqaure test to see if minority 
# coaches a more likley to have shorting tenure
t.test(Tenure ~ Minority, data=filtered_coach_tenure)

tenure_table <- table(filtered_coach_tenure$Minority,
                      filtered_coach_tenure$Tenure <= 2)

chisq.test(tenure_table)

# There is not a significant difference in length of tenure for white and minority coaches
# showing that once a minority coach is hired they are afforded the same amount time with 
# their team. The histogram plot belows shows while total amounts are different, distribution
# is around the same

ggplot(filtered_coach_tenure, aes(x = Tenure, fill = factor(Minority))) +
  geom_histogram(binwidth = 1, position = "dodge", color = "black", width=5.5) +
  scale_fill_manual(
    values = c("0" = "forestgreen", "1" = "orange"),
    labels = c("White", "Minority"),
    name = "Coach Race" 
  ) +
  labs(
    title = "Distribution of NFL Coach Tenure by Minority Status",
    x = "Tenure (Seasons)",
    y = "Number of Coaches"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


# Finally we will take a look at hiring percentages over the years, over the years to see if the NFL
# and more importantly do the owners have a bias in their hiring, again we will be looking only at new
# coaches in the league, no coaches hired prior to the 2005 season
# Again we only want non-interim coaches unless they are retained
new_coaches <- coach_tenure_summary %>% filter(Interim==0 | 
                                                 (Interim==1 & Tenure>1))
new_coaches <- new_coaches %>% filter(NewCoach==1)

# Now we will make a new data frame showing the amount of minority and white coaches hire each
# season
new_coaches_by_year <- new_coaches %>%
  group_by(Start_Season, Minority) %>%
  summarize(Count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = Minority,
    values_from = Count,
    values_fill = 0,
    names_prefix = "Minority_"
  )
# Update column names
colnames(new_coaches_by_year) <- c("Year", "White", "Minority")

# First we will perform a in total 114 white coaches have been hired compared to 
# 30 minority coaches in that same time, also you can see that during some seasons
# no minority coahces are hired, in the 2025 off season no minority coaches were hired
# we will create a long version of the data from new_coaches_by_year to plot the values

longversion_new_coaches_by_year <- new_coaches_by_year %>%
  pivot_longer(cols = c("White", "Minority"),
               names_to = "Race",
               values_to = "Count")


# Barplot showing the difference in hiring for minorites for NFL Head coaching positions
ggplot(longversion_new_coaches_by_year, aes(x = factor(Year), y = Count, fill = Race)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("White" = "forestgreen", "Minority" = "black")) +
  labs(
    title = "New NFL Head Coaches Hires 2005–2025",
    x = "Year",
    y = "Number of Coaches",
    fill = "Race"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))

# Is this difference in coaching hiring significant different, we will perform a binomial test and a
# t-test to see if it is, first the binomial test. 
binom.test(x = sum(new_coaches_by_year$Minority), n = sum(new_coaches_by_year$White)
           + sum(new_coaches_by_year$Minority), p = 0.3)

observed_hires <- c(sum(new_coaches_by_year$White), sum(new_coaches_by_year$Minority))
expected_hires <- sum(observed_hires) * c(0.7, 0.3)  

chisq.test(x = observed_hires, p = c(0.7, 0.3))

# Even assuming a 70% to 30% expected hiring ratio favoring white coaches, the observed rate of 
# minority coach hired in the NFL remains significantly lower than expected.


# Finally we will look at if minority coaches have the ability to be rehired at a rate equal to
# their white counterparts. We will look at all coaches in past 20 years who have at least one  
# tenures with two different teams

rehired_coaches <- coach_tenure_summary %>% filter((Interim== 0) | (Interim==1 & Tenure > 1))

rehired_coaches <- rehired_coaches %>%
  group_by(Coach) %>%
  filter(n() > 1) %>%
  ungroup()

# In total the amount of coaches who were rehired where 28 white coaches to 8 minority coaches
# We will run a 2 portion t-test to see if there is a difference race 
rehired_coaches_summary <- rehired_coaches %>%
  distinct(Coach, Minority) %>% 
  group_by(Minority) %>%
  summarize(Count = n(), .groups = "drop")

# Cont total amount of minority and non-minority coaches
nrow(coach_tenure_summary %>% filter(Minority == 0))
nrow(coach_tenure_summary %>% filter(Minority == 1))

prop.test(x = c(28, 8), n = c(164, 43))
# t-test shows that there is no significant difference in the hiring for white and minority
# coaches. 




