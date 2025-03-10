library(marinecs100b)
remotes::install_github("MarineCS-100B/marinecs100b")

# Review: write a function ------------------------------------------------

# P1 Describe succinctly what the following code does. This should be a
# high-level, one-sentence description, not a line-by-line breakdown.

site <- "Nuka_Pass"
season <- "Late winter"
n_cold <- sum(kefj_site == site &
                kefj_season == season &
                kefj_temperature <= -4 &
                kefj_exposure == "air")
n_total <- sum(kefj_site == site &
                 kefj_season == season)
hours_cold <- n_cold * 30 / 60
days_total <- n_total * 30 / 60 / 24
hours_cold_per_day <- hours_cold / days_total
hours_cold_per_day
# The code takes the data from a site and season and determines how many hours
# per day the temperature was less than negative four degrees

# P2 Let's turn that code chunk into a function. What would you call that
# function? How many parameters should it take and what would you call them?
# I would call the function cold_hours_day
#the parameters are the site and season

# P3 Write a function to encapsulate the code chunk above. Check that it
# contains all five parts of a function.
cold_hours_day <- function(site, season) {
  n_cold <- sum(kefj_site == site &
                  kefj_season == season &
                  kefj_temperature <= -4 &
                  kefj_exposure == "air")
  n_total <- sum(kefj_site == site &
                   kefj_season == season)
  hours_cold <- n_cold * 30 / 60
  days_total <- n_total * 30 / 60 / 24
  hours_cold_per_day <- hours_cold / days_total
  return(hours_cold_per_day)
}
cold_hours_day("Nuka_Pass", "Late winter")
# Make an extreme choice --------------------------------------------------

# P4 Fill in the code below to create a logical vector indicating extreme
# temperatures.

extreme_type <- "hot"
if (extreme_type == "cold") {
  is_extreme <- kefj_temperature <= -4
} else if (extreme_type == "hot") {
  is_extreme <- kefj_temperature >= 25
}

is_extreme

# P5 Copy-paste the code from P1 and edit it to incorporate the is_extreme
# vector into the extreme temperature exposure procedure.
site <- "Nuka_Pass"
season <- "Late winter"
n_extreme <- sum(kefj_site == site &
                kefj_season == season &
                is_extreme &
                kefj_exposure == "air" & is_extreme)
n_total <- sum(kefj_site == site &
                 kefj_season == season)
hours_extreme <- n_extreme * 30 / 60
days_total <- n_total * 30 / 60 / 24
hours_extreme_per_day <- hours_extreme / days_total
hours_extreme_per_day

# P6 Copy-paste the function you wrote in P3 and edit it to add a parameter that
# lets you switch between extreme heat and cold exposure.
extreme_hours_day <- function(site, season, hotcold) {
  if (hotcold == "cold") {
    is_extreme <- kefj_temperature <= -4
  } else  {
    is_extreme <- kefj_temperature >= 25

  }
  n_extreme <- sum(kefj_site == site &
                     kefj_season == season &
                     is_extreme &
                     kefj_exposure == "air")
  n_total <- sum(kefj_site == site &
                   kefj_season == season)
  hours_extreme <- n_extreme * 30 / 60
  days_total <- n_total * 30 / 60 / 24
  hours_extreme_per_day <- hours_extreme / days_total
  return(hours_extreme_per_day)
}
extreme_hours_day("Nuka_Pass", "Summer", "cold")
# Season to taste ---------------------------------------------------------

# P7 What seasons are in the kefj dataset? What function would you use to
# identify them?
#The seasons are spring, summer, fall, early winter, and late winter
#I would just run kefj_season in the console, it tells you the levels

# P8 Fill in the blanks below to make a for loop that prints the extreme hot and
# cold exposure across seasons at site Aialik.

seasons <- c("Late winter", "Spring", "Summer", "Fall", "Early winter")
  for (season in seasons) {
    heat_exposure <- extreme_hours_day("Aialik", season, "hot")
    cold_exposure <- extreme_hours_day("Aialik", season, "cold")
    print(paste("Aialik", season, heat_exposure, cold_exposure))
}
#"Aialik Late winter 0 0.0136632200886263"
# "Aialik Spring 0.0169398907103825 0.00273224043715847"
# "Aialik Summer 0.114288330776804 0"
# "Aialik Fall 0.0105072463768116 0"
# "Aialik Early winter 0 0.0109289617486339"

# P9 Copy-paste your answer to P8 and add a nested for loop to iterate across
# sites as well as seasons.
seasons <- unique(kefj_season)
sites <- unique(kefj_site)
for (season in seasons) {
  for (site in sites) {
  heat_exposure <- extreme_hours_day(site, season, "hot")
  cold_exposure <- extreme_hours_day(site, season, "cold")
  print(paste(site, season, heat_exposure, cold_exposure))
  }
}
#"Aialik Summer 0.114288330776804 0"
# "Harris Summer 0.00205189586628479 0"
# "McCarty Summer 0.000549111126364198 0"
# "Nuka_Bay Summer 0.0143900577782623 0"
# "Nuka_Pass Summer 0.0713972741738348 0"
# "Aialik Fall 0.0105072463768116 0"
# "Harris Fall 0 0"
# "McCarty Fall 0.00036231884057971 0"
# "Nuka_Bay Fall 0 0"
# "Nuka_Pass Fall 0.0166666666666667 0"
# "Aialik Early winter 0 0.0109289617486339"
# "Harris Early winter 0 0.00922131147540984"
# "McCarty Early winter 0 0.00655737704918033"
# "Nuka_Bay Early winter 0 0.0126103404791929"
# "Nuka_Pass Early winter 0 0.0202185792349727"
# "Aialik Late winter 0 0.0136632200886263"
# "Harris Late winter 0 0.0221453287197232"
# "McCarty Late winter 0 0.0228796358939664"
# "Nuka_Bay Late winter 0 0.0255759018169547"
# "Nuka_Pass Late winter 0 0.060930576070901"
# "Aialik Spring 0.0169398907103825 0.00273224043715847"
# "Harris Spring 0 0.00512295081967213"
# "McCarty Spring 0 0.00218579234972678"
# "Nuka_Bay Spring 0 0.00441361916771753"
# "Nuka_Pass Spring 0.00819672131147541 0.00109289617486339"

# P10 Examine your results from P9. You should find two outputs where both
# extreme heat and cold exposure were 0. What season were they in?
#They were both in Fall
