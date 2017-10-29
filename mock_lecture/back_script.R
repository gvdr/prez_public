library(tidyverse)
library(magrittr)
library(pillar)
library(skimr)

data_raw <- read_csv("Data/movie_metadata.csv", guess_max = 10000)

movies <- data_raw %>%
  select(-num_critic_for_reviews, -actor_3_facebook_likes, -actor_3_name, -num_voted_users,
         -movie_imdb_link, -movie_facebook_likes, -num_user_for_reviews, -imdb_score) %>%
  rename(actor1_fl = actor_1_facebook_likes,
         actor2_fl = actor_2_facebook_likes,
         cast_fl = cast_total_facebook_likes,
         director_fl = director_facebook_likes,
         poster_faces = facenumber_in_poster)

movies$aspect_ratio[movies$aspect_ratio == 16] <- 1.77

movies %<>%
  select(
    movie_title,
    title_year,
    gross,
    budget,
    duration,
    country,
    language,
    director_name,
    director_fl,
    actor_1_name,
    actor1_fl,
    actor_2_name,
    actor2_fl,
    cast_fl,
    genres,
    plot_keywords,
    color,
    aspect_ratio,
    poster_faces,
    content_rating
  )

write_rds(movies, path = "./mock_lecture/movies.Rds")
write_rds(HitOrFlop_raw, path = "./mock_lecture/HitOrFlop.Rds")
attach("./mock_lecture/movies.Rda")

movies_sm <- movies %>%
  skim()

glimpse(data_raw)
skim(data_raw) %>% knitr::kable()

data_raw %>%
  filter(title_year > 2000) %>%
  summarise(mid_budget = median(budget, na.rm = TRUE))

budget_gross_range <- data_raw %>%
  mutate(gross = as.double(gross)) %>%
  group_by(director_name) %>%
  summarise(sum_budget = sum(budget),
            max_budget = max(budget, na.rm = TRUE),
            min_budget = min(budget, na.rm = TRUE),
            median_budget = median(budget, na.rm = TRUE),
            sum_gross = sum(gross),
            max_gross = max(gross, na.rm = TRUE),
            min_gross = min(gross, na.rm = TRUE),
            median_gross = median(gross, na.rm = TRUE)) %>%
  top_n(100,sum_gross) %>%
  arrange(min_budget)

budget_gross_range %>%
  arrange(min_budget)

budget_gross_range %>%
  arrange(min_gross) %>%
  select(director_name, min_gross)

data_raw %>%
  filter(director_name == "Quentin Tarantino") %>%
  select(director_name, title_year, movie_title, budget)

data_raw %>%
  filter(director_name == "Tony Scott") %>%
  select(director_name, title_year, movie_title, gross)

HitOrFlop_r <- data_raw %>%
  mutate(net = gross - budget,
         gross_ratio = gross / budget,
         net_ratio = (gross - budget) / budget) %>%
  arrange(net_ratio)

HitOrFlop_bins <- movies %>%
  filter(!is.na(gross), !is.na(budget)) %>%
  mutate(gross_logratio = log(gross / budget),
         bin = ntile(gross_logratio, 4))

qplot(gross_logratio, geom = "density", data = HitOrFlop_bins)
 
HitOrFlop_bins %>%
  filter(bin == 1) %$% max(gross_logratio)

HitOrFlop_bins %>%
  filter(bin == 4) %$% min(gross_logratio)

quantile(HitOrFlop_bins$gross_logratio, 0.75)

ggplot(aes(x = log(net_ratio))) +
  geom_density()

data_sample <- data_raw %>% sample_n(10)

rm(data_raw)
