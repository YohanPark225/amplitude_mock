Amplitude Mock Data Analysis Practice
================
Yohan Park
2025-02-04

## About the dataset

This is a mock dataset containing the activities of e-book application
users during January. I created this Amplitude-style event data with R
and saved it as a .csv file.

The data includes user behavior logs with events like app_opened,
book_started, search_performed, and more. The data has key properties
like device_type, region, book_genre, and timestamps.

``` r
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
```

``` r
event_logs <- read.csv("C:/Users/yohan/OneDrive/문서/GitHub/amplitude_mock/data/amplitude_mock_data.csv", stringsAsFactors = FALSE)

# Let's first take a look at how our users perform overall activities with the app during the analysis period.

event_logs %>%
  mutate(event_date = as.Date(event_time)) %>%
  count(event_date) %>%
  ggplot(aes(x = event_date, y = n))+
  geom_line(color = "#0073C2FF", linewidth = 1)+ 
  geom_point(color = "#EFC000FF", size = 2)+
  labs(
    title = "Event Counts Over Time",
    subtitle = "Daily event trends in the application",
    x = "Date",
    y = "Event Count",
    caption = "Data Source: event_logs dataset"
  ) +
  theme_minimal(base_size = 14)+  # Use a clean theme with larger text
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray85", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )
```

![](amplitude_data_analysis_files/figure-gfm/load%20the%20dataset-1.png)<!-- -->

## 1. Retention Analysis

Retention analysis measures how long users continue engaging with a
product after their first interaction. It helps businesses understand
user loyalty, identify drop-off points, and evaluate product stickiness.

Since our original dataset, “event_logs”, includes only one month’s
worth of data, I’m going to determine the cohort of all users on a daily
basis and draw a heatmap to get a rough idea of how users stick to our
service as the day goes by.

``` r
retention_data <- event_logs %>%
  group_by(user_id) %>%
  summarise(cohort_day = as.Date(min(event_time)), .groups = "drop")

# Join back to count returning users per day

retention_analysis <- event_logs %>%
  inner_join(retention_data, by = "user_id") %>%
  filter(event_type %in% c("app_opened", "book_search_performed", "book_viewed", "book_started", "book_completed")) %>%
  mutate(active_day = as.Date(event_time)) %>%
  group_by(cohort_day, active_day) %>%
  summarize(active_users = n_distinct(user_id), .groups = "drop") %>%
  arrange(cohort_day, active_day)

# Calculate cohort size
cohort_size <- retention_analysis %>%
  filter(cohort_day == active_day) %>%
  select(cohort_day, cohort_users = active_users)

# Calculate retention rate
retention_rate <- retention_analysis %>%
  left_join(cohort_size, by = "cohort_day") %>%
  mutate(retention = active_users / cohort_users) %>%
  select(cohort_day, active_day, retention)

# Convert to wide format for heatmap
retention_matrix <- retention_rate %>%
  mutate(day_diff = as.numeric(difftime(active_day, cohort_day, units = "days"))) %>%
  pivot_wider(names_from = day_diff, values_from = retention)


# Retention heatmap
ggplot(retention_rate, aes(x = as.factor(active_day), y = as.factor(cohort_day), fill = retention)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Daily User Retention Heatmap", x = "Active Day", y = "Cohort Day", fill = "Retention") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](amplitude_data_analysis_files/figure-gfm/user%20retention%20analysis-1.png)<!-- -->

## 2. Funnel Analysis

Funnel analysis helps track how users move through key steps in a
process, identifying where they drop off. In our case, we’ll analyze how
users progress through a **book discovery and reading journey** in the
app.

In the first part of our funnel analysis, we’ll focus only on each
user’s *first cycle*, the first time a user moves through the funnel, to
get a clean view of their onboarding experience.

``` r
# define the funnel order

funnel_steps <- c("app_opened", "search_performed", "book_viewed", "book_started", "book_completed")

# filter only relevant events

funnel_data <- event_logs %>%
  filter(event_type %in% funnel_steps) %>%
  arrange(user_id, event_time) %>%
  group_by(user_id, event_type) %>%
  summarise(first_occurrence = min(event_time), .groups = "drop")

# count unique users at each step

funnel_counts <- funnel_data %>%
  group_by(event_type) %>%
  summarise(unique_users = n()) %>% # In the funnel_data, every user has only one step of each funnel step - their first occurrences of each event type
  mutate(step_order = match(event_type, funnel_steps)) %>%
  arrange(step_order)

# calculate conversion rate

funnel_counts <- funnel_counts %>%
  mutate(conversion_rate = unique_users / first(unique_users) * 100)


ggplot(funnel_counts, aes(x=reorder(event_type, -step_order), y=unique_users, fill = event_type))+
  geom_bar(stat = "identity", width = 0.6, color = "black")+
  geom_text(aes(label = paste0(round(conversion_rate, 1), "%")), vjust=-0.5, size = 5)+
  labs(title = "User Funnel Analysis", x = "Funnel Stage", y = "Number of Unique Users") +
  scale_fill_brewer(palette = "Blues")+
  theme_minimal()+
  coord_flip()
```

![](amplitude_data_analysis_files/figure-gfm/funnel%20analysis%20(1)%20focus%20on%20first%20cycle-1.png)<!-- -->

This time, we will aggregate the first behaviors in a cycle per session
for a user. This session-based funnel analysis helps capture repeated
engagement to provide a more realistic view of user behavior.

The session-based funnel provides a more comprehensive view of how users
interact with the app throughout the month, as it counts almost all
navigation behaviors within individual sessions.

*key trade-offs*

- **First-occurrence funnel:** Best for understanding where users drop
  off in their very first attempt to go through the content journey.

- **Session-based funnel:** Best for observing general navigation trends
  throughout a given period while still reducing overcounting from
  excessive repetitions.

``` r
funnel_data_2 <- event_logs %>%
  filter(event_type %in% funnel_steps) %>%
  arrange(user_id, session_id, event_time)

# assign step order based on funnel sequence

funnel_data_2 <- funnel_data_2 %>%
  mutate(step_order = match(event_type, funnel_steps))

# get the first occurrence (again) of each funnel step per session

session_funnel <- funnel_data_2 %>% 
  group_by(user_id, session_id, event_type) %>%
  summarise(first_time = min(event_time), .groups = "drop") %>%
  ungroup()

# Count number of sessions that reach each funnel step

session_counts <- session_funnel %>%
  group_by(event_type) %>%
  summarise(session_count = n(), .groups = "drop") %>% # count sessions, not users
  mutate(conversion_rate = session_count / max(session_count) * 100) %>%
  ungroup() %>%
  mutate(event_type = factor(event_type, levels = funnel_steps)) %>% # ensure order
  arrange(event_type) # force the order since apparently ensuring wasn't enough

ggplot(session_counts, aes(x = factor(event_type, levels = funnel_steps), y = conversion_rate))+
  geom_bar(stat = "identity", fill = "steelblue")+
  geom_text(aes(label = paste0(round(conversion_rate, 1), "%")), vjust = -0.5)+
  labs(title = "Session-Based Funnel Analysis",
       x = "Funnel Step",
       y = "Conversion Rate (%)")+
  theme_minimal()
```

![](amplitude_data_analysis_files/figure-gfm/funnel%20analysis%20(2)%20session-based-1.png)<!-- -->

## 3. Cohort Analysis

*Cohort Setup*

- Week 1 Cohort: Users whose first book_started event happened between
  Jan 1 - Jan 7, 2025.
- Week 2 Cohort: Users whose first book_started event happened between
  Jan 8 - Jan 14, 2025.
- We will track retention by checking whether users continue reading
  books (book_started or book_completed) in the following weeks.

By splitting cohorts into Week 1 vs. Week 2, we can compare how users
behave based on different marketing approaches applied in those weeks.
(Let’s assume there are). This allows us to evaluate the effectiveness
of each approach in driving continued reading engagement.

In this cohort analysis, we’re going to focus on “book_started” and
“book_completed” activities because they represent clear user engagement
milestones in the reading journey.

*Analysis goals*

1.  Compare Week 1 vs. Week 2 cohorts to see which group continues
    reading longer.
2.  Identify if Week 2 marketing efforts led to better user engagement
    than Week 1.
3.  Use visualization (cohort retention curve) to show how users from
    each cohort behave over time.

``` r
# convert event time to date

cohort_data <- event_logs %>%
  filter(event_type == "book_started") %>%
  group_by(user_id) %>%
  summarise(first_book_started = min(event_time)) %>% # list of users and the time when they started their first book ever.
  ungroup() %>%
  mutate(cohort_week = case_when(
    first_book_started >= as.Date("2025-01-01") & first_book_started < as.Date("2025-01-08") ~ "Week 1",
    first_book_started >= as.Date("2025-01-08") & first_book_started < as.Date("2025-01-15") ~ "Week 2",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(cohort_week))

# merge cohort info back into event logs (that's beautiful)

event_with_cohort <- event_logs %>%
  inner_join(cohort_data, by = "user_id") %>%
  mutate(week_num = floor(as.numeric(difftime(event_time, first_book_started, units = "weeks")))) %>%
  filter(week_num >= 0) # exclude negative values for "week_num"

# calculate retention rates

cohort_retention <- event_with_cohort %>%
  filter(event_type %in% c("book_started", "book_completed")) %>%
  group_by(cohort_week, week_num) %>%
  summarise(retained_users = n_distinct(user_id), .groups = "drop") %>%
  left_join(cohort_data %>% count(cohort_week, name = "cohort_size"), by = "cohort_week") %>%
  mutate(retention_rate = retained_users / cohort_size)

# visualization

ggplot(cohort_retention, aes(x=week_num, y=retention_rate, color=cohort_week))+
  geom_line(linewidth = 1.2)+
  geom_point(size = 2)+
  scale_y_continuous(labels = scales::percent)+
  labs(title = "Cohort Retention Analysis (Week 1 vs. Week 2)",
       x = "Weeks Since First Book Started",
       y = "Retention Rate",
       color = "Cohort")+
  theme_minimal()
```

![](amplitude_data_analysis_files/figure-gfm/cohort%20analysis-1.png)<!-- -->

## 4. Segmentation analysis

For segmentation analysis, our goal is to identify patterns and
differences in user behavior across different groups. Considering the
data we have, here are three segmentation questions that I think will
give us meaningful insights:

1.  Do subscribers engage more with books than non-subscribers?
2.  Do mobile users engage differently from web users?
3.  When do users start their book-reading journey? How is it different
    by region?

``` r
# To determine if a user is a subscriber or not, we'll assign users the subscription_plan from their latest recorded event. In other words, we'll identify each user's most recent event, extract the subscription_plan from that event, and use this latest subscription_plan for grouping in the analysis. And this will include not only determining most recent subscription status for each user but also merging the latest subscription status back into our original dataset.

# determine the most recent subscription status for each user

latest_subscription <- event_logs %>%
  filter(!is.na(subscription_plan)) %>%
  group_by(user_id) %>%
  filter(event_time == max(event_time)) %>%
  ungroup() %>%
  select(user_id, subscription_plan) %>%
  rename(subscription_plan_latest = subscription_plan)

# merge the latest subscription status back into the original dataset

event_logs <- event_logs %>%
  left_join(latest_subscription, by = "user_id") %>%
  mutate(subscription_plan_latest = coalesce(subscription_plan_latest, "Non-subscriber"))


# analyze book engagement by subscription status

book_engagement <- event_logs %>%
  filter(event_type %in% c("book_viewed", "book_started", "book_completed")) %>%
  group_by(user_id, subscription_plan_latest) %>%
  summarise(book_interactions = n(), .groups = "drop") %>%
  group_by(subscription_plan_latest) %>% # that's the skill you need!
  summarise(avg_book_interactions = mean(book_interactions), .groups = "drop")

print(book_engagement)
```

    ## # A tibble: 3 × 2
    ##   subscription_plan_latest avg_book_interactions
    ##   <chr>                                    <dbl>
    ## 1 Non-subscriber                            4.32
    ## 2 basic                                     4.74
    ## 3 premium                                   4.70

``` r
# analyze book engagement by device type

engagement_by_device <- event_logs %>%
  filter(event_type %in% c("book_viewed", "book_started", "book_completed")) %>%
  group_by(device_type) %>%
  summarise(avg_book_interactions = n() / n_distinct(user_id), .groups = "drop")

print(engagement_by_device)
```

    ## # A tibble: 3 × 2
    ##   device_type avg_book_interactions
    ##   <chr>                       <dbl>
    ## 1 Android                      4.60
    ## 2 Web                          4.37
    ## 3 iOS                          4.53

``` r
# calculate book start rates by region

book_start_by_region <- event_logs %>%
  filter(event_type == "book_started") %>% # That's different. But, we should be able to explain why we chose that activity.
  group_by(region, user_id) %>%
  summarise(first_book_started = min(event_time), .groups = "drop") %>%
  mutate(week_num = if_else(
  ceiling(as.numeric(difftime(first_book_started, as.Date("2025-01-01"), units = "weeks"))) == 0, 
  1, 
  ceiling(as.numeric(difftime(first_book_started, as.Date("2025-01-01"), units = "weeks")))
)) %>%
  filter(week_num >= 0) %>%
  group_by(region, week_num) %>%
  summarise(book_start_users = n_distinct(user_id), .groups = "drop") %>%
  left_join(event_logs %>% summarise(total_users = n_distinct(user_id), .by = "region"), by = "region") %>% # that's brilliant...(remember you always have two options: count() and summarise())
  mutate(book_start_rate = book_start_users / total_users)
  

print(book_start_by_region)
```

    ## # A tibble: 20 × 5
    ##    region        week_num book_start_users total_users book_start_rate
    ##    <chr>            <dbl>            <int>       <int>           <dbl>
    ##  1 Asia                 1               89         250          0.356 
    ##  2 Asia                 2               59         250          0.236 
    ##  3 Asia                 3               39         250          0.156 
    ##  4 Asia                 4               19         250          0.076 
    ##  5 Asia                 5                2         250          0.008 
    ##  6 Europe               1               90         248          0.363 
    ##  7 Europe               2               56         248          0.226 
    ##  8 Europe               3               41         248          0.165 
    ##  9 Europe               4               15         248          0.0605
    ## 10 Europe               5                3         248          0.0121
    ## 11 North America        1               91         254          0.358 
    ## 12 North America        2               54         254          0.213 
    ## 13 North America        3               35         254          0.138 
    ## 14 North America        4               30         254          0.118 
    ## 15 North America        5                4         254          0.0157
    ## 16 South America        1               86         248          0.347 
    ## 17 South America        2               64         248          0.258 
    ## 18 South America        3               30         248          0.121 
    ## 19 South America        4               27         248          0.109 
    ## 20 South America        5                4         248          0.0161

``` r
ggplot(book_start_by_region, aes(x = week_num, y = book_start_rate, group = region, color = region))+
  geom_line()+
  geom_point()+
  facet_wrap(~region, scales = "free_y")+
  labs(title = "Book Start Rate by Region",
       x = "Week Number in Jan 2025",
       y = "Book Start Rate",
       color = "Region")+
  theme_minimal()
```

![](amplitude_data_analysis_files/figure-gfm/segmentation%20analysis%2003-1.png)<!-- -->
