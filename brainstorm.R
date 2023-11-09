library(tidyverse)


scotus <- read_csv("scotus_approval_polls.csv")
scotus

scotus <- scotus |> 
  select(pollster, end_date, yes, no, alternate_answers) |> 
  rename("date" = end_date) |> 
  mutate(date = lubridate::mdy(date))

scotus

scotus <- scotus |> 
  group_by(date) |> 
  reframe(pollster = pollster,
          yes = sum(yes),
            no = sum(no),
            alt = sum(alternate_answers),
            per_yes = (yes/(yes + no + alternate_answers))*100,
            per_no = (no/(yes + no + alternate_answers))*100)            
write_csv(scotus, "scotus_approval.csv" )

scotus

# let's just view the pollster YouGov using the filter function

scotus_yg <- scotus |> 
  filter(pollster == "YouGov")

scotus_yg            

#set the coordinates

scotus.line <- ggplot(scotus_yg, aes(date, per_yes))
scotus.line

# create the graph

scotus.line + 
  geom_smooth()

#add color

scotus.line + 
  geom_smooth(fill = "skyblue", color = "coral")

# add a theme

scotus.line + 
  geom_smooth(fill = "skyblue", color = "coral") +
  theme_classic()

# add labels

scotus.line + 
  geom_smooth(fill = "skyblue", color = "coral") +
  theme_classic() +
  labs(
    title = "SCOTUS Approval",
    subtitle = "2023",
    caption = "polls from YouGov",
    y = "Approval",
    x = NULL
  )
  
# add scales

'''
 scales refer to the mapping between the values in your data and the visual representation of those values in your plot. Scales play a crucial role in transforming and mapping data onto the graphical elements of a plot, helping to make the visual representation more informative and meaningful.
 
 Understanding and customizing scales is essential for creating clear and accurate visualizations in ggplot2, as they help translate your data into a meaningful and visually interpretable representation.

1. x/y
2. color
3. fill
4. size
5. limits and breaks

'''

scotus.line + 
  geom_smooth(fill = "skyblue", color = "coral") +
  theme_classic() +
  labs(
    title = "SCOTUS Approval",
    subtitle = "2023",
    caption = "polls from YouGov",
    y = "Approval",
    x = NULL
  ) +
  scale_x_date( date_breaks = "6 weeks",
                date_labels = "%b %d")


# export options


ggsave("scotus_approval.png")

# histogram


sw_df <- read_csv("starwars.csv")


# lets look at the distribution of bmi

sw.hist <- ggplot(sw_df, aes(height))


sw.hist +
  geom_histogram()


# binning

sw.hist + 
  geom_histogram(binwidth = 25)

sw.hist + 
  geom_histogram(bins = 7)


# add color, fill, line-size, and transparency

sw.hist + 
  geom_histogram(bins = 7, color = "skyblue", fill = "coral", size = 2, alpha = .5)

# add a reference line (order matters)

sw.hist + 
  geom_vline(xintercept = 150, color = "navy", size = 3) +
  geom_histogram(bins = 7, color = "skyblue", fill = "coral", size = 2, alpha = .5)


# views the a categorical variable distribution using facets

sw.hist + 
  geom_vline(xintercept = 150, color = "navy", size = 3) +
  geom_histogram(bins = 7, color = "skyblue", fill = "coral", size = 2, alpha = .5) +
  facet_wrap(facet = vars(sex), ncol = 3)


# bar graph

sw_df <- sw_df |> 
  drop_na(hair_color)

sw.bar <- ggplot(sw_df, aes(hair_color, height))

sw.bar +
  geom_col()

# reorder the bars 

sw.bar <- ggplot(sw_df, aes(fct_reorder(hair_color, height), bmi))


