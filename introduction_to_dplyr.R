### The "Big Five" commands in dplyr
# filter: select rows of a dataset
# select: select columns of a dataset
# mutate: add a column to a dataset
# arrange: sort a dataset by values of a variable
# group_by + summarize: summarize a variable in each group

library(ISLR2)
library(dplyr)
library(stringr)

### Let's look at what we could do with the Hitters data
# View(Hitters)
# ncol(Hitters)

# Let's start by selecting only the variables of interest in this script
Hitters2 <- Hitters %>% select(Salary, AtBat, Hits, HmRun, League, Division, NewLeague)

# How do we read this syntax? The %>% is called a "pipe". 
# Whatever is on the left side of the pipe is implicitly read as the first argument
# to the function on the right side of the pipe.
# So really this is select(Hitters, Salary, AtBat, Hits, HmRun, League, NewLeague)
# When we write just one function, this difference isn't important.
# However, the pipe allows us to "chain" commands in a very human-readable way.
# We'll see examples of this at the end of this script.

# Let's filter to just the players with at least 200 At-Bats
Hitters3 <- Hitters2 %>% filter(AtBat >= 200)

# Notice that I'm creating a new dataset object here rather than overwriting the old one
# The upside to this approach is that if you want to fix a bit of code, you dont have to start over from the very beginning
# The downside is that  you have to remember what you named every intermediate step

# If we View the Hitters dataset, we find that the player names are the row names and
# not an actual variable in the dataset.
names(Hitters3)
head(rownames(Hitters3))

# This is annoying and we can use the mutate function to fix it
# Notice that we also have the little - in front of everyone's name
# We can use functions from the stringr package to get rid of it
# Here I choose to overwrite the Hitters3 object - it's a minor change and not worth it
Hitters3 <- Hitters3 %>% mutate(
  Name = str_remove(rownames(Hitters3), "-")
)
# See also: has_rownames, remove_rownames, and rownames_to_column in the tibble package


names(Hitters3)
head(Hitters3$Name)

# My convention with mutate is to start each new variable created on its own row
# It makes the code more readable and it's easy to find what I've named new variables

Hitters4 <- Hitters3 %>% mutate(
  BattingAvg = Hits/AtBat,
  DollarsPerHR = (Salary*1000)/HmRun
)

# Let's find the Top 10 home run hitters
Hitters4 %>% arrange(desc(HmRun)) %>% 
  select(Name, HmRun) %>% head(10)

# Note 1: arrange(desc(x)) sorts the dataset in descending order by x
# Note 2: Here we have started "chaining" pipes together. We read this as:
  ## First: find the Hitters4 dataset
  ## Then: arrange the dataset in descending order by HmRun
  ## Then: select just the Name and HmRun variables
  ## Then: grab the top 10 rows of the dataset
# Note 3: ties are broken in default row order, but we can add additional things to sort by

Hitters4 %>% arrange(desc(HmRun), AtBat) %>% 
  select(Name, AtBat, HmRun) %>% head(10)
# Now the tie at 31 home runs is broken by who had the fewest at-bats

# Finally, group_by() and summarize() go very nicely together to get summaries by group
Hitters4 %>% group_by(League) %>% summarize(
  n_players = n(), # code inside summarize to count the number of observations in a group
  MeanHR = mean(HmRun),
  SDHR = sd(HmRun)
)

# Again, note that we chain the pipes together.
  ## First, find the Hitters4 dataset
  ## Then, create separate groups - one for each value of League
  ## Then, within each group, find the values of the desired summaries

# We can group by multiple variables
Hitters4 %>% group_by(League, Division) %>% summarize(
  n_players = n(), # code inside summarize to count the number of observations in a group
  MeanHR = mean(HmRun),
  SDHR = sd(HmRun)
)

# There are two other major function groups in dplyr to know
# 1. Conditional statements - if_else and case_when
# 2. Functions that join two datasets - *_join

# Conditional statments are usually used inside mutate statements
# However, you can use them as replacements for the base R ifelse() and switch()
# Generally, I always use if_else() instead of ifelse()

# If you look at the guts of if_else, the idea is that it creates three vectors: out, true, and false
# out is the vector to output
# true is the vector computed assuming the condition is always true and false is the vector computed assuming it's always false
# then it just replaces the values of out with the appropriate values of true or false
# case_when does something similar, but more complicated, so the syntax is a bit different

Hitters4 %>% mutate(
  Million = if_else(Salary >= 1000, "Yes", "No", missing = "Unknown")
) %>% group_by(Million) %>% 
  summarize(n_players = n(), MedianSalary = median(Salary))

# The missing argument is nice because it allows you to replace NA values with an actual value
# But in practice I rarely use it unless I want to explicitly code the missingness

# Let's use a case_when statement to compute the z-scores for home runs relative to each division
Hitters4 %>% mutate(
  HR_zscores = case_when(
    League == "A" & Division == "E" ~ (HmRun - 13.6)/9.42,
    League == "A" & Division == "W" ~ (HmRun - 12.8)/9.54,
    League == "N" & Division == "E" ~ (HmRun - 9.63)/6.87,
    League == "N" & Division == "W" ~ (HmRun - 9.89)/8.17,
    TRUE ~ NA_real_ # have to specify it's a real NA because the rest are numbers 
  )
) %>% select(Name, League, Division, HmRun, HR_zscores) %>%
  arrange(desc(HR_zscores)) %>% head(10)

# For case_when, you use a series of formula arguments
# IF the statement on the left is true, THEN use the value on the right
# Always end with TRUE ~ something (just in case you end up with weird data issues)

# Strictly speaking, I'd use a grouped mutate here, but this is advanced code
Hitters4 %>% group_by(League, Division) %>%
  mutate(HR_zscores = (HmRun - mean(HmRun))/sd(HmRun)) %>%
  ungroup() %>%
  select(Name, League, Division, HmRun, HR_zscores) %>%
  arrange(desc(HR_zscores)) %>% head(10)


# Finally, joining functions
# The idea is to take two datasets and smash them together
# The four most often used joining functions in dplyr are called "mutating joins"
# Because they add columns to an existing dataset
?inner_join 

# For example, let's load the Hitters_Advanced file from Canvas
Hitters_Advanced <- readr::read_csv("Hitters_Advanced.csv")
# This file contains additional batting statistics from baseball-reference.com for the 1986 season

Hitters_Left <- left_join(Hitters4, Hitters_Advanced, by = c("Name" = "PlayerName"))
dim(Hitters4)
dim(Hitters_Advanced)
dim(Hitters_Left)
# This includes only the hitters in the Hitters4 dataset
# If a hitter is not in the Hitters_Advanced dataset, the variables in Hitters_Advanced will have NA values

Hitters_Right <- right_join(Hitters4, Hitters_Advanced, by = c("Name" = "PlayerName"))
dim(Hitters4)
dim(Hitters_Advanced)
dim(Hitters_Right)
# This includes only the hitters in the Hitters_Advanced dataset

Hitters_All <- full_join(Hitters4, Hitters_Advanced, by = c("Name" = "PlayerName"))
dim(Hitters4)
dim(Hitters_Advanced)
dim(Hitters_All)
# This includes all 778 hitters in EITHER dataset

Hitters_Both <- inner_join(Hitters4, Hitters_Advanced, by = c("Name" = "PlayerName"))
dim(Hitters4)
dim(Hitters_Advanced)
dim(Hitters_Both)
# This contains the 278 hitters in BOTH datasets

# Note that when joining by something like names, it is very easy to miss some matches
# because the same person will be referred to different ways in different datasets
# e.g. Andy VanSlyke vs. Andy Van Slyke, Cal Ripken vs. Cal Ripken Jr.
# You can investigate packages that minimize these problems on your own time
