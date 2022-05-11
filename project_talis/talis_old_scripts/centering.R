# Check that the mean of all values is the same as
# the mean of all cluster means

# Mean of all values
mean(df$T3WELS, na.rm = TRUE)

# Calculate the mean T3WELS within each school
df <- df %>%
  group_by(IDSCHOOL) %>%   # operate within schools
  mutate(school_stress = mean(T3WLOAD, na.rm = TRUE)) %>%
  ungroup()
# Check that actually 191 unique values are present
length(unique(df$school_stress))
# Calculate the mean of the school_stress,
## selecting only the first row per school
only_schhols <- df %>%
  group_by(IDSCHOOL) %>%
  filter(row_number()==1)

mean(only_schhols$T3WLOAD, na.rm = TRUE)
