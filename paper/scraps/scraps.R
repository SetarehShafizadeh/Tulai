
#---------------------------------------------------------------
# BM: diagnosing an incorrect tool_type value at around line 1840
# BM: this is what I saw in the qmd file around line 1844:

# Generate a summary table by grouping by 'depth'
summary_table_depth_tool_type <- tl_final %>%
  group_by(depth) %>%
  summarise(tool_type = n_distinct(tooltypegroup, na.rm = TRUE))

# Create a full summary table by joining it back to your original summary table based on 'depth'
summary_table_depth_area_layer <-
  summary_table_depth_area_layer %>%
  left_join(summary_table_depth_tool_type,
            by = "depth")

# Print column names
print(names(summary_table_depth_area_layer))

# BM: I'm wondering about the group_by(depth), seems like it will
# combine depths from multiple areas, is that right? My guess
# is that we want to group_by(area, depth) so we don't combine
# areas. I'll let you consider that question, I'm not 100% sure
# what your goal is here

# BM: here's how I found out why you get 2 intead of the expected
# value of 1 for tool_type. I saw that the suprise value of 2
# had a depth of 20-30, so I focus on those rows, since you
# group by depth:

tl_final %>%
  filter(depth == "20-30")

# BM: we see five rows, let's focus further on the tooltypegroup
# column, since you compute n_distinct on that to get the 2

tl_final %>%
  filter(depth == "20-30") %>%
  mutate(n_dist_tooltype = n_distinct(tooltypegroup, na.rm = TRUE)) %>%
  select(n_dist_tooltype)
















#########DO NOT RUN0
# BM: My preference is to delete this, or move it to a file called 'scraps.R' if you think you might need it later.

# Define the volumes for each area based on the given information
volumes <- data.frame(
  area = c("ta", "tp1", "d1"),
  volume = c(52, 5.76, 2.1375) # in m^3
)

tl_final_area <- tl_final %>%
  filter(area %in% c("ta", "d1", "tp1"))

# Count the number of artifacts by area
artifact_counts <- tl_final_area %>%
  filter(area %in% c("ta", "tp1", "d1")) %>%
  group_by(area) %>%
  tally()

# Calculate retouch frequency with explicit NA check
retouch_freq <- tl_final_area %>%
  filter(area %in% c("ta", "tp1", "d1")) %>%
  group_by(area) %>%
  summarise(retouchFreq = ifelse(all(is.na(retouch)), NA, mean(!is.na(retouch))), .groups = 'drop')

# Compute lithic density for each area and round it to 2 decimal places
lithic_density_df <- left_join(artifact_counts, volumes, by = "area") %>%
  mutate(lithic_density = round(n / volume, 2))

# Create the summary table
summary_table2_area <- tl_final_area %>%
  filter(area %in% c("ta", "tp1", "d1")) %>%
  group_by(area) %>%
  summarise(
    numb_retouched = sum(retouch == 1, na.rm = TRUE),
    total_lithics = n(),
    percent_retouch = round((numb_retouched / total_lithics) * 100, 2)
  )

# Merge all the calculated data
summary_table2_area <- left_join(summary_table2_area, lithic_density_df[, c("area", "lithic_density")], by = "area")
summary_table2_area <- left_join(summary_table2_area, retouch_freq, by = "area")

# Final modifications and print
summary_table2_area <- summary_table2_area %>%
  mutate(
    area = recode(area, 'ta' = 'TA', 'tp1' = 'TP1', 'd1' = 'D1'),
    lithic_volumetric_density = lithic_density
  ) %>%
  select(-lithic_density)  # Remove the unwanted column

print(summary_table2_area)

```







```{r}


##### DO NOT RUN ######

# BM: My preference is to delete this, or move it to a file called 'scraps.R' if you think you might need it later.


# layer-Create the summary table
summary_table2_layer <- tl_final_area %>%
  filter(layer %in% c(c("a", "b", "c" , "d", "e"))) %>%
  group_by(layer) %>%
  summarise(
    numb_retouched = sum(retouch == 1, na.rm = TRUE),
    numb_cores = sum(!is.na(`core-typology`), na.rm = TRUE),
    total_lithics = n(),
    percent_retouch = round((numb_retouched / total_lithics) * 100, 2)
  )

# Create a data frame for volumetric density values
density_data_layer <- data.frame(
  layer = c("a", "b", "c" , "d", "e"),
  `lithic_volumetric_density` = round(c(1.575, 6.12, 2.115, 47, 13), 2)
)


summary_table2_layer <- merge(summary_table2_layer, density_data_layer, by = "layer")



```

# regression plot-area

# Plotting
p_area <-
  ggplot(summary_table2_area,
         aes(x = lithic_volumetric_density,
             y = percent_retouch)) +
  # Blue regression line with shaded area
  geom_smooth(method = "lm",
              se = TRUE,
              aes(group = 1),
              color = "blue") +
  # Points colored by area
  geom_point(color = "black") +
  # Labels points with area names
  geom_text(aes(label = area),
            vjust = -1,
            hjust = -0.5) +
  # Regression equation, R-squared, and p-value
  stat_poly_eq(aes(label = paste(stat(eq.label),
                                 stat(rr.label),
                                 stat(p.value.label),
                                 sep = "~~~")),
               formula = y ~ x,
               parse = TRUE,
               size = 3) +

  # Titles and labels
  labs(title = "Relationship between Retouch Frequency and Lithic Volumetric Density",
       x = "Lithic Volumetric Density",
       y = "Retouch Frequency (%)")

p_area








