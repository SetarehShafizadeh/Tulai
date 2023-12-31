
# BM: new method to me! I found https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8858582/

library(PCAtest)

result <-
  PCAtest(pca_data,
          100,
          100,
          0.05,
          varcorr=FALSE,
          counter=FALSE,
          plot=TRUE)

# PerMANOVA - partitioning the euclidean distance matrix by species
# from https://stackoverflow.com/a/20267537/1036500
library(vegan)

pca_data_for_post_hoc_testing <-
pca_data %>%
  add_column(excavation_area)

# Is there are significant difference in the groups that
# we see in the PCA?
PerMANOVA_results <-
adonis2(pca_data[, 1:5] ~ excavation_area,
        data = pca_data_for_post_hoc_testing,
        method='eu')

# Which specific pairs of groups differ from each other?
# https://github.com/pmartinezarbizu/pairwiseAdonis
library(pairwiseAdonis)
pairwise.adonis2_results <-
pairwise.adonis2(pca_data[, 1:5] ~ excavation_area,
                data = pca_data_for_post_hoc_testing)

#---------------------------------------------------------------
# BM: PCA plot with convex hulls (I thought we did this already
# in our meeting because I can see the plot in the google doc
# but seems we didn't save-commit-push it?)

library(ggpubr)
library(ggbiplot)
pca_data.pca <- prcomp(pca_data, scale. = TRUE)

ggbiplot(
  pca_data.pca,
  obs.scale = 1,
  var.scale = 1,
  groups = excavation_area,
  ellipse = FALSE,
  circle = TRUE
) +
  stat_chull(aes(color = excavation_area,
                 fill = excavation_area),
             alpha = 0.1,
             geom = "polygon") +
  scale_colour_brewer(palette = "Set1",
                      name = '',
                      guide = 'none') +
  scale_fill_brewer(palette = "Set1",
                    name = '')  +
  theme_minimal()


# BM: some discrepancy found in artefact counts for

 rev(sort(table(tl_final_area$area)))

 rev(sort(table(tl_final_depth_area_unit$area)))



#---------------------------------------------------------------
# BM: we need a single sheet that has only the excavation data
# such as excavation area, chronological level, depth, area of excavation
# only that. The we join it to the lithic data. We only want the lithic
# data to exist in one sheet. Not multiple sheets, that will lead to errors.
# This is called 'normalisation' of the data.

# get data from google sheet
library(googlesheets4)

google_sheet_url <-
  "https://docs.google.com/spreadsheets/d/1Q0QZESk412ZQLE24yPs6Rg-7Y9OMX63DHoXeIPNAIFM/edit#gid=0"

sum_depth_sheet <-
  read_sheet(google_sheet_url) %>%
  mutate(level = unlist(level)) %>%
  mutate(level = ifelse(level == "clay", 1, level)) %>%
  mutate(label1 = paste(area,
                       layer,
                       level,
                       sep = "_")) %>%
  mutate(label2 = paste(area,
                        layer,
                        level,
                        upper,
                        lower,
                        sep = "_")) %>%
  select(label1,
         label2,
         upper_depth = upper,
         lower_depth = lower,
         excavation_area = area,
         chronological_unit = layer,
         grouped_level = level,
         `excavation area`) %>%
  separate_wider_delim(`excavation area`,
           names = c("excavation_dimension_1",
                    "excavation_dimension_2"),
           delim = "*")

write_csv(sum_depth_sheet,
          "data/excavation_data.csv")

sum_depth_sheet$label

summary_table_depth_area_unit$label

#---------------------------------------------------------------

# import table with depths that we have subdivided
depths_divided_tbl <-
  read_csv(here("data/Lookup Table - Sheet1.csv")) %>%
  mutate(division_id = rep(c("a", "b"),
                           nrow(.)/2))

# google sheet has 42 rows, and cols for
# area, chronological_unit, level, depth. What is the equivalent above? tp1_c_3_a

# This 'depths_divided_tbl' needs more information, for example, some depth ranges

# join with lithic data that
summary_table_depth_area_unit_to_join %>%
  left_join(depths_divided_tbl) %>%
  mutate(label = paste(area,
                       chronological_unit,
                       # level, # need this
                       division_id,
                       sep = "_")) %>% View



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








