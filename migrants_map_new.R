rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# 4. Map state codes to shapefile names
code_mapping <- c(
  AN = "Andaman & Nicobar",
  AP = "Andhra Pradesh",
  AR = "Arunachal Pradesh",
  AS = "Assam",
  BH = "Bihar",
  CH = "Chandigarh",
  CG = "Chhattisgarh",
  DN = "Dadra & Nagar Haveli and Daman & Diu",
  DL = "Delhi",
  GA = "Goa",
  GJ = "Gujarat",
  HR = "Haryana",
  HP = "Himachal Pradesh",
  JK = "Jammu & Kashmir",
  JH = "Jharkhand",
  KN = "Karnataka",
  KL = "Kerala",
  LD = "Lakshadweep",
  MP = "Madhya Pradesh",
  MH = "Maharashtra",
  MN = "Manipur",
  ML = "Meghalaya",
  MZ = "Mizoram",
  NL = "Nagaland",
  OD = "Orissa",
  PB = "Punjab",
  RJ = "Rajasthan",
  SK = "Sikkim",
  TN = "Tamil Nadu",
  TR = "Tripura",
  UP = "Uttar Pradesh",
  UK = "Uttarakhand",
  WB = "West Bengal"
)


# 1. Read the cleaned migration matrix (must still contain a row whose origin is exactly "Outside India")
df <- read_csv("Table3-Internal-Migration-Wide-Cleaned-2.csv")

df <- df[-c(37:40)]

orig_col  <- names(df)[1]
dest_cols <- setdiff(names(df), orig_col)

# 2. Extract the Outside-India row *before* we drop it for inter-state sums
outside_df <- df %>%
  filter(.data[[orig_col]] == "Outside India") %>%
  select(all_of(dest_cols)) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "destination",
    values_to = "outside_migrants"
  )

# 3. Now drop that row and pivot the rest for in/out
long_df <- df %>%
  filter(.data[[orig_col]] != "Outside India") %>%
  pivot_longer(
    cols      = all_of(dest_cols),
    names_to  = "destination",
    values_to = "count"
  )

# 4a. In-migrants: origin ??? destination
in_df <- long_df %>%
  filter(.data[[orig_col]] != destination) %>%
  group_by(destination) %>%
  summarise(in_migrants = sum(count, na.rm=TRUE), .groups="drop")

# 4b. Out-migrants: same filter, but grouped by origin
out_df <- long_df %>%
  filter(.data[[orig_col]] != destination) %>%
  group_by(origin = .data[[orig_col]]) %>%
  summarise(out_migrants = sum(count, na.rm=TRUE), .groups="drop") %>%
  rename(destination = origin)

# 5. Merge and compute net (including outside arrivals)
mig_df <- in_df %>%
  full_join(out_df,     by="destination") %>%
  full_join(outside_df, by="destination") %>%
  replace_na(list(
    in_migrants      = 0,
    out_migrants     = 0,
    outside_migrants = 0
  )) %>%
  mutate(net_all = in_migrants + outside_migrants - out_migrants)

# 6. Order by net_all and get long form for plotting
plot_df <- mig_df %>%
  arrange(desc(net_all)) %>%
  mutate(destination = factor(destination, levels=destination)) %>%
  pivot_longer(
    cols     = c(in_migrants, outside_migrants, out_migrants),
    names_to = "category",
    values_to= "count"
  ) %>% 
  filter(destination != "Unclassifiable")



# 7. Plot
ggplot(plot_df, aes(x=destination, y=count, fill=category)) +
  geom_col(position="dodge") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(
    values = c(
      in_migrants      = "goldenrod",
      outside_migrants = "steelblue",
      out_migrants     = "tomato"
    ),
    labels = c(
      in_migrants      = "In-migrants",
      outside_migrants = "Outside India",
      out_migrants     = "Out-migrants"
    )
  ) +
  # coord_flip() +
  theme_minimal() +
  labs(
    title    = "Inter-State Migration by State (Census 2011)",
    subtitle = "In-migrants, Outside India & Out-migrants",
    x        = NULL,
    y        = "Number of Migrants",
    fill     = "Category"
  ) +
  theme(
    axis.text.y    = element_text(size=8),
    legend.position= "top",
    axis.text = element_text(angle = 90)
  )

ggsave("interstate_migration_flipped.png", last_plot(), width = 7, height = 5)

# 8. Create combined category: external migrants = outside_migrants + out_migrants

combined_df <- mig_df %>%
  mutate(in_migrants = outside_migrants + in_migrants) %>%
  select(destination, in_migrants, out_migrants, net_all) %>%
  pivot_longer(
    cols      = c(in_migrants, out_migrants),
    names_to  = "category",
    values_to = "count"
  ) %>%
  # arrange(desc(count)) %>%
  arrange(desc(net_all)) %>%
  mutate(destination = factor(destination, levels = unique(destination)))%>% 
  filter(destination != "Unclassifiable")

# 9. Plot In-migrants vs External migrants
in_vs_external_plot <- ggplot(combined_df, aes(x = destination, y = count, fill = category)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(
    values = c(
      in_migrants       = "goldenrod",
      out_migrants = "steelblue"
    ),
    labels = c(
      in_migrants       = "In-migrants",
      out_migrants = "Out-migrants"
    )
  ) +
  # coord_flip() +
  theme_minimal() +
  labs(
    title    = "Inter-State Migration by State (Census 2011)",
    subtitle = "In- vs Out-migrants (excluding intra-state flows)",
    x        = NULL,
    y        = "Number of Migrants",
    fill     = "Category"
  ) +
  theme(
    axis.text.y     = element_text(size = 8),
    legend.position = "top",
    axis.text = element_text(angle = 90)
  )

# 10. Display and save
print(in_vs_external_plot)
ggsave("in_vs_external_migration.png", plot = in_vs_external_plot, width = 7, height = 5)


# 11. Compute pure net inter-state migration (in_migrants - out_migrants)
net_df <- mig_df %>%
  mutate(net_interstate = in_migrants + outside_migrants - out_migrants) %>%
  arrange(desc(-net_interstate)) %>%
  mutate(destination = factor(destination, levels = destination))

# 12. Plot net inter-state migration as a horizontal bar chart
(net_plot <- ggplot(net_df, aes(x = destination, y = net_interstate)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(
    title    = "Net Inter-State Migration by State (Census 2011)",
    subtitle = "In-migrants minus Out-migrants (excludes intra-state, includes outside India)",
    x        = NULL,
    y        = "Net Migrants",
    caption  = "Source: Census (2011), Debroy & Mishra (2024)"
  ))

print(net_plot)
ggsave("net_interstate_migration_bystate.png", plot = net_plot, width = 7, height = 5)

# 2. Identify the origin column and destination columns
orig_col  <- names(df)[1]    # should be the origin code column (e.g., "Unnamed: 0")
dest_cols <- setdiff(names(df), orig_col)

# 3. Extract out-migrants from West Bengal (origin == "WB")
wb_out <- df %>%
  filter(.data[[orig_col]] == "WB") %>%
  select(all_of(dest_cols)) %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "destination",
    values_to = "count"
  ) %>%
  filter(destination != "WB", destination != "Outside India") %>%  # exclude intra???state & outside India
  arrange(desc(count)) %>%
  slice(1:10) %>%
  mutate(destination = factor(destination, levels = rev(destination)))  # reverse for plotting


# wb_out <- wb_out %>%
  # mutate(state_name = code_mapping[destination])

# 4. Plot a horizontal lollipop chart with English labels
ggplot(wb_out, aes(x = destination, y = count)) +
  geom_segment(aes(xend = destination, y = 0, yend = count), color = "grey50") +
  geom_point(size = 4, color = "darkcyan") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  labs(
    title = "Top 10 Inter-State Destinations from West Bengal (Census 2011)",
    x     = "Destination State/UT",
    y     = "Number of Migrants"
  )

ggsave("wb_external_migration.png", plot = last_plot(), width = 7, height = 5)
