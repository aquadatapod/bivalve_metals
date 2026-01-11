################################################################################
# Title: Global Research Trends on Heavy Metal Contamination in Bivalve
# Description: R script used for data processing, analysis, and figure generation
# 
# Authors: <Sagar Gavas>, <Amit J Patil>, <Minakshi Gurav>
# Corresponding Author: <Amit J Patil> (<amit@iue.ac.cn>)
#
# Software:
#   R version 4.3.1 (R Core Team, 2025)
#
# Key Packages:
#   bibliometrix (version <5.1.0>)
#   tidyverse (version <2.0.0>)
#
#               <YYYY-MM-DD>
# Last Updated: <2026-01-10>
################################################################################
# Note:
# Figure 3c and Supplementary Figure 1 were generated using the biblioshiny() function.
# Country-level contribution metrics (MCP and SCP) and the Trend Topics plot were also
# produced via biblioshiny() within the same workflow.
################################################################################
# Core package
#install.packages("bibliometrix")
#install.packages("ggplot2")
#install.packages("ggthemes")     # publication style themes
#install.packages("igraph")       # for network layouts
#install.packages("ggraph")       # for advanced network plotting
################################################################################
# 1. Load Libraries
################################################################################
#install.packages(c("bibliometrix","ggplot2","igraph","ggraph","ggthemes"))
library(bibliometrix)
library(ggplot2)
library(igraph)
library(ggraph)
library(ggthemes)
library(scales)
library(dplyr)
################################################################################
# Import your WOS data
################################################################################
# Adjust filename accordingly
M <- convert2df("Final-refined/refined-305.bib", dbsource = "wos", format = "bibtex")

# Quick overview
results <- biblioAnalysis(M)
summary(results, k = 10, pause = FALSE)

################################################################################
# Descriptive plots
################################################################################
plot(x = results, k = 10, pause = FALSE)


# Count publications per year
annual <- as.data.frame(table(M$PY))
colnames(annual) <- c("Year", "Articles")
annual$Year <- as.numeric(as.character(annual$Year))

# Figure-3a---Number of articles-------------------------------------------------##Figure 3a
ggplot(annual, aes(x = Year, y = Articles)) +
  # soft filled area under curve
  geom_area(fill = "skyblue", alpha = 0.3) +
  # main line
  geom_line(size = 1.3, color = "steelblue4") +
  # points
  geom_point(size = 3, color = "white", fill = "steelblue4", shape = 21, stroke = 1.2) +
  # highlight last year
  geom_point(data = annual[which.max(annual$Year), ], 
             aes(x = Year, y = Articles), 
             size = 5, color = "darkred") +
  geom_text(data = annual[which.max(annual$Year), ], 
            aes(label = paste0(" ", Articles)), 
            vjust = -1, hjust = 0, size = 4, color = "darkred") +
  # style
  theme_classic(base_size = 15)+
  labs(
    x = "Publication Year", 
    y = "Number of Articles") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# Figure-3b---Average citation per year-----------------------------------------##Figure 3b
# Reproduce Biblioshiny metrics by year
current_year <- as.integer(format(Sys.Date(), "%Y"))

by_year <- M %>%
  mutate(PY = as.integer(PY),
         TC = as.numeric(TC)) %>%
  tidyr::replace_na(list(TC = 0)) %>%         # Biblioshiny treats missing TC as 0
  group_by(PY) %>%
  summarise(
    N = n(),
    MeanTCperArt = mean(TC, na.rm = TRUE),    # average total citations per article
    TotalTC = sum(TC, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    CitableYears = pmax(1, current_year - PY + 1),
    MeanTCperYear = MeanTCperArt / CitableYears
  )

# (optional) check it looks like Biblioshiny
# by_year %>% arrange(PY) %>% dplyr::select(PY, MeanTCperArt, N, MeanTCperYear, CitableYears)

# Plot: Average citations per article per year
ggplot(by_year, aes(x = PY, y = MeanTCperYear)) +
  geom_area(fill = "skyblue", alpha = 0.3) +
  geom_line(size = 1.3, color = "steelblue4") +
  geom_point(size = 3, color = "white", fill = "steelblue4", shape = 21, stroke = 1.2) +
  geom_point(data = by_year[which.max(by_year$PY), ],
             aes(x = PY, y = MeanTCperYear),
             size = 5, color = "darkred") +
  geom_text(data = by_year[which.max(by_year$PY), ],
            aes(label = paste0(" ", round(MeanTCperYear, 2))),
            vjust = -1, hjust = 0, size = 4, color = "darkred") +
  theme_classic(base_size = 15) +
  labs(x = "Publication Year",
       y = "Average citations per article per year") +
  scale_x_continuous(breaks = pretty_breaks(n = 10)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5, color = "gray30"),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )

# Figure-3d---Author production over time---------------------------------------##Figure 3d
# Build author-year production data
author_year <- M %>%
  separate_rows(AU, sep = ";") %>%
  mutate(AU = trimws(AU),
         PY = as.integer(PY),
         TC = as.numeric(TC)) %>%
  group_by(AU, PY) %>%
  summarise(
    Articles = n(),
    Citations = sum(TC, na.rm = TRUE),
    .groups = "drop"
  )

# Select top authors (by total number of articles)
top_authors <- author_year %>%
  group_by(AU) %>%
  summarise(TotalArticles = sum(Articles)) %>%
  arrange(desc(TotalArticles)) %>%
  slice_head(n = 20)   # change 15 to however many authors you want

# Filter and attach totals for reordering
author_year_top <- author_year %>%
  filter(AU %in% top_authors$AU) %>%
  left_join(top_authors, by = "AU")

# Plot like Biblioshiny
ggplot(author_year_top, aes(x = PY, y = reorder(AU, TotalArticles))) +
  geom_point(aes(size = Articles, color = Citations), alpha = 0.8) +
  scale_size_continuous(range = c(3, 10), name = "Articles") +
  scale_color_viridis_c(option = "plasma", name = "Citations") +
  theme_classic(base_size = 14) +
  labs(
    title = "Author Production Over Time",
    x = "Year",
    y = "Author"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "right"
  )


# Calculate start and end year per author
author_year_top <- author_year_top %>%
  group_by(AU) %>%
  mutate(
    StartYear = min(PY, na.rm = TRUE),
    EndYear   = max(PY, na.rm = TRUE)
  ) %>%
  ungroup()


# Plot like Biblioshiny with start/end year lines 
ggplot(author_year_top, aes(y = reorder(AU, TotalArticles))) +
  # Add line for publication span
  geom_segment(aes(x = StartYear, xend = EndYear, yend = reorder(AU, TotalArticles)), 
               color = "grey70", size = 1) +
  # Points for each year
  geom_point(aes(x = PY, size = Articles, color = Citations), alpha = 0.8) +
  scale_size_continuous(range = c(3, 10), name = "Articles") +
  scale_color_viridis_c(option = "plasma", name = "Citations") +
  theme_classic(base_size = 14) +
  labs(
    title = "Author Production Over Time",
    x = "Year",
    y = "Author"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11),
    legend.position = "right"
  )


# Define breaks every 5 years
year_breaks <- seq(min(author_year_top$PY, na.rm = TRUE),
                   max(author_year_top$PY, na.rm = TRUE),
                   by = 5)

ggplot(author_year_top, aes(y = reorder(AU, TotalArticles))) +
  # Add line for publication span
  geom_segment(aes(x = StartYear, xend = EndYear, yend = reorder(AU, TotalArticles)), 
               color = "grey70", size = 1) +
  # Points for each year
  geom_point(aes(x = PY, size = Articles, color = Citations), alpha = 0.8) +
  scale_size_continuous(range = c(3, 10), name = "Articles") +
  scale_color_viridis_c(option = "plasma", name = "Citations") +
  scale_x_continuous(breaks = year_breaks) +  # every 5 years
  theme_classic(base_size = 14) +
  labs(
    x = "Year",
    y = "Author"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.title = element_text(size = 13, face = "bold"),
    axis.text = element_text(size = 11, hjust = 1), # angled for readability
    legend.position = "right"
  )


# Figure-4a to c---Conceptual analysis------------------------------------------##Figure 4a to c & Supplementary Figure 2
# Factorial analysis (conceptual structure) on Keywords Plus
conceptualStructure(M,
                    field = "ID",    # "DE" for author keywords
                    method = "MCA",  # Multiple Correspondence Analysis
                    minDegree = 2,   # only keywords appearing ≥2 times
                    clust = 5,       # number of clusters
                    labelsize = 10)  # size of labels

# Figure-4d---Thematic analysis ------------------------------------------------##Figure 4d
# Thematic map using Keywords Plus (ID)
thematic_map <- thematicMap(
  M,
  field = "ID",        # "ID" = Keywords Plus; "DE" = Author Keywords
  n = 250,             # number of terms to include
  minfreq = 2,         # minimum frequency of a term
  stemming = FALSE,    # TRUE to merge similar word forms
  size = 0.05,          # scaling factor for bubble size
  n.labels = 3,        # max number of labels shown per cluster
  repel = TRUE,        # avoid overlapping labels
  cluster = "walktrap" # community detection algorithm
)


plot(thematic_map$map)

