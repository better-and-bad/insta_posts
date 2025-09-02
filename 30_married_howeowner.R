

### load libraries
rm(list=ls())

library(ipumsr)
library(tidyverse)
library(ggplot2)

### set key
set_ipums_api_key(EnterYourAPIKeyHERE, 
                  install = TRUE)

### query ipum micro data
lifestyle <- define_extract_micro(
  collection = "usa",
  description = "1950-2020 30 years old married & own home",
  samples = c(
  "us1950a",   # 1950 1% (a 100% file exists but is huge; 1% is practical)
  "us1960b",   # 1960 5%  (more comprehensive than 1%)
  "us1970b",   # 1970 Form 2 State (long-form; nationally representative)
  "us1980a",   # 1980 5%
  "us1990a",   # 1990 5%
  "us2000a",   # 2000 5% (long-form)
  "us2010e",   # 2006–2010 ACS 5-year (best stand-in for “2010”)
  "us2020c"    # 2016–2020 ACS 5-year (avoid 2020 1-yr experimental)
),
  variables = c("YEAR", "AGE", "PERNUM", "MARST", 
                "OWNERSHP", "RELATE", "PERWT")
)

### submit and download ipums query
request <- submit_extract(lifestyle)
files <- wait_for_extract(request)
data <- download_extract(files, download_dir = "data/ipums", 
                         overwrite = TRUE)
# Read microdata
ddi   <- read_ipums_ddi(data)     
micro <- read_ipums_micro(ddi)  

### PERWT defines the number of ppl matched by x row
made_man <- micro %>%
  mutate(
    age30    = AGE == 30,
    married  = MARST %in% c(1, 2),
    owner_hh = OWNERSHP == 1,          # using broad OWNERSHP as you’ve extracted
    is_ref   = RELATE == 1              # or: RELATED %/% 100 == 1
  ) %>%
  filter(age30)

# Married & in owner-occupied housing (exclude missing tenure)
by_year <- made_man %>%
  group_by(YEAR) %>%
  reframe(
    denom = sum(PERWT[!is.na(owner_hh)], na.rm = TRUE),
    pct_married_in_owner_occ =
      100 * sum(PERWT * (married & owner_hh), na.rm = TRUE) / denom,
    pct_married_householder_owner =
      100 * sum(PERWT * (married & owner_hh & is_ref), na.rm = TRUE) / denom
  ) %>% 
  drop_na()

library(showtext)

### Set the theme
font_add_google(name = "Roboto Slab", family = "roboto-slab")
showtext_auto()

### plot
by_year %>% 
  mutate(date = as.Date(paste0(YEAR, "-01-01"))) %>% 
ggplot(aes(date, pct_married_in_owner_occ)) +
  geom_line(linewidth = 2, color = "#0F3D89") +
  geom_point(size = 3.5, color = "#0F3D89") +
  
  # Correct policy marker: Aug 15, 1971 (Nixon ends dollar–gold convertibility)
  geom_vline(xintercept = as.Date("1971-08-15"),
             linetype = "dashed", linewidth = 0.9, color = "#6B7280") +
  annotate("label",
           x = as.Date("1972-01-01"), y = 35,
           label = "1971: U.S. ends\ndollar–gold\nconvertibility",
           hjust = 0, vjust = 1, family = "roboto-slab", size = 7,
           fill = "white", color = "#111827", label.size = 0) +
  
  scale_y_continuous(
    labels = percent_format(scale = 1),
    limits = c(20, NA),
    breaks = pretty_breaks(n = 6)
  ) +
  scale_x_date(
    date_labels = "%Y",
  ) +
  labs(
    title = "At 30: Married & Homeowner",
    subtitle = "See, Mom, It's Not Me, It's a 60-Year Trend",
    caption = "Source: IPUMS USA"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold", hjust = 0.5,
                              family = "roboto-slab",),
    plot.subtitle = element_text(size = 18, face = "bold",
                                 family = "roboto-slab", hjust = 0.5),
    axis.title = element_blank(),
    axis.text  = element_text(size = 16, color = "#111827",
                              family = "roboto-slab",)
  )

### look at marriage and homeownership separately
made_man %>% 
  group_by(YEAR) %>% 
  ### percent married 
  reframe(
    denom_married = sum(PERWT[!is.na(married)],   na.rm = TRUE),
    denom_owner   = sum(PERWT[!is.na(owner_hh)],  na.rm = TRUE),
    perc_married = sum(PERWT * married, na.rm=T) / denom_married * 100,
    perc_own = sum(PERWT * owner_hh, na.rm=T) / denom_owner * 100
  ) %>% 
  drop_na() 
  
