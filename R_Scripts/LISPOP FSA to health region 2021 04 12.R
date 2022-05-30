rm(list=ls())
setwd ("G:/My Drive/LISPOP")
options(scipen=999)
select <- dplyr::select
map <- purrr::map

library(tidyverse)
library(haven)
library(rgdal)
library(sf)


## Create FSA lat-lon coordinates from FSA centroids
fsa.shp <- read_sf("./FSA SHP/lfsa000b16a_e.shp") %>%
  st_transform(., crs = 4326) %>%
  mutate(FSA = CFSAUID) %>%
  mutate(PR = as.integer(PRUID)) %>%
  group_by(FSA, PR) %>%
  summarise() %>%
  st_point_on_surface() %>%
  ungroup() %>%
  as_tibble() %>%
  unnest_wider(., col = "geometry", names_repair = "unique") %>%
  mutate(Y = `...4`, X = `...3`) %>%
  select(FSA, PR, Y, X) %>%
  filter(PR %in% 10:59) %>%
  distinct(FSA, .keep_all = TRUE) %>%
  arrange(FSA)

#read_sf("./FSA SHP/lfsa000b16a_e.shp") %>%
#  st_transform(., crs = 4326) %>%
#  mutate(FSA = CFSAUID) %>%
#  plot()


## Read in PCCF; create FSA centroids from convex hulls of postal code lat-lon coordinates
pccf.0 <- read_fwf("./PCCF 2020.11/pccfNat_fccpNat_112020.txt",
                   guess_max = 500000, trim_ws = TRUE, skip = 0,
                   fwf_cols(postal.code = c(1, 6),
                            FSA = c(7,9),
                            PR = c(10,11),
                            Y = c(138,148),
                            X = c(149,161),
                            SLI = c(162,162),
                            PCtype = c(163,163),
                            Comm_Name = c(164,193),
                            #Birth_Date = c(196,203),
                            Ret_Date = c(204,211),
                            QI = c(213,215),
                            Source = c(216,216)
                            )) %>%
  filter(PR %in% 10:59 & Ret_Date == 19000001) %>%
  arrange(FSA)

pccf.1 <- pccf.0 %>%
  st_as_sf(coords = c("X", "Y")) %>% 
  st_set_crs(4326) %>%
  group_by(FSA) %>% 
  summarise() %>% 
  st_convex_hull() %>%
  st_point_on_surface() %>%
  ungroup() %>%
  as_tibble() %>%
  unnest_wider(., col = "geometry", names_repair = "unique") %>%
  mutate(Y = `...3`, X = `...2`) %>%
  select(FSA, Y, X) %>%
  left_join((pccf.0 %>%
               group_by(FSA, PR, Comm_Name) %>%
               summarise(n = n()) %>%
               ungroup() %>%
               arrange(FSA, desc(n)) %>%
               select(FSA, PR, Comm_Name) %>%
               distinct(FSA, .keep_all = TRUE)),
            by = "FSA")

#pccf.0 %>%
#  st_as_sf(coords = c("X", "Y")) %>% 
#  st_set_crs(4326) %>%
#  group_by(FSA) %>% 
#  summarise() %>% 
#  st_convex_hull() %>%
#  plot()


## Merge and dedupe; give precedence to FSA centroids from the 2016 Census; add new FSAs from the 2020 PCCF
lookup.0 <- fsa.shp %>%
  bind_rows(., pccf.1) %>%
  distinct(FSA, .keep_all = TRUE) %>%
  select(-Comm_Name) %>%
  left_join((select(pccf.1, FSA, Comm_Name)),
            by = c("FSA" = "FSA")) %>%
  arrange(FSA)

#lookup.0 %>%
#  select(-FSA, -Comm_Name) %>%
#  st_as_sf(coords = c("X", "Y")) %>%
#  st_set_crs(4326) %>%
#  st_transform(., crs = 3347) %>%
#  plot()


## LISPOP FSAs
data.0 <- read_csv("https://raw.githubusercontent.com/LISPOP/public-health-canada/main/data/fsa.csv") %>%
  mutate(OBS = row_number()) %>%
  mutate(FSA = str_to_upper(fsa2)) %>%
  select(OBS, FSA)

data.1 <- data.0 %>%
  left_join(lookup.0, by = "FSA") %>%
  filter(is.na(Y) == FALSE & is.na(X) == FALSE) %>%
  st_as_sf(., coords = c("X","Y"), crs = 4326)

no.match <- data.0 %>%
  left_join(lookup.0, by = "FSA") %>%
  filter(is.na(Y) == TRUE | is.na(X) == TRUE) %>%
  select(OBS, FSA)

print(no.match)


## Health region shapfiles
## Source: 
health.rgn.can <- read_sf("./HR SHP/HR_000b18a_e.shp") %>%
  st_transform(., crs = 4326) %>%
  mutate(HR = as.integer(HR_UID)) %>%
  rename(HR_NAME = ENGNAME) %>%
  mutate(PR = as.integer(str_sub(HR_UID, 1, 2))) %>%
  select(PR, HR, HR_NAME, geometry) %>%
  filter(!PR %in% c(35, 47, 59))


## Source: https://geohub.lio.gov.on.ca/datasets/ministry-of-health-public-health-unit-boundary
health.rgn.on <- read_sf("G:/My Drive/LISPOP/ON PHUs/Ministry_of_Health_Public_Health_Unit_Boundary.shp") %>%
  st_transform(., crs = 4326) %>%
  mutate(PR = 35) %>%
  rename(HR_NAME = NAME_ENG) %>%
  mutate(HR = case_when(
    HR_NAME == "Huron Perth Health Unit" ~ 3539,
    HR_NAME == "Southwestern Public Health" ~ 3575,
    TRUE ~ 3500 + (as.integer(str_sub(PHU_ID, 3, 4)))
  )) %>%
  select(PR, HR, HR_NAME, geometry)


## Source: https://catalogue.data.gov.bc.ca/dataset/health-authority-boundaries
health.rgn.bc <- read_sf("./BC HA/HA_2018.shp") %>%
  st_transform(., crs = 4326) %>%
  mutate(PR = 59) %>%
  mutate(HR = 5900 + as.integer(HA_CD)) %>%
  rename(HR_NAME = HA_Name) %>%
  select(PR, HR, HR_NAME, geometry)

health.rgn.bc %>% as_tibble %>% select(PR, HR, HR_NAME)


## Source: https://geohub.saskatchewan.ca/datasets/saskatchewan-covid-19-zones/
health.rgn.sk <- read_sf("./SK COVID zones/Saskatchewan_COVID_Zones.shp") %>%
  st_transform(., crs = 4326) %>%
  mutate(HR = 4700 + as.integer(AREAID)) %>%
  rename(HR_NAME = COVID_AREA) %>%
  group_by(HR, HR_NAME) %>%
  summarise() %>%
  ungroup() %>%
  mutate(PR = 47) %>%
  select(PR, HR, HR_NAME, geometry)

health.rgn.sk %>% as_tibble %>% select(PR, HR, HR_NAME)

health.rgn.can <- health.rgn.can %>%
  bind_rows(health.rgn.on) %>%
  bind_rows(health.rgn.sk) %>%
  bind_rows(health.rgn.bc) %>%
  arrange(PR, HR)


## Health region COVID data crosswalk
hr.crosswalk <- read_csv("https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/other/hr_map.csv") %>%
  select(HR_UID, health_region) %>%
  mutate(PR = as.integer(str_sub(HR_UID, 1, 2))) %>%
  mutate(HR = case_when(
    PR == 47 ~ (PR * 100) + as.integer(str_sub(HR_UID, 3, 3)),
    PR == 59 & str_detect(health_region, "Interior") ~ 5901,
    PR == 59 & str_detect(health_region, "Fraser") ~ 5902,
    PR == 59 & str_detect(health_region, "Coastal") ~ 5903,
    PR == 59 & str_detect(health_region, "Island") ~ 5904,
    PR == 59 & str_detect(health_region, "Northern") ~ 5905,
    TRUE ~ HR_UID
  ))
  

## The whole shebang
data.2 <- data.1 %>%
  select(-PR) %>%
  st_join(., health.rgn.can, join = st_within) %>%
  as_tibble() %>%
  select(OBS, FSA, Comm_Name, PR, HR, HR_NAME) %>%
  left_join(hr.crosswalk, by = c("PR" = "PR", "HR" = "HR"))

write_csv(data.2, "FSA to health region spatial join-2021 03 25.csv")

health.rgn.list <- health.rgn.can %>%
  as_tibble() %>%
  select(-geometry) %>%
  filter(PR %in% 10:59) %>%
  left_join((data.2 %>%
               group_by(HR) %>%
               summarise(n = n()) %>%
               ungroup()),
            by = "HR") %>%
  mutate(n = replace_na(n, 0)) %>%
  arrange(desc(n))

write_csv(health.rgn.list, "Sample sizes by health region-2021 03 29.csv")


## Map
health.rgn.prov <- health.rgn.can %>%
  st_transform(., crs = 3347) %>%
  filter(PR %in% 10:59)

territories <-  health.rgn.can %>%
  st_transform(., crs = 3347) %>%
  filter(PR >= 60) %>%
  select(HR, geometry)
  
usa <- read_sf("./CB state SHP/cb_2018_us_state_500k.shp") %>%
  st_transform(., crs = 3347) %>%
  mutate(HR = as.integer(STATEFP)) %>%
  filter(HR != 15 & HR <= 56) %>%
  select(HR, geometry)

xy.limits <- st_sfc(st_point(c(-140, 42)),
                    st_point(c(-140, 63)),
                    st_point(c(-54, 63)),
                    st_point(c(-54, 42)),
                    crs = 4326) %>%
  st_transform(., crs = 3347) %>%
  st_coordinates()

xy.limits

map.out <- ggplot() +
  geom_sf(data = territories, color = "grey90", fill = "grey95") +
  geom_sf(data = usa, color = "grey90", fill = "grey95") +
  geom_sf(data = health.rgn.prov, color = "grey20", fill = "grey80") +
  ggtitle("Health Regions") +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(crs = 3347, xlim = c(3900000, 8800000), ylim = c(740000, 3490000), expand = TRUE) +
  scale_y_continuous(breaks = seq(40, 60, by = 5)) +
  scale_x_continuous(breaks = seq(-120, -60, by = 10)) +
  theme_light() +
  theme(text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "in"))

ggsave(plot = map.out, file = "Health regions map-2021 03 25.png", device = "png", 
       width = 12, height = 8, units = "in", dpi = 600, family = "Arial")
