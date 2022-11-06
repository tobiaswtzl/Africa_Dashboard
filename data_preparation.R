################################################################################
#Dashboard Data Cleaning
################################################################################

#checks if pacman exists, if not it installs it
if (!require("pacman")) install.packages("pacman")

#install/load packages
pacman::p_load("dplyr",
               "readxl",
               "janitor",
               "stringr",
               "lubridate",
               "sf",
               "countrycode",
               "geojsonio",
               "sp",
               "tidyr", 
               "tibble",
               "wbstats",
               "rgeos",
               "rworldmap",
               "here",
               "httr",
               "jsonlite",
               "rhdx",
               "purrr",
               "raster")

  #set wd
here::i_am("data_prep.R")


add_country <- function(.data, country_to_add) {
  add_row(.data, country = country_to_add, do = "", title = "", objective = "", year_commitment = "2022",
          kernthema = NA,financial_commitment_planned_in_mio = 0, financial_commitment_paid_in_mio = 0,
          referat = "", haupt_aktionsfeld = "", zweites_aktionsfeld = "", drittes_aktionsfeld = "",
          haupt_initiativthema = "", zweites_initiativthema = "", fem_ez = "nicht verfügbar"
  ) 
}


#############################
####Text data #######
############################

ipc_glossar <- read_xlsx((here("data", "ipc_glossar.xlsx")))
save(ipc_glossar, file = here("data", "ipc_glossar.rdata"))

quellen <- read_xlsx((here("data", "quellen.xlsx")))


#############################
####Memphis data #######
############################

#load data
data_raw <- readxl::read_xlsx(here("data","daten_memphis.xlsx"), sheet = 2)

#prepare data
data_bmz <- data_raw %>% 
  
  #improve column names
  janitor::clean_names() %>% 
  
  #rename 
  dplyr::rename(
    country = mittelempfanger,
    referat = projektfuhrendes_referat,
    title = bezeichnung_ez_massnahme_deutsch,
    year_commitment = haushaltsjahr_letzte_zusage,
    financial_commitment = zusagebetrag_gesamt_inkl_reprogrammierungen,
    financial_commitment_paid = auszahlungsbetrag_gesamt,
    objective = zielsetzung,
    fem_ez = gg
  ) %>% 
  
  #remove dot from financial commitments
  dplyr::mutate(financial_commitment_planned_in_mio = as.numeric(financial_commitment)) %>% 
  
  #remove dots
  dplyr:: mutate(financial_commitment_paid_in_mio = as.numeric(financial_commitment_paid)) %>% 
  
  #NAs to 0
  dplyr::mutate(financial_commitment_paid_in_mio = ifelse(is.na(financial_commitment_paid_in_mio), 0, financial_commitment_paid_in_mio)) %>% 
  
  #remove projects when year is missing
  dplyr::filter(year_commitment != "nicht verfügbar") %>% 
  
  #fem_ez as factor
  dplyr::mutate(fem_ez = as.factor(
    dplyr::case_when(
      fem_ez == 0 ~ "Keine Genderkomponente",
      fem_ez == 1 ~ "Genderkomponente",
      fem_ez == 2 ~ "Gender Hauptkomponente",
      fem_ez == "nicht verfügbar" ~ "nicht verfügbar"
    )
  )
  ) %>% 
  
  #keep only relevant variables
  dplyr::select(
    country, referat, do, title, objective, year_commitment, contains("in_mio"),
    kernthema, contains("feld"), contains("initiativ"), financial_commitment_paid_in_mio, fem_ez
  ) %>% 
  
  #remove RECs
  dplyr::filter(!(country %in% c(
    "Afrika na (nur EL)", "AU", "BOAD", "CEMAC", "CEPGL - Communaute economique des pays des grands lacs",
    "COMIFAC", "EAC", "ECOWAS", "Fragile Staaten Westafrika", "IGAD", "OMVG", "SADC"
  ))) %>% 
  
  #add missing countries
  add_country(
    c("Gambia", "Guinea Bissau", "Äquatorialguinea", "Gabun", "Kongo",
      "Dschibuti", "Somaliland", "Westsahara"
    )) %>%
  
  #add missing data combinations
  tidyr::complete(country, year_commitment,
                  fill = list(
                    do = "", title = "", objective = "", 
                    kernthema = NA,financial_commitment_planned_in_mio = 0, financial_commitment_paid_in_mio = 0,
                    referat = "", haupt_aktionsfeld = "", zweites_aktionsfeld = "", drittes_aktionsfeld = NA,
                    haupt_initiativthema = "", zweites_initiativthema = "", fem_ez = "nicht verfügbar"
                  ),
                  explicit = FALSE
  ) %>% 
  
  #year as numeric
  dplyr::mutate(year = as.integer(year_commitment)) %>% 
  dplyr::mutate(year_commitment = as.Date(paste0(year_commitment, "-01-01"))) %>% 
  
  #substitute Kernthema NAs
  dplyr::mutate(kernthema = as.factor(ifelse(is.na(kernthema), "Sonstiges", kernthema))) %>% 
  
  #add Ref. to referat
  dplyr::mutate(referat = ifelse(referat == "", referat,paste("Ref.", referat))) %>% 
  
  #get iso
  dplyr::mutate(ISO_A3 = countrycode::countrycode(country, origin = "country.name.de", destination = "iso3c")) %>% 
  
  #country name in english
  dplyr::mutate(country_en = countrycode::countrycode(country, origin = "country.name.de", destination = "country.name.en")) %>% 
  dplyr::mutate(country_en = ifelse(country_en == "Congo - Kinshasa", "Democratic Republic of the Congo", country_en)) %>% 
  dplyr::mutate(country_en = ifelse(country_en == "Côte d’Ivoire", "Ivory Coast", country_en))  %>% 
  dplyr::mutate(country_en = ifelse(country_en == "Tanzania", "United Republic of Tanzania", country_en))  %>% 
  
  #group by variables used for filtering later
  dplyr::group_by(country_en, ISO_A3, do, referat, year, fem_ez, kernthema) %>% 
  dplyr::summarise(
    across(
      contains("financial"),
      ~ sum(.x)
    )
  ) %>% 
  dplyr::ungroup()


#get list of countries in data
countries <- data_bmz %>%
  dplyr::select(ISO_A3) %>%
  dplyr::distinct() 

########################
#Population data 
#######################

#get data from the world bank
data_wb_raw <- wb_data(
  indicator = c("SP.POP.TOTL"),
  start_date = 2019,
  end_date = lubridate::year(Sys.Date()),
  country = countries$ISO_A3
)

#wrangle
data_wb <- data_wb_raw %>%
  dplyr::rename(
    ISO_A3 = iso3c,
    population_wb = `SP.POP.TOTL`
  ) %>% 
  dplyr::group_by(ISO_A3) %>% 
  
  #choose only most recent data
  dplyr::top_n(1, date) %>% 
  dplyr::select(ISO_A3, population_wb)

#merge with population data
data_memphis <- data_bmz %>% 
  left_join(data_wb, by = "ISO_A3")


#save
save(data_memphis, file = here("data", "data_memphis.rdata"))



################################################################################
#Map for EZ data
################################################################################

#get world map with central location of each country
wmap <- rworldmap::getMap(resolution="low")

#prepare for merging
centroids <- as.data.frame(gCentroid(wmap, byid=TRUE)) %>%
  tibble::rownames_to_column("country") %>%
  dplyr::rename(lng_center = x,
                lat_center = y) %>%
  dplyr::mutate(ISO_A3 = countrycode::countrycode(country, origin = "country.name.en", destination = "iso3c")) %>% 
  
  #create variable with German country names
  dplyr::mutate(country_de = countrycode::countrycode(ISO_A3, origin = "iso3c", destination = "country.name.de"))



#load geodata
world_map_data <- geojsonio::geojson_read(here("data","countries.geojson"), what = "sp")

#use Somalia iso for Somaliland
world_map_data@data$ISO_A3[208] <- "SOM"
world_map_data@data$ADMIN[208] <- "Somalia"

#keep only Africa
world_map_data_africa <- subset(world_map_data, ISO_A3 %in% countries$ISO_A3)

#merge Somalia and Somaliland Polygon
world_map_data_africa <- aggregate(world_map_data_africa, by = c("ISO_A3", "ADMIN"))

#add data with central location of each country
world_map_data_africa <- sp::merge(world_map_data_africa, centroids, by.x = 'ISO_A3', by.y = 'ISO_A3') 


###############################################################################
#Normal map for food data
###############################################################################

########################
#CH data 
#######################
#pull CH data
data_ch_raw <- rhdx::pull_dataset("cadre-harmonise") %>%
  rhdx::get_resource(5) %>%
  rhdx::read_resource()


data_ch <- data_ch_raw %>% 
  
  #rename
  dplyr::rename(
    country_en = adm0_name,
    ISO_A3 = adm0_pcod3,
    year = reference_year,
    period = reference_code,
    period_prediction = exercise_code,
    year_prediction = exercise_year,
    region = adm1_name,
    subregion = adm2_name,
    population_analysed = population
  ) %>% 
  
  #select only key variables
  dplyr::select(
    country_en, ISO_A3, year, period, population_analysed, period_prediction, year_prediction, region, subregion, phase1:phase35
  ) %>%
  
  #logical order of periods
  dplyr::mutate(period = dplyr::case_when(
    period == 1 ~ 3,
    period == 2 ~ 1,
    period == 3 ~ 2
  )) %>% 
  dplyr::group_by(ISO_A3, year, subregion) %>%
  
  #only current year
  dplyr::top_n(1, year)  %>% 
  
  #only recent period
  dplyr::top_n(1, period)  %>% 
  
  #create variable to use last prediction (predictions for the same period are sometimes made multiple times)
  mutate(counter_prediction =
           case_when(
             year_prediction == 2021 & period_prediction == 2 ~ 1, 
             year_prediction == 2021 & period_prediction == 3 ~ 2, 
             year_prediction == 2021 & period_prediction == 1 ~ 3, 
             year_prediction == 2022 & period_prediction == 2 ~ 4, 
             year_prediction == 2022 & period_prediction == 3 ~ 5, 
             year_prediction == 2022 & period_prediction == 1 ~ 6, 
             year_prediction == 2023 & period_prediction == 2 ~ 7, 
             year_prediction == 2023 & period_prediction == 3 ~ 8, 
             year_prediction == 2023 & period_prediction == 1 ~ 9 
           )
  ) %>% 
  
  #only recent predictions
  dplyr::filter(counter_prediction > (max(counter_prediction) -1)) %>% 
  dplyr::ungroup() %>%
  dplyr::group_by(ISO_A3, year, period) %>%
  
  #sum per country
  dplyr::summarise(
    dplyr::across(
      c(
        contains("phase"), 
        population_analysed
      ),
      ~sum(.x, na.rm = TRUE)
    )) %>% 
  
  #add country names
  dplyr::mutate(country_en = countrycode(ISO_A3, origin = "iso3c", destination = "country.name.en")) %>% 
  
  #add variable that captures share of people in phases 3-5
  dplyr::mutate(
    dplyr::across(
      contains("phase"),
      ~round(.x /  population_analysed * 100, 0),
      .names = "{.col}_percent"
    )
  ) %>% 
  dplyr::ungroup() %>% 
  
  #periods as strings
  dplyr::mutate(period = dplyr::case_when(
    period == 1 ~ paste("Jan",year , "-", "May", year),
    period == 2 ~ paste("Jun", year , "-", "Aug", year),
    period == 3 ~ paste("Sep",year , "-", "Dec", year)
      )) %>% 
  
  #remove CAF because ICP also measures this country
  dplyr::filter(ISO_A3 != "CAF")

########################
#ICP / CH data 
#######################

#token for api
token <- readr::read_file(here("data", "token_ipc.txt"))

#get json file
res <- GET("https://api.ipcinfo.org/country?format=json",
           query = list(key = token))

#json to df
data_ipc_raw <- fromJSON(rawToChar(res$content))

#wrangle
data_ipc <- bind_rows(data_ipc_raw$phases, .id = "column_label") %>% 
  pivot_wider(
    id_cols = everything(),
    names_from = "phase",
    values_from = c("percentage", "population", "color")
  ) %>% 
  
  #remove unnecessary column
  dplyr::select(-column_label) %>% 
  
  #merge with country names
  bind_cols(data_ipc_raw) %>% 
  
  #create country names
  mutate(country_en = countrycode::countrycode(country, origin = "iso2c", destination = "country.name.en")) %>% 
  mutate(ISO_A3 = countrycode::countrycode(country, origin = "iso2c", destination = "iso3c")) %>% 
  mutate(phase35 = population_3 + population_4 + population_5) %>%
  
  #as percent
  mutate(
    across(
      contains("percentage"),
      ~ .x * 100
    )
  ) %>% 
  
  #add variable that captures share of people in phases 3-5
  mutate(phase35_percent = round(percentage_3 + percentage_4 + percentage_5, 0)) %>% 
  
  #calculate population
  mutate(population = population_1 / percentage_1 * 100) %>% 
  rename(
    phase1 = population_1,
    phase2 = population_2,
    phase3 = population_3,
    phase4 = population_4,
    phase5 = population_5
  ) %>% 
  
  #rename
  rename(
    phase1_percent = percentage_1,
    phase2_percent = percentage_2,
    phase3_percent = percentage_3,
    phase4_percent = percentage_4,
    phase5_percent = percentage_5 
  ) %>% 
  
  #select only relevant vars
  dplyr::select(title, country_en, ISO_A3, from, to, year, phase35_percent, population,
                contains("phase"),
                -phases
  ) %>% 
  
  #get year
  mutate(year = year(parse_date_time(to, order = "bY"))) %>% 
  mutate(period = paste(from,"-", to)) %>% 
  
  #select most recent analysis
  mutate(period_numeric = parse_date_time(to, "%m %Y")) %>%
  group_by(country_en) %>% 
  top_n(1, period_numeric)  %>% 
  ungroup() %>% 
  
  #select row with updated data
  mutate(data_update = ifelse(str_detect(title, "Proj.Update"), 1, 0)) %>% 
  group_by(country_en) %>% 
  top_n(1, data_update) %>% 
  ungroup() %>% 
  
  #drop variables
  dplyr::select(-from, -to, -period_numeric, -title) %>% 
  rename(population_analysed = population)


#merge IPC data
data_icp_ch <- dplyr::bind_rows(data_ipc, data_ch) %>% 
  
  #only recent data
  dplyr::filter(year > 2021 & ! is.na(country_en))


########################
#food basket data 
#######################

#pull data
data_foodbasket_raw <- rhdx::pull_dataset("cost-of-the-food-basket") %>%
  rhdx::get_resource(1) %>%
  rhdx::read_resource()

#wrangle data
data_foodbasket <- data_foodbasket_raw %>% 
  dplyr::rename(
    ISO_A3 = iso3,
    change_foodbasket_percent = ChangeInFoodBasketCost
  ) %>% 
  
  #in percent
  dplyr::mutate(change_foodbasket_percent = change_foodbasket_percent * 100)




########################
#debt data 
#######################

#currently not in use since data cannot be refreshed automatically (yet)
#get debt data
# data_debt_raw <- readxl::read_excel(here("data", "debt.xlsx"))
# 
# #clean
# data_debt <- data_debt_raw %>% 
#   
#   #format excel dates to real dates
#   dplyr::mutate(date_publication = as.character(as.Date(as.numeric(date_publication), origin = "1899-12-30"))) %>% 
#   
#   #get iso
#   dplyr::mutate(ISO_A3 = countrycode::countrycode(country_en, origin = "country.name.en", destination = "iso3c")) %>% 
#   
#   #to prevent mistakes
#   dplyr::mutate(risk_debt_distress = tolower(risk_debt_distress)) %>% 
#   
#   # #to numeric
#   dplyr::mutate(risk_debt_distress_numeric = dplyr::case_when(
#     risk_debt_distress == "low" ~ 1,
#     risk_debt_distress == "moderate" ~ 2,
#     risk_debt_distress == "high" ~ 3,
#     risk_debt_distress == "in distress" ~ 4
#   )
#   ) %>% 
#   
#   #german text
#   
#   # #to numeric
#   dplyr::mutate(risk_debt_distress = dplyr::case_when(
#     risk_debt_distress == "low" ~ "niedrig",
#     risk_debt_distress == "moderate" ~ "moderat",
#     risk_debt_distress == "high" ~ "hoch",
#     risk_debt_distress == "in distress" ~ "Staatsschuldenkrise vorhanden"
#   )
#   )


########################
#Merge 
#######################

data_external <- countries %>% 
  
  #add population data
  left_join(data_icp_ch, by = "ISO_A3") %>% 
  
  #add population data
  dplyr::left_join(data_wb, by = "ISO_A3") %>% 
  
  #calculate share of analysed population
  dplyr::mutate(analysed_population_percent = round(population_analysed / population_wb * 100, 1)) %>% 
  
  #add debt data
  # dplyr::left_join(data_debt, by = "ISO_A3") %>% 

  #add food basket data
  dplyr::left_join(data_foodbasket, by = "ISO_A3")
  


#merge with spatial data
map_data <- sp::merge(world_map_data_africa, data_external, by.x = 'ISO_A3', by.y = 'ISO_A3') 

#save
save(map_data, file = here("data", "map_data.rdata"))


################################################################################
#Prepare data for valueBoxes
################################################################################


########################
#ACLED 
#######################

#currently without using the api because receiving the data without special access is only possible  three times
data_acled_raw <- read.csv(here("data", "data_acled_full.csv")) %>% 
  
  #only events from last 6 month
  dplyr::filter(lubridate::parse_date_time(event_date, "%d %m %Y") > Sys.Date() %m-% months(6)) %>% 
  dplyr::mutate(date = as.character(lubridate::parse_date_time(event_date, "%d %m %Y"))) 

#get dates of oldest and most recent date
data_acled_dates <- data_acled_raw %>% 
  dplyr::mutate(oldest_date = min(date)) %>% 
  dplyr::mutate(most_recent_date = max(date)) %>% 
  dplyr::slice(1) %>% 
  dplyr::select(oldest_date, most_recent_date)

#get data of fatalities
data_acled_fatalities <- data_acled_raw %>% 
  dplyr::filter(event_type %in% c("Battles", "Strategic developments", "Violence against civilians", "Explosions/Remote violence")) %>% 
  dplyr::rename(ISO_A3 = iso3) %>% 
  dplyr::group_by(ISO_A3) %>% 
  dplyr::summarise(fatalities_sum = sum(fatalities)) %>% 
  dplyr::ungroup()

#get data of protests/riots
data_acled_protest <- data_acled_raw %>% 
  dplyr::filter(event_type %in% c("Protests", "Riots")) %>% 
  dplyr::rename(ISO_A3 = iso3) %>% 
  dplyr::mutate(count = 1) %>% 
  dplyr::group_by(ISO_A3) %>% 
  dplyr::summarise(protest_riot_sum = sum(count)) %>% 
  dplyr::ungroup()



########################
#Internal displacement 
#######################

#pull data
data_displacement_raw <- rhdx::pull_dataset("preliminary-internal-displacement-updates") %>%
  rhdx::get_resource(2) %>%
  rhdx::read_resource() 

#get oldest and most recent date
data_displacement_dates <- data_displacement_raw %>% 
  dplyr::mutate(oldest_date = min(displacement_date)) %>% 
  dplyr::mutate(most_recent_date = max(displacement_date)) %>% 
  dplyr::slice(1) %>% 
  dplyr::select(oldest_date, most_recent_date)

#calculate sum of internal displacements
data_displacement <- data_displacement_raw %>% 
  dplyr::mutate(disp_month = str_extract(displacement_date, "20[0-9]{2}-[0-9]{2}")) %>% 
  dplyr::rename(ISO_A3 = iso3) %>% 
  dplyr::group_by(ISO_A3) %>% 
  dplyr::summarise(int_displacement = sum(figure)) 



########################
#Disasters 
#######################

#pull data
data_disasters_raw <- rhdx::pull_dataset("emdat-country-profiles") %>%
  rhdx::get_resource(1) %>%
  rhdx::read_resource() 

data_disasters <- data_disasters_raw %>% 
  dplyr::rename(ISO_A3 = ISO) %>% 
  
  #only data for the current year
  dplyr::filter(Year == year(Sys.Date())) %>% 
  dplyr::group_by(ISO_A3) %>% 
  dplyr::summarise(affected_sum = sum(`Total Affected`, na.rm = TRUE))


########################
#Merge 
#######################

data_boxes <- countries %>% 
  
  #join data
  left_join(data_acled_fatalities, by = "ISO_A3") %>% 
  left_join(data_acled_protest, by = "ISO_A3") %>% 
  left_join(data_displacement, by = "ISO_A3") %>% 
  left_join(data_disasters, by = "ISO_A3") %>% 
  left_join(data_external, by = "ISO_A3") %>% 

  #new column with english country name
  mutate(country_en = countrycode::countrycode(ISO_A3, origin = "iso3c", destination = "country.name.en")) %>% 
  
  #NAs to 0
  dplyr::mutate(
    dplyr::across(
      c(
        fatalities_sum, protest_riot_sum, int_displacement, affected_sum,
        contains("phase"), population_analysed, change_foodbasket_percent
      ),
      ~ifelse(is.na(.x), 0, .x)
    )
  ) %>% 
  dplyr::ungroup() %>% 
  
  #row summarising all data 
  dplyr::add_row(          country_en = "Africa",
          fatalities_sum = sum(data_acled_fatalities$fatalities_sum),
          protest_riot_sum = sum(data_acled_protest$protest_riot_sum),
          int_displacement = sum(data_displacement$int_displacement),
          affected_sum = sum(data_disasters$affected_sum),
          phase35_percent = round(sum(data_external$phase35, na.rm = TRUE) / sum(data_external$population_analysed, na.rm = TRUE) * 100, 2),
          change_foodbasket_percent = round(mean(data_foodbasket$change_foodbasket_percent, na.rm = TRUE, 2)),
          population_wb = round(sum(data_wb$population_wb, na.rm = TRUE, 2))
    ) %>% 

  
  #rename countries due to different names in the map dataset
  dplyr::mutate(country_en = ifelse(country_en == "Congo - Kinshasa", "Democratic Republic of the Congo", country_en)) %>% 
  dplyr::mutate(country_en = ifelse(country_en == "Côte d’Ivoire", "Ivory Coast", country_en))  %>% 
  dplyr::mutate(country_en = ifelse(country_en == "Tanzania", "United Republic of Tanzania", country_en)) %>% 
  
  #most recent and old date in all rows
  dplyr::mutate(int_displacement_oldest_date = data_displacement_dates$oldest_date) %>% 
  dplyr::mutate(int_displacement_most_recent_date = data_displacement_dates$most_recent_date) %>% 
  dplyr::mutate(acled_oldest_date = data_acled_dates$oldest_date) %>% 
  dplyr::mutate(acled_most_recent_date = data_acled_dates$most_recent_date) %>% 
  dplyr::mutate(country_de = countrycode::countrycode(ISO_A3, origin = "iso3c", destination = "country.name.de")) %>% 
  
  #indicators per head
  dplyr::mutate(
    dplyr::across(
      c(fatalities_sum, protest_riot_sum, int_displacement, affected_sum),
      
      #calculate per 100.000
      ~ round(.x / (population_wb / 100000))
    ))
  
#save
save(data_boxes, file = here("data", "data_boxes.rdata"))


##################################
#Einschätzung der Regionalreferate
##################################

data_referate_raw <- read_xlsx(here("data", "einschätzung_referate_clean.xlsx"))

data_referate <- data_referate_raw %>% 
  dplyr::rename(einschaetzung = `...6`) %>% 
  dplyr::mutate(
    dplyr::across(
    c(grün, orange, rot),
    ~ tolower(.x)
  )) %>% 
  dplyr::mutate(ampel = case_when(
    grün == "x" ~ 1,
    orange == "x" ~ 2,
    rot == "x" ~  3
  )) %>% 
  
  #get iso code
  dplyr::mutate(ISO_A3 = countrycode::countrycode(Land, origin = "country.name.de", destination = "iso3c")) %>% 
  dplyr::mutate(country_en = countrycode::countrycode(ISO_A3, origin = "iso3c", destination = "country.name.en")) %>% 
  dplyr::select(- c(grün, orange, rot)) %>% 
  
  #hand code afrika
  dplyr::mutate(country_en = ifelse(Land == "Afrika", "Afrika", country_en))

save(data_referate, file = here("data", "data_referate.rdata"))

