babies_raw <- 
  readRDS(SMR02_filename) %>% 
  filter(year >= 2018 & condis == 3) %>% 
  mutate(dataset = "SMR02+",
         hbtype = "Treatment",
         hbname = "Scotland",
         date_of_delivery = as.Date(date_of_delivery),
         quarter_of_delivery = as.Date(as.yearqtr(date_of_delivery)), # quarter beginning
         fin_year = extract_fin_year(date_of_delivery),
         calendar_year = format(ymd(date_of_delivery), "%Y"),
         period = "Q",
         estgest = na_if(estgest, 99)
  ) %>% 
  group_by(upi, date_of_delivery) %>% 
  mutate(same_birth = (upi == lag(upi) & date_of_delivery == lag(date_of_delivery))
  ) %>% 
  select(marker, same_birth, upi, hosp, hbtname, date_of_delivery, quarter_of_delivery, numbir,
         outcome = outcome1, outcome_name = outcome1name, gestation_weeks = estgest, contains("admission"), contains("discharge")) %>%
  arrange(upi, quarter_of_delivery, marker) %>% 
  ungroup()

test2 <- filter(babies_raw, !is.na(upi)) %>%
  select(- same_birth) %>% 
  group_by(upi, date_of_delivery) %>% 
  mutate(count = n(), 
         matches = count == numbir, 
         under = count < numbir,
         same_birth = (upi == lag(upi) & date_of_delivery == lag(date_of_delivery))
         ) %>%
  select(upi, date_of_delivery, same_birth, count, numbir, matches, under) %>%
  filter(matches == FALSE) %>% 
  filter(under == FALSE) %>% 
  select(- under)


