library(readxl)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)

# grab the HUD homeless data
URL <- "https://www.hudexchange.info/resources/documents/2007-2015-PIT-Counts-by-CoC.xlsx"
fil <- basename(URL)
if (!file.exists(fil)) download.file(URL, fil, mode="wb")

# turn the excel tabs into a long data.frame
yrs <- 2015:2007
names(yrs) <- 1:9
homeless <- map_df(names(yrs), function(i) {
  df <- suppressWarnings(read_excel(fil, as.numeric(i)))
  df[,3:ncol(df)] <- suppressWarnings(lapply(df[,3:ncol(df)], as.numeric))
  new_names <- tolower(make.names(colnames(df)))
  new_names <- str_replace_all(new_names, "\\.+", "_")
  df <- setNames(df, str_replace_all(new_names, "_[[:digit:]]+$", ""))
  bind_cols(df, data_frame(year=rep(yrs[i], nrow(df))))
})

# clean it up a bit
homeless <- mutate(homeless,
                   state=str_match(coc_number, "^([[:alpha:]]{2})")[,2],
                   coc_name=str_replace(coc_name, " CoC$", ""))
homeless <- select(homeless, year, state, everything())
homeless <- filter(homeless, !is.na(state))

# read in the us population data
URL2 <- "https://raw.githubusercontent.com/52vis/2016-14/master/uspop.csv"
fil2 <- basename(URL2)
if (!file.exists(fil2)) download.file(URL2, fil2, mode="wb")

uspop <- read.csv(fil2, stringsAsFactors=FALSE)
uspop_long <- gather(uspop, year, population, -name, -iso_3166_2)
uspop_long$year <- sub("X", "", uspop_long$year)

# normalize the values
states <- count(homeless, year, state, wt=total_homeless)
states <- left_join(states, albersusa::usa_composite()@data[,3:4], by=c("state"="iso_3166_2"))
states <- ungroup(filter(states, !is.na(name)))
states$year <- as.character(states$year)
states <- mutate(left_join(states, uspop_long), homeless_per_100k=(n/population)*100000)

# we want to order from worst to best
group_by(states, name) %>%
  summarise(mean=mean(homeless_per_100k, na.rm=TRUE)) %>%
  arrange(desc(mean)) -> ordr

states$year <- factor(states$year, levels=as.character(2006:2016))
states$name <- factor(states$name, levels=ordr$name)


head(states)
head(homeless)
