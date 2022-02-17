library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)

##Reading in the cleaned up pdfs
wsib_fund_activity <- read_csv("~/WSIB_project/Data/Cleaned/Combined_WSIB_cleaned_filtered_sheets.pdf - Sheet1.csv", 
                               col_types = cols(report_date = col_date(format = "%m/%d/%Y"), 
                                                initial_investment_date = col_date(format = "%m/%d/%Y"), 
                                                strategy = col_factor(levels = c("Corporate Finance/Buyout - Small", 
                                                                                 "Corporate Finance/Buyout - Mid", 
                                                                                 "Growth Equity", "Corporate Finance/Buyout - Mega", 
                                                                                 "Corporate Finance/Buyout - Large", 
                                                                                 "Distressed Debt", "Venture Capital", 
                                                                                 "Special Situation", "Co-Investment", 
                                                                                 "Mezzanine"))))

wsib_fund_activity$fund_name <- str_remove(wsib_fund_activity$fund_name, ", L.P.$")

str(wsib_fund_activity)

##I'll start by normalizing the reporting and seeking out any additional issues where data should be removed or fixed
##I want to introduce an artificial time 0 for all ids
funds_to_examine_clipped_beginning <- wsib_fund_activity %>%
  group_by(assigned_unid) %>%
  mutate(initial_investment_year = year(initial_investment_date), initial_investment_month = month(initial_investment_date),
         report_year = year(report_date), report_month = month(report_date)) %>%
  filter(report_date == min(as.integer(report_date))) %>%
  mutate(matching_year = initial_investment_year == report_year, matching_month = initial_investment_month == report_month) %>%
  filter(matching_year == FALSE) %>%
  select(assigned_unid)

wsib_fund_activity %>%
  group_by(assigned_unid) %>%
  mutate(report_year = year(report_date), report_month = month(report_date)) %>%
  filter(report_date == max(as.integer(report_date))) %>%
  filter(report_year != 2021)

funds_to_examine <- wsib_fund_activity %>%
  filter(assigned_unid %in% funds_to_examine_clipped_beginning$assigned_unid)

##With some examination - I see that TPG Growth III does have an incorrect initial investment date. I went back to the source
##documents to completely affirm. I'll replace their value and set up a useful attribute called vintage. 

wsib_fund_activity$initial_investment_date[which(wsib_fund_activity$assigned_unid == "f0hx32ntdvxi8ezsuj1x")] <- as.Date("2016-03-31")

wsib_fund_activity_added_attributes <- wsib_fund_activity %>%
  group_by(assigned_unid) %>%
  arrange(assigned_unid, report_date) %>%
  mutate(vintage = year(initial_investment_date), series_count = 1:n()) %>%
  ungroup()

##It appears that we have complete fund histories as the starting point matches the signaled investment date and the fund histories
##end at the last report date. Now is the time to add an artificial time 0 so that I can capture all quarterly interval sums correctly
##i'll do so by creating a series count that starts at 1 for each fund and counts that number of reporting quarters

uniform_starting_point_series_time_zero <- wsib_fund_activity_added_attributes %>%
  filter(series_count == 1) %>%
  mutate(series_count = 0, unfunded_commitment = 0, contributions = 0, distributions = 0, 
         current_market_value = 0, total_value = 0, net_benefit = 0)

uniform_starting_point_artificial_zero <- rbind(wsib_fund_activity_added_attributes, uniform_starting_point_series_time_zero)

##Going to introduce new rows that we are going to leverage. I'm first checking and replacing any leftover nas in the set
##with the values we are going to be calculating. Then I'm going to calculate out the quaterly intervals between time periods for
##contributions and distributions and lastly I filter the artificial zero counts right back out of the set when completed

uniform_starting_point_artificial_zero %>%
  filter(is.na(distributions) | is.na(contributions)) %>%
  select(report_date, assigned_unid, contributions, distributions)

uniform_starting_point_artificial_zero$distributions <- replace_na(uniform_starting_point_artificial_zero$distributions, 0)

wsib_fund_activity_quarterly <- uniform_starting_point_artificial_zero %>%
  arrange(assigned_unid, series_count) %>%
  group_by(assigned_unid) %>%
  mutate(quarterly_contributions = contributions - lag(contributions), 
         quarterly_distributions = distributions - lag(distributions),
         quarterly_net_cashflow = quarterly_distributions - quarterly_contributions) %>%
  filter(series_count != 0)

##Now it is time to introduce our benchmark and the corresponding data that I'm pulling from FRED - I have added a column to the data
##the column simply aligns the index_date to a normalized end of quarter date that aligns with the report_dates in my WSIB data

sp500 <- read_csv("~/WSIB_project/Data/Cleaned/S&P 500 Historical Data - investingdotcom_2001_01_01_to_2021_07_01 - Filtered and Cleaned - S&P 500 Historical Data.csv", 
    col_types = cols(index_date = col_date(format = "%b %d, %Y"), 
        normalized_eoq_date = col_date(format = "%m/%d/%Y")))


wsib_fund_activity_quarterly_sp500 <- merge(wsib_fund_activity_quarterly, sp500, by.x = "report_date", 
                                            by.y = "normalized_eoq_date", all.x = TRUE)

##I need to get the latest sp500 value that matches my latest report_date for WSIB to create a growth ratio over time for the sp500

sp500_current <- sp500[[1,2]]

##Now I'm going to create that growth ratio and use it as a way to convert the quarterly contributions and distributions to model
##the concept of purchasing and selling sp500 shares at the same time these same actions are happening at the fund level. 

wsib_fund_activity_quarterly_sp500_value <- wsib_fund_activity_quarterly_sp500 %>%
  mutate(sp500_growth_ratio = sp500_current / sp500_value, 
         sp500_quarterly_contributions = sp500_growth_ratio * quarterly_contributions,
         sp500_quarterly_distributions = sp500_growth_ratio * quarterly_distributions,
         sp500_quarterly_net_cashflow = sp500_quarterly_distributions - sp500_quarterly_contributions)

##I can run a KS PME calculation now as I have all the right attributes to be able to run the calculation per fund and we can
##start to summarize and start to visualize the data and start to create some takeaways

ks_pme_results <- wsib_fund_activity_quarterly_sp500_value %>%
  group_by(assigned_unid) %>%
  mutate(sp500_cumulative_contributions = cumsum(sp500_quarterly_contributions),
         sp500_cumulative_distributions = cumsum(sp500_quarterly_distributions)) %>%
  filter(report_date == "2021-06-30") %>%
  summarize(sp500_ks_pme = (sp500_cumulative_distributions + current_market_value) / sp500_cumulative_contributions)

##KS PME works well to create natural groupings as the calculation revolves around the value of 1. Above 1 means outperformance
##of the underlying fund as compared to the sp500, conversely, below 1 implies that the fund underperformed compared to the 
##sp500 and you would have generated greater value committing those funds to the index over that same timeframe. Also, I'm goign
##to add back some of the static fund attributes to include as part of the analysis. I'm creating a new attribute which is called
##vintage. It's a common way to create peers as you group funds based on the intial year investments began. 

unique(wsib_fund_activity$strategy)
buyout <- c("Corporate Finance/Buyout - Small", "Corporate Finance/Buyout - Mid", "Corporate Finance/Buyout - Large", "Corporate Finance/Buyout - Mega")
venture <- c("Growth Equity", "Venture Capital")
strategy_levels <- c("Venture Capital", "Growth Equity", "Corporate Finance/Buyout - Small", "Corporate Finance/Buyout - Mid", "Corporate Finance/Buyout - Large", "Corporate Finance/Buyout - Mega", "Distressed Debt", "Special Situations", "Co-Investment", "Mezzanine")

wsib_fund_vintage <- wsib_fund_activity %>%
  group_by(assigned_unid) %>%
  filter(report_date == min(as.integer(report_date))) %>%
  transmute(assigned_unid, vintage = year(report_date))

wsib_fund_activity_added_attributes <- wsib_fund_activity %>%
  merge(wsib_fund_vintage, by = "assigned_unid") %>%
  select(report_date, assigned_unid, fund_name, strategy, vintage, tvpi, irr, commitment, contributions, distributions, current_market_value) %>%
  group_by(assigned_unid) %>%
  filter(report_date == "2021-06-30") %>%
  transmute(assigned_unid, fund_name, strategy, vintage, tvpi, irr, current_percent_called = contributions / commitment, 
            current_dpi = distributions / contributions, current_rvpi = current_market_value / contributions, 
            current_tvpi_calculated = (distributions + current_market_value) / contributions)

key_metrics_results_analysis <- merge(wsib_fund_activity_added_attributes, ks_pme_results, by = "assigned_unid") %>% 
  transmute(assigned_unid, fund_name, strategy, grouped_strategy = ifelse(strategy %in% buyout, "buyout", 
                                                                          ifelse(strategy %in% venture, "venture","other")),
            vintage, tvpi, irr, current_percent_called, current_dpi, current_rvpi, 
            current_tvpi_calculated, sp500_ks_pme, outperformance = sp500_ks_pme > 1) 

key_metrics_results_analysis$grouped_strategy <- factor(key_metrics_results_analysis$grouped_strategy, levels = c("venture", "buyout", "other"))

##Alright things are pretty well set up to run some analysis and compare WSIB's private portfolio against a common public index
##at times I make use of funds in their first couple of years, but largely we want to exclude them from most of our analysis. 
##They should be treated as a curiosity as these large investments are designed to be long term vehicles (traditionally 10 to 12 years)
##and there are a lot of fees loaded upfront that acts as a drag on the investment until the pitched value add kicks in during later
##maturing years of the investment.There aren't any clear cut rules, but generally you wouldn't start to evaluate a fund's performance
##until it has had three years of investment history. I'll be a little more generous to WSIB and exclude 2018 and later vintages.

library(ggplot2)
key_metrics_results_analysis_mature <- key_metrics_results_analysis %>%
  filter(vintage < 2016)

wsib_fund_activity %>%
     mutate(net_cashflow = distributions - contributions, vintage = as.factor(year(initial_investment_date))) %>%
     filter(!vintage %in% c("2021", "2020", "2019")) %>%
     ggplot(aes(report_date, net_cashflow, color = vintage)) +
     geom_smooth(se = FALSE) +
     scale_y_continuous(labels = scales::label_dollar(prefix = "$")) +
     labs(title = "Demonstration of J-Curve Effect", x = "End of Quarter Date", y = "Net Cashflow") +
     theme(plot.title = element_text(hjust = 0.5))

ks_pme_results_analysis_mature %>%
  count(outperformance) %>%
  mutate(pct_of_total = n / sum(n))

key_metrics_results_analysis_mature %>%
  group_by(grouped_strategy) %>%
  summarize(mean_pme = mean(sp500_ks_pme), sd_pme = sd(sp500_ks_pme), median_pme = median(sp500_ks_pme), 
            mean_tvpi = mean(current_tvpi_calculated), sd_tvpi = sd(current_tvpi_calculated), median_tvpi = median(current_tvpi_calculated))

key_metrics_results_analysis_mature %>%
  arrange(desc(current_tvpi_calculated, sp500_ks_pme)) %>%
  select(fund_name, strategy, vintage, current_dpi, current_rvpi, current_tvpi_calculated, sp500_ks_pme) %>%
  top_n(10)

key_metrics_results_analysis_mature %>%
  group_by(strategy) %>%
  summarise(total_count = n()) %>%
  mutate(percent_strategy = total_count / sum(total_count)*100) %>%
  arrange(desc(percent_strategy))

key_metrics_results_analysis_mature %>%
  group_by(grouped_strategy) %>%
  summarise(total_count = n()) %>%
  ggplot(aes(x = 1, y = total_count, fill = grouped_strategy)) +
  geom_col() +
  coord_polar(theta = "y") +
  labs(title = "Proportions of Grouped Strategy in Mature Analysis Set", fill = "Grouped Strategy") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_void()

##Come back to this and demonstrate this same concept with a commitment weight being put on to show
##diversification by dollar amounts. 
key_metrics_results_analysis_mature %>%
  mutate(strategy = factor(strategy, levels = c("Venture Capital", "Growth Equity", "Corporate Finance/Buyout - Small", 
                                      "Corporate Finance/Buyout - Mid", "Corporate Finance/Buyout - Large", 
                                      "Corporate Finance/Buyout - Mega", "Distressed Debt", "Special Situations", 
                                      "Co-Investment"))) %>%
  group_by(strategy, vintage) %>%
  summarise(total_count = n()) %>%
  mutate(percent_strategy = total_count / sum(total_count)*100) %>%
  ggplot(aes(x = as.factor(vintage), y = percent_strategy, fill = strategy)) +
  geom_col(position = "fill") +
  labs(title = "Proportions of Strategy in Mature Analysis Set Over Vintage Years", x = "Vintage Year", y = "Percent of Total", fill = "Strategy") +
  theme(plot.title = element_text(hjust = 0.5))
  

key_metrics_results_analysis_mature %>%
  arrange(desc(current_dpi)) %>%
  mutate(fund_name = forcats::as_factor(fund_name)) %>%
  ggplot( aes(y = fund_name, x = current_dpi, fill = as.factor(vintage))) +
  geom_col() +
  facet_grid(rows = vars(grouped_strategy), scales = "free_y", space= "free_y") +
  labs(title = "Distributions to Paid In Performance by Vintage and Grouped Strategy", x = "Distributions to Paid In", y = "Fund Name") +
  theme(plot.title = element_text(hjust = 0.5))

key_metrics_results_analysis_mature %>%
  arrange(desc(tvpi)) %>%
  mutate(fund_name = forcats::as_factor(fund_name)) %>%
  ggplot(aes(y = fund_name, x = current_tvpi_calculated, fill = as.factor(vintage))) +
  geom_col() +
  facet_grid(rows = vars(grouped_strategy), scales = "free_y", space= "free_y") +
  labs(title = "Total Value to Paid In Performance by Vintage and Grouped Strategy", x = "Total Value to Paid In", y = "Fund Name") +
  theme(plot.title = element_text(hjust = 0.5))

key_metrics_results_analysis_mature %>%
  arrange(desc(sp500_ks_pme)) %>%
  mutate(fund_name = forcats::as_factor(fund_name)) %>%
  ggplot(aes(y = fund_name, x = sp500_ks_pme)) +
  geom_col(fill = "dark blue") +
  facet_grid(rows = vars(grouped_strategy), scales = "free_y", space= "free_y") +
  labs(title = "SP500 KS PME Performance Comparison by Vintage and Grouped Strategy", subtitle = "KS PME Value of >1 Implies Over-Performance",
       x = "SP500 KS PME", y = "Fund Name", col = "Vintage") +
  theme(plot.title = element_text(hjust = 0.5))

wsib_fund_activity %>%
  mutate(net_cashflow = distributions - contributions, vintage = as.factor(year(initial_investment_date)), 
         grouped_strategy = ifelse(strategy %in% buyout, "Buyout", 
                                   ifelse(strategy %in% venture, "Venture","Other"))) %>%                                                                                                                             
  filter(!vintage %in% c("2021", "2020", "2019", "2018") & !is.na(vintage)) %>%
ggplot(aes(report_date, net_benefit, color = grouped_strategy)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::label_dollar(prefix = "$")) +
  facet_wrap(vars(as.factor(vintage))) +
  labs(title = "Grouped Strategies Net Benefit Over Time by Vintage", x = "End of Quarter Date", 
       y = "Net Benefit", col = "Grouped Strategy") +
  theme(plot.title = element_text(hjust = 0.5))

##and fin