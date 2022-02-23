library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stringr)
library(ggplot2)
library(jrvFinance)

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
wsib_fund_activity$distributions <- replace_na(wsib_fund_activity$distributions, 0)

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
         quarterly_distributions = distributions - lag(distributions)) %>%
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


##Direct Alpha takes two steps to complete, only because the last value in the cash flow array needs to be amended to include
##any remaining value. 

wsib_fund_activity_quarterly_sp500_filtered_cashflow <- wsib_fund_activity_quarterly_sp500_value %>%
  transmute(assigned_unid, series_count, quarterly_contributions, quarterly_distributions, current_market_value, 
            quarterly_net_cashflow = quarterly_distributions - quarterly_contributions, sp500_quarterly_net_cashflow) %>%
  group_by(assigned_unid) %>%
  filter(series_count != max(series_count))

da_pme_results <- wsib_fund_activity_quarterly_sp500_filtered_cashflow %>%
  transmute(assigned_unid, series_count, current_market_value, 
            quarterly_net_cashflow = quarterly_distributions - quarterly_contributions, 
            sp500_quarterly_net_cashflow) %>%
  group_by(assigned_unid) %>%
  filter(series_count == max(series_count)) %>%
  transmute(assigned_unid, series_count, quarterly_net_cashflow = quarterly_net_cashflow + current_market_value, 
            sp500_quarterly_net_cashflow = sp500_quarterly_net_cashflow + current_market_value) %>%
  rbind(wsib_fund_activity_quarterly_sp500_filtered_cashflow) %>%
  arrange(assigned_unid, series_count) %>%
  merge(wsib_fund_activity_quarterly, by = c("assigned_unid", "series_count")) %>%
  filter(report_date <= as.Date(report_date)) %>%
  arrange(assigned_unid, series_count) %>%
  group_by(assigned_unid) %>%
  mutate(alpha_calc_sp500 = round(irr(sp500_quarterly_net_cashflow) * 4, digits = 4)) %>%
  distinct(assigned_unid, alpha_calc_sp500)


##Combining all of the results into our activity set, but filtered to the most recent report date as all of the pme values are
##in reference to Q2 2021. 

public_market_comparisons <- wsib_fund_activity %>%
  arrange(assigned_unid, report_date) %>%
  group_by(assigned_unid) %>%
  mutate(series_count = 1:n()) %>%
  filter(report_date == "2021-06-30" & year(initial_investment_date) <= 2018) %>%
  merge(ks_pme_results, by = "assigned_unid") %>%
  merge(da_pme_results, by = "assigned_unid") %>%
  select(assigned_unid, fund_name, strategy, series_count, tvpi, irr, sp500_ks_pme, alpha_calc_sp500)

public_market_comparisons %>%
  mutate(grouped_strategy = ifelse(strategy %in% buyout, "buyout", 
                                   ifelse(strategy %in% venture, "venture","other"))) %>%
  ggplot(aes(grouped_strategy, sp500_ks_pme)) +
  geom_boxplot(color = "dodgerblue4") +
  geom_jitter(color = "forestgreen", alpha = 0.4) +
  labs(title = "Boxplot Analysis of KS PME Comparison", subtitle = "Grouped Strategies with 2018 and Earlier Vintage Funds",
       x = "Grouped Strategy", y = "KS PME") +
  chosen_theme()


##########################         I am stuck here at the moment trying to create a full portfolio level PME analysis

wsib_fund_portfolio_beginning <- wsib_fund_activity %>%
  filter(report_date == min(report_date)) %>%
  transmute(report_date, total_contributions = sum(contributions), total_distributions = sum(distributions), 
            total_market_value = sum(current_market_value), 
            total_quarterly_contributions = total_contributions,
            total_quarterly_distributions = total_distributions,
            total_quarterly_net_cashflow = total_quarterly_distributions - total_quarterly_contributions) %>%
  distinct(.)

wsib_fund_portfolio_ending <- wsib_fund_activity %>%
  group_by(report_date) %>%
  transmute(report_date, total_contributions = sum(contributions), total_distributions = sum(distributions), 
            total_market_value = sum(current_market_value)) %>%
  ungroup() %>%
  distinct(.) %>%
  mutate(total_quarterly_contributions = total_contributions - lead(total_contributions),
         total_quarterly_distributions = total_distributions - lead(total_distributions),
         total_quarterly_net_cashflow = total_quarterly_distributions - total_quarterly_contributions) %>%
  filter(report_date == max(report_date)) %>%
  mutate(total_quarterly_net_cashflow = total_quarterly_net_cashflow + total_market_value)
  
  
wsib_fund_portfolio_filtered <- wsib_fund_activity %>%
  group_by(report_date) %>%
  transmute(report_date, total_contributions = sum(contributions), total_distributions = sum(distributions), 
            total_market_value = sum(current_market_value)) %>%
  distinct(.) %>%
  ungroup() %>%
  mutate(total_quarterly_contributions = total_contributions - lead(total_contributions),
         total_quarterly_distributions = total_distributions - lead(total_distributions),
         total_quarterly_net_cashflow = total_quarterly_distributions - total_quarterly_contributions) %>%
  filter(report_date != min(report_date)) %>%
  rbind(wsib_fund_portfolio_beginning) %>%
  filter(report_date != max(report_date)) %>%
  rbind(wsib_fund_portfolio_ending) %>%
  arrange(desc(report_date)) %>%
  merge(sp500, by.x = "report_date", by.y = "normalized_eoq_date") %>%
  mutate(sp500_growth_ratio = sp500_current / sp500_value, 
         sp500_quarterly_contributions = sp500_growth_ratio * total_quarterly_contributions,
         sp500_quarterly_distributions = sp500_growth_ratio * total_quarterly_distributions,
         sp500_quarterly_net_cashflow = sp500_quarterly_distributions - sp500_quarterly_contributions)

ks_pme_portfolio_results <- wsib_fund_portfolio_filtered %>%
  mutate(sp500_cumulative_contributions = cumsum(sp500_quarterly_contributions),
         sp500_cumulative_distributions = cumsum(sp500_quarterly_distributions)) %>%
  filter(report_date == "2021-06-30") %>%
  summarize(sp500_ks_pme = (sp500_cumulative_distributions + total_market_value) / sp500_cumulative_contributions)

wsib_fund_activity_quarterly_sp500_filtered_cashflow <- wsib_fund_activity_quarterly_sp500_value %>%
  transmute(assigned_unid, series_count, quarterly_contributions, quarterly_distributions, current_market_value, 
            quarterly_net_cashflow = quarterly_distributions - quarterly_contributions, sp500_quarterly_net_cashflow) %>%
  group_by(assigned_unid) %>%
  filter(series_count != max(series_count))

  


##KS PME works well to create natural groupings as the calculation revolves around the value of 1. Above 1 means outperformance
##of the underlying fund as compared to the sp500, conversely, below 1 implies that the fund underperformed compared to the 
##sp500 and you would have generated greater value committing those funds to the index over that same timeframe. Also, I'm goign
##to add back some of the static fund attributes to include as part of the analysis. I'm creating a new attribute which is called
##vintage. It's a common way to create peers as you group funds based on the initial year investments began. 

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

wsib_fund_activity %>%
  arrange(report_date) %>%
  group_by(assigned_unid) %>%
  mutate(vintage = year(initial_investment_date), series_count = 1:n()) %>%
  filter(fund_name == "Oaktree Opportunities Fund VIII") %>%
  arrange(desc(series_count)) %>%
  ungroup() %>%
  select(report_date, fund_name, initial_investment_date, vintage, strategy, series_count)
  distinct(fund_name) %>% 
  mutate(strategy = factor(strategy, levels = c("Venture Capital", "Growth Equity", "Corporate Finance/Buyout - Small", 
                                                "Corporate Finance/Buyout - Mid", "Corporate Finance/Buyout - Large", 
                                                "Corporate Finance/Buyout - Mega", "Co-Investment", "Distressed Debt", 
                                                "Mezzanine", "Special Situation"))) %>%
  arrange(strategy)



chosen_theme <-   function() {
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, family = "Optima"),
        plot.subtitle = element_text(hjust = 0.5, family = "Optima"),
        panel.background = element_rect(fill = "gray90"),
        axis.title = element_text(family = "Optima"))
}


wsib_fund_activity %>%
  group_by(report_date) %>%
  summarize(total_net_benefit = sum(net_benefit)) %>%
  ggplot(aes(report_date, total_net_benefit)) +
  geom_line(color = "dodgerblue4") +
  scale_y_continuous(labels = scales::label_dollar(prefix = "$")) +
  labs(title = "Total Net Benefit to WSIB Over Time", x = "Quarterly Date", y = "Total Net Benefit") +
  chosen_theme()
  

wsib_fund_activity %>%
  group_by(report_date) %>%
  select(report_date, assigned_unid, fund_name, contributions, total_value) %>%
  ggplot(aes(report_date, contributions)) +
  geom_col(color = "forestgreen") +
  geom_col(aes(report_date, total_value), fill = "dodgerblue4", alpha = 0.9) +
  scale_y_continuous(labels = scales::label_dollar(prefix = "$")) +
  labs(title = "Summed Contributions Compared to Summed Total Value Over Time", 
       subtitle = "Summed Total Value (Blue) versus Summed Contributions (Green) by Quarter Date", 
       x = "Quarterly Date", y = "$ Amount") +
  chosen_theme()

wsib_fund_activity %>%
  mutate(tvpi_calc = total_value / contributions, dpi = distributions / contributions,
         grouped_strategy = ifelse(strategy %in% buyout, "Buyout", 
                                   ifelse(strategy %in% venture, "Venture","Other"))) %>%
  filter(report_date == max(report_date)) %>%
  group_by(grouped_strategy) %>%
  summarize(mean_dpi = mean(dpi), median_dpi = median(dpi), sd_dpi = sd(dpi), summed_distributions = sum(distributions),
            mean_tvpi = mean(tvpi_calc), median_tvpi = median(tvpi_calc), sd_tvpi = sd(tvpi_calc), summed_total_value = sum(total_value))

key_metrics_results_analysis_mature <- key_metrics_results_analysis %>%
  filter(vintage < 2016)

wsib_fund_activity %>%
     mutate(net_cashflow = distributions - contributions, vintage = as.factor(year(initial_investment_date))) %>%
     filter(!vintage %in% c("2021", "2020", "2019", "2018", "2017")) %>%
     ggplot(aes(report_date, net_cashflow, color = vintage)) +
     geom_smooth(se = FALSE) +
     scale_y_continuous(labels = scales::label_dollar(prefix = "$")) +
     labs(title = "Demonstration of J-Curve Effect by Vintage Year", x = "Quarterly Date", y = "Net Cashflow", color = "Vintage") +
     theme(plot.title = element_text(hjust = 0.5, family = "Optima"),
        plot.subtitle = element_text(hjust = 0.5, family = "Optima"),
        panel.background = element_rect(fill = "gray90"),
        axis.title = element_text(family = "Optima"))

wsib_fund_activity %>%
  filter(report_date == max(report_date)) %>%
  mutate(tvpi_calc = total_value / contributions, 
         grouped_strategy = ifelse(strategy %in% buyout, "Buyout", 
            ifelse(strategy %in% venture, "Venture","Other"))) %>%
  ggplot(aes(grouped_strategy, tvpi_calc)) +
  geom_boxplot(color = "dodgerblue4") +
  geom_jitter(color = "forestgreen", alpha = 0.4) +
  labs(title = "Boxplot Analysis of Current TVPI Performance", subtitle = "Grouped Strategies with All Funds Plotted",
       x = "Grouped Strategy", y = "Calculated TVPI") +
  chosen_theme()

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

wsib_fund_activity %>%
  filter(report_date == max(report_date)) %>%
  mutate(tvpi_calc = total_value / contributions, 
         grouped_strategy = ifelse(strategy %in% buyout, "Buyout", 
                                   ifelse(strategy %in% venture, "Venture","Other"))) %>%
  group_by(grouped_strategy) %>%
  summarise(total_count = n()) %>%
  ggplot(aes(x = 1, y = total_count, fill = grouped_strategy)) +
  geom_col() +
  geom_text(aes(label = paste(round(total_count / sum(total_count) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 8) +
  coord_polar(theta = "y") +
  labs(title = "Proportions of Grouped Strategy in Total Portfolio", fill = "Grouped Strategy") +
  scale_fill_manual(values = c("dodgerblue4","forestgreen", "snow3")) +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, family = "Optima"),
        plot.subtitle = element_text(hjust = 0.5, family = "Optima"))

wsib_fund_activity %>%
  group_by(assigned_unid) %>%
  filter(report_date == min(report_date)) %>%
  mutate(vintage = year(initial_investment_date), 
         strategy = factor(strategy, levels = c("Venture Capital", "Growth Equity", "Corporate Finance/Buyout - Small", 
                                                "Corporate Finance/Buyout - Mid", "Corporate Finance/Buyout - Large", 
                                                "Corporate Finance/Buyout - Mega", "Co-Investment", "Distressed Debt", 
                                                "Mezzanine", "Special Situation"))) %>%
  group_by(vintage) %>%
  mutate(total_vintage_commitment = sum(commitment)) %>%
  ggplot(aes(year(initial_investment_date), total_vintage_commitment, fill = strategy)) +
  geom_col() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "$")) +
  scale_fill_manual(values = c("snow3", "snow2", "dodgerblue4", "dodgerblue3", "dodgerblue2", 
                               "dodgerblue1", "forestgreen", "seagreen4", "seagreen3", "seagreen2")) +
  labs(title = "Total Commitment Made by Vintage Year", subtitle = "Broken Down by Strategy", 
       x = "Year", y = "Total Commitment", fill = "Strategy") +
  chosen_theme()

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

wsib_fund_activity %>%
  mutate(vintage = year(initial_investment_date)) %>%
  filter(report_date == max(report_date)) %>%
  group_by(vintage) %>%
  summarize(total_commitment = sum(commitment)) %>%
  ggplot(aes(vintage, total_commitment)) +
  geom_col(fill = "dodgerblue4") +
  scale_y_continuous(labels = scales::label_dollar(prefix = "$")) +
  labs(title = "Total Commitments Made by WSIB per Year", x = "Year", y = "Total Commitment") +
  chosen_theme

wsib_fund_activity %>%
  mutate(vintage = year(initial_investment_date)) %>%
  filter(vintage <= 2010 & report_date == max(report_date)) %>%
  summarize(fund_name, vintage, percent_called = round(contributions / commitment, 2), dpi = round(distributions / contributions, 2),
            rvpi = round(current_market_value / contributions, 2), tvpi = round((distributions + current_market_value) / contributions, 2), strategy) %>%
  arrange(desc(rvpi)) %>%
  mutate(breakeven = dpi >= 1, expected_breakeven = tvpi >= 1) %>%
  summarize(breakeven_fraction = sum(breakeven) / n(), expected_breakeven_fraction = sum(expected_breakeven) / n())

buyout <- c("Corporate Finance/Buyout - Small", "Corporate Finance/Buyout - Mid", "Corporate Finance/Buyout - Large", "Corporate Finance/Buyout - Mega")

wsib_fund_activity %>%
  mutate(tvpi_calc = total_value / contributions, rvpi = current_market_value / contributions, vintage = year(initial_investment_date)) %>%
  filter(strategy %in% buyout & report_date == "2021-06-30") %>%
  mutate(strategy = forcats::fct_relevel(strategy, buyout)) %>%
  ggplot(aes(y = strategy, x = tvpi_calc)) +
  geom_violin(color = "dodgerblue4") +
  geom_jitter(aes(y = strategy, x = tvpi_calc), color = "forestgreen", height = 0.2, alpha = 0.7) +
  labs(title = "Violin Plot of Buyout Grouped Strategy", subtitle = "Underlying Point Plot of Individual Funds to Illustrate",
       x = "TVPI", y = "Strategy") +
  chosen_theme()

wsib_fund_activity %>%
  mutate(tvpi_calc = total_value / contributions, rvpi = current_market_value / contributions, vintage = year(initial_investment_date)) %>%
  filter(strategy %in% buyout & report_date == "2021-06-30") %>%
  mutate(strategy = forcats::fct_relevel(strategy, buyout)) %>%
  top_n(1, tvpi_calc) %>%
  select(fund_name, vintage, tvpi, strategy)

wsib_fund_activity %>%
  arrange(assigned_unid, report_date) %>%
  group_by(assigned_unid) %>%
  mutate(series_count = 1:n()) %>%
  filter(series_count >= 7) %>%
  mutate(q_over_q_growth_ratio = (total_value / contributions) / (lag(total_value) / lag(contributions)),
         validation = tvpi / lag(tvpi)) %>%
  select(report_date, series_count, assigned_unid, contributions, total_value, tvpi, q_over_q_growth_ratio, validation) %>%
  filter(q_over_q_growth_ratio != Inf & q_over_q_growth_ratio == max(q_over_q_growth_ratio, na.rm = TRUE)) %>%
  ggplot(aes(report_date)) +
  geom_histogram(fill = "dodgerblue4", color = "forestgreen") +
  labs(title = "Frequency of Highest Q over Q Growth", subtitle = "Examining the Timing of Highest Quarter over Quarter Value Creation",
       x = "Quarterly Date", y = "No. of Funds") +
  chosen_theme()
 
public_market_comparisons %>%
  filter(strategy %in% buyout) %>%
  group_by(strategy) %>%
  summarize(strategy, mean_ks_pme = round(mean(sp500_ks_pme),2), sd_ks_pme = round(sd(sp500_ks_pme), 2), 
            mean_direct_alpha = round(mean(alpha_calc_sp500), 2), sd_direct_alpha = round(sd(alpha_calc_sp500), 2), counts = n()) %>%
  distinct(.) %>%
  arrange(desc(mean_direct_alpha))
  ggplot(aes(y = strategy, x = alpha_calc_sp500 * 100)) +
  geom_violin(color = "dodgerblue4") +
  geom_jitter(color = "forestgreen", alpha = 0.4, height = 0.2) +
  labs(title = "Violin Analysis of Direct Alpha PME Comparison", subtitle = "Buyout Substrategies with 2018 and Earlier Vintage Funds",
       x = "Direct Alpha Percent", y = "Strategy") +
  chosen_theme()
  
  
top_performers <- wsib_fund_activity %>%
  filter(report_date == "2021-06-30") %>%
  mutate(tvpi_calc = total_value / contributions, vintage = year(initial_investment_date), 
         grouped_strategy = ifelse(strategy %in% buyout, "buyout", 
                            ifelse(strategy %in% venture, "venture","other"))) %>%
  group_by(grouped_strategy) %>%
  mutate(max_tvpi = max(tvpi_calc)) %>%
  filter(max_tvpi == tvpi_calc) %>%
  select(assigned_unid, fund_name, vintage, strategy, max_tvpi) %>%
  arrange(desc(max_tvpi))

bottom_performers <- wsib_fund_activity %>%
  filter(report_date == "2021-06-30" & year(initial_investment_date) < 2021) %>%
  mutate(tvpi_calc = total_value / contributions, vintage = year(initial_investment_date), 
         grouped_strategy = ifelse(strategy %in% buyout, "buyout", 
                                   ifelse(strategy %in% venture, "venture","other"))) %>%
  group_by(grouped_strategy) %>%
  mutate(min_tvpi = min(tvpi_calc)) %>%
  filter(min_tvpi == tvpi_calc) %>%
  select(assigned_unid, fund_name, vintage, strategy, min_tvpi) %>%
  arrange(desc(min_tvpi))

wsib_fund_activity %>%
  filter(assigned_unid %in% c(top_performers$assigned_unid, bottom_performers$assigned_unid)) %>%
  group_by(assigned_unid) %>%
  arrange(assigned_unid, report_date) %>%
  mutate(series_count = 1:n()) %>%
  ggplot(aes(series_count, total_value, group = assigned_unid, color = strategy)) +
  geom_line() +
  geom_line(aes(series_count, distributions, group = assigned_unid, linetype = strategy))

wsib_fund_activity %>%
  filter(assigned_unid %in% c(top_performers$assigned_unid)) %>%
  group_by(assigned_unid) %>%
  arrange(assigned_unid, report_date) %>%
  mutate(series_count = 1:n(), grouped_strategy = ifelse(strategy %in% buyout, "buyout", 
                                                         ifelse(strategy %in% venture, "venture","other"))) %>%
  ggplot(aes(series_count, total_value, group = assigned_unid, color = grouped_strategy)) +
  geom_line() +
  geom_line(aes(series_count, distributions, group = assigned_unid), linetype = "dashed", size = 1) +
  scale_y_continuous(labels = scales::label_dollar(prefix = "$")) +
  scale_color_manual(values = c("dodgerblue4","forestgreen", "snow4")) +
  labs(title = "Total Value and Distributions Over Top Performing Funds", subtitle = "The Top 3 Grouped Strategy Funds in Absolute Terms",
       x = "Series Count", y = "$ Total/Realized Value", color = "Grouped Strategy") +
  chosen_theme()


wsib_fund_activity %>%
  filter(report_date == "2021-06-30" & year(initial_investment_date) < 2021) %>%
  mutate(tvpi_calc = total_value / contributions, vintage = year(initial_investment_date), 
         grouped_strategy = ifelse(strategy %in% buyout, "buyout", 
                                   ifelse(strategy %in% venture, "venture","other"))) %>%
  filter(grouped_strategy == "other") %>%
  group_by(strategy) %>%
  mutate(min_tvpi = min(tvpi_calc)) %>%
  filter(min_tvpi == tvpi_calc) %>%
  transmute(grouped_strategy, fund_name, vintage, strategy, tvpi = round(min_tvpi, 2)) %>%
  arrange(desc(tvpi))

top_alpha <- public_market_comparisons %>%
  filter(strategy %in% buyout) %>%
  top_n(3, alpha_calc_sp500) %>%
  select(assigned_unid, fund_name, strategy, tvpi, irr, alpha_calc_sp500)

wsib_fund_activity %>%
  group_by(assigned_unid) %>%
  filter(assigned_unid %in% top_alpha$assigned_unid) %>%
  arrange(assigned_unid, report_date) %>%
  mutate(series_count = 1:n(), vintage = year(initial_investment_date)) %>%
  ggplot(aes(series_count, total_value, color = fund_name)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_dollar(prefix = "$")) +
  scale_color_manual(values = c("dodgerblue4","forestgreen", "snow4")) +
  labs(title = "Total Value and Distributions Over Top Performing Direct Alpha Funds", subtitle = "The Top 3 Buyout Funds in Absolute Terms",
       x = "Series Count", y = "$ Total Value", color = "Fund Name") +
  chosen_theme()