# First execute Libraries.R
# for tidyverse, other libraries, and self-made utility functions

# Yellen's labor market dashboard
# note: https://www.bloomberg.com/graphics/2015-yellens-labor-market-dashboard/
# symbols in FRED
yellen_labor_mkt_symbols <- c(
  "UNRATE", # Unemployment rate
  "U6RATE", # U-6, underemployment rate
  "LNS13025703", # Long-term unemployed share
  "PAYEMS", # Non-farm payrolls, total
  "JTSJOR", # Job openings rate
  "JTSLDR", # Layoffs/discharged rate
  "JTSQUR", # Quits rate
  "JTSHIR", # Hires rate
  "CEU0500000003", # Average hourly earnings, total private
  "ECIALLCIV", # Employment cost index, all civilians, quaterly
  "CIVPART", # Labor force participation rate
  "PCEPILFE" # PCE excluding food and energy
  ) 

# get data from this date
START = "2006-01-01"

# set xlim for plot
XLIM <- c(as.Date("2008-01-01"), as.Date("2019-12-01"))

# get data from FRED
labor_mkt_m <- yellen_labor_mkt_symbols %>% 
  tidyquant::tq_get(get = "economic.data", from = START)

# transform Non-farm payrolls, total to differences, 3 month moving average
payems <- labor_mkt_m %>% 
  filter(symbol == "PAYEMS") %>% 
  tq_diff() %>% 
  tq_ma(n = 3)

# transform PCE to growth rates, YoY
pce <- labor_mkt_m %>% 
  filter(symbol == "PCEPILFE") %>% 
  tq_gr(n = 12)

# transform Average hourly earnings to YoY, 3 ma
ceu <- labor_mkt_m %>% 
  filter(symbol == "CEU0500000003") %>% 
  tq_gr(n = 12) %>% 
  tq_ma(n = 3)

# transform Employment cost index, quarterly data to YoY
eci <- labor_mkt_m %>% 
  filter(symbol == "ECIALLCIV") %>% 
  tq_gr(n = 4)

# Atlanta Fed Wage Trucker
# source: https://www.frbatlanta.org/chcs/wage-growth-tracker.aspx
wage_tracker <- read_csv("data/wage-growth-data.csv", na = c(".", ""),
                         col_types = cols(X1 = col_date(format = "%Y/%m/%d"),
                                          `Unweighted Overall` = col_double()))
names(wage_tracker) <- c("date", "price")
wage_tracker$symbol <- "WAGETR"
wage_tracker <- wage_tracker %>% 
  filter(date >= START)

# combine transformed data
labor_mkt <- labor_mkt_m %>% 
  filter(!(symbol %in% c("PAYEMS", "PCEPILFE", "CEU0500000003", "ECIALLCIV"))) %>% 
  bind_rows(payems) %>% 
  bind_rows(pce) %>% 
  bind_rows(ceu) %>% 
  bind_rows(eci) %>% 
  bind_rows(wage_tracker)

# symbol from chr to fctr
labor_mkt$symbol <- factor(labor_mkt$symbol,
                           levels = c("UNRATE", "LNS13025703", "U6RATE", "CIVPART",
                                      "JTSJOR", "JTSLDR", "JTSQUR", "JTSHIR",
                                      "PAYEMS", "CEU0500000003", "ECIALLCIV", "WAGETR",
                                      "PCEPILFE"))

# recode symbol
labor_mkt <- labor_mkt %>% 
  mutate(
         symbol = fct_recode(symbol,
                             "Unemployment rate" = "UNRATE",
                             "Long-term unemployed share" = "LNS13025703",
                             "U-6 underemployment rate" = "U6RATE",
                             "Labor force participation rate" = "CIVPART",
                             "Job openings rate" = "JTSJOR",
                             "Layoffs/discharged rate" = "JTSLDR",
                             "Quits rate" = "JTSQUR",
                             "Hires rate" = "JTSHIR",
                             "Non-farm payroll increase, 3 mo avg" = "PAYEMS",
                             "Avg hourly earnings, YoY, 3 mo avg" = "CEU0500000003",
                             "Employment cost index, YoY, quarterly" = "ECIALLCIV",
                             "Atlanta Fed wage tracker, YoY, 3 mo avg" = "WAGETR",
                             "PCE excluding food and energy, YoY" = "PCEPILFE"
                             )
           )

# prepare for plot month
Sys.setlocale(category = "LC_TIME", locale = "C")

# plot with label only on the latest data
labor_mkt %>% 
  group_by(symbol) %>% 
  mutate(
    label = if_else(date == max(date), 
                    paste(lubridate::month(date, label = TRUE, abbr = TRUE),
                          as.character(round(price, 2))
                          , sep = " "),
                    NA_character_)
  ) %>% 
  ungroup() %>% 
  ggplot(aes(x = date, y = price)) + 
  geom_line() +
  facet_wrap(~ symbol, ncol = 4, scales = "free_y") +
  geom_text(aes(label = label), na.rm = TRUE,
            hjust = 1, vjust = 0) +
  coord_cartesian(xlim = XLIM) +
  labs(
    title = "Yellen's US labor market dashboard",
    x = "",
    y = "")

# save pdf in (nearly) A4 format wide
ggsave(filename = "output/Yellen.pdf",
       width = 10, height = 8, units = "in", dpi = 300)
