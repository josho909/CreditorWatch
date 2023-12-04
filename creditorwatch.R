# CreditorWatch case study

# Import Libraries
library(tidyverse)

# Import data as df
dtypes <- cols(
  customerID = col_character(),
  date = col_date(format = '%d/%m/%Y'),
  itemDescription = col_factor(),
  amount = col_double()
)
df <- read_csv("./xyz-billing.csv", col_types = dtypes)

# Store last date of imported data
latest_date <- max(df$date)

# Cross join to get records associated with each relevant month for annual subscriptions
annual <- df[df$itemDescription == "annual subscription",] %>% 
  cross_join(data.frame(rank = seq(0,12))) %>% 
  mutate(date = add_with_rollback(date,months(rank)),
         amount = amount/12,
         itemDescription = "Monthly Recurring Revenue") %>% 
  filter(rank != 0) %>% 
  select(customerID,date,itemDescription,amount)

# Merge df with disaggregated annual subscriptions and create mrr and churn flag columns
all <- rbind(df,annual) %>% 
  mutate(mrr = ifelse(itemDescription %in% c("Monthly Recurring Revenue","monthly subscription"),amount,NA),
         churn_flag = 0)

# Create churn row for all customers who cease subscriptions
churn <- all %>% 
  filter(mrr > 0) %>% 
  arrange(customerID,desc(date)) %>% 
  group_by(customerID) %>% 
  slice_head() %>% 
  filter(date < latest_date) %>% 
  mutate(date = ceiling_date(add_with_rollback(date,months(1)),"months")-1,
         churn_flag = 1)

# Merge all with churn rows
out <- rbind(all,churn)

# Write to csv
write_csv(out,"C:/Users/josho/Downloads/xyz-billion_w_mrr.csv", na = '')