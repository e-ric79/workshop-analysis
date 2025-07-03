library(tidyverse)
library(readxl)
workshop <- read_excel("workshop.xlsx")
head(workshop)
str(workshop)
summary(workshop)
colSums(is.na(workshop))
calaculated_revenue <- workshop |>
  mutate(
    total_revenue_calculated = `Units Sold`* `Unit Price`,
    `Total Revenue`= ifelse(is.na(`Total Revenue`), total_revenue_calculated, `Total Revenue`)
  )
region_revenue <- workshop |>
  group_by(Region) |>
  summarise(`Total Revenue`=sum(`Total Revenue`,na.rm=TRUE))|>
  arrange(desc(`Total Revenue`))
average_feedback <- workshop |>
  group_by(Salesperson) |>
  summarise(average_feedback=mean(`Customer Feedback Score`,na.rm=TRUE)) |>
  arrange(desc(average_feedback))
top_selling_product <- workshop |>
  group_by(Product) |>
  summarise(`Units Sold`=sum(`Units Sold`,na.rm=TRUE)) |>
  arrange(desc(`Units Sold`))
ggplot(workshop,aes(x=Region,y=`Total Revenue`,fill=Region))+
  geom_bar(stat = "summary", fun = sum)+
  labs(title="total revenue by region",
       x="region",
       y="revenue(ksh)")+
  theme_minimal()
ggplot(workshop,aes(x=Product,y=`Unit Price`,fil=Product))+
  geom_boxplot()+
  labs(title="unit price by product")+
  theme_minimal()
average_product_price <- workshop |>
  group_by(Product) |>
  summarise(average_product_price=mean(`Unit Price`))|>
  arrange(desc(average_product_price))
daily_revenue <- workshop |>
  group_by(Date) |>
  summarise(daily_revenue=sum(`Total Revenue`,na.rm=TRUE))|>
  ggplot(aes(x=Date,y=daily_revenue))+
  geom_line(color="steel blue")+
  labs(title = "revenue over time",
       x="date",
       y="total revenue")+
  theme_minimal()
library(caret) 
model <-lm(`Total Revenue`~`Units Sold`+`Unit Price`,data=workshop)
summary(model)