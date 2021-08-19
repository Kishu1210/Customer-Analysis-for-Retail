d1.df = read.csv("C:\\Users\\Kanishka\\Documents\\BA360\\R case study 1 (Retail)\\Customer.csv", header = TRUE)
d2.df = read.csv("C:\\Users\\Kanishka\\Documents\\BA360\\R case study 1 (Retail)\\Transactions.csv", header = TRUE)
d3.df = read.csv("C:\\Users\\Kanishka\\Documents\\BA360\\R case study 1 (Retail)\\prod_cat_info.csv", header = TRUE)

library(dplyr)
library(lubridate)
library(ggplot2)

colnames(d1.df) [colnames(d1.df)=="customer_Id"] <- "cust_id"
colnames(d3.df) [colnames(d3.df)=="prod_sub_cat_code"] <- "prod_subcat_code"

# Q1.a using merge function
f1.df <- merge(x = d2.df,y = d3.df,by=c("prod_cat_code","prod_subcat_code"),all.x = TRUE)
Customer_final.df <- merge(x=f1.df,y=d1.df,by="cust_id",all.x = TRUE) 

#Q1.b using left_join in dplyr
f2.df <- left_join(d2.df,d3.df,by=c("prod_cat_code","prod_subcat_code"))
Customer_final_1.df <- left_join(f2.df,d1.df,by="cust_id")

df <- Customer_final_1.df
df$tran_date <- dmy(df$tran_date)
df$DOB <- dmy(df$DOB)

max_date <- max(df$tran_date)
min_date <- min(df$tran_date)

mydate1 <- as.Date("2014-01-01")
mydate2 <- as.Date("2014-03-01")

df$Age = round(as.numeric(difftime(max_date, df$DOB, units = "weeks"))/52.25) #calculating Age

df$cust_id <- as.factor(df$cust_id)
df$transaction_id <- as.factor(df$transaction_id)
df$city_code <- as.factor(df$city_code)
df$prod_subcat <- as.factor(df$prod_subcat)
df$Gender <- as.factor(df$Gender)
df$prod_cat <- as.factor(df$prod_cat)
df$Store_type <- as.factor(df$Store_type)

#Q2.a Datatypes
str(df)

#Q2.b Top 10 rows
df[1:10,]

#Q2.c Summary Of variables Total amount and Quantity
summarise(df, Median_qty = median(Qty, na.rm = T))
summarise(df, Median_amt = median(total_amt, na.rm = F))
summarise(df, Min_amt = min(total_amt, na.rm = T))     
summarise(df, Min_qty = min(Qty, na.rm = T))
summarise(df, Max_amt = max(total_amt, na.rm = T))
summarise(df, Max_qty = max(Qty, na.rm = T))
Q2c <- select(df,Qty,total_amt)
quantile(Q2c$Qty)
quantile(Q2c$total_amt)

#Q2.d Frequency table for categorical variables 
table(df$Store_type)
table(df$Gender)
table(df$prod_cat)
table(df$prod_subcat)
table(df$city_code)

#Q3.
hist(df$Qty)
hist(df$Rate)
hist(df$Tax)
hist(df$total_amt)

ggplot(df) + geom_bar(aes(x=Store_type))
ggplot(df) + geom_bar(aes(x=Gender))
ggplot(df) + geom_bar(aes(x=prod_cat))
ggplot(df) + geom_bar(aes(x=prod_subcat))
ggplot(df) + geom_bar(aes(x=city_code))

#Q4. Time Range
cat(round(as.numeric(difftime(max_date,min_date,units="weeks"))/52.25), "years")
cat(round(as.numeric(difftime(max_date,min_date,units="days"))/(365.25/12)), "months")
cat(difftime(max_date,min_date), "days")

#Q4. Count of -ve Transactions
Q4 <- df %>% group_by(transaction_id) %>% summarise(Amt=sum(total_amt))
Q4.1 <- Q4 %>% select(transaction_id,Amt) %>% filter(Amt < 0)  
count(Q4.1)

#Q5.
Q5<-df %>% select(Gender,prod_cat,Qty)
Q5a <- Q5 %>% group_by(Gender,prod_cat)  %>% summarise(Total=sum(Qty))
Q5a %>% select(Gender,prod_cat,Total) %>% filter(Total==max(Total))

#Q6.
Q6 <- df %>% group_by(city_code) %>% summarise(count=n())
Q6a <- mutate(Q6, percentage = count/sum(count)*100)
Q6a %>% select(city_code,count,percentage) %>% filter(count==max(count))

#Q7.
Q7 <- df %>% group_by(Store_type) %>% summarise(Total_qty=sum(Qty),Total_revenue=sum(total_amt))
Q7 %>% select(Store_type,Total_qty,Total_revenue) %>% filter(Total_qty==max(Total_qty) & Total_revenue==max(Total_revenue))

#Q8.
Q8 <- df %>% filter(Store_type=="Flagship store" & (prod_cat %in% c("Electronics","Clothing")))
Q8 %>% group_by(prod_cat) %>% summarise(Total_revenue=sum(total_amt))

#Q9.
Q9 <- df %>% filter(Gender=="M" & prod_cat=="Electronics")
sum(Q9$total_amt)

#Q10.
Q10 <- df %>% select(cust_id,transaction_id,total_amt) %>% filter(total_amt > 0)
Q10a <- Q10 %>% group_by(cust_id,transaction_id) %>% summarise(count=n())
Q10b <- Q10a %>% group_by(cust_id) %>% summarise(count=n_distinct(transaction_id)) %>% filter(count > 10)
cat("There are",nrow(Q10b),"customers")

#Q11
Q11 <- df %>% select(cust_id,prod_cat,prod_subcat,Age,tran_date,total_amt) %>% filter(prod_cat %in% c("Electronics","Books"))
Q11a <- Q11 %>% filter(Age > 25 & Age < 35)
Q11a %>% group_by(prod_cat) %>% summarise(Net=sum(total_amt))

Q11b <- Q11a %>% filter(tran_date>mydate1 & tran_date<mydate2)
Q11b %>% group_by(prod_cat) %>% summarise(Net=sum(total_amt))
