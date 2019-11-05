library(tidyverse)
library(readxl)
library(lubridate)
library(scales)
library(gridExtra)



# excel files must be in the working directory
setwd("~/Desktop/MEF/DataVisualization/Nov5")
wd <- getwd()
wd

col_names <- c("brand_name","auto_dom","auto_imp","auto_total","comm_dom",
               "comm_imp","comm_total","total_dom","total_imp","total_total")

# Import all monthly sales data from 2016 to 2019
# Excel files must fit the indicated pattern
# make sure file ends in .xlsx and not XLSX or anything else

file.list <- list.files(pattern='201[0-9].ODD.monthly.[0-1][0-9].xlsx')

df.list <- lapply(file.list, read_excel, skip = 7, col_names = col_names)
names(df.list) <- file.list


# add a column year and month for each data frame in df.list

year <- 2016
counter <- 1

for (i in 1:length(df.list)) {
  
  if (counter > 12) {
    counter <- 1
    year <- year + 1
  }
  
  df.list[[i]] <- df.list[[i]] %>% mutate(date = ymd(paste(year,counter,28, sep = "-")),
                                          month = month(date),
                                          year = year(date),
                                          quarterly = paste(year,"Q",quarter(date),sep = ""))
  counter <- counter + 1
}

## Merge & Clean-up data
ODD_all <- bind_rows(df.list)

ODD_all <- ODD_all %>% filter(brand_name != "TOPLAM:" & brand_name != "TOPLAM" & brand_name!="")

## From stackoverflow, another approach to use replace_na
## myList <- setNames(lapply(vector("list", ncol(ODD_all)), function(x) x <- 0), names(ODD_all))
## ODD_all %>% replace_na(myList)

ODD_all[is.na(ODD_all)] <- 0

ODD_all <- filter(ODD_all,!grepl("ODD", brand_name, fixed = TRUE))

ODD_all <- ODD_all %>% mutate(brand_name=replace(brand_name, brand_name 
                                                 %in% c("ASTON MART?N","ASTON MARTİN"),"ASTON MARTIN"))


## Analysis


ODD_top5_quarterly_sale_Q1 <- ODD_all %>%
  filter(quarterly %in% c("2016Q1","2017Q1","2018Q1","2019Q1")) %>%
  group_by(quarterly) %>%
  top_n(wt = total_total, n = 5) %>%
  select(brand_name,total_total,year,quarterly)

Q1 <- ggplot(ODD_top5_quarterly_sale_Q1, mapping = aes(quarterly, total_total,fill = brand_name)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma)+
  labs(x ="Quarter", y ="Total Sales")+
  labs(title = "Q1 Comparison 2016 - 2019",subtitle = "Top 5 Brands", x ="Quarter", y ="Total Sales", caption="(based on data from ODD)")+
  guides(fill=guide_legend(title="Brand Name"))

ODD_top5_quarterly_sale_Q2 <- ODD_all %>%
  filter(quarterly %in% c("2016Q2","2017Q2","2018Q2","2019Q2")) %>%
  group_by(quarterly) %>%
  top_n(wt = total_total, n = 5) %>%
  select(brand_name,total_total,year,quarterly)

Q2 <- ggplot(ODD_top5_quarterly_sale_Q2, mapping = aes(quarterly, total_total,fill = brand_name)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma)+
  labs(x ="Quarter", y ="Total Sales")+
  labs(title = "Q2 Comparison 2016 - 2019",subtitle = "Top 5 Brands", x ="Quarter", y ="Total Sales", caption="(based on data from ODD)")+
  guides(fill=guide_legend(title="Brand Name"))

ODD_top5_quarterly_sale_Q3 <- ODD_all %>%
  filter(quarterly %in% c("2016Q3","2017Q3","2018Q3","2019Q3")) %>%
  group_by(quarterly) %>%
  top_n(wt = total_total, n = 5) %>%
  select(brand_name,total_total,year,quarterly)
  
Q3 <- ggplot(ODD_top5_quarterly_sale_Q3, mapping = aes(quarterly, total_total,fill = brand_name)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma)+
  labs(x ="Quarter", y ="Total Sales")+
  labs(title = "Q3 Comparison 2016 - 2019",subtitle = "Top 5 Brands", x ="Quarter", y ="Total Sales", caption="(based on data from ODD)")+
  guides(fill=guide_legend(title="Brand Name"))

ODD_top5_quarterly_sale_Q4 <- ODD_all %>%
  filter(quarterly %in% c("2016Q4","2017Q4","2018Q4","2019Q4")) %>%
  group_by(quarterly) %>%
  top_n(wt = total_total, n = 5) %>%
  select(brand_name,total_total,year,quarterly)

Q4 <- ggplot(ODD_top5_quarterly_sale_Q4, mapping = aes(quarterly, total_total,fill = brand_name)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = comma)+
  labs(title = "Q4 Comparison 2016 - 2018",subtitle = "Top 5 Brands", x ="Quarter", y ="Total Sales", caption="(based on data from ODD)")+
  guides(fill=guide_legend(title="Brand Name"))


grid.arrange(Q1,Q2,Q3,Q4, layout_matrix = rbind(c(1,1,2,2),c(3,3,4,4)))


#Analysis 2

ODD_auto_first8 <- ODD_all %>% group_by(brand_name) %>%
  summarize(sum_auto=sum(auto_total)) %>% arrange(desc(sum_auto)) %>% slice(1:8)

ggplot(ODD_auto_first8,aes(x=reorder(brand_name,sum_auto), y=sum_auto)) + geom_bar(stat="identity") +  coord_flip() +
  theme(axis.text.x = element_text(face="bold", color="#993333", size=10), axis.text.y = element_text(face="bold", color="#993333", size=10)) +
  labs(title = "ODD Auto", subtitle = "First 8 brand for auto", caption="(based on data from ODD)", y="Total Auto", x="Brand Name") +
  scale_y_continuous(labels=comma) 

#Analysis 3

pl_df<- ODD_all%>% group_by(month,year) %>% summarise(Total=sum(total_total),Domestic=sum(total_dom),Import=sum(total_imp)) %>% arrange(year)
ggplot(data = pl_df) + geom_smooth(aes(x=year, y=Total, color = "Total")) + geom_smooth(aes(x=year, y=Import, color = "Import")) + geom_smooth(aes(x=year, y=Domestic, color = "Domestic")) + guides(fill=guide_legend(title="Type"))



