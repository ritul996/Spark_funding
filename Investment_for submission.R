# Load relevant libraries
library(tidyr)
library(dplyr)
library(stringr)
library(gdata)

#Checkpoint 1 - Data cleaning

# Import companies text file
companies <- read.delim("companies.txt")

#Import rounds2 csv file
rounds2 <- read.csv("rounds2.csv")

#View both DFs and their properties
View(rounds2)
str(rounds2)
View(companies)
str(companies)

#Convert the company permalink unique identifier to lower case for simplfying further operations
rounds2$company_permalink <- str_to_lower(rounds2$company_permalink, locale = "en")

#Convert the permalink unique identifier to lower case for simplfying further operations
companies$permalink <- str_to_lower(companies$permalink, locale = "en")

#Check if the operation has worked properly
View(rounds2)
View(companies)

#How many unique companies in rounds2 df, store value in uniquecompaniesround2
uniquecompaniesrounds2 <- length(unique(rounds2$company_permalink))
uniquecompaniesrounds2

#How many unique companies in companies df, store value in uniquecompanies
uniquecompanies <- length(unique(companies$permalink))
uniquecompanies

#Total companies in rounds2 not present in companies df. The not in will give us a logical vectore with all companies from rounds 2 not in companies df and the sum will give the total number of TRUE
sum(!(rounds2$company_permalink %in% companies$permalink))

#Merge companies df & rounds2 df into master_frame df and check the DF
master_frame <- merge(rounds2,companies,by.x = "company_permalink", by.y = "permalink")
View(master_frame)

#Checkpoint 1 - Funding Type Analysis

#Group by funding_round_type into a new DF called master_frame_fundtype and view results
master_frame_fundtype <- group_by(master_frame, funding_round_type)
View(master_frame_fundtype)

#Get averages by funding type and store it in a df named fundingbytype containing venture, angel, seed and private equity round types
fundingbytype <- summarise(master_frame_fundtype,mean(raised_amount_usd, na.rm = TRUE)) # Summary of all by type
fundingbytype <- filter(fundingbytype, funding_round_type == "seed" | funding_round_type == "venture" | funding_round_type == "angel" | funding_round_type == "private_equity") # Filter the 4 we need
colnames(fundingbytype) <- c("funding_round_type","avg_raised_amt_usd")
View(fundingbytype)

#Checkpoint 3 - Country Analysis

#Arrive at Top 9 countries by investment value for round type - venture
#Step 1 - Filter venture round type & group by country to be able to summarise by total investment amount
venture_bycountry <- group_by(filter(master_frame, funding_round_type == "venture"),country_code) #Filter only venture as that's what we are interested in
View(venture_bycountry)

#Step 2 - Get summary by country by amount raised
fundingbycountry <- summarise(venture_bycountry,sum(raised_amount_usd, na.rm = TRUE))

#Define column names for table
colnames(fundingbycountry) <- c("country_code","amount_raised_usd")
View(fundingbycountry)

#Step 3 - Exclude row where the country name is blank
countryfunding <- fundingbycountry[fundingbycountry$country_code != "",]

#Step 4 - Create top9 df with top 9 countries sorted by investment (use df above, which excludes blank country code) 
top9 <- head(countryfunding[order(-countryfunding$amount_raised_usd),],9)
View(top9)

#Checkppoint 4 - Sector Analysis 1

#Creating the mapping of primary sector & main sectors for all companies in master_frame
#Step 1 - Create a new column (primary_sector) in master_frame df which only considers the primary sector before the pipe symbol in the category_list column
master_frame$primary_sector <- str_split_fixed(master_frame$category_list,"\\|",n=2) [,1]
View(master_frame)

#Step 2 - Import sector mapping file
mapping <- read.csv("mapping.csv")
View(mapping)


#Step 3 - Create long format table with category and main sector, eliminate rows with zero value and delete the value column and blank sector
sectormapping <- gather(mapping,main_sector,value,2:10)
sectormapping <- sectormapping[!(sectormapping$value == 0),] # Remove zeroes
sectormapping <- sectormapping[,-3] #remove value column
colnames(sectormapping) <- c("primary_sector","main_sector") #assign column names to the df
sectormapping <- sectormapping[!(sectormapping$primary_sector == ""),] #Remove blanks in primary sector column
View(sectormapping)

#Step 4- Checking and fixing data quality issues in the primary sector column before proceeding
sum(master_frame$primary_sector == "") #3410 blanks
master_frame$primary_sector[master_frame$primary_sector == ""] <- NA #Replace blanks with NA
View(master_frame)

#Step 5 - Map the main sector to the primary sector using the mapping table
sectormapping$primary_sector <- str_replace_all(sectormapping$primary_sector,"0","na") #Text "na" seems to be replaced by "0" so replace it back with "na"
sectormapping$primary_sector <- str_replace_all(sectormapping$primary_sector,"\\.na",".0") #Fix issues caused by above step in certain cases where there were supposed to be zeroes

#Step 6 - Standardise data and map main sector in sectormapping df to primary_sector in master_frame df 
master_frame$primary_sector <- str_to_lower(master_frame$primary_sector, locale = "en") #convert field to lower case
sectormapping$primary_sector <- str_to_lower(sectormapping$primary_sector, locale = "en") #convert field to lower case
master_frame_sector <- merge(master_frame,sectormapping,by.x = "primary_sector", by.y = "primary_sector",all.x = TRUE)
View(master_frame_sector)
str(master_frame_sector)
write.csv(master_frame_sector,"sector_frame.csv", row.names = F)


#Checkpoint 5 - Sector Analysis 2

# Creating three separate DFs D1, D2 & D3 for each of the top 3 English speaking countries containing the funding type between 5 & 15 million USD 

#Step 1 - Create data frames for funding type venture and the three countries
D1 <- filter(master_frame_sector,master_frame_sector$country_code == "USA" & master_frame_sector$funding_round_type == "venture" & master_frame_sector$raised_amount_usd >= 5000000 & master_frame_sector$raised_amount_usd <= 15000000)
D2 <- filter(master_frame_sector,master_frame_sector$country_code == "GBR" & master_frame_sector$funding_round_type == "venture" & master_frame_sector$raised_amount_usd >= 5000000 & master_frame_sector$raised_amount_usd <= 15000000)
D3 <- filter(master_frame_sector,master_frame_sector$country_code == "IND" & master_frame_sector$funding_round_type == "venture" & master_frame_sector$raised_amount_usd >= 5000000 & master_frame_sector$raised_amount_usd <= 15000000)

#Step 2 - Get count of investments and total amount invested by main sector
D1_sectorcount <- aggregate(D1$funding_round_permalink, by = list(D1$main_sector), FUN = length) #aggregate by main sector to get count of investments
D2_sectorcount <- aggregate(D2$funding_round_permalink, by = list(D2$main_sector), FUN = length) #aggregate by main sector to get count of investments
D3_sectorcount <- aggregate(D3$funding_round_permalink, by = list(D3$main_sector), FUN = length) #aggregate by main sector to get count of investments
colnames(D1_sectorcount) <- c("main_sector","no_of_investments") #rename columns of aggregated tables
colnames(D2_sectorcount) <- c("main_sector","no_of_investments") #rename columns of aggregated tables
colnames(D3_sectorcount) <- c("main_sector","no_of_investments") #rename columns of aggregated tables

D1_sectorsum <- aggregate(D1$raised_amount_usd, by = list(D1$main_sector), FUN = sum, na.rm = TRUE) #aggregate by main sector to get sum of investments
D2_sectorsum <- aggregate(D2$raised_amount_usd, by = list(D2$main_sector), FUN = sum, na.rm = TRUE) #aggregate by main sector to get sum of investments
D3_sectorsum <- aggregate(D3$raised_amount_usd, by = list(D3$main_sector), FUN = sum, na.rm = TRUE) #aggregate by main sector to get sum of investments
colnames(D1_sectorsum) <- c("main_sector","sum_of_investments") #rename columns of aggregated tables
colnames(D2_sectorsum) <- c("main_sector","sum_of_investments") #rename columns of aggregated tables
colnames(D3_sectorsum) <- c("main_sector","sum_of_investments") #rename columns of aggregated tables


#Step 3 - Add the results from above back into D1, D2 & D3 and verify results
D1 <- merge(D1,D1_sectorcount,by = "main_sector", all.x = TRUE)
D1 <- merge(D1,D1_sectorsum,by = "main_sector", all.x = TRUE)
D2 <- merge(D2,D2_sectorcount,by = "main_sector", all.x = TRUE)
D2 <- merge(D2,D2_sectorsum,by = "main_sector", all.x = TRUE)
D3 <- merge(D3,D3_sectorcount,by = "main_sector", all.x = TRUE)
D3 <- merge(D3,D3_sectorsum,by = "main_sector", all.x = TRUE)
View(D1)
View(D2)
View(D3)

# To capture data in table 5.1
# Total number of investments (count)
summarise(D1,length(D1$raised_amount_usd))
summarise(D2,length(D2$raised_amount_usd))
summarise(D3,length(D3$raised_amount_usd))

#Amount of investment
summarise(D1,sum(D1$raised_amount_usd,na.rm = TRUE))
summarise(D2,sum(D2$raised_amount_usd,na.rm = TRUE))
summarise(D3,sum(D3$raised_amount_usd,na.rm = TRUE))

# Top 3 sectors based on count of investments & the no of investments in each
D1_top3_count <- top_n(D1_sectorcount,3,D1_sectorcount$no_of_investments)
arrange(D1_top3_count,desc(D1_top3_count$no_of_investments))
D2_top3_count <- top_n(D2_sectorcount,3,D2_sectorcount$no_of_investments)
arrange(D2_top3_count,desc(D2_top3_count$no_of_investments))
D3_top3_count <- top_n(D3_sectorcount,3,D3_sectorcount$no_of_investments)
arrange(D3_top3_count,desc(D3_top3_count$no_of_investments))

# Company with highest investment value in top sector across the three countries
D1_sector_top1<-filter(D1,D1$main_sector=="Others")
D1_sector_top1_amt<-aggregate(D1_sector_top1$raised_amount_usd,by=list(D1_sector_top1$company_permalink),FUN=sum, na.rm=T)
top_n(D1_sector_top1_amt,1,x)

D2_sector_top1<-filter(D2,D2$main_sector=="Others")
D2_sector_top1_amt<-aggregate(D2_sector_top1$raised_amount_usd,by=list(D2_sector_top1$company_permalink),FUN=sum, na.rm=T)
top_n(D2_sector_top1_amt,1,x)

D3_sector_top1<-filter(D3,D3$main_sector=="Others")
D3_sector_top1_amt<-aggregate(D3_sector_top1$raised_amount_usd,by=list(D3_sector_top1$company_permalink),FUN=sum, na.rm=T)
top_n(D3_sector_top1_amt,1,x)

# Company with highest investment value in second highest sector across the three countries
D1_sector_top2<-filter(D1,D1$main_sector=="Social..Finance..Analytics..Advertising")
D1_sector_top2_amt<-aggregate(D1_sector_top2$raised_amount_usd,by=list(D1_sector_top2$company_permalink),FUN=sum, na.rm=T)
top_n(D1_sector_top2_amt,1,x)

D2_sector_top2<-filter(D2,D2$main_sector=="Social..Finance..Analytics..Advertising")
D2_sector_top2_amt<-aggregate(D2_sector_top2$raised_amount_usd,by=list(D2_sector_top2$company_permalink),FUN=sum, na.rm=T)
top_n(D2_sector_top2_amt,1,x)

D3_sector_top2<-filter(D3,D3$main_sector=="Social..Finance..Analytics..Advertising")
D3_sector_top2_amt<-aggregate(D3_sector_top2$raised_amount_usd,by=list(D3_sector_top2$company_permalink),FUN=sum, na.rm=T)
top_n(D3_sector_top2_amt,1,x)

