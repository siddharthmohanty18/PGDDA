####################  Investment Case Group Project 1 ####################
#
# Group Members:
# Abhijeet Verma
# Siddharth Mohanty
# Yogesh Deo
# Swaroop Venigalla
#
# Loading of file companies.txt into data frame
#
companies <- read.delim("companies.txt", header=T, sep="\t", stringsAsFactors = F)
#
# Loading of file rounds2.csv into data frame
#
rounds2 <- read.csv("rounds2.csv", header=T, stringsAsFactors = F)
#
#
################# Checkpoint 1: Data Cleaning 1 ##################
#
#
# Let's check the number of rows in the companies file.
#
nrow(companies)
#
# As per the metadata shared, permalink is the unique identifier for each company.
# 
# Let's check number of unique permalinks in the companies data frame
#
unique.permalink.companies <- length(unique(toupper(trimws(companies$permalink, which = c("both")))))
unique.permalink.companies
#
# The number of records in companies data frame is same as the unique permalinks.
#
# Hence, indeed permalink is the unique identifier for each record in companies data frame.
#
# Let's find out the number of unique company names in the companies data frame.
#
unique.name.companies <- length(unique(companies$name))
unique.name.companies
#
# As the number of unique companies is less than number of unique permalinks, this shows for one company name there are multiple permalinks.
#
# Its clearly mentioned that permalink refers to a unique id for a company.
#
# Hence, we conclude that permalink is an unique identifier for each record in companies.txt and refers to a unique company.
# There are multiple records with different permalink for same company name if they are serving in different sectors.
# There are multiple records with different permalink for same company name if the status is different 
# 
# Let's merge the companies and rounds2 data frame after normalizing the permalink column
#
# Convert the permalink of companies to upper case and remove white spaces
#
companies$permalink <- toupper(trimws(companies$permalink, c("b")))
#
# Convert the company_permalink of rounds2 to upper case and remove white spaces
#
rounds2$company_permalink <- toupper(trimws(rounds2$company_permalink,c("b")))
#
# Lets use the permalink column in companies and company_permalink for merging of two data frames
#
master_frame <- merge(companies,rounds2, by.x = "permalink", by.y = "company_permalink", all = F)
#
# Num of rows in master_frame
#
nrow(master_frame)
#
# Num of rows in rounds2
nrow(rounds2)
#
# After natural join as number of records in master_frame is same as rounds2, ideally all records in rounds2 has a company associated with it in companies
#
# Table 1.1)Q1)How many unique companies are present in rounds2?	
#
length(unique(rounds2$company_permalink))
#
# Table 1.1)Q2) How many unique companies are present in the companies file?
#
length(unique(companies$permalink))
#
# Table 1.1)Q3) In the companies data frame, which column can be used as the unique key for each company? Write the name of the column.
#
# Companies name have duplicate values so unique.permalink.companies > unique.name.companies. Hence, permalink is chosen as the unique key. 
#
# Table 1.1)Q4) Are there any companies in the rounds2 file which are not present in companies ? Answer Y/N.
#
# After performing a natural join of companies and rounds2 as the number of rows in master_frame is same as that in round2 we can say that there are no companies in rounds 2 which are not present in companies file.
# Hence, the answer is N
#
# Using subset we can verify the same. Below command retrieves those 
#
rounds2_permalink_not_in_companies <- subset(rounds2, !(rounds2$company_permalink %in% companies$permalink))
#
nrow(rounds2_permalink_not_in_companies)
#
# As the size of rounds2_permalink_not_in_companies is 0 we can say that all companies in rounds2 are present in companies
#
# Table 1.1)Q5) Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.Name the merged frame master_frame.How many observations are present in master_frame ?
#
nrow(master_frame)
#
#
################# Checkpoint 2: Data Cleaning 2 ##################
#
#
# Table 2.1)Q1) How many NA values are present in the column raised_amount_usd??
#
sum(is.na(master_frame$raised_amount_usd))
#
# Here as the NA values are 17% of the total data its not advisable to ignore these records
#
# selecting the records with raised_amount_usd not equal to NA.
#
master_frame_amnt_not_na <- subset(master_frame, is.na(master_frame$raised_amount_usd)==F)
#
# We will be checking the distribution of the raised_amount_usd field whether it is normal or skewed.
# 
# Let us plot a histogram with bin size as 1lak.
#
library(ggplot2)
#
ggplot(master_frame_amnt_not_na, aes(master_frame_amnt_not_na$raised_amount_usd)) + geom_histogram(binwidth = 100000) + ylim(0, 5000) + xlim(0,25000000)
#
# The plot is skewed towards the right. So, the ideal central value for this distribution is a median
# 
# The median value of the raised_amount_usd is computed below:
#
median.raised.amount.usd <- median(master_frame_amnt_not_na$raised_amount_usd)
#
# Let's replace all the NA values with the median value
#
master_frame$raised_amount_usd[which(is.na(master_frame$raised_amount_usd) == T)] <- median.raised.amount.usd
#
#
################# Checkpoint 3: Funding Type Analysis ##################
#
#
# Finding average of raised_amount_usd based on the investment type
#
mean_funding_round_type <- aggregate(master_frame$raised_amount_usd, by = list(master_frame$funding_round_type), FUN = "mean")
#
# Lets rename the column names where first col is Funding_Type
#
names(mean_funding_round_type) <- c("Funding_Type","Average")
#
# Table 3.1)Q1) Average funding amount of venture type
#
mean_funding_round_type$Average[which(mean_funding_round_type$Funding_Type == "venture")]
#
# Table 3.1)Q2) Average funding amount of angel type
#
mean_funding_round_type$Average[which(mean_funding_round_type$Funding_Type == "angel")]
#
# Table 3.1)Q3) Average funding amount of seed type
#
mean_funding_round_type$Average[which(mean_funding_round_type$Funding_Type == "seed")]
#
# Table 3.1)Q4) Average funding amount of private equity type
#
mean_funding_round_type$Average[which(mean_funding_round_type$Funding_Type == "private_equity")]
#
# Retrieving all the funding type whose average investment value lies between 5 million to 15 million
#
ideal_investment_types <- subset(mean_funding_round_type, mean_funding_round_type$Average>=5000000 & mean_funding_round_type$Average<=15000000)
#
# Table 3.1)Q5) Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, which investment type is the most suitable for them?
#
preferred.ideal.investment.type <- ideal_investment_types$Funding_Type[which(ideal_investment_types$Funding_Type == "venture" | ideal_investment_types$Funding_Type == "angel" | ideal_investment_types$Funding_Type == "seed" | ideal_investment_types$Funding_Type == "private_equity")]
#
preferred.ideal.investment.type
#
#
################# Checkpoint 4: Country Analysis ##################
#
#
# For country level analysis there are missing value in country code field
#
nrow(subset(master_frame, master_frame$country_code == ""))
#
# As the country_code is a nominal attribute ideally missing values are replaced by mode.
#
# Records with non missing country code
#
master_frame_country_not_na <- subset(master_frame, master_frame$country_code != "")
#
# Function to calculate mode 
#
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#
# Mode of country code field is 
#
mode.country <- getmode(master_frame_country_not_na$country_code)
#
mode.country
#
# Replacing the missing country code with mode.country
#
master_frame$country_code[which(master_frame$country_code == "")] <- mode.country
#
# Filtering out all the records with investment type as venture
#
venture_master_frame <- subset(master_frame, master_frame$funding_round_type == "venture")
#
# Aggregating venture.master.frame based on country code
#
venture_master_frame_countrywise_investment <- aggregate(venture_master_frame$raised_amount_usd, by = list(venture_master_frame$country_code), FUN = "sum")
#
# Renaming the column names to appropriate names
#
names(venture_master_frame_countrywise_investment) <- c("Country_Code", "Total_Invested_Amnt")
#
# Ordering of the records in venture.master.frame.country in descending order of Total_Invested_Amnt
#
venture_master_frame_countrywise_investment_desc <- venture_master_frame_countrywise_investment[order(venture_master_frame_countrywise_investment$Total_Invested_Amnt, decreasing = T),]
#
# Fetching top 9 records from venture.master.frame.country.desc to top9 data frame
#
top9 <- venture_master_frame_countrywise_investment_desc[1:9,]
top9
#
# In the country list provided United Kingdom is considered as an English speaking country.
# GBR : Great Britain is a subset of UK, so GBR is also considered as an English speaking country.
#
#
################# Checkpoint 5: Sector Analysis1 ##################
#
# checking for the blank values in category_list of master_frame
#
nrow(subset(master_frame, master_frame$category_list == ""))
#
# There are 3410 missing values in the category_list
# 
# As category_list in a categorical variable hence we replace the blank values by mode.
#
# First extracting the values that are not blank
#
master_frame_category_not_na <- subset(master_frame, master_frame$category_list != "")
#
# Finding the mode 
#
mode.category <- getmode(master_frame_category_not_na$category_list)
mode.category
#
# Replacing the blank values in category_list of master_file with mode
#
master_frame$category_list[which(master_frame$category_list == "")] <- mode.category
#
# Checking the solution
#
nrow(subset(master_frame, master_frame$category_list == ""))
#
# This shows that all the blank values have been replaced.
#
# Extracting the primary category from the category list column in master_frame.
#
primary_category_vector <- sapply(strsplit(master_frame$category_list,"|", fixed = T),"[",1)
#
# Adding the primary_category column to master_frame
#
master_frame$primary_category <- primary_category_vector
#
# Converting the primary category upper case and removing leading or trailing spaces
#
master_frame$primary_category <- toupper(trimws(master_frame$primary_category, which = c("both")))
#
# Reading the mapping.csv file to mapping data frame
#
mapping <- read.csv("mapping_file.csv", stringsAsFactors = F)
#
# Converting the category_list column in mapping to upper case and removing white spaces
#
mapping$category_list <- toupper(trimws(mapping$category_list, which = c("both")))
#
# Merging the master_frame with mapping for getting main sector of each company
#
master_frame_main_category <- merge(master_frame, mapping, by.x = "primary_category", by.y = "category_list", all.x = T)
#
# Checking for NA values in main_sector
#
sum(is.na(master_frame_main_category$main_sector))
#
# As main_sector is a categorical variable should replace the NA values by mode of that field
#
mode.main.sector <- getmode(master_frame_main_category$main_sector)
mode.main.sector
#
# Replace NA values with the mode value
#
master_frame_main_category$main_sector[which(is.na(master_frame_main_category$main_sector) == T)] <- mode.main.sector
#
sum(is.na(master_frame_main_category$main_sector))
#
# As the number of NA values is 0, we have successfully removed the NA values
#
#
################# Checkpoint 6: Sector Analysis 2 ##################
#
#
# Create three separate data frames D1, D2 and D3 for each of the 3 countries containing the observations of funding type FT falling between 5 to 15 million USD range. 
#
# Creation of data frame D1 for USA
#
D1_NO_AGGREGATE <- subset(master_frame_main_category, master_frame_main_category$funding_round_type == "venture" & master_frame_main_category$country_code == "USA" & master_frame_main_category$raised_amount_usd >= 5000000 & master_frame_main_category$raised_amount_usd <= 15000000)
#
# Creation of data frame D2 for GBR
#
D2_NO_AGGREGATE <- subset(master_frame_main_category, master_frame_main_category$funding_round_type == "venture" & master_frame_main_category$country_code == "GBR" & master_frame_main_category$raised_amount_usd >= 5000000 & master_frame_main_category$raised_amount_usd <= 15000000)
#
# Creation of data frame D3 for GBR
#
D3_NO_AGGREGATE <- subset(master_frame_main_category, master_frame_main_category$funding_round_type == "venture" & master_frame_main_category$country_code == "IND" & master_frame_main_category$raised_amount_usd >= 5000000 & master_frame_main_category$raised_amount_usd <= 15000000)
#
# Total count of main sector in USA
#
USA_MAIN_SECTOR_CNT <- data.frame(table(D1_NO_AGGREGATE$main_sector))
names(USA_MAIN_SECTOR_CNT) <- c("main_category","count")
#
# Total sum of amount for each main sector in USA
#
USA_MAIN_SECTOR_AMNT <- aggregate(D1_NO_AGGREGATE$raised_amount_usd, by = list(D1_NO_AGGREGATE$main_sector), FUN = "sum")
names(USA_MAIN_SECTOR_AMNT) <- c("main_category", "total_invest_amnt")
#
# Merge USA_MAIN_SECTOR_CNT and USA_MAIN_SECTOR_AMNT to a single table
#
USA_MAIN_SECTOR_AGGREGATE <- merge(USA_MAIN_SECTOR_CNT,USA_MAIN_SECTOR_AMNT, by.x = "main_category", by.y = "main_category", all = F)
#
# Formation of D1 data frame
#
D1 <- merge(D1_NO_AGGREGATE, USA_MAIN_SECTOR_AGGREGATE, by.x = "main_sector", by.y = "main_category", all = F)
#
# Total count of main sector in GBR
#
GBR_MAIN_SECTOR_CNT <- data.frame(table(D2_NO_AGGREGATE$main_sector))
names(GBR_MAIN_SECTOR_CNT) <- c("main_category","count")
#
# Total sum of amount for each main sector in GBR
#
GBR_MAIN_SECTOR_AMNT <- aggregate(D2_NO_AGGREGATE$raised_amount_usd, by = list(D2_NO_AGGREGATE$main_sector), FUN = "sum")
names(GBR_MAIN_SECTOR_AMNT) <- c("main_category", "total_invest_amnt")
#
# Merge GBR_MAIN_SECTOR_CNT and GBR_MAIN_SECTOR_AMNT to a single table
#
GBR_MAIN_SECTOR_AGGREGATE <- merge(GBR_MAIN_SECTOR_CNT,GBR_MAIN_SECTOR_AMNT, by.x = "main_category", by.y = "main_category", all = F)
#
# Formation of D2 data frame
#
D2 <- merge(D2_NO_AGGREGATE, GBR_MAIN_SECTOR_AGGREGATE, by.x = "main_sector", by.y = "main_category", all = F)
#
# Total count of main sector in IND
#
IND_MAIN_SECTOR_CNT <- data.frame(table(D3_NO_AGGREGATE$main_sector))
names(IND_MAIN_SECTOR_CNT) <- c("main_category","count")
#
# Total sum of amount for each main sector in IND
#
IND_MAIN_SECTOR_AMNT <- aggregate(D3_NO_AGGREGATE$raised_amount_usd, by = list(D3_NO_AGGREGATE$main_sector), FUN = "sum")
names(IND_MAIN_SECTOR_AMNT) <- c("main_category", "total_invest_amnt")
#
# Merge IND_MAIN_SECTOR_CNT and IND_MAIN_SECTOR_AMNT to a single table
#
IND_MAIN_SECTOR_AGGREGATE <- merge(IND_MAIN_SECTOR_CNT,IND_MAIN_SECTOR_AMNT, by.x = "main_category", by.y = "main_category", all = F)
#
# Formation of D3 data frame
#
D3 <- merge(D3_NO_AGGREGATE, IND_MAIN_SECTOR_AGGREGATE, by.x = "main_sector", by.y = "main_category", all = F)
#
# Table 6.1)Q1)a)  Total number of investments for USA
#
nrow(D1)
#
# Table 6.1)Q1)b)  Total number of investments for GBR
#
nrow(D2)
#
# Table 6.1)Q1)c)  Total number of investments for IND
#
nrow(D3)
#
# Table 6.1)Q2)a) Total amount of investment for USA
#
sum(D1$raised_amount_usd)
#
# Table 6.1)Q2)b) Total amount of investment for GBR
#
sum(D2$raised_amount_usd)
#
# Table 6.1)Q2)c) Total amount of investment for IND
#
sum(D3$raised_amount_usd)
#
# Table 6.1)Q3)a) Top sector name (no. of  investment-wise) USA
#
USA.TOP.SECTOR.NAME <- as.character(USA_MAIN_SECTOR_CNT[order(USA_MAIN_SECTOR_CNT$count, decreasing = T),1][1])
USA.TOP.SECTOR.NAME
#
# Table 6.1)Q3)b) Top sector name (no. of  investment-wise) GBR
#
GBR.TOP.SECTOR.NAME <- as.character(GBR_MAIN_SECTOR_CNT[order(GBR_MAIN_SECTOR_CNT$count, decreasing = T),1][1])
GBR.TOP.SECTOR.NAME
#
# Table 6.1)Q3)c) Top sector name (no. of  investment-wise) IND
#
IND.TOP.SECTOR.NAME <- as.character(IND_MAIN_SECTOR_CNT[order(IND_MAIN_SECTOR_CNT$count, decreasing = T),1][1])
IND.TOP.SECTOR.NAME
#
# Table 6.1)Q4)a) Second sector name (no. of  investment-wise) for USA
#
as.character(USA_MAIN_SECTOR_CNT[order(USA_MAIN_SECTOR_CNT$count, decreasing = T),1][2])
#
# Table 6.1)Q4)b) Second sector name (no. of  investment-wise) for GBR
#
as.character(GBR_MAIN_SECTOR_CNT[order(GBR_MAIN_SECTOR_CNT$count, decreasing = T),1][2])
#
# Table 6.1)Q4)c) Second sector name (no. of  investment-wise) for IND
#
as.character(IND_MAIN_SECTOR_CNT[order(IND_MAIN_SECTOR_CNT$count, decreasing = T),1][2])
#
# Table 6.1)Q5)a) Third sector name (no. of  investment-wise) for USA
#
as.character(USA_MAIN_SECTOR_CNT[order(USA_MAIN_SECTOR_CNT$count, decreasing = T),1][3])
#
# Table 6.1)Q5)b) Third sector name (no. of  investment-wise) for GBR
#
as.character(GBR_MAIN_SECTOR_CNT[order(GBR_MAIN_SECTOR_CNT$count, decreasing = T),1][3])
#
# Table 6.1)Q5)c) Third sector name (no. of  investment-wise) for IND
#
as.character(IND_MAIN_SECTOR_CNT[order(IND_MAIN_SECTOR_CNT$count, decreasing = T),1][3])
#
# Table 6.1)Q6)a) Number of investments in top  sector (3) for USA
#
USA_MAIN_SECTOR_CNT[order(USA_MAIN_SECTOR_CNT$count, decreasing = T),2][1]
#
# Table 6.1)Q6)b) Number of investments in top  sector (3) for GBR
#
GBR_MAIN_SECTOR_CNT[order(GBR_MAIN_SECTOR_CNT$count, decreasing = T),2][1]
#
# Table 6.1)Q6)c) Number of investments in top  sector (3) for IND
#
IND_MAIN_SECTOR_CNT[order(IND_MAIN_SECTOR_CNT$count, decreasing = T),2][1]
#
# Table 6.1)Q7)a) Number of investments in second sector (4) for USA
#
USA_MAIN_SECTOR_CNT[order(USA_MAIN_SECTOR_CNT$count, decreasing = T),2][2]
#
# Table 6.1)Q7)b) Number of investments in second sector (4) for GBR
#
GBR_MAIN_SECTOR_CNT[order(GBR_MAIN_SECTOR_CNT$count, decreasing = T),2][2]
#
# Table 6.1)Q7)c) Number of investments in second sector (4) for IND
#
IND_MAIN_SECTOR_CNT[order(IND_MAIN_SECTOR_CNT$count, decreasing = T),2][2]
#
# Table 6.1)Q8)a) Number of investments in third sector (5) for USA
#
USA_MAIN_SECTOR_CNT[order(USA_MAIN_SECTOR_CNT$count, decreasing = T),2][3]
#
# Table 6.1)Q8)b) Number of investments in third sector (5) for GBR
#
GBR_MAIN_SECTOR_CNT[order(GBR_MAIN_SECTOR_CNT$count, decreasing = T),2][3]
#
# Table 6.1)Q8)c) Number of investments in third sector (5) for IND
#
IND_MAIN_SECTOR_CNT[order(IND_MAIN_SECTOR_CNT$count, decreasing = T),2][3]
#
# Table 6.1)Q9)a)  For point 3 (top sector count-wise), which company received the  highest investment for USA
#
USA_TOP_SECTOR <- subset(D1, D1$main_sector == USA.TOP.SECTOR.NAME)  
#
USA_TOTAL_INVESTMENT_TOP_COMPANY <- aggregate(USA_TOP_SECTOR$raised_amount_usd, by = list(USA_TOP_SECTOR$permalink), FUN = "sum")
#
unique(subset(USA_TOP_SECTOR, USA_TOP_SECTOR$permalink == USA_TOTAL_INVESTMENT_TOP_COMPANY[which(USA_TOTAL_INVESTMENT_TOP_COMPANY[,2] == max(USA_TOTAL_INVESTMENT_TOP_COMPANY[,2])),1])[,4])
#
# Table 6.1)Q9)b)  For point 3 (top sector count-wise), which company received the  highest investment for GBR
#
GBR_TOP_SECTOR <- subset(D2, D2$main_sector == GBR.TOP.SECTOR.NAME)  
#
GBR_TOTAL_INVESTMENT_TOP_COMPANY <- aggregate(GBR_TOP_SECTOR$raised_amount_usd, by = list(GBR_TOP_SECTOR$permalink), FUN = "sum")
#
unique(subset(GBR_TOP_SECTOR, GBR_TOP_SECTOR$permalink == GBR_TOTAL_INVESTMENT_TOP_COMPANY[which(GBR_TOTAL_INVESTMENT_TOP_COMPANY[,2] == max(GBR_TOTAL_INVESTMENT_TOP_COMPANY[,2])),1])[,4])
#
# Table 6.1)Q9)c)  For point 3 (top sector count-wise), which company received the  highest investment for IND
#
IND_TOP_SECTOR <- subset(D3, D3$main_sector == IND.TOP.SECTOR.NAME)  
#
IND_TOTAL_INVESTMENT_TOP_COMPANY <- aggregate(IND_TOP_SECTOR$raised_amount_usd, by = list(IND_TOP_SECTOR$permalink), FUN = "sum")
#
unique(subset(IND_TOP_SECTOR, IND_TOP_SECTOR$permalink == IND_TOTAL_INVESTMENT_TOP_COMPANY[which(IND_TOTAL_INVESTMENT_TOP_COMPANY[,2] == max(IND_TOTAL_INVESTMENT_TOP_COMPANY[,2])),1])[,4])
#
# Table 6.1)Q10)a) For point 4 (second best sector count-wise), which company received the highest investment for USA?
#
unique(subset(USA_TOP_SECTOR, USA_TOP_SECTOR$permalink == USA_TOTAL_INVESTMENT_TOP_COMPANY[which(USA_TOTAL_INVESTMENT_TOP_COMPANY[,2] == USA_TOTAL_INVESTMENT_TOP_COMPANY[order(USA_TOTAL_INVESTMENT_TOP_COMPANY[,2], decreasing = T),2][2]),1])[,4])
#
# Table 6.1)Q10)b) For point 4 (second best sector count-wise), which company received the highest investment for GBR?
#
unique(subset(GBR_TOP_SECTOR, GBR_TOP_SECTOR$permalink == GBR_TOTAL_INVESTMENT_TOP_COMPANY[which(GBR_TOTAL_INVESTMENT_TOP_COMPANY[,2] == GBR_TOTAL_INVESTMENT_TOP_COMPANY[order(GBR_TOTAL_INVESTMENT_TOP_COMPANY[,2], decreasing = T),2][2]),1])[,4])
#
# Table 6.1)Q10)c) For point 4 (second best sector count-wise), which company received the highest investment for IND?
#
unique(subset(IND_TOP_SECTOR, IND_TOP_SECTOR$permalink == IND_TOTAL_INVESTMENT_TOP_COMPANY[which(IND_TOTAL_INVESTMENT_TOP_COMPANY[,2] == IND_TOTAL_INVESTMENT_TOP_COMPANY[order(IND_TOTAL_INVESTMENT_TOP_COMPANY[,2], decreasing = T),2][2]),1])[,4])
#
#
################# Checkpoint 7: Plots ##################
#
#
# 1. Pie chart plotting
#
# Filtering mean for venture, seed, angel and private equity from master_frame
#
mean_funding_round_type_an_se_ve_pe <- subset(mean_funding_round_type, mean_funding_round_type$Funding_Type == "venture" | mean_funding_round_type$Funding_Type == "angel" | mean_funding_round_type$Funding_Type == "seed" | mean_funding_round_type$Funding_Type == "private_equity")
#
# finding the sum investment wise 
#
sum_funding_round_type <- aggregate(master_frame$raised_amount_usd, by = list(master_frame$funding_round_type), FUN = "sum")
#
# Renaming the columns
#
names(sum_funding_round_type) <- c("Funding_Type","Sum")
#
# Retrieving sum records specific to angel, seed, venture and private equity
#
sum_funding_round_type_an_se_ve_pe <- subset(sum_funding_round_type, sum_funding_round_type$Funding_Type == "venture" | sum_funding_round_type$Funding_Type == "seed" | sum_funding_round_type$Funding_Type == "private_equity") 
#
# Retriving mean records specific to angel, seed, venture and private equity
#
mean_funding_round_type_an_se_ve_pe <- subset(mean_funding_round_type, mean_funding_round_type$Funding_Type == "venture" | mean_funding_round_type$Funding_Type == "seed" | mean_funding_round_type$Funding_Type == "private_equity")
#
# Merging the sum and average into a single data frame
#
funding_round_type_an_se_ve_pe_aggregate <- merge(sum_funding_round_type_an_se_ve_pe,mean_funding_round_type_an_se_ve_pe, by.x = "Funding_Type", by.y = "Funding_Type", all = F)
#
# Rounding up the average values
#
funding_round_type_an_se_ve_pe_aggregate$Average <- round(funding_round_type_an_se_ve_pe_aggregate$Average, 0)
#
# Plotting the pie chart
#
bp<- ggplot(funding_round_type_an_se_ve_pe_aggregate, aes(x="", y=Sum, fill=Funding_Type))+
  geom_bar(width = 1, stat = "identity") + geom_text(aes(label = Average))
#
pie <- bp +  coord_polar("y", start=0)
#
pie
#
# Plotting the bar chart
#
top9_bar <- ggplot(top9, aes(x=Country_Code, y=Total_Invested_Amnt)) + geom_bar(position = "stack", stat = "identity")
top9_bar
#
# Plotting of a chart
#
# Adding country name to USA_MAIN_SECTOR_AGGREGATE
#
country_name <- c("USA","USA","USA","USA","USA","USA","USA","USA")
#
USA_MAIN_SECTOR_AGGREGATE_CNTRY <- cbind(USA_MAIN_SECTOR_AGGREGATE, country_name)
#
# Adding country name to GBR_MAIN_SECTOR_AGGREGATE
#
country_name <- c("GBR","GBR","GBR","GBR","GBR","GBR","GBR","GBR")
#
GBR_MAIN_SECTOR_AGGREGATE_CNTRY <- cbind(GBR_MAIN_SECTOR_AGGREGATE, country_name)
#
# Adding country name to IND_MAIN_SECTOR_AGGREGATE
#
country_name <- c("IND","IND","IND","IND","IND","IND","IND","IND")
#
IND_MAIN_SECTOR_AGGREGATE_CNTRY <- cbind(IND_MAIN_SECTOR_AGGREGATE, country_name)
#
# Union of records of top 3 country into one frame
#
TOP3_MAIN_SECTOR_AGGREGATE_CNTRY <- rbind(USA_MAIN_SECTOR_AGGREGATE_CNTRY, GBR_MAIN_SECTOR_AGGREGATE_CNTRY, IND_MAIN_SECTOR_AGGREGATE_CNTRY)
#
# Creating a mapping frame having short names for main sector
#
short_name <- c("A&S", "Cl/Smcnd", "Ent", "Hlth", "Mfg", "NS&M", "Oth", "SFAA")
#
main_sec_mapping_frame <- data.frame(IND_MAIN_SECTOR_AGGREGATE_CNTRY$main_category,short_name)
#
names(main_sec_mapping_frame) <- c("main_category", "short_name")
#
# Including short names in TOP3_MAIN_SECTOR_AGGREGATE_CNTRY
#
TOP3_MAIN_SECTOR_AGGREGATE_CNTRY_SNMS <- merge(TOP3_MAIN_SECTOR_AGGREGATE_CNTRY,main_sec_mapping_frame, by.x = "main_category", by.y = "main_category", all = F)
#
# Line chart showing top sectors for each country
#
ggplot(TOP3_MAIN_SECTOR_AGGREGATE_CNTRY_SNMS, aes(x = short_name,y = count, group = country_name, col = factor(country_name))) + geom_line() + geom_point()
