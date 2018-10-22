# Development of Common Path Distortion (CPD) Algorithm for Liberty Global.

# Project (Coding) Begin Date: 08/10/2018.

# Authors: Mete Sevinc, Subroto Mallick, Alejandra, Claudia, Hassan.

# Last Update / Modification: 22/10/2018.
# Latest version: v1.8.

# Modules;
# Module 1: Ben's code for establishing a database connection.
# Module 2: Time Series Analyses using Nodes and MAC Addresses datasets.
# Module 3: Exploratory Data Analyses, basic statistics & histograms.
# Module 4: Bayesian Network Development and printing the resulting graphs to pdf.

# Notes / Comments / Logs;
# Log 1; 17/10/2018: Some EDA functions from the young graduates were added.
# Log 2; 17/10/2018: Average SNR dataset can be obtained.
# Log 3; 18/10/2019: EDA Analyses can be performed.
# Log 4; 21/10/2018: Bayesian Network coding was initiated.
# Log 5; 22/10/2018: Added the latest version of time series code from Alejandra and Claudia.
# Log 6; 22/10/2018: Added the code to obtain the Nodes and MAC addresses time series datasets.
# Log 7; 22/10/2018: Added Time-series plotting code from Hassan.
# Log 8; 22/10/2018: BNs can be produced as a result of Module 4 and printed on a pdf file.

# To-do List;
# Next tasks in line:
#  1. PCA Analyses. (Hassan)
#  2. Bayesian network Analyses. (Mete)
#  3. Imputation & time series Analyses. (Alejandra & Claudia)

######################################################################################################
##### --- Module 1. Ben's code for database connection --- ###########################################
######################################################################################################

# Use local library
.libPaths("/app/RPackageLibrary/R-3.3")

# Package Calls
library(RPostgreSQL)

# Create DB connection to localhost
drv <- RPostgreSQL::PostgreSQL()
con <- DBI::dbConnect(drv, dbname = 'PNM_CH_SA', user = 'postgres', host = '127.0.0.1', port = 5432, password = 'postgres')

# Show tables
tables <- DBI::dbListTables(con)
print(tables)

# Get Polling Data
pollingTest <- DBI::dbGetQuery(con,"SELECT * FROM TEST_CASES.test_cases_polling_data LIMIT 10;")
print(pollingTest)

# Reference Date
referenceTest <- DBI::dbGetQuery(con, "SELECT * FROM TEST_CASES.TEST_CASES_REFERENCE_DATA;")

# Create Query to get polling data for specific test case
queryPollingData <- function(testCaseRef, connection = con){
  require(glue)
  
  nodeName <- testCaseRef$node_name
  query = glue("SELECT *
               FROM TEST_CASES.test_cases_polling_data
               WHERE NODE_NAME='{nodeName}' AND reference_date = '{testCaseRef$polling_reference_date}'")
  queryResult = dbGetQuery(connection, query)
  return(as.data.frame(queryResult))
}

testCasePolling <- queryPollingData(referenceTest[4,])

###### Libraries #######

# Already installed;
#library(RPostgreSQL) # For database connection.
#library(plyr)
#library(tidyverse)
#library(DataExplorer) # for easy EDA
#library(dlookr)
#library(dplyr) # for data manipulation
#library(ggplot2) # for plotting
#library(mise)
#library(TSdist) # for time series similarity analyses

######################################################################################################
##### --- Module 2. Time Series Analyses --- #########################################################
######################################################################################################

# Create dataframe with node name, hour stamp and average SNR_UP.
node_data <- DBI::dbGetQuery(con,"SELECT ref.node_name, hour_stamp, AVG(snr_up) 
                             FROM TEST_CASES.test_cases_polling_data poll, TEST_CASES.TEST_CASES_REFERENCE_DATA ref 
                             WHERE poll.node_name=ref.node_name AND poll.topo_node_type='CPE'
                             GROUP BY ref.node_name, hour_stamp;")

# Create dataframe containig all CPE information.
pollingTest_CPE <- DBI::dbGetQuery(con,"SELECT * FROM TEST_CASES.test_cases_polling_data WHERE topo_node_type='CPE';")

# Create a dataframe containing all mac addresses time series information.
mac_data <- pollingTest_CPE[,c("node_name", "mac_address","hour_stamp", "snr_dn", "pathloss")]

# SNR Averages dataset query alternative (from Hassan);
# NodeData1 <- DBI::dbGetQuery(con,"SELECT DISTINCT a.node_name, a.hour_stamp, AVG(a.snr_up)
#                             FROM TEST_CASES.test_cases_polling_data a, TEST_CASES.TEST_CASES_REFERENCE_DATA b
#                             WHERE a.topo_node_type = 'CPE' and a.node_name = b.node_name
#                             GROUP by a.node_name, a.hour_stamp ORDER by a.node_name, a.hour_stamp; ")

# -------------------------------Functions to obtain the reference nodes-----

# A. Obtain the reference nodes
# @return array of unique reference node names
get_reference_nodes <- function(){
  return(unique(node_data$node_name))
}

# B. Obtain all mac addresses connected to a node
# @param node_name: node name from which we want to obtain the mac addresses
# @return array of unique mac addresses
node_get_macaddress <- function(node_name){
  return(unique(pollingTest_CPE$mac_address[pollingTest_CPE$node_name == node_name]))
}

# C. Obtain time series of a mac address
# @param mac_address: MAC address from which we want to obtain the time series
# @return dataframe with mac address, hour stamp, SNR down and pathloss values
mac_get_time_series <- function(mac_address){
  return(pollingTest_CPE[pollingTest_CPE$mac_address == mac_address,c("mac_address","hour_stamp","snr_dn", "pathloss")])
}

# D. Obtain time series of a node
# @param node_name: node name from which we want to obtain the time series
# @return dataframe with node name, hour stamp and average SNR up values
node_get_time_series <- function(node_name){
  return(node_data[node_data$node_name == node_name,c("node_name", "hour_stamp","avg")])
}

# --------Code for time series plot (used for presentation / Visualization)-----------------------------------

# Obtain time series for random node and a few random macs
ts = node_get_time_series("1636N01")
df_mac <- mac_get_time_series("54FA3E3712E1")
df_mac2 <- mac_get_time_series("905C44E7F3D6")
df_mac3 <- mac_get_time_series("905C4418A0CC")
df_mac4 <- mac_get_time_series("3C6200779B8C")

# Node 1636N01 and MAC address 54FA3E3712E1
a <- ggplot() + geom_line(data=ts, aes(x=hour_stamp, y=avg, colour="Node average upstream SNR")) + 
  geom_line(data=df_mac, aes(x=hour_stamp, y=snr_dn, colour="MAC address No. 1  downstream SNR")) +
  geom_line(data=df_mac, aes(x=hour_stamp, y=pathloss, colour="MAC address No. 1 pathloss"))+
  labs(title="Comparing node and MAC address No. 1 time series") + 
  xlab("Time") + ylab("dB") + theme_bw() + theme(legend.title=element_blank(), legend.position="bottom")

# Node 1636N01 and MAC address 905C44E7F3D6
b <- ggplot() + geom_line(data=ts, aes(x=hour_stamp, y=avg, colour="Node average upstream SNR")) + 
  geom_line(data=df_mac2, aes(x=hour_stamp, y=snr_dn, colour="MAC address No. 2  downstream SNR")) +
  geom_line(data=df_mac2, aes(x=hour_stamp, y=pathloss, colour="MAC address No. 2 pathloss"))+
  labs(title="Comparing node and MAC address No. 2 time series") + 
  xlab("Time") + ylab("dB") + theme_bw() + theme(legend.title=element_blank(), legend.position="bottom")

# Node 1636N01 and MAC address 905C4418A0CC
c <- ggplot() + geom_line(data=ts, aes(x=hour_stamp, y=avg, colour="Node average upstream SNR")) + 
  geom_line(data=df_mac3, aes(x=hour_stamp, y=snr_dn, colour="MAC address No. 3  downstream SNR")) +
  geom_line(data=df_mac3, aes(x=hour_stamp, y=pathloss, colour="MAC address No. 3 pathloss"))+
  labs(title="Comparing node and MAC address No. 3 time series") + 
  xlab("Time") + ylab("dB") + theme_bw() + theme(legend.title=element_blank(), legend.position="bottom")

# Node 1636N01 and MAC address 3C6200779B8C
d <- ggplot() + geom_line(data=ts, aes(x=hour_stamp, y=avg, colour="Node average upstream SNR")) + 
  geom_line(data=df_mac4, aes(x=hour_stamp, y=snr_dn, colour="MAC address No. 4  downstream SNR")) +
  geom_line(data=df_mac4, aes(x=hour_stamp, y=pathloss, colour="MAC address No. 4 pathloss"))+
  labs(title="Comparing node and MAC address No. 4 time series") + 
  xlab("Time") + ylab("dB") + theme_bw() + theme(legend.title=element_blank(), legend.position="bottom")

#----------- Code for mac addresses table (used for presentation) --------------

#Get the dataframe of the particular node and the selected columns
df_table <- pollingTest_CPE[pollingTest_CPE$node_name=="1636N01",c("node_name", "mac_address", "hour_stamp", "pathloss", "snr_dn")]

#Remove the empty strings mac addresses
df_table <- df_table[df_table$mac_address!="",]

#Get all the variables
min_hs <- aggregate(hour_stamp~mac_address, df_table, min)
names(min_hs) <- c("mac_address", "min_hour_stamp")

max_hs <- aggregate(hour_stamp~mac_address, df_table, max)
names(max_hs) <- c("mac_address", "max_hour_stamp")

min_snrdn <- aggregate(snr_dn~mac_address, df_table, min)
names(min_snrdn) <- c("mac_address", "min_snr_dn")

max_snrdn <- aggregate(snr_dn~mac_address, df_table, max)
names(max_snrdn) <- c("mac_address", "max_snr_dn")

median_snrdn <- aggregate(snr_dn~mac_address, df_table, median)
names(median_snrdn) <- c("mac_address", "median_snr_dn")

sd_snrdn <- aggregate(snr_dn~mac_address, df_table, sd)
names(sd_snrdn) <- c("mac_address", "sd_snr_dn")

min_pathloss <- aggregate(pathloss~mac_address, df_table, min)
names(min_pathloss) <- c("mac_address", "min_pathloss")

max_pathloss <- aggregate(pathloss~mac_address, df_table, max)
names(max_pathloss) <- c("mac_address", "max_pathloss")

median_pathloss <- aggregate(pathloss~mac_address, df_table, median)
names(median_pathloss) <- c("mac_address", "median_pathloss")

sd_pathloss <- aggregate(pathloss~mac_address, df_table, sd)
names(sd_pathloss) <- c("mac_address", "sd_pathloss")

# Merge all the dataframes by mac address
final_dataf <- Reduce(function(x,y) merge(x,y, by="mac_address", all=TRUE), list(min_hs, max_hs, min_snrdn, max_snrdn, median_snrdn, sd_snrdn,
                                                                                 min_pathloss, max_pathloss, median_pathloss, sd_pathloss))
#View(final_dataf)

# ----------------------------------------------------------------
# NA imputation tests
# library(zoo)
# 
# # pollingTest_CPE:
# #   NA ("") in mac_address = 2933933 (40.91%) == NA in snr_up_median
# #   NA in snr_up = 2935086 (40.93%)
# 
# # pollingTest_SNR:
# # NA ("") in node_name = 0
# # NA in avg = 461 (4%)
# 
# # Example of NA interpolation using zoo package
# # Get time series from node
# ts = node_get_time_series("1636N01")[2]
# plot.ts(ts)
# 
# # Use linear interpolation
# ts_linear = na.approx(ts)
# plot.ts(ts_linear)
# 
# # Use polynomial interpolation
# ts_poly = na.spline(ts)
# plot.ts(ts_poly)
# 
# # Interpolation using last observation
# ts_locf = na.locf(ts)
# plot.ts(ts_locf)
# 
# # Example 2
# ts2 = node_get_time_series("1029N01")[2]
# plot.ts(ts2)
# 
# plot.ts(na.locf(ts2))
# 
# plot.ts(na.approx(ts2))
# 
# plot.ts(na.spline(ts2))

# Time Series Plots (from Hassan)

#Get Polling Data
NodeData <- DBI::dbGetQuery(con,"SELECT DISTINCT a.node_name, a.hour_stamp, AVG(a.snr_up) 
                            FROM TEST_CASES.test_cases_polling_data a, TEST_CASES.TEST_CASES_REFERENCE_DATA b 
                            WHERE a.topo_node_type = 'CPE' and a.node_name = b.node_name 
                            GROUP by a.node_name, a.hour_stamp ORDER by a.node_name, a.hour_stamp; ")

library(plyr)
#Time series per node
df <- ddply(NodeData, .(node_name), nrow) 
colnames(df) <- c("node_name", "occurence")
#Na time series per node
df_na <- aggregate(avg ~ node_name, data=NodeData, function(x) {sum(is.na(x))}, na.action = NULL) 
colnames(df_na) <- c("node_name", "na")
#Combbine Time series and NA's
df_combi <- merge(df, df_na, by=0, all=TRUE) 
df_combi$Row.names = NULL
df_combi$node_name.y = NULL

df2 <- melt(df_combi, id.vars='node_name.x')
#Time seires per node
ggplot(df2, aes(x = reorder(node_name.x, -value), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Summary Statistics of avg snr up per node name 
library(dplyr)
NodeData %>% group_by(node_name) %>% 
  summarise(
    avrg=mean(avg,na.rm = TRUE), 
    min=min(avg,na.rm = TRUE), 
    max=max(avg,na.rm = TRUE), 
    SD=sd(avg,na.rm = TRUE))
node_summary1 <- copy(node_summary)
node_summary1$SD <- NULL
df3 <- melt(node_summary1, id.vars='node_name')
#Plot summary stats
ggplot(df3, aes(x = reorder(node_name, -value), y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Node name")


######################################################################################################
##### --- Module 3. Exploratory Data Analyses --- ####################################################
######################################################################################################

# Summaries
summary(testCasePolling)
summary(pollingTest)
summary(referenceTest)

# 3.1 Report the first few lines of the datasets.
head(referenceTest)
head(pollingTest)
head(testCasePolling)

# 3.2 Datasets in visuals
plot_str(testCasePolling)
plot_str(pollingTest)
plot_str(referenceTest)

# 3.3 Missing values
plot_missing(testCasePolling)
plot_missing(pollingTest)
plot_missing(referenceTest)

# 3.3 Continuous variables (Histogram & Density plots)
plot_histogram(testCasePolling)
plot_histogram(pollingTest)
plot_histogram(referenceTest)

plot_density(testCasePolling)
plot_density(pollingTest)
plot_density(referenceTest)

# 3.4 Barplots
plot_bar(testCasePolling)
plot_bar(pollingTest)
plot_bar(referenceTest)

# 3.5 Create report
create_report(testCasePolling)
create_report(pollingTest)
create_report(referenceTest)

# YOUNG GRADUATES FINDINGS
# From Alejandra & Claudia

# TestCasePolling Dataset Analysis

# 1.	In order to find potential problematic buildings due to bad location, weather conditions, etc. we search those where the snr_up_median is below 25. When this happens, it means there would be dropped connections, packet loss or slow transfers.

table(testCasePolling$building_id[testCasePolling$snr_up_median<25])

# We found out that there are 51 out of 76 which meet this condition.  
# Now that we know that there could be problematic buildings, we want to calculate the actual percentage of instances (not including NA values) per building that meets the condition of snr_up_median being below 25:

table_bl_less_25<-table(testCasePolling$building_id[testCasePolling$snr_up_median<25])
count_bl<- count(testCasePolling$building_id[!is.na(testCasePolling$snr_up_median)])
table_bl_less_25<-as.data.frame(table_bl_less_25)
rm("bl_pct_less_25")
bl_pct_less_25 <- data.frame(building_id=integer(),percentage_less_25=double())
for( elem in table_bl_less_25$Var1){
  if(elem %in% count_bl$x){
    val <- table_bl_less_25$Freq[table_bl_less_25$Var1==elem]/count_bl$freq[count_bl$x==elem]
    bl_pct_less_25[nrow(bl_pct_less_25)+1, ] <- c(elem,val*100)
  }else{
    paste(elem,"not in count_bl")
  }
}

# 2. We now want to see the snr_up distribution in terms of the type of network device.

boxplot(snr_up~factor(topo_node_type), data=testCasePolling)

# We only find values for CPEs.
# 83% of the snr_up values are NA.

# 3.	We want to know if there are some models more potential to fail than others, so we sum the snr_up_median values that are less than 25 per model of network device.
sort(table(testCasePolling$src_node_model[testCasePolling$snr_up_median<25]), decreasing=TRUE) 

# Furthermore, we want to see the distribution of the variable snr_up_median for each device model.

boxplot(snr_up_median~src_node_model, data=testCasePolling)

# The model which stands out is MEDIABOX HD PHILIPS DCR7101/03. It has only 81 instances where snr_up_median has values different from NA (out of 1482 instances), and 45 of those are below 25.

# 4.	Are there some arcs where fails tend to occur more?

count(sort(table(testCasePolling$edge_id[testCasePolling$snr_up_median<25]),decreasing=TRUE))

# 174 out of 478 edges have snr_up_median values below 25. We still don't know if this will be helpful for the problem of estimating the CPD location, but we see common vertices in some of the edges.

# 5.	According to internet research (https://www.speedguide.net/faq/what-cable-modem-signal-levels-are-considered-good-78), "upstream power lower than 40dB may start introducing some packet loss (especially if you have much noise on the line)"

sort(table(testCasePolling$building_id[testCasePolling$tx_pwr_up<40]), decreasing=TRUE)

# We have searched for the buildings which present this problem, but this could be done with other variables.

# 6.	We also found that "If you hit 58 the modem will likely drop the connection and resync."

sort(table(testCasePolling$building_id[testCasePolling$tx_pwr_up>50]), decreasing=TRUE)

# Just like in the previous point, this is done with building IDs but it could be done with other variables as well.

# ----------------Variable-based investigation on testCasePolling dataset ------------------
# List of variables in testCasePolling

# 1. reference_date. <String variable>
mean(testCasePolling$reference_date)

# 2. vertex_topo_node_id <String variable>
mean(testCasePolling$vertex_topo_node_id)

# 3. edge_id <String variable>
mean(testCasePolling$edge_id)

# 4. building_id <Integer variable>
mean(testCasePolling$building_id)

# 5. pathorder <Integer variable>
mean(testCasePolling$pathorder)

# 6. hour_stamp <Date stamp>
mean(testCasePolling$hour_stamp)

# 7. topo_node_type / String variable
topo_node_types = table(testCasePolling$topo_node_type)
topo_node_types
plot(topo_node_types)

# 8. node_key <String variable>
mean(testCasePolling$node_key)

# 9. src_node_model / String variable
src_node_model = table(testCasePolling$src_node_model)
src_node_model
plot(src_node_model)

# 10. src_node_id <String variable>
mean(testCasePolling$src_node_id)

# 11. src_node_name <String variable>
mean(testCasePolling$src_node_name)

# 12. node_name <String variable>
mean(testCasePolling$node_name)

# 13. mac_address <String variable>
mean(testCasePolling$mac_address)

# 14. tx_pwr_up / <Numerical variable>
mean(testCasePolling$tx_pwr_up)
median(testCasePolling$tx_pwr_up)
plot(testCasePolling$tx_pwr_up)

# 15. rx_pwr_up / <Numerical variable>
mean(testCasePolling$rx_pwr_up)
median(testCasePolling$rx_pwr_up)
plot(testCasePolling$rx_pwr_up)

# 16. rx_pwr_dn / <Numerical variable>
mean(testCasePolling$rx_pwr_dn)
median(testCasePolling$rx_pwr_dn)
plot(testCasePolling$rx_pwr_dn)

# 17. snr_dn / <Numerical variable>
mean(testCasePolling$snr_dn)
median(testCasePolling$snr_dn)
plot(testCasePolling$snr_dn)

# 18. snr_up / <Numerical variable>
mean(testCasePolling$snr_up)
median(testCasePolling$snr_up)
plot(testCasePolling$snr_up)

# 19. pathloss
mean(testCasePolling$pathloss)

# 20. snr_up_median / <Numerical variable>
mean(testCasePolling$snr_up_median)
median(testCasePolling$snr_up_median)
plot(testCasePolling$snr_up_median)

# -------------------------------Reporting on the datasets------------------------------

# Report the dimensions of the input datasets.
dim(testCasePolling)
dim(referenceTest)
dim(pollingTest)

# Report the structure of the input datasets.
str(testCasePolling)
str(referenceTest)
str(pollingTest)

# Report the column sums for each variable.
# colSums(sapply(testCasePolling, !is.na))

# Report the summary of numeric values and structure of the data.
# summary(testCasePolling[,.SD, .SDcols =numeric_var])

# Report the percentage of data missing.
# sum(is.na(testCasePolling)) / (nrow(testCasePolling) *ncol(testCasePolling))

# Report the number of duplicated rows.
# cat("The number of duplicated rows are", nrow(testCasePolling) - nrow(unique(testCasePolling)))

# Get Polling Data
pollingTest_CPE <- DBI::dbGetQuery(con,"SELECT * FROM TEST_CASES.test_cases_polling_data
                                   WHERE topo_node_type='CPE';")

# Distribution or descriptive statistics of snr_dn per mac address
snr_dist <- aggregate(snr_dn ~ mac_address, pollingTest_CPE, mean)
snr_dist <- snr_dist[snr_dist$mac_address!="",]
summary(snr_dist$snr_dn)
hist(snr_dist$snr_dn, xlim=c(15,45), xlab="Average snr_dn", main="Histogram of average snr_dn per mac address")

#Distribution or descriptive statistics of pathloss per mac address
pathloss_dist <- aggregate(pathloss ~ mac_address, pollingTest_CPE, mean)
pathloss_dist <- pathloss_dist[pathloss_dist$mac_address!="",]
summary(pathloss_dist$pathloss)
hist(pathloss_dist$pathloss, xlim=c(10,60), xlab="Average pathloss", main="Histogram of average pathloss per mac address")

##################################################################################################################
##### --- Module 4. Bayesian Network Development --- #############################################################
##################################################################################################################

library(bnlearn) # The package to develop Bayesian Networks and make use of the utilities, methods and the structure learning / Scoring Algorithms
#library(ggplot2)
#library(graphics)
#library(graph)
#library(igraph)
#library(lattice) # for plotting the bn.fit instances
#library(Rgraphviz)
#library(gRain)

# To Read & Write Excel files
library(readxl)
library(xlsx)

# Supplementary packages
#library(deal) ## Used for rgraphviz
#library(pcalg) ## Used for rgraphviz
#library(stringr) ## Pipe
#library(BiocInstaller) ## Advance Graphics
#library(BayesianNetwork) 
#library(datasets) ## Advance manipulation of Datasets
#library(gRbase)   ## part of Shiny
#library(networkD3) ## part of Shiny
#library(dplyr)
#library(Magrittr)
#library(ROCR)
#library(Rmpfr)
#library(gmp)

Nodes_dataframe <- read.csv(file="Inputs/Toy_Nodes_Dataset.csv", header = TRUE)
MACs_dataframe <- read.csv(file="Inputs/Toy_MACs_Dataset.csv", header = TRUE)

# 4.1 DATA CLEANING & PRE-PROCESSING
# Convert mac_address from integer to numeric.
MACs_dataframe$mac_address <- as.numeric(MACs_dataframe$mac_address)

# 4.2 OBTAINING THE NETWORKS FROM EMBEDDED ALGORITHMS
# Algorithms for learning structure from the dataset (Constraint-based, Score-based, and Hybrid algorithms);

# 4.2.1. Grow-shrink algorithm
Nodes_gs_Network = gs(Nodes_dataframe)
MACs_gs_Network = gs(MACs_dataframe)

# 4.2.2. Hill climb (a.k.a Greedy search) algorithm
Nodes_hc_Network = hc(Nodes_dataframe)
MACs_hc_Network = hc(MACs_dataframe)

# 4.2.3. Fast incremental association algorithm
Nodes_iamb_Network = iamb(Nodes_dataframe)
MACs_iamb_Network = iamb(MACs_dataframe)

# 4.2.4 Tabu network
Nodes_tabu_Network = tabu(Nodes_dataframe)
MACs_tabu_Network = tabu(MACs_dataframe)

# 4.3 BASIC PLOTTING OF THE BNs

# 4.3.1. Plotting the Grow-shrink algorithm network
plot(Nodes_gs_Network)
plot(MACs_gs_Network)

# 4.3.2. plotting the Hill-climb algorithm network
plot(Nodes_hc_Network)
plot(MACs_hc_Network)

# 4.3.3. plotting the IAMB algorithm network
plot(Nodes_iamb_Network)
plot(MACs_iamb_Network)

# 4.3.4. plotting the Tabu algorithm results
plot(Nodes_tabu_Network)
plot(MACs_tabu_Network)

# 4.4 PRINTING GRAPHS TO A PDF

# Begin writing to pdf.
pdf("Liberty Global - Bayesian Network Graphs for Time Series Analyses.pdf")

# 4.4.1 Draw the networks derived from grow shrink algorithm
graphviz.plot(Nodes_gs_Network, highlight = NULL, layout = "dot", shape = "circle", main = "Grow-Shrink Algorithm Network, Nodes Time Series", sub = NULL)
graphviz.plot(MACs_gs_Network, highlight = NULL, layout = "dot", shape = "circle", main = "Grow-Shrink Algorithm Network, MAC Addressses Time Series", sub = NULL)

# 4.4.2 Draw the network derived from hill climb algorithm
graphviz.plot(Nodes_hc_Network, highlight = NULL, layout = "dot", shape = "circle", main = "Hill-Climb Algorithm Network, Nodes Time Series", sub = NULL)
graphviz.plot(MACs_hc_Network, highlight = NULL, layout = "dot", shape = "circle", main = "Hill-Climb Algorithm Network, MAC Addresses Time Series", sub = NULL)

# 4.4.3 Draw the network derived from iamb algorithm
graphviz.plot(Nodes_iamb_Network, highlight = NULL, layout = "dot", shape = "circle", main = "IAMB Algorithm Network, Nodes Time Series", sub = NULL)
graphviz.plot(MACs_iamb_Network, highlight = NULL, layout = "dot", shape = "circle", main = "IAMB Algorithm Network, MAC Addresses Time Series", sub = NULL)

# 4.4.4 Draw the network derived from tabu algorithm
graphviz.plot(Nodes_iamb_Network, highlight = NULL, layout = "dot", shape = "circle", main = "Tabu Algorithm Network, Nodes Time Series", sub = NULL)
graphviz.plot(MACs_iamb_Network, highlight = NULL, layout = "dot", shape = "circle", main = "Tabu Algorithm Network, MAC Addresses Time Series", sub = NULL)

# End writing to pdf.
dev.off()