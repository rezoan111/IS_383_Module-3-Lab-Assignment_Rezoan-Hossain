
# Name: Rezoan HOssain
# Chapter 4 - Case Problem 1: Usman Solutions
# Step 1
# Here I am loading the two text files into RStudio.
# I am also checking that both files opened correctly.

library(readr)
library(dplyr)

# Load the Knoxville file
knoxville <- read_tsv("knoxville.txt")

# Load the Boise file
boise <- read_tsv("boise.txt")

# View both data sets
View(knoxville)
View(boise)

# Check the first few rows
head(knoxville)
head(boise)

# Check the column names
names(knoxville)
names(boise)


# In this part, I am adding a Market field to both data sets.
# Then I am combining both files into one data set.
# After that, I am saving the combined file.

library(writexl)

# Add Market field to each data set
knoxville$Market <- "Knoxville"
boise$Market <- "Boise"

# Combine both data sets into one file
porter_hinkle <- bind_rows(knoxville, boise)

# View the combined data
View(porter_hinkle)

# Check the first few rows
head(porter_hinkle)

# Check the last few rows
tail(porter_hinkle)

# Check total rows and columns
dim(porter_hinkle)

# Save the combined file
write_xlsx(porter_hinkle, "porter_hinkle.xlsx")


# In this part, I am counting the missing values in each field.
# I am also making a report to show which records have missing values.

library(dplyr)
library(tidyr)

# Count missing values in each required field
missing_count <- porter_hinkle %>%
  summarise(
    Respondent = sum(is.na(Respondent)),
    Age = sum(is.na(Age)),
    `Regularly Consume Crispy Spuds?` = sum(is.na(`Regularly Consume Crispy Spuds?`)),
    Crispiness = sum(is.na(Crispiness)),
    Saltiness = sum(is.na(Saltiness)),
    Sweetness = sum(is.na(Sweetness)),
    Flavor = sum(is.na(Flavor)),
    Texture = sum(is.na(Texture)),
    Value = sum(is.na(Value)),
    Overall = sum(is.na(Overall))
  )

# Show the missing value counts
missing_count

# Make a report of rows with missing values
missing_report <- porter_hinkle %>%
  mutate(across(c(Age, `Regularly Consume Crispy Spuds?`, Crispiness,
                  Saltiness, Sweetness, Flavor, Texture, Value, Overall),
                as.character)) %>%
  pivot_longer(
    cols = c(Age, `Regularly Consume Crispy Spuds?`, Crispiness,
             Saltiness, Sweetness, Flavor, Texture, Value, Overall),
    names_to = "Field",
    values_to = "Value_Check"
  ) %>%
  filter(is.na(Value_Check)) %>%
  select(Market, Respondent, Field)

# Show the missing value report
missing_report
View(missing_report)


# In this part, I am checking for duplicate records.
# I am also making a list of the market and respondent number
# for the duplicate records so they can be reviewed.

# Count duplicate records
duplicate_count <- sum(duplicated(porter_hinkle))

# Show the number of duplicate records
duplicate_count

# Show all duplicate rows
duplicate_rows <- porter_hinkle %>%
  filter(duplicated(.) | duplicated(., fromLast = TRUE))

duplicate_rows
View(duplicate_rows)

# Make a smaller report with only market and respondent number
duplicate_report <- duplicate_rows %>%
  select(Market, Respondent) %>%
  distinct()

duplicate_report
View(duplicate_report)



# In this part, I am creating a new field called Taste.
# Taste is the average of Saltiness, Sweetness, and Flavor.

# Create the Taste field
porter_hinkle <- porter_hinkle %>%
  mutate(Taste = (Saltiness + Sweetness + Flavor) / 3)

# View the updated data
View(porter_hinkle)

# Check the first few rows
head(porter_hinkle)

# Check the last few rows
tail(porter_hinkle)

# Check the number of rows and columns
dim(porter_hinkle)


# In this part, I am creating a new data set for respondents
# who are under 35 years old and answered No to the question
# about regularly consuming Crispy Spuds.
# Then I am saving that file as young_no.xlsx.

# Create the filtered data
young_no <- porter_hinkle %>%
  filter(Age < 35, `Regularly Consume Crispy Spuds?` == "No")

# View the filtered data
View(young_no)

# Check the first few rows
head(young_no)

# Check the number of rows and columns
dim(young_no)

# Save the filtered file
write_xlsx(young_no, "young_no.xlsx")

# Load the package again so I can save the Excel file
library(writexl)

# Save the filtered file
write_xlsx(young_no, "young_no.xlsx")

# Check the files in the folder
list.files()

# Chapter 6 - Case Problem 2: Know Thy Customer
# Step 1
# In this part, I am loading the KTC file into RStudio.
# I am checking the data and removing blank extra columns.
# I am also cleaning the column names.

library(readr)
library(dplyr)
library(stringr)

# Load the KTC file
ktc <- read_csv("ktc.csv")

# Check the original file
View(ktc)
head(ktc)
dim(ktc)
names(ktc)

# Remove blank extra columns
ktc <- ktc %>%
  select(where(~ !all(is.na(.))))

# Clean the column names
names(ktc) <- str_trim(names(ktc))

# Check the cleaned file
View(ktc)
head(ktc)
dim(ktc)
names(ktc)

# Chapter 6 - Step 2
# In this part, I am creating the binary variables
# that will be used for clustering.

ktc_binary <- ktc %>%
  mutate(
    Retirement = if_else(Age > 62, 1, 0),
    MiddleUpper = if_else(Income > 85000, 1, 0),
    Married = if_else(Marital == "married", 1, 0),
    FamilyPlan = if_else(Children > 0, 1, 0),
    MastersPlus = if_else(Education %in% c("masters", "more_than_masters"), 1, 0),
    Own = if_else(Home == "own", 1, 0)
  )

# Check the updated file
View(ktc_binary)
head(ktc_binary)
dim(ktc_binary)
names(ktc_binary)

# Check the totals for each binary field
ktc_binary %>%
  summarise(
    Retirement = sum(Retirement),
    MiddleUpper = sum(MiddleUpper),
    Married = sum(Married),
    FamilyPlan = sum(FamilyPlan),
    MastersPlus = sum(MastersPlus),
    Own = sum(Own)
  )

# Chapter 6 - Step 3A
# In this part, I am applying hierarchical clustering
# using the 6 binary variables I created.
# I am also making a dendrogram and checking different
# values of k using average silhouette.

library(cluster)

# Keep only the binary variables
ktc_binary_only <- ktc_binary %>%
  select(Retirement, MiddleUpper, Married, FamilyPlan, MastersPlus, Own)

# Create the binary distance matrix
# In R, method = "binary" gives Jaccard-style distance for binary data
binary_dist <- dist(ktc_binary_only, method = "binary")

# Apply hierarchical clustering
# I am using average linkage
hc_binary <- hclust(binary_dist, method = "average")

# Make the dendrogram
plot(hc_binary,
     labels = FALSE,
     hang = -1,
     main = "Hierarchical Clustering Dendrogram - KTC Binary Variables",
     xlab = "Customers",
     ylab = "Height")

# Check possible numbers of clusters with average silhouette
k_values <- 2:6

sil_values <- sapply(k_values, function(k) {
  cluster_id <- cutree(hc_binary, k = k)
  mean(silhouette(cluster_id, binary_dist)[, 3])
})

sil_summary <- data.frame(
  k = k_values,
  Avg_Silhouette = round(sil_values, 3)
)

# Show the silhouette summary
sil_summary

# Plot the silhouette values
plot(k_values, sil_values,
     type = "b",
     xlab = "Number of Clusters (k)",
     ylab = "Average Silhouette",
     main = "Average Silhouette for Hierarchical Clustering")

# Chapter 6 - Step 3B
# In this part, I am choosing 2 clusters because
# k = 2 had the highest average silhouette value.
# I am adding the cluster number to the data
# and making a profile table for both clusters.

# Choose the 2-cluster solution
ktc_clustered <- ktc_binary %>%
  mutate(Cluster = factor(cutree(hc_binary, k = 2)))

# Check the updated file
View(ktc_clustered)
head(ktc_clustered)
dim(ktc_clustered)
names(ktc_clustered)

# Count customers in each cluster
cluster_sizes <- ktc_clustered %>%
  count(Cluster)

cluster_sizes

# Make a cluster profile table
# The mean of each binary field shows the proportion of 1's in that cluster
cluster_profile <- ktc_clustered %>%
  group_by(Cluster) %>%
  summarise(
    Customers = n(),
    Retirement = round(mean(Retirement), 3),
    MiddleUpper = round(mean(MiddleUpper), 3),
    Married = round(mean(Married), 3),
    FamilyPlan = round(mean(FamilyPlan), 3),
    MastersPlus = round(mean(MastersPlus), 3),
    Own = round(mean(Own), 3)
  )

cluster_profile
View(cluster_profile)