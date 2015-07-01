library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)

# Get data from UNDP API.
rawApiData <- GET(url = "https://data.undp.org/resource/p79w-icq5.json")
# Transform content to JSON and parse it back as data frame.
apiData <- jsonlite::fromJSON(toJSON(content(rawApiData), auto_unbox = TRUE))
dplyrApiData <- tbl_df(apiData)
# Remove unnecessary columns from the API data.
dplyrApiData <- select(dplyrApiData, country, hdi_rank, starts_with("perceptions"))
# Filter all the rows without Human Development Index rank (these rows do not represent actual countries).
filtered <- dplyrApiData %>% filter(hdi_rank != "NULL")
# Transform all dot dot values into NA.
filtered[filtered == ".."] <- NA
# Transform HDI rank and perception indices into numeric columns.
final_dataset <- mutate_each(filtered, funs(as.numeric), hdi_rank, starts_with("perceptions"))
# According to original data, the following is the HDI index classification:
#   [1 50):    Very High Human Development Index
#   [50 103):  High HDI
#   [103 145): Medium HDI
#   145+:      Low HDI
hdi_ranges <- cut(x = range(final_dataset$hdi_rank)[1]:range(final_dataset$hdi_rank)[2], breaks = c(0, 49, 102, 144, range(final_dataset$hdi_rank)[2]), labels = c("Very high HDI", "High HDI", "Medium HDI", "Low HDI"))
# Remove unused variables from environment.
rm("rawApiData", "apiData", "dplyrApiData", "filtered")

# Explore overall life satisfaction index: Select top 12
overall <- final_dataset %>%
  arrange(desc(perceptions_of_individual_well_being_overall_life_satisfaction_index_0_least_satisfied_10_most_satisfied_2007_2012)) %>%
  filter(row_number() <= 12) %>%
  select(country, hdi_rank,perceptions_of_individual_well_being_overall_life_satisfaction_index_0_least_satisfied_10_most_satisfied_2007_2012) %>%
  mutate(hdi_classification = hdi_ranges[hdi_rank])
# Add a ordered column for pretty plots.
overall$countryOrdered <- reorder(overall$country, desc(overall$perceptions_of_individual_well_being_overall_life_satisfaction_index_0_least_satisfied_10_most_satisfied_2007_2012))
# Calculate the overall mean of life satisfaction.
overallSatisMean <- mean(final_dataset$perceptions_of_individual_well_being_overall_life_satisfaction_index_0_least_satisfied_10_most_satisfied_2007_2012, na.rm = TRUE)
# Draw a plot with top 12 rank, overall mean and HDI rank.
ggplot(data = overall, aes(x = countryOrdered, y = perceptions_of_individual_well_being_overall_life_satisfaction_index_0_least_satisfied_10_most_satisfied_2007_2012, ymax = max(perceptions_of_individual_well_being_overall_life_satisfaction_index_0_least_satisfied_10_most_satisfied_2007_2012), fill = hdi_classification)) + 
  geom_bar(colour = "black", stat = "identity", position = position_dodge()) +
  geom_text(aes(label = hdi_rank), position = position_dodge(width = 0.9), vjust = -0.5, colour = "red", fontface = "bold") +
  geom_text(aes(label=perceptions_of_individual_well_being_overall_life_satisfaction_index_0_least_satisfied_10_most_satisfied_2007_2012), position = position_dodge(width = 0.9), vjust = 10) +
  scale_fill_hue(name = "HDI classification") +
  coord_cartesian(ylim = c(5, 10)) +
  scale_y_continuous(breaks = seq(from = 5, to = 10, by = 0.2)) +
  xlab("Country") + ylab("Perception index (0: Least satisf. - 10: Most satisf.)") +
  ggtitle("Overall life satisfaction") +
  geom_hline(yintercept = overallSatisMean, colour = "black", linetype = "dotted") + 
  annotate("text", 3, 5.6, label = "Global satisfaction average", colour = "black", fontface = "bold")

# Explore perception about government: Select top 12
government <- final_dataset %>%
  arrange(desc(perceptions_about_government_trust_in_national_government_satisfied_2007_2013)) %>%
  filter(row_number() <= 12) %>%
  select(country, hdi_rank,perceptions_about_government_trust_in_national_government_satisfied_2007_2013) %>%
  mutate(hdi_classification = hdi_ranges[hdi_rank])

government$countryOrdered <- reorder(government$country, desc(government$perceptions_about_government_trust_in_national_government_satisfied_2007_2013))

governmentSatisMean <- mean(final_dataset$perceptions_about_government_trust_in_national_government_satisfied_2007_2013, na.rm = TRUE)

ggplot(data = government, aes(x = countryOrdered, y = perceptions_about_government_trust_in_national_government_satisfied_2007_2013, ymax = max(perceptions_about_government_trust_in_national_government_satisfied_2007_2013), fill = hdi_classification)) + 
  geom_bar(colour = "black", stat = "identity", position = position_dodge()) +
  geom_text(aes(label = hdi_rank), position = position_dodge(width = 0.9), vjust = -0.5, colour = "red", fontface = "bold") +
  geom_text(aes(label = perceptions_about_government_trust_in_national_government_satisfied_2007_2013), position = position_dodge(width = 0.9), vjust = 20) +
  scale_fill_hue(name = "HDI classification") +
  coord_cartesian(ylim = c(40, 100)) +
  scale_y_continuous(breaks = seq(from = 40, to = 100, by = 5)) +
  xlab("Country") + ylab("Percentage of positive responses") +
  ggtitle("Government trust") +
  geom_hline(yintercept = governmentSatisMean, colour = "black", linetype = "dotted") + 
  annotate("text", 3, 50, label = "Government trust average", colour = "black", fontface = "bold")

# Explore perception about education quality: Select top 12
education <- final_dataset %>%
  arrange(desc(perceptions_of_individual_well_being_education_quality_satisfied_2012)) %>%
  filter(row_number() <= 12) %>%
  select(country, hdi_rank,perceptions_of_individual_well_being_education_quality_satisfied_2012) %>%
  mutate(hdi_classification = hdi_ranges[hdi_rank])

education$countryOrdered <- reorder(education$country, desc(education$perceptions_of_individual_well_being_education_quality_satisfied_2012))

educationSatisMean <- mean(final_dataset$perceptions_of_individual_well_being_education_quality_satisfied_2012, na.rm = TRUE)

ggplot(data = education, aes(x = countryOrdered, y = perceptions_of_individual_well_being_education_quality_satisfied_2012, ymax = max(perceptions_of_individual_well_being_education_quality_satisfied_2012), fill = hdi_classification)) + 
  geom_bar(colour = "black", stat = "identity", position = position_dodge()) +
  geom_text(aes(label = hdi_rank), position = position_dodge(width = 0.9), vjust = -0.5, colour = "red", fontface = "bold") +
  geom_text(aes(label = perceptions_of_individual_well_being_education_quality_satisfied_2012), position = position_dodge(width = 0.9), vjust = 20) +
  scale_fill_hue(name = "HDI classification") +
  coord_cartesian(ylim = c(55, 100)) +
  scale_y_continuous(breaks = seq(from = 55, to = 100, by = 5)) +
  xlab("Country") + ylab("Percentage of positive responses") +
  ggtitle("Education quality satisfaction") +
  geom_hline(yintercept = educationSatisMean, colour = "black", linetype = "dotted") + 
  annotate("text", 3.5, 61, label = "Education quality satisfaction average", colour = "black", fontface = "bold")

# Explore job satisfaction: Select top 12
jobs <- final_dataset %>%
  arrange(desc(perceptions_of_individual_well_being_job_satisfied_2007_2012)) %>%
  filter(row_number() <= 12) %>%
  select(country, hdi_rank, perceptions_of_individual_well_being_job_satisfied_2007_2012) %>%
  mutate(hdi_classification = hdi_ranges[hdi_rank])

jobs$countryOrdered <- reorder(jobs$country, desc(jobs$perceptions_of_individual_well_being_job_satisfied_2007_2012))

jobsSatisMean <- mean(final_dataset$perceptions_of_individual_well_being_job_satisfied_2007_2012, na.rm = TRUE)

ggplot(data = jobs, aes(x = countryOrdered, y = perceptions_of_individual_well_being_job_satisfied_2007_2012, ymax = max(perceptions_of_individual_well_being_job_satisfied_2007_2012), fill = hdi_classification)) + 
  geom_bar(colour = "black", stat = "identity", position = position_dodge()) +
  geom_text(aes(label = hdi_rank), position = position_dodge(width = 0.9), vjust = -0.5, colour = "red", fontface = "bold") +
  geom_text(aes(label = perceptions_of_individual_well_being_job_satisfied_2007_2012), position = position_dodge(width = 0.9), vjust = 15) +
  scale_fill_hue(name = "HDI classification") +
  coord_cartesian(ylim = c(55, 100)) +
  scale_y_continuous(breaks = seq(from = 55, to = 100, by = 5)) +
  xlab("Country") + ylab("Percentage of positive responses") +
  ggtitle("Job satisfaction") +
  geom_hline(yintercept = jobsSatisMean, colour = "black", linetype = "dotted") + 
  annotate("text", 3.5, 72, label = "Global job satisfaction average", colour = "black", fontface = "bold")

# Explore local labour market satisfaction: Select top 12
market <- final_dataset %>%
  arrange(desc(perceptions_about_community_local_labour_market_answering_good_2007_2012)) %>%
  filter(row_number() <= 12) %>%
  select(country, hdi_rank, perceptions_about_community_local_labour_market_answering_good_2007_2012) %>%
  mutate(hdi_classification = hdi_ranges[hdi_rank])

market$countryOrdered <- reorder(market$country, desc(market$perceptions_about_community_local_labour_market_answering_good_2007_2012))

marketSatisMean <- mean(final_dataset$perceptions_about_community_local_labour_market_answering_good_2007_2012, na.rm = TRUE)

ggplot(data = market, aes(x = countryOrdered, y = perceptions_about_community_local_labour_market_answering_good_2007_2012, ymax = max(perceptions_about_community_local_labour_market_answering_good_2007_2012), fill = hdi_classification)) + 
  geom_bar(colour = "black", stat = "identity", position = position_dodge()) +
  geom_text(aes(label = hdi_rank), position = position_dodge(width = 0.9), vjust = -0.5, colour = "red", fontface = "bold") +
  geom_text(aes(label = perceptions_about_community_local_labour_market_answering_good_2007_2012), position = position_dodge(width = 0.9), vjust = 15) +
  scale_fill_hue(name = "HDI classification") +
  coord_cartesian(ylim = c(20, 100)) +
  scale_y_continuous(breaks = seq(from = 20, to = 100, by = 5)) +
  xlab("Country") + ylab("Percentage of positive responses") +
  ggtitle("Local labour market satisfaction") +
  geom_hline(yintercept = marketSatisMean, colour = "black", linetype = "dotted") + 
  annotate("text", 4, 30, label = "Local labour market satisfaction average", colour = "black", fontface = "bold")

# Explore perception about community: Select top 12
community <- final_dataset %>%
  arrange(desc(perceptions_about_community_community_answering_yes_2007_2012)) %>%
  filter(row_number() <= 12) %>%
  select(country, hdi_rank, perceptions_about_community_community_answering_yes_2007_2012) %>%
  mutate(hdi_classification = hdi_ranges[hdi_rank])

community$countryOrdered <- reorder(community$country, desc(community$perceptions_about_community_community_answering_yes_2007_2012))

communitySatisMean <- mean(final_dataset$perceptions_about_community_community_answering_yes_2007_2012, na.rm = TRUE)

ggplot(data = community, aes(x = countryOrdered, y = perceptions_about_community_community_answering_yes_2007_2012, ymax = max(perceptions_about_community_community_answering_yes_2007_2012), fill = hdi_classification)) + 
  geom_bar(colour = "black", stat = "identity", position = position_dodge()) +
  geom_text(aes(label = hdi_rank), position = position_dodge(width = 0.9), vjust = -0.5, colour = "red", fontface = "bold") +
  geom_text(aes(label = perceptions_about_community_community_answering_yes_2007_2012), position = position_dodge(width = 0.9), vjust = 15) +
  scale_fill_hue(name = "HDI classification") +
  coord_cartesian(ylim = c(65, 100)) +
  scale_y_continuous(breaks = seq(from = 65, to = 100, by = 5)) +
  xlab("Country") + ylab("Percentage of positive responses") +
  ggtitle("Perceptions about community") +
  geom_hline(yintercept = communitySatisMean, colour = "black", linetype = "dotted") + 
  annotate("text", 4, 76, label = "Perceptions about community average", colour = "black", fontface = "bold")

# Explore perception about freedom of choice: Select top 12
freedom <- final_dataset %>%
  arrange(desc(perceptions_of_individual_well_being_freedom_of_choice_satisfied_2007_2012)) %>%
  filter(row_number() <= 12) %>%
  select(country, hdi_rank, perceptions_of_individual_well_being_freedom_of_choice_satisfied_2007_2012) %>%
  mutate(hdi_classification = hdi_ranges[hdi_rank])

freedom$countryOrdered <- reorder(freedom$country, desc(freedom$perceptions_of_individual_well_being_freedom_of_choice_satisfied_2007_2012))

freedomSatisMean <- mean(final_dataset$perceptions_of_individual_well_being_freedom_of_choice_satisfied_2007_2012, na.rm = TRUE)

ggplot(data = freedom, aes(x = countryOrdered, y = perceptions_of_individual_well_being_freedom_of_choice_satisfied_2007_2012, ymax = max(perceptions_of_individual_well_being_freedom_of_choice_satisfied_2007_2012), fill = hdi_classification)) + 
  geom_bar(colour = "black", stat = "identity", position = position_dodge()) +
  geom_text(aes(label = hdi_rank), position = position_dodge(width = 0.9), vjust = -0.5, colour = "red", fontface = "bold") +
  geom_text(aes(label = perceptions_of_individual_well_being_freedom_of_choice_satisfied_2007_2012), position = position_dodge(width = 0.9), vjust = 20) +
  scale_fill_hue(name = "HDI classification") +
  coord_cartesian(ylim = c(65, 100)) +
  scale_y_continuous(breaks = seq(from = 65, to = 100, by = 5)) +
  xlab("Country") + ylab("Percentage of positive responses") +
  ggtitle("Perceptions about freedom of choice") +
  geom_hline(yintercept = freedomSatisMean, colour = "black", linetype = "dotted") + 
  annotate("text", 4, 68, label = "Perceptions about freedom of choice average", colour = "black", fontface = "bold")

# Explore perception about safety: Select top 12
safety <- final_dataset %>%
  arrange(desc(perceptions_of_individual_well_being_safety_answering_yes_2007_2012)) %>%
  filter(row_number() <= 12) %>%
  select(country, hdi_rank, perceptions_of_individual_well_being_safety_answering_yes_2007_2012) %>%
  mutate(hdi_classification = hdi_ranges[hdi_rank])

safety$countryOrdered <- reorder(safety$country, desc(safety$perceptions_of_individual_well_being_safety_answering_yes_2007_2012))

safetySatisMean <- mean(final_dataset$perceptions_of_individual_well_being_safety_answering_yes_2007_2012, na.rm = TRUE)

ggplot(data = safety, aes(x = countryOrdered, y = perceptions_of_individual_well_being_safety_answering_yes_2007_2012, ymax = max(perceptions_of_individual_well_being_safety_answering_yes_2007_2012), fill = hdi_classification)) + 
  geom_bar(colour = "black", stat = "identity", position = position_dodge()) +
  geom_text(aes(label = hdi_rank), position = position_dodge(width = 0.9), vjust = -0.5, colour = "red", fontface = "bold") +
  geom_text(aes(label = perceptions_of_individual_well_being_safety_answering_yes_2007_2012), position = position_dodge(width = 0.9), vjust = 20) +
  scale_fill_hue(name = "HDI classification") +
  coord_cartesian(ylim = c(50, 100)) +
  scale_y_continuous(breaks = seq(from = 50, to = 100, by = 5)) +
  xlab("Country") + ylab("Percentage of positive responses") +
  ggtitle("Perceptions about safety") +
  geom_hline(yintercept = safetySatisMean, colour = "black", linetype = "dotted") + 
  annotate("text", 4, 60, label = "Perceptions about safety average", colour = "black", fontface = "bold")

# Explore perception about standard of living: Select top 12
standard <- final_dataset %>%
  arrange(desc(perceptions_of_individual_well_being_standard_of_living_satisfied_2007_2013)) %>%
  filter(row_number() <= 12) %>%
  select(country, hdi_rank, perceptions_of_individual_well_being_standard_of_living_satisfied_2007_2013) %>%
  mutate(hdi_classification = hdi_ranges[hdi_rank])

standard$countryOrdered <- reorder(standard$country, desc(standard$perceptions_of_individual_well_being_standard_of_living_satisfied_2007_2013))

standardSatisMean <- mean(final_dataset$perceptions_of_individual_well_being_standard_of_living_satisfied_2007_2013, na.rm = TRUE)

ggplot(data = standard, aes(x = countryOrdered, y = perceptions_of_individual_well_being_standard_of_living_satisfied_2007_2013, ymax = max(perceptions_of_individual_well_being_standard_of_living_satisfied_2007_2013), fill = hdi_classification)) + 
  geom_bar(colour = "black", stat = "identity", position = position_dodge()) +
  geom_text(aes(label = hdi_rank), position = position_dodge(width = 0.9), vjust = -0.5, colour = "red", fontface = "bold") +
  geom_text(aes(label = perceptions_of_individual_well_being_standard_of_living_satisfied_2007_2013), position = position_dodge(width = 0.9), vjust = 20) +
  scale_fill_hue(name = "HDI classification") +
  coord_cartesian(ylim = c(50, 100)) +
  scale_y_continuous(breaks = seq(from = 50, to = 100, by = 5)) +
  xlab("Country") + ylab("Percentage of positive responses") +
  ggtitle("Perceptions about standard of living") +
  geom_hline(yintercept = standardSatisMean, colour = "black", linetype = "dotted") + 
  annotate("text", 4, 56, label = "Perceptions about standard of living average", colour = "black", fontface = "bold")

# Explore perception about government dealing with the poor: Select top 12
dealWithPoor <- final_dataset %>%
  arrange(desc(perceptions_about_government_efforts_to_deal_with_the_poor_satisfied_2007_2013)) %>%
  filter(row_number() <= 12) %>%
  select(country, hdi_rank, perceptions_about_government_efforts_to_deal_with_the_poor_satisfied_2007_2013) %>%
  mutate(hdi_classification = hdi_ranges[hdi_rank])

dealWithPoor$countryOrdered <- reorder(dealWithPoor$country, desc(dealWithPoor$perceptions_about_government_efforts_to_deal_with_the_poor_satisfied_2007_2013))

dealWithPoorSatisMean <- mean(final_dataset$perceptions_about_government_efforts_to_deal_with_the_poor_satisfied_2007_2013, na.rm = TRUE)

ggplot(data = dealWithPoor, aes(x = countryOrdered, y = perceptions_about_government_efforts_to_deal_with_the_poor_satisfied_2007_2013, ymax = max(perceptions_about_government_efforts_to_deal_with_the_poor_satisfied_2007_2013), fill = hdi_classification)) + 
  geom_bar(colour = "black", stat = "identity", position = position_dodge()) +
  geom_text(aes(label = hdi_rank), position = position_dodge(width = 0.9), vjust = -0.5, colour = "red", fontface = "bold") +
  geom_text(aes(label = perceptions_about_government_efforts_to_deal_with_the_poor_satisfied_2007_2013), position = position_dodge(width = 0.9), vjust = 20) +
  scale_fill_hue(name = "HDI classification") +
  coord_cartesian(ylim = c(30, 100)) +
  scale_y_continuous(breaks = seq(from = 30, to = 100, by = 5)) +
  xlab("Country") + ylab("Percentage of positive responses") +
  ggtitle("Perceptions about government dealing with the poor") +
  geom_hline(yintercept = dealWithPoorSatisMean, colour = "black", linetype = "dotted") + 
  annotate("text", 4, 35.5, label = "Dealing with poor perception average", colour = "black", fontface = "bold")

# Explore perception about government preserving environment: Select top 12
envPreservation <- final_dataset %>%
  arrange(desc(perceptions_about_government_actions_to_preserve_the_environment_satisfied_2007_2013)) %>%
  filter(row_number() <= 12) %>%
  select(country, hdi_rank, perceptions_about_government_actions_to_preserve_the_environment_satisfied_2007_2013) %>%
  mutate(hdi_classification = hdi_ranges[hdi_rank])

envPreservation$countryOrdered <- reorder(envPreservation$country, desc(envPreservation$perceptions_about_government_actions_to_preserve_the_environment_satisfied_2007_2013))

envPreservationSatisMean <- mean(final_dataset$perceptions_about_government_actions_to_preserve_the_environment_satisfied_2007_2013, na.rm = TRUE)

ggplot(data = envPreservation, aes(x = countryOrdered, y = perceptions_about_government_actions_to_preserve_the_environment_satisfied_2007_2013, ymax = max(perceptions_about_government_actions_to_preserve_the_environment_satisfied_2007_2013), fill = hdi_classification)) + 
  geom_bar(colour = "black", stat = "identity", position = position_dodge()) +
  geom_text(aes(label = hdi_rank), position = position_dodge(width = 0.9), vjust = -0.5, colour = "red", fontface = "bold") +
  geom_text(aes(label = perceptions_about_government_actions_to_preserve_the_environment_satisfied_2007_2013), position = position_dodge(width = 0.9), vjust = 20) +
  scale_fill_hue(name = "HDI classification") +
  coord_cartesian(ylim = c(40, 100)) +
  scale_y_continuous(breaks = seq(from = 40, to = 100, by = 5)) +
  xlab("Country") + ylab("Percentage of positive responses") +
  ggtitle("Perceptions about government preserving environment") +
  geom_hline(yintercept = envPreservationSatisMean, colour = "black", linetype = "dotted") + 
  annotate("text", 4, 49, label = "Environment preservation perception average", colour = "black", fontface = "bold")

# Explore perception about health care quality: Select top 12
healthCare <- final_dataset %>%
  arrange(desc(perceptions_of_individual_well_being_health_care_quality_satisfied_2008_2012)) %>%
  filter(row_number() <= 12) %>%
  select(country, hdi_rank, perceptions_of_individual_well_being_health_care_quality_satisfied_2008_2012) %>%
  mutate(hdi_classification = hdi_ranges[hdi_rank])

healthCare$countryOrdered <- reorder(healthCare$country, desc(healthCare$perceptions_of_individual_well_being_health_care_quality_satisfied_2008_2012))

healthCareSatisMean <- mean(final_dataset$perceptions_of_individual_well_being_health_care_quality_satisfied_2008_2012, na.rm = TRUE)

ggplot(data = healthCare, aes(x = countryOrdered, y = perceptions_of_individual_well_being_health_care_quality_satisfied_2008_2012, ymax = max(perceptions_of_individual_well_being_health_care_quality_satisfied_2008_2012), fill = hdi_classification)) + 
  geom_bar(colour = "black", stat = "identity", position = position_dodge()) +
  geom_text(aes(label = hdi_rank), position = position_dodge(width = 0.9), vjust = -0.5, colour = "red", fontface = "bold") +
  geom_text(aes(label = perceptions_of_individual_well_being_health_care_quality_satisfied_2008_2012), position = position_dodge(width = 0.9), vjust = 20) +
  scale_fill_hue(name = "HDI classification") +
  coord_cartesian(ylim = c(50, 100)) +
  scale_y_continuous(breaks = seq(from = 50, to = 100, by = 5)) +
  xlab("Country") + ylab("Percentage of positive responses") +
  ggtitle("Perceptions about health care quality") +
  geom_hline(yintercept = healthCareSatisMean, colour = "black", linetype = "dotted") + 
  annotate("text", 4, 54, label = "Health care quality perception average", colour = "black", fontface = "bold")

# Explore perception about community trusting others: Select top 12
trustOthers <- final_dataset %>%
  arrange(desc(perceptions_about_community_trust_in_other_people_answering_can_be_trusted_2009_2011)) %>%
  filter(row_number() <= 12) %>%
  select(country, hdi_rank, perceptions_about_community_trust_in_other_people_answering_can_be_trusted_2009_2011) %>%
  mutate(hdi_classification = hdi_ranges[hdi_rank])

trustOthers$countryOrdered <- reorder(trustOthers$country, desc(trustOthers$perceptions_about_community_trust_in_other_people_answering_can_be_trusted_2009_2011))

trustOthersSatisMean <- mean(final_dataset$perceptions_about_community_trust_in_other_people_answering_can_be_trusted_2009_2011, na.rm = TRUE)

ggplot(data = trustOthers, aes(x = countryOrdered, y = perceptions_about_community_trust_in_other_people_answering_can_be_trusted_2009_2011, ymax = max(perceptions_about_community_trust_in_other_people_answering_can_be_trusted_2009_2011), fill = hdi_classification)) + 
  geom_bar(colour = "black", stat = "identity", position = position_dodge()) +
  geom_text(aes(label = hdi_rank), position = position_dodge(width = 0.9), vjust = -0.5, colour = "red", fontface = "bold") +
  geom_text(aes(label = perceptions_about_community_trust_in_other_people_answering_can_be_trusted_2009_2011), position = position_dodge(width = 0.9), vjust = 8) +
  scale_fill_hue(name = "HDI classification") +
  coord_cartesian(ylim = c(15, 100)) +
  scale_y_continuous(breaks = seq(from = 15, to = 100, by = 5)) +
  xlab("Country") + ylab("Percentage of positive responses") +
  ggtitle("Perceptions about community trusting other people") +
  geom_hline(yintercept = trustOthersSatisMean, colour = "black", linetype = "dotted") + 
  annotate("text", 4, 22, label = "Community trust perception average", colour = "black", fontface = "bold")

# Obtain a dataset with complete cases only.
complete_cases_dataset <- final_dataset %>% filter(complete.cases(.))
# Obtain another witn non-NA values in the overall life satisfaction index (0-10).
non_na_overall_index_dataset <- final_dataset %>% filter(!is.na(perceptions_of_individual_well_being_overall_life_satisfaction_index_0_least_satisfied_10_most_satisfied_2007_2012))

# Add a column with the total score.
complete_cases_dataset <- complete_cases_dataset %>% mutate(total_score = rowSums(complete_cases_dataset[c(3:11, 13:15)], na.rm = TRUE))
non_na_overall_index_dataset <- non_na_overall_index_dataset %>% mutate(total_score = rowSums(non_na_overall_index_dataset[c(3:11, 13:15)], na.rm = TRUE)) %>% mutate(number_of_nas = apply(non_na_overall_index_dataset, 1, function(z) sum(is.na(z))))

# Explore totals: Select top 15 (complete cases only)
complete <- complete_cases_dataset %>%
  arrange(desc(total_score)) %>%
  filter(row_number() <= 15) %>%
  select(country, hdi_rank, total_score) %>%
  mutate(hdi_classification = hdi_ranges[hdi_rank])

complete$countryOrdered <- reorder(complete$country, desc(complete$total_score))

ggplot(data = complete, aes(x = countryOrdered, y = total_score, ymax = max(total_score), fill = hdi_classification)) + 
  geom_bar(colour = "black", stat = "identity", position = position_dodge()) +
  geom_text(aes(label = hdi_rank), position = position_dodge(width = 0.9), vjust = -0.5, colour = "red", fontface = "bold") +
  geom_text(aes(label = total_score), position = position_dodge(width = 0.9), vjust = 15) +
  scale_fill_hue(name = "HDI classification") +
  coord_cartesian(ylim = c(700, 1000)) +
  scale_y_continuous(breaks = seq(from = 700, to = 1000, by = 25)) +
  xlab("Country") + ylab("Percentage of positive responses") +
  ggtitle("Total positive perception scores")

# Explore totals: Select top 15 (including NAs)
non_na_top <- non_na_overall_index_dataset %>%
  arrange(desc(total_score)) %>%
  filter(row_number() <= 15) %>%
  select(country, hdi_rank, total_score, number_of_nas) %>%
  mutate(hdi_classification = hdi_ranges[hdi_rank])

non_na_top$countryOrdered <- reorder(non_na_top$country, desc(non_na_top$total_score))

ggplot(data = non_na_top, aes(x = countryOrdered, y = total_score, ymax = max(total_score), fill = hdi_classification)) + 
  geom_bar(colour = "black", stat = "identity", position = position_dodge()) +
  geom_text(aes(label = hdi_rank), position = position_dodge(width = 0.9), vjust = -0.5, colour = "red", fontface = "bold") +
  geom_text(aes(label = number_of_nas), position = position_dodge(width = 0.9), vjust = -2.5, colour = "blue", fontface = "bold") +
  geom_text(aes(label = total_score), position = position_dodge(width = 0.9), vjust = 15) +
  scale_fill_hue(name = "HDI classification") +
  coord_cartesian(ylim = c(700, 1000)) +
  scale_y_continuous(breaks = seq(from = 700, to = 1000, by = 25)) +
  xlab("Country") + ylab("Percentage of positive responses") +
  ggtitle("Total positive perception scores (incl. categories w/NA values)")

library(GGally)

final_dataset <- final_dataset %>% mutate(hdi_classification = hdi_ranges[hdi_rank])

lm_eqn <- function(m) {

  l <- list(a = format(coef(m)[1], digits = 2),
    b = format(abs(coef(m)[2]), digits = 2),
    r2 = format(summary(m)$r.squared, digits = 3));

  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }

  as.character(as.expression(eq));                 
}

relevantColumns <- colnames(final_dataset)[3:15]

plotCorrelation <- function(column) {
  print(ggplot(final_dataset, aes(x = hdi_rank, y = column)) +
    geom_smooth(method = "lm") +
    geom_point(aes(color = hdi_classification)) +
    geom_text(aes(x = 50, y = 100, label = lm_eqn(lm(column ~ hdi_rank, final_dataset))), parse = TRUE))
}

for (i in 1:length(relevantColumns)) {
  funCall = paste(relevantColumns[i], "~", "hdi_rank", collapse=" + ")
  lmRes = do.call("lm", list(as.formula(funCall), data=as.name("final_dataset")))
  print(ggplot(final_dataset, aes_string(x = "hdi_rank", y = relevantColumns[i])) +
      geom_smooth(method = "lm") +
      geom_point(aes(color = hdi_classification)) +
      geom_text(aes(x = 50, y = 100, label = lm_eqn(lmRes), parse = TRUE)))
}

ggplot(final_dataset, aes(x = hdi_rank, y = perceptions_about_community_local_labour_market_answering_good_2007_2012)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = hdi_classification)) +
  geom_text(aes(x = 50, y = 100, label = lm_eqn(lm(perceptions_about_community_local_labour_market_answering_good_2007_2012 ~ hdi_rank, final_dataset))), parse = TRUE)

ggplot(final_dataset, aes(x = hdi_rank, y = perceptions_about_community_local_labour_market_answering_good_2007_2012)) +
  geom_smooth(method = "lm") +
  geom_point(aes(color = hdi_classification)) +
  geom_text(aes(x = 50, y = 100, label = lm_eqn(lm(perceptions_about_community_local_labour_market_answering_good_2007_2012 ~ hdi_rank, final_dataset))), parse = TRUE)


