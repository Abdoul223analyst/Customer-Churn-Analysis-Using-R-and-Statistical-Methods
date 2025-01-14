# Load necessary libraries
library(ggplot2)
library(dplyr)
install.packages("dplyr")
# Load the dataset
Telco=read.csv(file.choose())
View(Telco)

# Separate discrete and continuous variables
discrete_vars <- c("CustomerID", "Count", "Country", "State", "City", 
                   "Zip.Code", "Gender", "Senior.Citizen", "Partner", 
                   "Dependents", "Phone.Service", "Multiple.Lines", 
                   "Internet.Service", "Online.Security", "Online.Backup", 
                   "Device.Protection", "Tech.Support", "Streaming.TV", 
                   "Streaming.Movies", "Contract", "Paperless.Billing", 
                   "Payment.Method", "Churn.Label", "Churn.Value")

continuous_vars <- c("Tenure.Months", "Monthly.Charge", "Total.Charges", 
                     "Churn.Score", "CLTV")

# Calculate probabilities for binomial distributions
p_stay <- mean(Telco$Churn.Value == 0)
p_stay
# P(X = 350) for 500 customers
dbinom(350, size = 500, prob = p_stay)

# P(X = 160) for 500 customers leaving
p_leave <- 1 - p_stay
p_leave
dbinom(160, size = 500, prob = p_leave)

# P(X < 300) for 1000 customers leaving
pbinom(299, size = 1000, prob = p_leave)

# P(X > 300) for 1000 customers leaving
1 - pbinom(300, size = 1000, prob = p_leave)

senior_citizens <- subset(Telco, Senior.Citizen == "Yes")
p_senior_stay <- mean(senior_citizens$Churn.Value == 0)
p_senior_stay

# P(X = 200) for 500 senior citizens staying
dbinom(200, size = 500, prob = p_senior_stay)

# P(X = 285) for 500 senior citizens leaving
p_senior_leave <- 1 - p_senior_stay
p_senior_leave
dbinom(285, size = 500, prob = p_senior_leave)

# P(X > 285) for 500 senior citizens leaving
1 - pbinom(285, size = 500, prob = p_senior_leave)

# Probabilities from 160 to 260
probs <- dbinom(160:260, size = 500, prob = p_senior_stay)
probs
plot(160:260, probs, type = "h", col = "blue", 
     xlab = "Number of Customers Staying", ylab = "Probability")
# Churn rate by gender
gender_churn <- Telco %>%
  group_by(Gender) %>%
  summarise(ChurnRate = mean(Churn.Value == 1) * 100)

# Visualization
ggplot(gender_churn, aes(x = Gender, y = ChurnRate, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Churn Rate by Gender", x = "Gender", y = "Churn Rate (%)") +
  theme_minimal()

# Churn rate by contract type
contract_churn <- Telco %>%
  group_by(Contract) %>%
  summarise(ChurnRate = mean(Churn.Value == 1) * 100)

# Visualization
ggplot(contract_churn, aes(x = Contract, y = ChurnRate, fill = Contract)) +
  geom_bar(stat = "identity") +
  labs(title = "Churn Rate by Contract Type", x = "Contract Type", y = "Churn Rate (%)") +
  theme_minimal()


# Monthly charges distribution by churn status
ggplot(Telco, aes(x = Monthly.Charge, fill = factor(Churn.Value))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Monthly Charges by Churn Status",
       x = "Monthly Charges", y = "Density", fill = "Churn Status") +
  theme_minimal()

# Tenure distribution by contract type
ggplot(Telco, aes(x = Tenure.Months, fill = Contract)) +
  geom_density(alpha = 0.5) +
  labs(title = "Tenure Distribution by Contract Type",
       x = "Tenure (Months)", y = "Density", fill = "Contract Type") +
  theme_minimal()

#correlation 
# Select numeric variables
numeric_data <- Telco %>%
  select_if(is.numeric)

# Calculate correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

# Visualize the heatmap
library(reshape2)
cor_melted <- melt(cor_matrix)

ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "Variables", y = "Variables", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Churn rate by payment method
payment_churn <- Telco %>%
  group_by(Payment.Method) %>%
  summarise(ChurnRate = mean(Churn.Value == 1) * 100)

# Visualization
ggplot(payment_churn, aes(x = Payment.Method, y = ChurnRate, fill = Payment.Method)) +
  geom_bar(stat = "identity") +
  labs(title = "Churn Rate by Payment Method",
       x = "Payment Method", y = "Churn Rate (%)") +
  theme_minimal()

# Churn rate by senior citizen status
senior_churn <- Telco %>%
  group_by(Senior.Citizen) %>%
  summarise(ChurnRate = mean(Churn.Value == 1) * 100)

# Visualization
ggplot(senior_churn, aes(x = Senior.Citizen, y = ChurnRate, fill = Senior.Citizen)) +
  geom_bar(stat = "identity") +
  labs(title = "Churn Rate by Senior Citizen Status",
       x = "Senior Citizen", y = "Churn Rate (%)") +
  theme_minimal()






