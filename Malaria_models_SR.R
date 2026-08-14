##A. Setting a working directory
setwd("C:/Users/Dorcas Nzilani/Desktop/Jalorche Projects/Malaria Project/Malaria Visuals")
##B. Load packages
install.packages("patchwork")
install.packages("ggthemes")
install.packages("ggpubr")
install.packages("viridis")
install.packages("data.table")
library(data.table)
library(tidyverse)
library(ggplot2)
library(patchwork)  ### for combining plots
library(ggthemes)
library(ggpubr)
library(viridis)
library(dplyr)      # Data manipulation
library(labelled)   # Variable labels
library(janitor)    # Tabulations
library(haven)

##C. Import dataset
Malaria_Models_dta <- read_dta("Malaria_Model_Extraction_Template.xlsx - Sheet.dta")
view(Malaria_Models_dta)

##D. Renaming variables
Malaria_Models_dta <- Malaria_Models_dta %>% 
  rename(
    country = Country,
    policy_question = Primary_Policy_Question,
    target_decision_maker = Target_Decision_Maker,
    vaccine_evaluated = Vaccine_Evaluated,
    vaccine_delivery = Vaccine_Delivery_Strategy,
    comparator = Comparators_,
    model_type = Model_Type,
    model_structure = Model_Structure,
    model_purpose = Purpose_of_Model,
    time_horizon = Time_horizon,
    perpective_ = Perspective,
    discount_rate = Discounting_rate
  )
##E. Maintaining variable labels
factor_vars <- c(
  "country",
  "policy_question",
  "target_decision_maker",
  "vaccine_evaluated",
  "vaccine_delivery",
  "comparator",
  "model_type",
  "model_structure",
  "model_purpose",
  "time_horizon",
  "perpective_",
  "discount_rate"
)

Malaria_Models_dta <- Malaria_Models_dta %>%
  mutate(
    across(
      all_of(factor_vars),
      haven::as_factor
    )
  )

##E. Subset selection
selected <- c("authorsyear","country", "policy_question","target_decision_maker","vaccine_evaluated","vaccine_delivery","comparator","model_type","model_structure","model_purpose","time_horizon","perpective_","discount_rate")
subset_malaria_data <- Malaria_Models_dta %>% 
  select(all_of(selected))

##F. Checking var structure
str(subset_malaria_data)
to_factor <- c("country", "policy_question","target_decision_maker","vaccine_evaluated","vaccine_delivery","comparator","model_type","model_structure","model_purpose","perpective_")
subset_malaria_data <- subset_malaria_data %>%
  mutate(across(all_of(to_factor), as.factor))
str(subset_malaria_data)

##G. Creating a bar graph for Publications by country and author-year

country_plot <- ggplot(
  subset_malaria_data,
  aes(
    x = country,
    fill = authorsyear
  )
) +
  geom_bar() +
  labs(
    title = "Distribution of studies by Country",
    x = "Country",
    y = "Number of Publications",
    fill = "Author-Year"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "right"
  )
## F. Distribution of studies by target decision maker and delivery strategy

Delivery_strategy_plot <- ggplot(
  subset_malaria_data,
  aes(
    x = target_decision_maker,
    fill = vaccine_delivery
  )
) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribution of Studies by Target decision maker and Delivery Strategy",
    x = "Target decision maker",
    y = "Number of Studies",
    fill = "Vaccine Delivery Strategy"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

## Distribution by country by vaccine type
vaccine_type_plot <- ggplot(
  subset_malaria_data,
  aes(
    x = country,
    fill = vaccine_evaluated
  )
) +
  geom_bar() +
  labs(
    title = "Distribution of studies by Country and vaccine evaluated",
    x = "Country",
    y = "Number of Publications",
    fill = "Vaccine evaluated"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    legend.position = "right"
  )

library(patchwork)
combined_figure <- (vaccine_type_plot) /
  Delivery_strategy_plot+
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(
        size = 14,
        face = "bold"
      )
    )
  )

combined_figure

## Distribution by comparators
comparator_plot <- ggplot(
  subset_malaria_data,
  aes(x = comparator)
) +
  geom_bar(fill = "grey") +
  labs(
    x = "Comparator",
    y = "Number of Studies"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

comparator_plot

## Distribution by model type
model_type_plot <- ggplot(
  subset_malaria_data,
  aes(x = model_type)
) +
  geom_bar(fill = "grey") +
  labs(
    x = "Model type",
    y = "Number of Studies"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

model_type_plot

## Distribution by model structure
model_structure_plot <- ggplot(
  subset_malaria_data,
  aes(x = model_structure)
) +
  geom_bar(fill = "grey") +
  labs(
    x = "Model structure",
    y = "Number of Studies"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

model_structure_plot

## Distribution by purpose of model
model_purpose_plot <- ggplot(
  subset_malaria_data,
  aes(x = model_purpose)
) +
  geom_bar(fill = "grey") +
  labs(
    x = "Model purpose",
    y = "Number of Studies"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

model_purpose_plot

## Distribution by time horizon----to check
time_horizon_plot <- ggplot(
  subset_malaria_data,
  aes(x = time_horizon)
) +
  geom_bar(fill = "grey") +
  labs(
    x = "time horizon",
    y = "Number of Studies"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

time_horizon_plot

## Distribution by perspective
perspective_plot <- ggplot(
  subset_malaria_data,
  aes(x = perpective_)
) +
  geom_bar(fill = "grey") +
  labs(
    x = "Perspective",
    y = "Number of Studies"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

perspective_plot

## Distribution by discounting rate -----to check
discount_plot <- ggplot(
  subset_malaria_data,
  aes(x = discount_rate)
) +
  geom_bar(fill = "grey") +
  labs(
    x = "Discounting rate",
    y = "Number of Studies"
  ) +
  theme_classic() +
  theme(
    panel.grid.major = element_line(),
    panel.grid.minor = element_line(),
    legend.position = "none",
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

discount_plot

library(patchwork)
combined_figure <- (model_type_plot + model_structure_plot) /
  (model_purpose_plot + perspective_plot)+
  plot_annotation(
    tag_levels = "A",
    theme = theme(
      plot.tag = element_text(
        size = 14,
        face = "bold"
      )
    )
  )

combined_figure

