library(dplyr)
library(readr)
library(purrr)

library(rpart)
library(ggplot2)

set.seed(1234)

# DATA, CREATE TRAIN/TEST SPLIT

# https://www.kaggle.com/harlfoxem/housesalesprediction/data
data <- read_csv("wk02/lab/data/kc_house_data.csv")

test_ratio <- 0.5
data_train <- data %>% sample_frac(test_ratio)
data_test <- anti_join(data, data_train, by = "id")

# ESTIMATION AND EVALUATION

RMSE <- function(x, true_x) sum((x - true_x)^2)

regularization_parameters <- c(1, 2, 5, 10, 20, 30, 40, 50) # minbucket

results <- map_df
    regularization_parameters,
    ~ {
        param <- .
        model <- rpart(
            formula = log(price) ~ .,
            data = data_train %>% select(-id, - date),
            control = rpart.control(xval = 0, cp = 0.00001, minbucket = param),
            method = "anova"
        )

        train_error <- RMSE(predict(model, data_train), log(data_train[["price"]]))
        test_error <- RMSE(predict(model, data_test), log(data_test[["price"]]))
        data_frame("error_type" = c("train", "test"),
                   "error_value" = c(train_error, test_error),
                   "parameter" = param)
    }
) %>% rbind()

# PLOT TRAIN AND TEST ERRORS

ggplot(results) + geom_line(aes(x = parameter, y = error_value, color = error_type))
