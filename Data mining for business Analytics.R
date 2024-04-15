 # Load the dataset into R

 library(readr)
 fitness_zone_1_ <- read_csv("C:/Users/USER/Downloads/fitness_zone(1).csv")
 
 View(fitness_zone_1_)
 
 # Identify numeric  variables
 
 numeric_vars <- sapply(fitness_zone_1_, is.numeric)
 
 # Identify categorical variables
 
 categorical_vars <- sapply(fitness_zone_1_, function(x) !is.numeric(x))
 
 # Check which variables are categorical
  categorical_vars <- sapply(fitness_zone_1_, function(x) is.factor(x) || is.character(x))
  categorical_vars_names <- names(categorical_vars[categorical_vars == TRUE])
 
 # Check which variables are numeric
  numeric_vars <- sapply(fitness_zone_1_, is.numeric)
  numeric_vars_names <- names(numeric_vars[numeric_vars == TRUE])
 
 # Print the lists of categorical and numeric variables
  print("Categorical Variables:")
  print(categorical_vars_names)
  print("Numeric Variables:")
  print(numeric_vars_names)
 
  # Generate table showing missingness by column
  missing_table <- sapply(fitness_zone_1_, function(x) sum(is.na(x)))
  missing_table
  
  # Assuming word-based variables are factors, convert them to factors
  word_based_vars <- c("booking_id", "month_as_member", "weight", "days_before","day_of_week", "time", "category", "attended" )
  
  # Replace with actual variable names
  fitness_zone_1_ [word_based_vars] <- lapply(fitness_zone_1_[word_based_vars], as.factor)
  
  # Define the word-based variables that you want to convert to factors
  word_based_vars <- c("variable1", "variable2", ...)  # Replace with actual variable names
  
  # Check if the variables exist in the dataset
  existing_vars <- word_based_vars[word_based_vars %in% names(fitness_zone_1_)]
  
  # Convert existing word-based variables to factors
  fitness_zone_1_[existing_vars] <- lapply(fitness_zone_1_[existing_vars], as.factor)
  
  # Print existing_vars
  print("existing_vars:")
  print(existing_vars)
  
  # Identify outcome classes for 'attended'
  outcome_classes <- unique(fitness_zone_1_$attended)
  
   # Print outcome
  print("outcome_clases:")
  print(outcome_classes)
  
  prevalence <- prop.table(table(fitness_zone_1_$attended))
  
  # Print outcome
  print("prevalece:")
  print(prevalence)
  
  # Convert 'attended' into a factor
  fitness_zone_1_$attended <- as.factor(fitness_zone_1_$attended)
  
  # Print outcome
  print("fitness_zone_1_$attended:")
  print(fitness_zone_1_$attended)
  
  # Drop 'booking_id' variable
  fitness_zone_1_ <- subset(fitness_zone_1_, select = -c(booking_id))
  
  # Assuming 'numeric_var' is the numeric variable to be binned
  # Bin numeric variables using equal frequency binning
  fitness_zone_1_$numeric_var_binned <- cut(fitness_zone_1_$numeric_vars, breaks = nlevels, labels = c("month_as_member", "weight", "days_before","day_of_week", "time", "category", "attended"))
  
  
  
  
  
  # List of numeric variable names you want to bin
  numeric_vars <- c("months_as_member", "weight", "booking_id")  # Replace with actual variable names
  
  # Choose the number of bins
  num_bins <- 3  # You can adjust this number as needed
  
  # Loop through each numeric variable and bin them
  for (var in numeric_vars) {
    # Bin the numeric variable using equal frequency binning
    fitness_zone_1_[[paste0(var, "_binned")]] <- cut(fitness_zone_1_[[var]], 
                                                    breaks = quantile(fitness_zone_1_[[var]], probs = seq(0, 1, length.out = num_bins + 1), na.rm = TRUE), 
                                                    labels = c("bin1", "bin2", "bin3"), 
                                                    include.lowest = TRUE)
  }
  
  class(fitness_zone_1_[[var]])
  
  selected_columns <- fitness_zone_1_[["months_as_member", "weight", "booking_id"]]
  
  
  
  fitness_zone_1_[[var]] <- as.numeric(fitness_zone_1_[[var]])
  
  
  # Check variable type
  class(fitness_zone_1[[var]])
  
  # Convert to numeric if needed
  fitness_zone_1[[var]] <- as.numeric(fitness_zone_1[[var]])
  
  # Handle missing values
  fitness_zone_1_ <- na.omit(fitness_zone_1_)
  
  # Bin the numeric variable using equal frequency binning
  fitness_zone_1_[[paste0(var, "_binned")]] <- cut(fitness_zone_1_[[var]], 
                                                  breaks = quantile(fitness_zone_1_[[var]], probs = seq(0, 1, length.out = num_bins + 1), na.rm = TRUE), 
                                                  labels = c("bin1", "bin2", "bin3"), 
                                                  include.lowest = TRUE)
  
  str(fitness_zone_1_)
  
  
  selected_column <- fitness_zone_1_[[numeric_vars_names]]
  
  
  
  # Re-run binning process
  fitness_zone_1_$numeric_var_binned <- cut(fitness_zone_1_$numeric_var)
  
  
  
  
  # Assuming 'numeric_var_binned' is the name of the binned variable
  # Display the frequency table
  freq_table <- table(fitness_zone_1_[[paste0(var, "_binned")]])
  print(freq_table)
  
  
  # Handling NA values for the numeric variable
  levels(fitness_zone_1_[[paste0(var, "_binned")]])[is.na(fitness_zone_1_[[paste0(var, "_binned")]])] <- "NA"
  
  # Show the levels for the variable that originally contained some NA values
  table(fitness_zone_1_[[paste0(var, "_binned")]], useNA = "ifany")
  
  
  # Bin the numeric variable with handling missing values
  fitness_zone_1$numeric_var_binned <- cut(fitness_zone_1$numeric_var, 
                                           breaks = quantile(fitness_zone_1$numeric_var, probs = seq(0, 1, length.out = num_bins + 1), na.rm = TRUE), 
                                           labels = c("bin1", "bin2", "bin3", "bin4", "bin5"), 
                                           include.lowest = TRUE)
  
  # Replace NAs with a new level in the binned variable
  levels(fitness_zone_1$numeric_var_binned)[is.na(fitness_zone_1$numeric_var_binned)] <- "NA"
  
  
  # Use table() function to count frequency of each level in the variable
  level_counts <- table(fitness_zone_1_[[paste0(var, "_binned")]], useNA = "always")
  
  # Print the frequency table
  print(level_counts)
  
  # Set seed for reproducibility (use the same seed value from Assignment #2)
  set.seed(123) 
  
  # Partition the data into training (60%) and validation (40%) sets
  train_indices <- sample(seq_len(nrow(fitness_zone_1_)), size = 0.6 * nrow(fitness_zone_1_))
  print(train_indices)
  
  train_data <- fitness_zone_1_[train_indices, ]
  validation_data <- fitness_zone_1_[-train_indices, ]
  
  
 ####for months_as_member
    
    
    
    # Load necessary libraries
    library(ggplot2)
    
    # Sample data (replace this with your actual train_data)
    train_data <- data.frame(
      month_as_member = sample(1:1500, 100, replace = TRUE),
      attended = sample(c("Yes", "No"), 100, replace = TRUE)
    )
    
    # Bin numeric values of 'month_as_member' into categories
    train_data$month_category <- cut(train_data$month_as_member, 
                                     breaks = c(0, 250, 500, 750, 1000, 1500),
                                     labels = c("0-250", "251-500", "501-750", "751-1000", "1001-1500"))
    
    # Define the input variable for the barplot
    input_variable <- "month_category"
    
    # Generate the barplot
    p <- ggplot(train_data, aes(x = factor(.data[[input_variable]]), fill = attended)) +
      geom_bar(position = "fill") +
      labs(title = paste("Proportional Barplot for", input_variable),
           x = input_variable, y = "Proportion") +
      theme_minimal()
    
    # Print the plot
    print(p)
    
    
    ### for weight
    
    # Load necessary libraries
    library(ggplot2)
    
    # Sample data (replace this with your actual train_data)
    train_data1 <- data.frame(
      weight = sample(1:1500, 100, replace = TRUE),  # Example numeric values for weight
      attended = sample(c("Yes", "No"), 100, replace = TRUE)
    )
    
    # Bin numeric values of 'weight' into categories
    train_data1$weight_category <- cut(train_data1$weight, 
                                      breaks = c(0, 300, 600, 900, 1200, 1500),
                                      labels = c("0-300", "301-600", "601-900", "901-1200", "1201-1500"))
    
    # Define the input variable for the barplot
    input_variable <- "weight_category"
    
    # Generate the barplot
    p <- ggplot(train_data, aes(x = factor(.data[[input_variable]]), fill = attended)) +
      geom_bar(position = "fill") +
      labs(title = paste("Proportional Barplot for", input_variable),
           x = "Weight Category", y = "Proportion") +
      theme_minimal()
    
    # Print the plot
    print(p)
    
    # Check column names of train_data
    colnames(train_data1)
    
    
    # Bin numeric values of 'weight' into categories
    train_data$weight_category <- cut(train_data$weight, 
                                      breaks = c(0, 300, 600, 900, 1200, 1500),
                                      labels = c("0-300", "301-600", "601-900", "901-1200", "1201-1500"))
    
    
    
    
    # Check the data type of the 'weight' column
    class(train_data1$weight)
    
    # Convert the 'weight' column to numeric if it's not already
    train_data1$weight <- as.numeric(as.character(train_data$weight))
    
    # Now, bin numeric values of 'weight' into categories
    train_data$weight_category <- cut(train_data$weight, 
                                      breaks = c(0, 300, 600, 900, 1200, 1500),
                                      labels = c("0-300", "301-600", "601-900", "901-1200", "1201-1500"))
    
    # Define the input variable for the barplot
    input_variable <- "weight_category"
    
    # Generate the barplot
    p <- ggplot(train_data, aes(x = factor(.data[[input_variable]]), fill = attended)) +
      geom_bar(position = "fill") +
      labs(title = paste("Proportional Barplot for", input_variable),
           x = "Weight Category", y = "Proportion") +
      theme_minimal()
    
    # Print the plot
    print(p)
    
    
    
    
    
    
    
    
    
    # Check the contents of the 'weight' column
    head(train_data1$weight)
    
    
    # Remove rows with missing values in the 'weight' column
    train_data <- na.omit(train_data1)
    
    # Convert the 'weight' column to numeric
    train_data1$weight <- as.numeric(as.character(train_data1$weight))
    
    
    # Load the dplyr package
    library(dplyr)
    
    # Replace missing values in the 'weight' column with 0
    train_data <- train_data %>%
      mutate(weight = coalesce(weight, 0))
    
    # Convert the 'weight' column to numeric
    train_data1$weight <- as.numeric(as.character(train_data1$weight))
    
    
    ## for days_before
    
    # Load necessary libraries
    library(ggplot2)
    
    
    
    # Sample data (replace this with your actual train_data)
    train_data2 <- data.frame(
      days_before = sample(1:1500, 100, replace = TRUE),  # Example numeric values for days_before
      attended = sample(c("Yes", "No"), 100, replace = TRUE)
    )
    
    # Bin numeric values of 'days_before' into categories
    train_data2$days_before_category <- cut(train_data2$days_before, 
                                           breaks = c(0, 300, 600, 900, 1200, 1500),
                                           labels = c("0-300", "301-600", "601-900", "901-1200", "1201-1500"))
    
    # Define the input variable for the barplot
    input_variable <- "days_before_category"
    
    # Generate the barplot
    p <- ggplot(train_data2, aes(x = factor(.data[[input_variable]]), fill = attended)) +
      geom_bar(position = "fill") +
      labs(title = paste("Proportional Barplot for", input_variable),
           x = "Days Before Category", y = "Proportion") +
      theme_minimal()
    
    # Print the plot
    print(p)
    
    
    ### for day_of_week error
    
    # Load necessary libraries
    library(ggplot2)
    
    # Sample data (replace this with your actual train_data)
    train_data3 <- data.frame(
      day_of_week = sample(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 100, replace = TRUE),
      attended = sample(c("Yes", "No"), 100, replace = TRUE)
    )
    
    # Generate the barplot
    p <- ggplot(train_data, aes(x = factor(day_of_week), fill = attended)) +
      geom_bar(position = "fill") +
      labs(title = "Proportional Barplot for Day of Week",
           x = "Day of Week", y = "Proportion") +
      theme_minimal()
    
    # Print the plot
    print(p)
    
    ### day_of_week
    
    # Load necessary libraries
    library(ggplot2)
    
    # Sample data (replace this with your actual train_data)
    train_data3 <- data.frame(
      day_of_week = sample(1:7, 100, replace = TRUE),  # Example numeric values for day_of_week (1-7)
      attended = sample(c("Yes", "No"), 100, replace = TRUE)
    )
    
    # Map numeric values of 'day_of_week' to corresponding days of the week
    train_data3$day_of_week <- factor(train_data3$day_of_week,
                                     levels = 1:7,
                                     labels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    
    # Define the input variable for the barplot
    input_variable <- "day_of_week"
    
    # Generate the barplot
    p <- ggplot(train_data, aes(x = factor(.data[[input_variable]]), fill = attended)) +
      geom_bar(position = "fill") +
      labs(title = paste("Proportional Barplot for", input_variable),
           x = "Day of Week", y = "Proportion") +
      theme_minimal()
    
    # Print the plot
    print(p)
    
    # Check column names of train_data
    colnames(train_data3)
    
    
    str(train_data3)
    
    head(train_data3)
    
    train_data3$day_of_week
    
    
    
    
    
    
    # Load necessary libraries
    library(ggplot2)
    
    # Generate the barplot
    p <- ggplot(train_data, aes(x = factor(day_of_week), fill = attended)) +
      geom_bar(position = "fill") +
      labs(title = "Proportional Barplot for Day of Week",
           x = "Day of Week", y = "Proportion") +
      theme_minimal()
    
    # Print the plot
    print(p)
    
    
    
    
    ## day_of_week correct codes
    
    # Load necessary libraries
    library(ggplot2)
    
    # Generate the barplot
    p <- ggplot(train_data3, aes(x = day_of_week, fill = attended)) +
      geom_bar(position = "fill") +
      labs(title = "Proportional Barplot for Day of Week",
           x = "Day of Week", y = "Proportion") +
      theme_minimal()
    
    # Print the plot
    print(p)
    
    
    
    
    ### for time
    
    
    # Load necessary libraries
    library(ggplot2)
    
    # Sample data (replace this with your actual train_data)
    train_data4 <- data.frame(
      time = sample(c("am", "pm"), 100, replace = TRUE),  # Example values for time ('am' or 'pm')
      attended = sample(c("Yes", "No"), 100, replace = TRUE)
    )
    
    # Create time_category variable based on 'time'
    train_data4$time_category <- ifelse(train_data4$time == "am", "Morning", "Afternoon")
    
    # Generate the barplot
    p <- ggplot(train_data4, aes(x = time_category, fill = attended)) +
      geom_bar(position = "fill") +
      labs(title = "Proportional Barplot for Time of Day",
           x = "Time of Day", y = "Proportion") +
      theme_minimal()
    
    # Print the plot
    print(p)
    
    
    
    ## for category
    
    # Load necessary libraries
    library(ggplot2)
    
    # Sample data (replace this with your actual train_data)
    train_data3 <- data.frame(
      category = sample(c("HIIT", "cycling", "yoga", "strength", "aqua", "null"), 100, replace = TRUE),  # Example values for category
      attended = sample(c("Yes", "No"), 100, replace = TRUE)
    )
    
    # Create category_type variable based on 'category'
    train_data3$category_type <- ifelse(train_data3$category == "null", "Other", train_data3$category)
    
    # Generate the barplot
    p <- ggplot(train_data3, aes(x = category_type, fill = attended)) +
      geom_bar(position = "fill") +
      labs(title = "Proportional Barplot for Category",
           x = "Category", y = "Proportion") +
      theme_minimal()
    
    # Print the plot
    print(p)
    
    
    # Load necessary library
    install.packages("e1071")
    
    library(e1071)
    
    # Build naive Bayes model
    naive_bayes_model <- naiveBayes(attended ~ ., data = train_data5)
    
    # Show model results
    print(naive_bayes_model)
    
    
    
    
    # Load necessary library
    install.packages("caret")
    library(caret)
    
    # Predict on training data
    train_predictions <- predict(naive_bayes_model, newdata = train_data5)
    
    # Predict on validation data (assuming you have a separate validation dataset named 'validation_data')
    validation_predictions <- predict(naive_bayes_model, newdata = validation_data)
    
    # Create confusion matrices
    train_conf_matrix <- confusionMatrix(train_predictions, train_data5$attended)
    validation_conf_matrix <- confusionMatrix(validation_predictions, validation_data$attended)
    
    # Show confusion matrices
    print("Confusion Matrix for Training Data:")
    print(train_conf_matrix)
    
    print("Confusion Matrix for Validation Data:")
    print(validation_conf_matrix)
    
    
    
    
    
    
    
    
    # Assuming you have your training dataset loaded as 'train_data'
    library(e1071)
    
    # Train a naive Bayes model
    nb_model <- naiveBayes(attended ~ ., data = train_data5)
    
    
    # Assuming you have a model called "nb_model" and a validation set called "validation_data"
    # Step 1: Predict probabilities of missing fitness class
    validation_data$prob_missed_class <- predict(nb_model, newdata = validation_data, type = "raw")[, "1"]
    
    # Step 2: Sort the validation set based on predicted probabilities in descending order
    validation_data <- validation_data[order(-validation_data$prob_missed_class), ]
    
    # Step 3: Take the top 100 records
    top_100_missed_class <- validation_data[1:100, ]
    
    # Step 4: Count how many actually missed their class
    actual_missed_count <- sum(top_100_missed_class$attended == "No")
    
    # Step 5: Compare with overall model's accuracy
    overall_accuracy <- sum(predict(nb_model, newdata = validation_data) == validation_data$attended) / nrow(validation_data)
    
    # Step 6: Provide insights
    
    
    
    
    
    
    
    # a. Check if the person attended the class
    # Let's assume the chosen record is the first record in the training set
    attended_actual <- train_data5$attended[1]  # Replace [1] with the index of your chosen record
    
    # b. Use predict() to see what the model predicted for this person
    prediction <- predict(nb_model, newdata = train_data5[1, ])  # Replace [1, ] with the index of your chosen record
    predicted_attended <- prediction[1]
    
    # c. Use predict() with type = "raw" to get the probability of attending the class
    probability_prediction <- predict(nb_model, newdata = train_data5[1, ], type = "raw")  # Replace [1, ] with the index of your chosen record
    probability_attended <- probability_prediction[1]
    
    # d. Calculate the probability manually
    # Assuming the features and their corresponding coefficients are available
    # Let's say we have two features: feature1 and feature2, and their coefficients are 0.3 and -0.2 respectively
    # The intercept is -1.5
    # Then the probability can be calculated as:
    # exp(intercept + coefficient1 * feature1 + coefficient2 * feature2) / (1 + exp(intercept + coefficient1 * feature1 + coefficient2 * feature2))
    
    # Replace the coefficients and intercept with your model's parameters
    intercept <- -1.5
    coefficient1 <- 0.3
    coefficient2 <- -0.2
    attended_numeric <- train_data5$attended_numeric  # Replace [1] with the index of your chosen record's feature1 value
    predicted_attended <- train_data5$predicted_attended  # Replace [1] with the index of your chosen record's feature2 value
    probability_manual <- exp(intercept + coefficient1 * attended_numeric + coefficient2 *predicted_attended ) / (1 + exp(intercept + coefficient1 * attended_numeric + coefficient2 * predicted_attended))
    
    
    # Convert factors to numeric
    attended_numeric <- as.numeric(as.character(attended))
    predicted_attended <- as.numeric(as.character(predicted_attended))
    
    
    
    
    print(attended_actual)
    
    print(prediction)
    
    print(probability_prediction)
    
    print(probability_attended)
    
    