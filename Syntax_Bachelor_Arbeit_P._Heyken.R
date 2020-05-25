# 1.  	  Loading all required packages
# 2.	    Sample description
# 2.1.  	Importing the data for sample description
# 2.2.	  Data cleaning and preparation
# 2.2.1.	Deleting unnecessary variables
# 2.2.2.	Recoding the data
# 2.2.3.	Coding the classes
# 2.2.4.	Checking for missing values
# 2.2.5.	Code violations of inclusion criteria as NAs
# 2.2.6.	Delete NAs listwise
# 2.3.	  Exploring the data
# 2.3.1.	Descriptive statistics
# 2.3.2.	Visualization of continuous variables
# 3.	    Heatmap for correlations
# 3.1.	  Importing the data for random forest
# 3.2.  	Data cleaning
# 3.2.1.	Deleting unnecessary variables
# 3.2.2.	Checking for missing values
# 3.2.3.	Code violations of inclusion criteria as NAs
# 3.2.4.	Deleting NAs listwise
# 3.2.5.	Deleting variables for inclusion check
# 3.3.	  Creating heatmap
# 3.3.1.	Creating correlation matrix
# 3.3.2.	Remove higher triangle of correlation matrix
# 3.3.3.	Plot heatmap
# 3.3.4   Calculate CI, and p-values for selected correlations
# 4.	    Preparation for random forest
# 4.1.  	Assigning correct level to variables
# 4.2.	  Exploring the data
# 4.2.1.	Descriptive statistics
# 4.2.2.	Frequencies of non-continuous variables
# 4.2.3.	Visualization of continuous variables and checking for outliers
# 5.	    Random forest model 1
# 5.1.	  Splitting the dataset into training and testing dataset
# 5.2.	  Growing a random forest with default settings
# 5.3.  	Analyzing optimal value of ntree
# 5.4.  	Analyzing optimal value of mtry
# 5.5.	  Creating final model 1
# 5.6.	  Check Mean Decrease in Gini
# 5.7.	  Predicting testing data from model
# 5.7.1.	Confusion matrix
# 5.7.2.	Heatmap actual and predicted scores
# 5.7.3.	Spearman´s correlation actual and predicted scores
# 6.	    Random forest model 2
# 6.1.  	Creating training and testing dataset without target_popularity
# 6.2.	  Growing a random forest with default settings
# 6.3.	  Analyzing optimal values of ntree
# 6.4.	  Analyzing optimal value of mtry
# 6.5.	  Creating final model 2
# 6.6.	  Check Mean Decrease in Gini
# 6.7.	  Predicting testing data from model
# 6.7.1.	Confusion matrix
# 6.7.2.	Heatmap actual and predicted scores
# 6.7.3.	Correlation actual and predicted scores
# 7.      Test for dependent overlapping correlations
# 7.1     Calculation of correlation between predicted scores of both models
# 7.2     Loading required functions
# 7.3     Calculation of CI

#1. Loading all required packages
  library(ggplot2) #for heatmaps, and OBB error rate/ Tree graph
  library(randomForest) #for the random forest analysis
  library(caret) #for the confusion matrixes
  library(e1071) #required for package caret
  library(reshape2) #for heatmap
  library(psych) #for descriptive statistics
  library(dplyr) #for frequencies

#2.	Ssample description
#2.1.	Importing the data for sample description
  sampledata <-
    read.table(
      file = 'sample_description.txt',
      sep = '\t',
      header = TRUE,
      stringsAsFactors = FALSE
    )
#2.2.	Data cleaning and preparation
#2.2.1.	Deleting unnecessary variables
  sampledata <-
    sampledata [, c(2, 3, 4, 5, 6, 7, 8, 12, 13, 15, 16, 18, 20, 21, 36, 37, 38)]
#2.2.2.	Recoding the data
  #sex
  sampledata$sex[sampledata$sex == 2] = "M"
  sampledata$sex[sampledata$sex == 1] = "F"
  #dating apps
  sampledata$dating_apps [sampledata$dating_apps == 2] = "No"
  sampledata$dating_apps [sampledata$dating_apps == 1] = "Yes"
  #education
  sampledata$education [sampledata$education == 1] = "VBO"
  sampledata$education [sampledata$education == 2] = "HAVO"
  sampledata$education [sampledata$education == 3] = "VWO"
  sampledata$education [sampledata$education == 4] = "MBO"
  sampledata$education [sampledata$education == 5] = "HBO"
  sampledata$education [sampledata$education == 6] = "WO"
  #last relationship
  sampledata$last_relationship [sampledata$last_relationship == 1] = "Previous Relationship"
  sampledata$last_relationship [sampledata$last_relationship == 2] = "No Previous Relationship"
  #contraceptive
  sampledata$contraceptive [sampledata$contraceptive == 1] = "Yes"
  sampledata$contraceptive [sampledata$contraceptive == 2] = "No"
  #looking for relationship
  sampledata$looking_for_relation [sampledata$looking_for_relation == 1] =
    "Yes"
  sampledata$looking_for_relation [sampledata$looking_for_relation == 2] =
    "No"
  #dating experience
  sampledata$date_experience [sampledata$date_experience == 1] = "No Dating Experience"
  sampledata$date_experience [sampledata$date_experience == 2] = "1-5 Dates"
  sampledata$date_experience [sampledata$date_experience == 3] = "6-10 Dates"
  sampledata$date_experience [sampledata$date_experience == 4] = "11-20 Dates"
  sampledata$date_experience [sampledata$date_experience == 5] = "More than 20 Dates"
  #hearing aid
  sampledata$hearing_aid [sampledata$hearing_aid == 1] = "Yes, not corrected for"
  sampledata$hearing_aid [sampledata$hearing_aid == 2] = "Yes, but corrected for"
  sampledata$hearing_aid [sampledata$hearing_aid == 3] = "No"
  #looking aid
  sampledata$looking_aid [sampledata$looking_aid == 1] = "Yes, not correceted for"
  sampledata$looking_aid [sampledata$looking_aid == 2] = "Yes, but corrected for"
  sampledata$looking_aid [sampledata$looking_aid == 3] = "No"
#2.2.3.	Assigning correct level to variables
  sampledata$sex <- as.factor(sampledata$sex)
  sampledata$education <- as.factor(sampledata$education)
  sampledata$dating_apps <- as.factor(sampledata$dating_apps)
  sampledata$last_relationship <-
    as.factor(sampledata$last_relationship)
  sampledata$date_experience <-
    as.factor(sampledata$date_experience)
  sampledata$contraceptive <-
    as.factor(sampledata$contraceptive)
  sampledata$looking_for_relation <-
    as.factor(sampledata$looking_for_relation)
  sampledata$looking_aid <- as.factor(sampledata$looking_aid)
  sampledata$hearing_aid <- as.factor(sampledata$hearing_aid)
  sampledata$self_attraction <-
    as.factor(sampledata$self_attraction)
  sampledata$popularity <- as.factor(sampledata$popularity)
  str(sampledata)
#2.2.4.	Checking for missing values
  sum(is.na(sampledata)) #in this case all NAs are associated with dropouts
#2.2.5.	Code violations of inclusion criteria as NAs
  #participants older than 26
  sampledata$age [sampledata$age > 26] = NA
  #participants who are not looking for a relationship
  sampledata$looking_for_relation [sampledata$looking_for_relation == "No"] = NA
  #participants with uncorrected hearing difficulties
  sampledata$hearing_aid [sampledata$hearing_aid == "Yes, not corrected for"]
  #particioants with uncorrected vision difficulties
  sampledata$looking_aid [sampledata$looking_aid == "Yes, not corrected for"] = NA
#2.2.6.	Delete NAs listwise
  sampledata <- na.omit(sampledata)
#2.3.	Exploring the data
#2.3.1.	Descriptive statistics (min, max, mean)
  #of the full sample
  describe(sampledata, skew = FALSE)
  #for each sex
  describeBy(sampledata,
             skew = FALSE,
             group = sampledata$sex,
             mat = FALSE)
#2.3.2. Visualization of continuous variables
  #age
  #complete sample
  hist(
    x = sampledata$age,
    main = "Age Distribution Sample",
    xlab = "Age in Years",
    ylab = "Frequency",
    breaks = 10,
    xlim = c(17, 27),
    ylim = c(0, 20),
    col = "gray"
  )
  #only male
  hist(
    x = sampledata$age [sampledata$sex == "M"],
    main = "Age Distribution Male",
    xlab = "Age in Years",
    ylab = "Frequency",
    xlim = c(17, 27),
    ylim = c(0, 20),
    col = "gray"
  )
  #only female
  hist(
    x = sampledata$age [sampledata$sex == "F"],
    main = "Age Distribution Female",
    xlab = "Age in Years",
    ylab = "Frequency",
    xlim = c(17, 27),
    ylim = c(0, 20),
    col = "gray"
  )
  #height
  #complete sample
  hist(
    x = sampledata$height,
    main = "height Distribution Sample",
    xlab = "height in cm",
    ylab = "Frequency",
    breaks = 10,
    xlim = c(150, 210),
    ylim = c(0, 20),
    col = "gray"
  )
  #only male
  hist(
    x = sampledata$height [sampledata$sex == "M"],
    main = "height Distribution Male",
    xlab = "height in cm",
    ylab = "Frequency",
    xlim = c(150, 210),
    ylim = c(0, 20),
    col = "gray"
  )
  #only female
  hist(
    x = sampledata$height [sampledata$sex == "F"],
    main = "height Distribution Female",
    xlab = "height in cm",
    ylab = "Frequency",
    xlim = c(150, 210),
    ylim = c(0, 20),
    col = "gray"
  )
  #weigth
  #complete sample
  hist(
    x = sampledata$weight,
    main = "Weight Distribution Sample",
    xlab = "Weight in kg",
    ylab = "Frequency",
    breaks = 10,
    xlim = c(40, 110),
    ylim = c(0, 20),
    col = "gray"
  )
  #only male
  hist(
    x = sampledata$weight [sampledata$sex == "M"],
    main = "Weight Distribution Male",
    xlab = "Weight in kg",
    ylab = "Frequency",
    xlim = c(40, 110),
    ylim = c(0, 20),
    col = "gray"
  )
  #only female
  hist(
    x = sampledata$weight [sampledata$sex == "F"],
    main = "Weight Distribution Female",
    xlab = "Weight in kg",
    ylab = "Frequency",
    xlim = c(40, 110),
    ylim = c(0, 20),
    col = "gray"
  )
  #SDI
  #complete sample
  hist(
    x = sampledata$SDI.2,
    main = "Score Sexual Desire Inventory 2 Sample",
    xlab = "SDI-2 Score",
    ylab = "Frequency",
    breaks = 10,
    xlim = c(0, 130),
    ylim = c(0, 30),
    col = "gray"
  )
  #only male
  hist(
    x = sampledata$SDI.2 [sampledata$sex == "M"],
    main = "Score Sexual Desire Inventory 2 Male",
    xlab = "SDI-2 Score",
    ylab = "Frequency",
    xlim = c(0, 130),
    ylim = c(0, 30),
    col = "gray"
  )
  #only female
  hist(
    x = sampledata$SDI.2 [sampledata$sex == "F"],
    main = "Score Sexual Desire Inventory 2 Female",
    xlab = "SDI-2 Score",
    ylab = "Frequency",
    xlim = c(0, 130),
    ylim = c(0, 30),
    col = "gray"
  )
  #BMI
  #complete sample
  hist(
    x = sampledata$BMI,
    main = "Body Mass Index Sample",
    xlab = "BMI",
    ylab = "Frequency",
    breaks = 10,
    xlim = c(15, 35),
    ylim = c(0, 20),
    col = "gray"
  )
  #only male
  hist(
    x = sampledata$BMI [sampledata$sex == "M"],
    main = "Body Mass Index Male",
    xlab = "BMI",
    ylab = "Frequency",
    xlim = c(15, 35),
    ylim = c(0, 20),
    col = "gray"
  )
  #only female
  hist(
    x = sampledata$BMI [sampledata$sex == "F"],
    main = "Body Mass Index Female",
    xlab = "BMI",
    ylab = "Frequency",
    xlim = c(15, 35),
    ylim = c(0, 20),
    col = "gray"
  )
  rm(sampledata)

#3.	Heatmap for correlations
#3.1.	Importing the data for RF
  data <-
    read.table(
      file = 'RandomForest.txt',
      sep = '\t',
      header = TRUE,
      stringsAsFactors = FALSE
    )
#3.2.	Data cleaning
#3.2.1.	Deleting unnecessary variables
  data <-
    data [, c(8, 9, 14, 15, 16, 17, 18, 26, 27, 35, 36, 38, 39, 40, 41, 42, 43, 46, 53, 58, 63, 65)]
#3.2.2.	Checking for missing values
  sum(is.na(data))
#3.2.3 Code violations of inclusion criteria as NAs
  #perceiver older than 26
  data$perceiver_age [data$perceiver_age > 26] = NA
  #perceiver who are not looking for a relationship
  data$perceiver_looking_for_relation [data$perceiver_looking_for_relation == 2] = NA
  #perceiver with uncorrected hearing difficulties
  data$perceiver_hearing_aid [data$perceiver_hearing_aid == 1] = NA
  #perceiver with uncorrected vision difficulties
  data$perceiver_looking_aid [data$perceiver_looking_aid == 1] = NA
  #perceiver who already knew their partner
  #check if NA are present
  sum(is.na(data$previous_knowle))
  #recode actual NAs as 1, and "y" as NA to ensure that only explicit violations will be deleted from the sample
  data$previous_knowle [is.na(data$previous_knowle)] <- 1
  data$previous_knowle [data$previous_knowle == "y"] = NA
#3.2.4.	Deleting NAs listwise
  data <- na.omit(data)
#3.2.5.	Deleting variables for inclusion check
  data <-
    data [, c(2, 3, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22)]
#3.3.	Creating Heatmap
#3.3.1.	Creating correlation matrix
  cormat <- round (cor(data, method = "spearman"), 2)
#3.3.2.	Remove higher triangle of correlation matrix
  get_upper_tri <-
    function(cormat) {
      cormat[lower.tri(cormat)] <- NA
      return(cormat)
    }
  upper_tri <- get_upper_tri(cormat)
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
#3.3.3.	Plot heatmap
  heatmap_correlation <-
    ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "blue",
      high = "red",
      mid = "white",
      midpoint = 0,
      limit = c(-1, 1),
      space = "Lab",
      name = "Spearman´s\nCorrelation"
    ) +
    theme(axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      size = 10,
      hjust = 1
    )) +
    coord_fixed() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal"
    ) +
    guides(fill = guide_colorbar(
      barwidth = 7,
      barheight = 1,
      title.position = "top",
      title.hjust = 0.5
    ))
  heatmap_correlation
#3.3.4 Calculate CI, and p-values for selected correlations
  #post_date_attraction & target_popularity
  print(
    corr.test(data$post_date_attraction, data$target_popularity , method = "spearman"),
    short = FALSE
  )
  #post_date_attraction & target_education
  print(
    corr.test(data$post_date_attraction, data$target_education , method = "spearman"),
    short = FALSE
  )
  #post_date_attraction & targer_height
  print(
    corr.test(data$post_date_attraction, data$target_height , method = "spearman"),
    short = FALSE
  )
  #post_date_attraction & target_BMI
  print(corr.test(data$post_date_attraction, data$target_BMI, method = "spearman"),
        short = FALSE)
  #post_date_attraction & perceiver_sex
  print(
    corr.test(data$post_date_attraction, data$perceiver_sex , method = "spearman"),
    short = FALSE
  )
  #post_date_attraction & perceiver_self_attr.
  print(
    corr.test(
      data$post_date_attraction,
      data$perceiver_self_attr. ,
      method = "spearman"
    ),
    short = FALSE
  )
  #post_date_attraction & perceiver_SDI_II
  print(
    corr.test(data$post_date_attraction, data$perceiver_SDI_II , method = "spearman"),
    short = FALSE
  )
  #post_date_attraction & difference_age
  print(
    corr.test(data$post_date_attraction, data$difference_age , method = "spearman"),
    short = FALSE
  )
  #post_date_attraction & difference_education
  print(
    corr.test(
      data$post_date_attraction,
      data$difference_education ,
      method = "spearman"
    ),
    short = FALSE
  )
  #post_date_attraction & difference_BMI
  print(
    corr.test(data$post_date_attraction, data$difference_BMI , method = "spearman"),
    short = FALSE
  )
  #post_date_attraction & difference_height
  print(
    corr.test(data$post_date_attraction, data$difference_height , method = "spearman"),
    short = FALSE
  )
  #post_date_attraction & difference_dating_apps
  print(
    corr.test(
      data$post_date_attraction,
      data$difference_dating_apps ,
      method = "spearman"
    ),
    short = FALSE
  )
  #post_date_attraction & attractivity_picture
  print(
    corr.test(
      data$post_date_attraction,
      data$attractivity_picture ,
      method = "spearman"
    ),
    short = FALSE
  )
  #post_date_attraction & attractivity_voice
  print(
    corr.test(data$post_date_attraction, data$attractivity_voice , method = "spearman"),
    short = FALSE
  )
  #post_date_attraction & attractivity_olfactory
  print(
    corr.test(
      data$post_date_attraction,
      data$attractivity_olfactory ,
      method = "spearman"
    ),
    short = FALSE
  )
  #attractivity_olfactory & attractivity_voice
  print(
    corr.test(
      data$attractivity_olfactory,
      data$attractivity_voice ,
      method = "spearman"
    ),
    short = FALSE
  )
  #attractivity_olfactory & attractivity_picture
  print(
    corr.test(
      data$attractivity_olfactory,
      data$attractivity_picture ,
      method = "spearman"
    ),
    short = FALSE
  )
  #attractivity_picture & attractivity_voice
  print(
    corr.test(data$attractivity_picture, data$attractivity_voice , method = "spearman"),
    short = FALSE
  )
  #target_height & difference_height
  print(corr.test(data$target_height, data$difference_height , method = "spearman"),
        short = FALSE)
  #targer_BMI & difference_BMI
  print(corr.test(data$target_BMI, data$difference_BMI , method = "spearman"),
        short = FALSE)
  #remove heatmap_cprrelation, cormant, melted_cormat, get_upper_tri from workspace
  rm(heatmap_correlation,
     cormat,
     melted_cormat,
     get_upper_tri,
     upper_tri)
  
#4.  Preperation for random forest
#4.1.  	Assigning correct level to variables
  data$perceiver_sex <- as.factor(data$perceiver_sex)
  data$perceiver_self_attr. <-
    as.ordered(data$perceiver_self_attr.)
  data$target_education <- as.ordered(data$target_education)
  data$difference_education <-
    as.ordered(data$difference_education)
  data$difference_dating_apps <-
    as.factor(data$difference_dating_apps)
  data$attractivity_voice <- as.ordered(data$attractivity_voice)
  data$attractivity_picture <-
    as.ordered(data$attractivity_picture)
  data$attractivity_olfactory <-
    as.ordered(data$attractivity_olfactory)
  data$post_date_attraction <-
    as.ordered(data$post_date_attraction)
  str(data)
#4.2.	  Exploring the data
#4.2.1.	Descriptive statistics (min, max, mean)
  #of the full sample
  describe(data, skew = FALSE)
  #for each sex
  describeBy(data,
             skew = FALSE,
             group = data$perceiver_sex,
             mat = FALSE)
#4.2.2.	Frequencies of non-continuous variables
  data %>%
    group_by(perceiver_sex, perceiver_self_attr.) %>%
    summarise(count = n())
  data %>%
    group_by(perceiver_sex, target_education) %>%
    summarise(count = n())
  data %>%
    group_by(perceiver_sex, difference_dating_apps) %>%
    summarise(count = n())
  data %>%
    group_by(perceiver_sex, attractivity_voice) %>%
    summarise(count = n())
  data %>%
    group_by(perceiver_sex, attractivity_olfactory) %>%
    summarise(count = n())
  data %>%
    group_by(perceiver_sex, attractivity_picture) %>%
    summarise(count = n())
  data %>%
    group_by(perceiver_sex, post_date_attraction) %>%
    summarise(count = n())
#4.2.3.	Visualization of continuous variables and checking for outliers
  #perceiver_SDI_II
  #total dataset
  hist(
    x = data$perceiver_SDI_II,
    main = "perceiver_SDI_II",
    xlab = "Score",
    ylab = "Frequency",
    breaks = 10,
    xlim = c(17, 150),
    ylim = c(0, 150),
    col = "gray"
  )
  boxplot(data$perceiver_SDI_II)
  #for men
  boxplot(data$perceiver_SDI_II [data$perceiver_sex == 2])
  #for women
  boxplot(data$perceiver_SDI_II [data$perceiver_sex == 1]) 
  #target_height
  #total dataset
  hist(
    x = data$target_height,
    main = "target_height",
    xlab = "height",
    ylab = "Frequency",
    breaks = 10,
    xlim = c(150, 210),
    ylim = c(0, 150),
    col = "gray"
  )
  boxplot(data$target_height)
  #for men
  boxplot(data$target_height [data$perceiver_sex == 2])
  #for women
  boxplot(data$target_height [data$perceiver_sex == 1])
  #target_BMI
  #total dataset
  hist(
    x = data$target_BMI,
    main = "target_BMI",
    xlab = "BMI",
    ylab = "Frequency",
    breaks = 10,
    xlim = c(17, 40),
    ylim = c(0, 150),
    col = "gray"
  )
  boxplot(data$target_BMI)
  #for men
  boxplot(data$target_BMI [data$perceiver_sex == 2])
  #for women
  boxplot(data$target_BMI [data$perceiver_sex == 1])
  #target_popularity
  #total dataset
  hist(
    x = data$target_popularity,
    main = "target_popularity",
    xlab = "popularity",
    ylab = "Frequency",
    breaks = 10,
    xlim = c(0, 7),
    ylim = c(0, 150),
    col = "gray"
  )
  boxplot(data$target_popularity)
  #for men
  boxplot(data$target_popularity [data$perceiver_sex == 2])
  #for women
  boxplot(data$target_popularity [data$perceiver_sex == 1])
  #difference_age
  #total dataset
  hist(
    x = data$difference_age,
    main = "difference_age",
    xlab = "years",
    ylab = "Frequency",
    breaks = 10,
    xlim = c(-10, 10),
    ylim = c(0, 150),
    col = "gray"
  )
  boxplot(data$difference_age)
  #for men
  boxplot(data$difference_age [data$perceiver_sex == 2])
  #for women
  boxplot(data$difference_age [data$perceiver_sex == 1])
  #difference_BMI
  #total dataset
  hist(
    x = data$difference_BMI,
    main = "difference_BMI",
    xlab = "BMI",
    ylab = "Frequency",
    breaks = 10,
    xlim = c(-20, 20),
    ylim = c(0, 150),
    col = "gray"
  )
  boxplot(data$difference_BMI) 
  #for men
  boxplot(data$difference_BMI[data$perceiver_sex == 2])
  #for women
  boxplot(data$difference_BMI[data$perceiver_sex == 1]) 
  #difference_height
  #total dataset
  hist(
    x = data$difference_BMI,
    main = "difference_height",
    xlab = "height",
    ylab = "Frequency",
    breaks = 10,
    xlim = c(-20, 20),
    ylim = c(0, 150),
    col = "gray"
  )
  boxplot(data$difference_height)
  #for men
  boxplot(data$difference_height [data$perceiver_sex == 2])
  #for women
  boxplot(data$difference_height [data$perceiver_sex == 1])
  #difference_SDI_II
  #total dataset
  hist(
    x = data$difference_SDI_II,
    main = "difference_SDI_II",
    xlab = "Difference SDI II Score",
    ylab = "Frequency",
    breaks = 10,
    xlim = c(-80, 80),
    ylim = c(0, 150),
    col = "gray"
  )
  boxplot(data$difference_SDI_II) 
  #for men
  boxplot(data$difference_SDI_II [data$perceiver_sex == 2]) 
  #for women
  boxplot(data$difference_SDI_II [data$perceiver_sex == 1]) 

#5.  Random forest model 1 
#5.1.  Splitting the dataset into training (60%) and testing dataset (40%)
  set.seed(200)
  ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.6, 0.4))
  trainingdata_model_1 <- data [ind == 1,]
  testingdata_model_1 <- data [ind == 2,]
  #remove data and ind from workspace
  rm(data, ind)
#5.2.  Growing a random forest with default settings
  set.seed(200)
  model_1 <-
    randomForest(post_date_attraction ~ .,
                 data = trainingdata_model_1,
                 proximity = TRUE)
  model_1
#5.3.  Analyzing optimal value of ntree
  set.seed(200)
  model_1 <-
    randomForest(
      post_date_attraction ~ .,
      data = trainingdata_model_1,
      ntree = 1500,
      proximity = TRUE
    )
  model_1
  #plotting errors for each score seperatly
  test.ntree_model_1 <- data.frame(
    Trees = rep(1:nrow(model_1$err.rate), times = 8),
    Type = rep(
      c("OOB", "1", "2", "3", "4", "5", "6", "7"),
      each = nrow(model_1$err.rate)
    ),
    Error = c(
      model_1$err.rate[, "OOB"],
      model_1$err.rate[, "1"],
      model_1$err.rate[, "2"],
      model_1$err.rate[, "3"],
      model_1$err.rate[, "4"],
      model_1$err.rate[, "5"],
      model_1$err.rate[, "6"],
      model_1$err.rate[, "7"]
    )
  )
  ggplot(data = test.ntree_model_1, aes(x = Trees, y = Error)) +
    geom_line(aes(color = Type))
  #plotting only OBB error rate
  test.ntree_model_1 <-
    data.frame(
      Trees = rep(1:nrow(model_1$err.rate)),
      Type = rep(c("OOB")),
      Error = c(model_1$err.rate[, "OOB"])
    )
  ggplot(data = test.ntree_model_1) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    geom_line(mapping = aes(x = Trees, y = Error, ), color = "grey 40") +
    xlim (0, 1500) +
    ylim (.64, .74)
#5.4.  	Analyzing optimal value of mtry
  #create a loop for mtry counting from 1 to 8
  oob.values.mtry <- vector(length = 8)
  for (i in 1:8) {
    set.seed(200)
    temp.model <-
      randomForest(
        post_date_attraction ~ .,
        data = trainingdata_model_1,
        mtry = i,
        ntree = 650,
        proximity = TRUE
      )
    oob.values.mtry[i] <-
      temp.model$err.rate[nrow(temp.model$err.rate), 1]
  }
  oob.values.mtry
  #create bar plot
  test.mtry_model_1 <-
    data.frame ("mtry" = c(1, 2, 3, 4, 5, 6, 7, 8),
                "Error" = c(oob.values.mtry))
  ggplot(data = test.mtry_model_1, aes(x = mtry, y = Error)) +
    geom_bar(stat = "identity", fill = "grey 40") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    coord_cartesian(ylim = c(.63, .69)) +
    scale_x_continuous(breaks = 0:8)
  #remove i, temp.model, obb.values.mtry from workspace
  rm(i,
     temp.model,
     oob.values.mtry,
     test.mtry_model_1,
     test.ntree_model_1)
#5.5.	  Creating final model 1
  set.seed(200)
  model_1 <-
    randomForest(
      post_date_attraction ~ .,
      data = trainingdata_model_1,
      ntree = 650,
      mtry = 4,
      proximity = TRUE
    )
  model_1
#5.6.	  Check Mean Decrease in Gini
  importance(model_1, type = 2, scale = TRUE)
  varImpPlot(model_1)
  varUsed(model_1) #indicates how often a specific varibale was used in the model
#5.7.	  Predicting testing data from model
  prediction <- predict(model_1, testingdata_model_1)
#5.7.1.	Confusion matrix
  confusionMatrix(prediction, testingdata_model_1$post_date_attraction)
#5.7.2.	Heatmap actual and predicted scores
  #combining prediction und post.date.attraction in data frame
  heat <-
    data.frame(prediction, testingdata_model_1$post_date_attraction)
  heat$prediction <- as.integer(heat$prediction)
  heat$testingdata_model_1.post_date_attraction <-
    as.integer(heat$testingdata_model_1.post_date_attraction)
  colnames(heat) <- c("Predicted", "Actual")
  #creating a table with the predicted, actual and their corresponding frequencies
  heat_pred <- as.data.frame(table(heat$Predicted, heat$Actual))
  colnames(heat_pred) <- c("Predicted", "Actual", "Frequency")
  #creating the heatmap with ggplot
  ggplot(heat_pred, aes(Predicted, Actual)) +
    geom_tile(aes(fill = Frequency), colour = "black") +
    scale_fill_gradient(low = "white", high = "black")
#5.7.3.	Spearman´s correlation actual and predicted scores
  print(corr.test(heat$Predicted, heat$Actual, method = "spearman"),
        short = FALSE)
  #safe predicted score in data frame "sign.test_r" for later significance testing
  sign.test_r <- data.frame(heat$Predicted)
  colnames(sign.test_r) <- c("Prediction Model 1")
  #removing prediction, heat, heat_pred from workspace
  rm(prediction, heat, heat_pred)
  
#6.  Random forest model 2
#6.1.  Creating training and testing dataset without target_popularity
  trainingdata_model_2 <- trainingdata_model_1
  trainingdata_model_2 <-
    trainingdata_model_2 [, c (1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)]
  testingdata_model_2 <- testingdata_model_1
  testingdata_model_2 <-
    testingdata_model_2 [, c (1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)]
#6.2.  Growing a random forest with default settings
  set.seed(200)
  model_2 <-
    randomForest(post_date_attraction ~ .,
                 data = trainingdata_model_2,
                 proximity = TRUE)
  model_2
#6.3.  Analyzing optimal values of ntree
  set.seed(200)
  model_2 <-
    randomForest(
      post_date_attraction ~ .,
      data = trainingdata_model_2,
      ntree = 1500,
      proximity = TRUE
    )
  model_2
  #plotting errors for each score seperatly
  test.ntree_model_2 <- data.frame(
    Trees = rep(1:nrow(model_2$err.rate), times = 8),
    Type = rep(
      c("OOB", "1", "2", "3", "4", "5", "6", "7"),
      each = nrow(model_2$err.rate)
    ),
    Error = c(
      model_2$err.rate[, "OOB"],
      model_2$err.rate[, "1"],
      model_2$err.rate[, "2"],
      model_2$err.rate[, "3"],
      model_2$err.rate[, "4"],
      model_2$err.rate[, "5"],
      model_2$err.rate[, "6"],
      model_2$err.rate[, "7"]
    )
  )
  ggplot(data = test.ntree_model_2, aes(x = Trees, y = Error)) +
    geom_line(aes(color = Type))
  #plotting only OBB error rate
  test.ntree_model_2 <-
    data.frame(
      Trees = rep(1:nrow(model_2$err.rate)),
      Type = rep(c("OOB")),
      Error = c(model_2$err.rate[, "OOB"])
    )
  ggplot(data = test.ntree_model_2) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    geom_line(mapping = aes(x = Trees, y = Error, ), color = "grey 40") +
    xlim (0, 1500) +
    ylim (.64, .83)
#6.4.  Analyzing optimal value of mtry
  #create a loop for mtry counting from 1 to 8
  oob.values.mtry <- vector(length = 8)
  for (i in 1:8) {
    set.seed(200)
    temp.model <-
      randomForest(
        post_date_attraction ~ .,
        data = trainingdata_model_1,
        mtry = i,
        ntree = 870,
        proximity = TRUE
      )
    oob.values.mtry[i] <-
      temp.model$err.rate[nrow(temp.model$err.rate), 1]
  }
  oob.values.mtry
  #create bar plot
  test.mtry_model_1 <-
    data.frame ("mtry" = c(1, 2, 3, 4, 5, 6, 7, 8),
                "Error" = c(oob.values.mtry))
  ggplot(data = test.mtry_model_1, aes(x = mtry, y = Error)) +
    geom_bar(stat = "identity", fill = "grey 40") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.line = element_line(colour = "black")
    ) +
    coord_cartesian(ylim = c(.63, .69)) +
    scale_x_continuous(breaks = 0:8)
  #remove i, temp.model, obb.values.mtry from workspace
  rm(i,
     temp.model,
     oob.values.mtry,
     test.ntree_model_2,
     test.mtry_model_1)
#6.5.  Creating final model 2
  set.seed(200)
  model_2 <-
    randomForest(
      post_date_attraction ~ .,
      data = trainingdata_model_2,
      ntree = 870,
      mtry = 3,
      proximity = TRUE
    )
  model_2
#6.6.  Check Mean Decrease in Gini
  importance(model_2, type = 2, scale = TRUE)
  varImpPlot(model_2)
  varUsed(model_2) #indicates how often a specific varibale was used in the model
#6.7.  Predicting testing data from model
  prediction <- predict(model_2, testingdata_model_2)
#6.7.1.  Confusion matrix
  confusionMatrix(prediction, testingdata_model_2$post_date_attraction)
#6.7.2.	Heatmap actual and predicted scores
  #combining prediction und post.date.attraction in data frame
  heat <-
    data.frame(prediction, testingdata_model_2$post_date_attraction)
  heat$prediction <- as.integer(heat$prediction)
  heat$testingdata_model_2.post_date_attraction <-
    as.integer(heat$testingdata_model_2.post_date_attraction)
  colnames(heat) <- c("Predicted", "Actual")
  #creating a table with the predicted, actual and their corresponding frequencies
  heat_pred <- as.data.frame(table(heat$Predicted, heat$Actual))
  colnames(heat_pred) <- c("Predicted", "Actual", "Frequency")
  #creating the heatmap with ggplot
  ggplot(heat_pred, aes(Predicted, Actual)) +
    geom_tile(aes(fill = Frequency), colour = "black") +
    scale_fill_gradient(low = "white", high = "black")
#6.7.3.  Spearman´s correlation actual and predicted scores
  print(corr.test(heat$Predicted, heat$Actual, method = "spearman"),
        short = FALSE)
  #safe predicted score in data frame "sign.test_r" for later significance testing
  sign.test_r <-
    data.frame(sign.test_r$`Prediction Model 1`, heat$Predicted)
  colnames(sign.test_r) <-
    c("Prediction Model 1", "Prediction Model 2")
  #removing prediction, heat, heat_pred from workspace
  rm(prediction, heat, heat_pred)

#7. Test for dependent overlapping correlations
#7.1  Calculation of correlation between predicted scores of both models
  print(
    corr.test(
      sign.test_r$`Prediction Model 1`,
      sign.test_r$`Prediction Model 2` ,
      method = "spearman"
    ),
    short = FALSE
  )
  rm(sign.test_r)
#7.2 Loading required functions
  rho.rxy.rxz <- function(rxy, rxz, ryz) {
    num <- (ryz - 1 / 2 * rxy * rxz) * (1 - rxy ^ 2 - rxz ^ 2 - ryz ^ 2) + ryz ^
      3
    den <- (1 - rxy ^ 2) * (1 - rxz ^ 2)
    num / den
  }
  r.dol.ci <- function(r12, r13, r23, n, conf.level = 0.95) {
    L1 <- rz.ci(r12, n, conf.level = conf.level)[1]
    U1 <- rz.ci(r12, n, conf.level = conf.level)[2]
    L2 <- rz.ci(r13, n, conf.level = conf.level)[1]
    U2 <- rz.ci(r13, n, conf.level = conf.level)[2]
    rho.r12.r13 <- rho.rxy.rxz(r12, r13, r23)
    lower <-
      r12 - r13 - ((r12 - L1) ^ 2 + (U2 - r13) ^ 2 - 2 * rho.r12.r13 * (r12 -
                                                                          L1) * (U2 - r13)) ^ 0.5
    upper <-
      r12 - r13 + ((U1 - r12) ^ 2 + (r13 - L2) ^ 2 - 2 * rho.r12.r13 * (U1 -
                                                                          r12) * (r13 - L2)) ^ 0.5
    c(lower, upper)
  }
  rz.ci <- function(r, N, conf.level = 0.95) {
    zr.se <- 1 / (N - 3) ^ 0.5
    moe <- qnorm(1 - (1 - conf.level) / 2) * zr.se
    zu <- atanh(r) + moe
    zl <- atanh(r) - moe
    tanh(c(zl, zu))
  }
#7.3 Calculation of CI
  r.dol.ci(.65, .54, .79, 176)
  rm (r.dol.ci, rho.rxy.rxz, rz.ci)

