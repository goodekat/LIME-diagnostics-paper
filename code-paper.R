## ----setup, echo = FALSE, include = FALSE----------------

# Specify global options
knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  echo = FALSE,
  fig.keep = 'high',
  fig.align = 'center',
  dev = c("cairo_pdf", "cairo_ps"),
  dev.args = list(fallback_resolution = 800),
  dpi = 1200
)

# Load packages
library(Cairo)
library(caret)
library(cowplot)
library(dplyr)
library(forcats)
library(ggforce)
library(ggplot2)
library(ggpcp) #devtools::install_github("yaweige/ggpcp", build_vignettes = TRUE)
library(glmnet)
library(gower)
library(graphics)
library(gretchenalbrecht) #devtools::install_github("dicook/gretchenalbrecht")
library(latex2exp)
library(lime) #devtools::install_github("goodekat/lime")
library(limeaid) #devtools::install_github("goodekat/limeaid")
library(magick)
library(purrr)
library(randomForest)
library(stringr)
library(tidyr)

# Specify the (base) font size (fs) and font (ff) for all plots
fs = 7
ff = "Helvetica"


## ----concept-data----------------------------------------

# Simulate example data
set.seed(20190624)
lime_data <-
  data.frame(
    feature1 = sort(runif(250, 0, 1)),
    feature2 = sample(x = 1:250, size = 250, replace = FALSE)) %>%
  mutate(
    feature1_stnd = (feature1 - mean(feature1)) / sd(feature1),
    feature2_stnd = (feature2 - mean(feature2)) / sd(feature2),
    prediction = if_else(feature1 >= 0 & feature1 < 0.1,
                         (0.3 * feature1) + rnorm(n(), 0, 0.01),
                 if_else(feature1 >= 0.1 & feature1 < 0.3,
                         rbeta(n(), 1, 0.5),
                 if_else(feature1 >= 0.3 & feature1 < 0.5,
                         sin(pi* feature1) + rnorm(n(), 0, 0.5),
                 if_else(feature1 >= 0.5 & feature1 < 0.8,
                         -(sin(pi* feature1) + rnorm(n(), 0, 0.1)) + 1,
                 if_else(feature1 >= 0.8 & feature1 < 0.9,
                         0.5 + runif(n(), -0.5, 0.5),
                         0.5 + rnorm(n(), 0, 0.3))))))) %>%
  bind_rows(
    data.frame(feature1 = rep(1, 30) + rnorm(5, 0, 0.05),
           feature2 = rep(245, 30) + rnorm(5, 0, 1),
           prediction = rep(0, 30) + rnorm(5, 0, 0.05)) %>%
  mutate(feature1_stnd = (feature1 - mean(feature1)) / sd(feature1),
         feature2_stnd = (feature2 - mean(feature2)) / sd(feature2)))

# Specify a prediction of interest
prediction_of_interest <-
  data.frame(feature1 = 0.07, feature2 = 200) %>%
  mutate(feature1_stnd = (feature1 - mean(lime_data$feature1)) /
           sd(lime_data$feature1),
         feature2_stnd = (feature2 - mean(lime_data$feature2)) /
           sd(lime_data$feature2),
         prediction = 0.05,
         color = factor("Prediction \nof Interest"))

# Specify the gower exponents
good_gower_power <- 50
bad_gower_power <- 1

# Compute the good distances between the prediction of interest
# and all other observations
lime_data$distance_good <-
  (1 - gower_dist(x = prediction_of_interest %>%
                    select(feature1_stnd, feature2_stnd),
                  y = lime_data %>%
                    select(feature1_stnd,
                           feature2_stnd)))^good_gower_power

# Compute the bad distances between the prediction of interest
# and all other observations
lime_data$distance_bad <-
  (1 - gower_dist(x = prediction_of_interest %>%
                    select(feature1_stnd, feature2_stnd),
                  y = lime_data %>%
                    select(feature1_stnd,
                           feature2_stnd)))^bad_gower_power

# Prepare the data for plotting
lime_data_gathered <- lime_data %>%
  gather(feature, feature_stnd_value,
         feature1_stnd:feature2_stnd) %>%
  gather(distance, distance_value, distance_good:distance_bad) %>%
  mutate(distance = fct_recode(distance,
                               "good" = "distance_good",
                               "bad" = "distance_bad"),
         feature = fct_recode(feature,
                              "Feature 1" = "feature1_stnd",
                              "Feature 2" = "feature2_stnd"))

# Prepare the prediction of interest data for plotting
prediction_of_interest_gathered <- prediction_of_interest %>%
  select(-feature1, -feature2) %>%
  gather(feature, feature_value, feature1_stnd, feature2_stnd) %>%
  mutate(feature = fct_recode(feature,
                              "Feature 1" = "feature1_stnd",
                              "Feature 2" = "feature2_stnd"))



## ----concept-explainers----------------------------------

# Fit the good interpretable explainer model
explainer_good <-
  glmnet(x = lime_data %>% select(feature1_stnd, feature2_stnd)
         %>% as.matrix(),
         y = lime_data$prediction,
         alpha = 0,
         lambda = 1,
         weights = lime_data$distance_good)

# Fit the bad interpretable explainer model
explainer_bad <-
  glmnet(x = lime_data %>% select(feature1_stnd, feature2_stnd)
         %>% as.matrix(),
         y = lime_data$prediction,
         alpha = 0,
         lambda = 1,
         weights = lime_data$distance_bad)

# Join the coefficients from the explainer model into a dataframe
coefs_data <- data.frame(case = c("good", "bad"),
                         b0 = c(coef(explainer_good)[1],
                                coef(explainer_bad)[1]),
                         b1 = c(coef(explainer_good)[2],
                                coef(explainer_bad)[2]),
                         b2 = c(coef(explainer_good)[3],
                                coef(explainer_bad)[3])) %>%
  mutate(feature1 = b0 + b2*prediction_of_interest$feature2_stnd,
         feature2 = b0 + b1*prediction_of_interest$feature1_stnd) %>%
  gather(key = feature, value = int, feature1:feature2) %>%
  mutate(slope = c(coef(explainer_good)[2],
                   coef(explainer_good)[3],
                   coef(explainer_bad)[2],
                   coef(explainer_bad)[3]),
         feature = fct_recode(feature,
                              "Feature 1" = "feature1",
                              "Feature 2" = "feature2"))



## ----figure-01, out.width = '6.5in', fig.width = 7.95, fig.height = 3.75, warning = FALSE----

# Specify the figure size (for determining font size)
f1_ow = 6.5
f1_fw = 7.95
f1_fs = fs * (f1_fw / f1_ow)

# Plot of good explainer model
ggplot() +
  facet_grid(. ~ feature, switch = "x") +
  geom_point(
    data = lime_data_gathered,
    mapping = aes(
      x = feature_stnd_value,
      y = prediction,
      size = distance_value,
      alpha = distance_value,
      color = distance
    )
  ) +
  geom_abline(
    data = coefs_data %>% filter(case == "good"),
    mapping = aes(intercept = int, slope = slope),
    size = 1
  ) +
  scale_alpha(range = c(0.3, 1)) +
  scale_color_manual(values = c(NA, "grey30"), guide = "none") +
  geom_point(
    data = prediction_of_interest_gathered,
    mapping = aes(x = feature_value,
                  y = prediction,
                  fill = color),
    color = "black",
    size = 5,
    shape = 23,
    alpha = 0.75
  ) +
  scale_fill_manual(values = "#FAAA72") +
  geom_text(
    data = data.frame(
      feature_stnd_value = 1.2,
      prediction = 1.6,
      feature = c("Feature 1", "Feature 2"),
      slope = c(
        paste(
          "Slope:",
          coefs_data %>%
            filter(case == "good",
                   feature == "Feature 1") %>%
            pull(slope) %>%
            round(3)
        ),
        paste(
          "Slope:",
          coefs_data %>%
            filter(case == "good",
                   feature == "Feature 2") %>%
            pull(slope) %>%
            round(3)
        )
      )
    ),
    mapping = aes(x = feature_stnd_value,
                  y = prediction,
                  label = slope),
    family = ff,
    size = f1_fs / 2.85
  ) +
  labs(
    x = "",
    y = "Black-Box Prediction",
    title = "Conceptual Depiction of a Faithful Local Explainer Model",
    subtitle = paste("Gower Distance Metric Exponent:", good_gower_power),
    fill = "",
    alpha = "Weight",
    size = "Weight"
  ) +
  theme_linedraw(base_family = ff, base_size = f1_fs) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.placement = "outside",
    strip.background = element_rect(color = "white", fill = "white"),
    strip.text.x = element_text(size = f1_fs, color = "black"),
    plot.title = element_text(size = f1_fs)
  ) +
  guides(
    fill = guide_legend(order = 1),
    size = guide_legend(order = 2),
    alpha = guide_legend(order = 2)
  )



## ----figure-02, out.width = '6.5in', fig.width = 7.95, fig.height = 3.75, warning = FALSE----

# Specify the figure size (for determining font size)
f2_ow = 6.5
f2_fw = 7.95
f2_fs = fs * (f2_fw / f2_ow)

# Plot of good explainer model
ggplot() +
  facet_grid(. ~ feature, switch = "x") +
  geom_point(
    data = lime_data_gathered,
    mapping = aes(
      x = feature_stnd_value,
      y = prediction,
      size = distance_value,
      alpha = distance_value,
      color = distance
    )
  ) +
  geom_abline(
    data = coefs_data %>% filter(case == "bad"),
    mapping = aes(intercept = int, slope = slope),
    size = 1
  ) +
  scale_alpha(range = c(0.3, 1)) +
  scale_color_manual(values = c("grey30", NA), guide = "none") +
  geom_point(
    data = prediction_of_interest_gathered,
    mapping = aes(x = feature_value,
                  y = prediction,
                  fill = color),
    color = "black",
    size = 5,
    shape = 23,
    alpha = 0.75
  ) +
  scale_fill_manual(values = "#FAAA72") +
  geom_text(
    data = data.frame(
      feature_stnd_value = 1.2,
      prediction = 1.6,
      feature = c("Feature 1", "Feature 2"),
      slope = c(
        paste(
          "Slope:",
          coefs_data %>%
            filter(case == "bad",
                   feature == "Feature 1") %>%
            pull(slope) %>%
            round(3)
        ),
        paste(
          "Slope:",
          coefs_data %>%
            filter(case == "bad",
                   feature == "Feature 2") %>%
            pull(slope) %>%
            round(3)
        )
      )
    ),
    mapping = aes(x = feature_stnd_value,
                  y = prediction,
                  label = slope),
    family = ff,
    size = f2_fs / 3
  ) +
  labs(
    x = "",
    y = "Black-Box Prediction",
    title = "Conceptual Depiction of an Unfaithful Local Explainer Model",
    subtitle = paste("Gower Distance Metric Exponent:", bad_gower_power),
    fill = "",
    alpha = "Weight",
    size = "Weight"
  ) +
  theme_linedraw(base_family = ff, base_size = f2_fs) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.placement = "outside",
    strip.background = element_rect(color = "white", fill = "white"),
    strip.text.x = element_text(size = f2_fs, color = "black"),
    plot.title = element_text(size = f2_fs)
  ) +
  guides(
    fill = guide_legend(order = 1),
    size = guide_legend(order = 2),
    alpha = guide_legend(order = 2)
  )



## ----sine-data, echo = FALSE, include = FALSE------------

# Functions for rotating the data
rot_x <- function(x, y, theta) (x * cos(theta)) - (y * sin(theta))
rot_y <- function(x, y, theta) (x * sin(theta)) + (y * cos(theta))

# Generate the data
theta = -0.9
min = -10
max = 10
set.seed(20190913)
sine_data <- data.frame(x1 = runif(600, min, max),
                        x2 = sort(runif(600, min, max))) %>%
  mutate(x1new = rot_x(x1, x2, theta),
         x2new = rot_y(x1, x2, theta),
         y = ifelse(x2new > 5 * sin(x1new), "blue", "red")) %>%
  slice(sample(1:n())) %>%
  mutate(x3 = rnorm(600),
         case = 1:600) %>%
  select(case, everything())

# Separate the data into training and testing parts
set.seed(20191003)
rs <- sample(1:600, 500, replace = FALSE)
sine_data_train <- sine_data[rs,]
sine_data_test <- sine_data[-rs,]

# Fit a random forest
set.seed(20191003)
rfsine <- randomForest(x = sine_data_train %>% select(x1, x2, x3),
                       y = factor(sine_data_train$y))

# Obtain predictions on the training and testing data
sine_data_train$rfpred <- predict(rfsine)
sine_data_train <- cbind(sine_data_train, predict(rfsine, type = "prob"))
sine_data_train <- sine_data_train %>% 
  rename(rfprob_blue = blue, rfprob_red = red)
sine_data_test$rfpred <- predict(rfsine, sine_data_test %>% select(x1, x2, x3))
sine_data_test <- cbind(sine_data_test,
                        predict(rfsine, sine_data_test
                                %>% select(x1, x2, x3),
                                type = "prob"))
sine_data_test <- sine_data_test %>% 
  rename(rfprob_blue = blue, rfprob_red = red)

# Extract the prediction of interest from the sine test data
sine_poi <- sine_data_test %>% filter(y != rfpred, x1 > 0, x1 < 5, x2 > 5)



## ----figure-03, out.width = '6.5in', fig.width = 8, fig.height = 3.1----

# Specify the figure size (for determining font size)
f3_ow = 6.5
f3_fw = 8
f3_fs = fs * (f3_fw / f3_ow)
f3_ls = 0.5 * (f3_fw / f3_ow)

# Create points representing the rotated since curve
sinefun_data <- data.frame(xnew = seq(min(sine_data$x1new),
                                      max(sine_data$x1new),
                                      by = 0.01)) %>%
  mutate(ynew = 5 * sin(xnew)) %>%
  mutate(x = rot_x(xnew, ynew, -theta),
         y = rot_y(xnew, ynew, -theta)) %>%
  filter(y >= -10, y <= 10)

# Specify colors for predictions
sinecolor_red = "firebrick"
sinecolor_blue = "steelblue"

# Plot the training data observed classes
sine_plot_obs <- ggplot(sine_data_train, aes(x = x1, y = x2, color = y)) +
  geom_point(alpha = 0.8) +
  geom_path(data = sinefun_data, aes(x = x, y = y),
            color = "black", 
            size = f3_ls) +
  scale_colour_manual(values = c(sinecolor_blue, sinecolor_red)) +
  theme_bw(base_family = ff, base_size = f3_fs) +
  theme(aspect.ratio = 1, 
        plot.title = element_text(size = f3_fs)) +
  labs(x = TeX("$x_1$"),
       y = TeX("$x_2$"),
       color = TeX("y"),
       title = "Training Data")

# Plot the testing data rf predictions
sine_plot_pred <- ggplot() +
  geom_point(data = sine_poi %>%
               mutate(shape = "Prediction \nof Interest"),
             mapping = aes(x = x1,
                           y = x2,
                           shape = shape),
             size = 5,
             alpha = 0.8,
             color = "black") +
  geom_point(data = sine_data_test %>% filter(y != rfpred),
             mapping = aes(x = x1, y = x2),
             shape = 1,
             size = 3,
             color = "black") +
  geom_point(data = sine_data_test,
             mapping =  aes(x = x1,
                            y = x2,
                            color = rfprob_blue,
                            fill = rfprob_blue),
             shape = 21,
             alpha = 0.8) +
  geom_path(data = sinefun_data, aes(x = x, y = y),
            color = "black", 
            size = f3_ls) +
  scale_color_gradient2(low = sinecolor_red,
                        mid = '#f7f7f7',
                        high = sinecolor_blue,
                        midpoint = 0.5) +
  scale_fill_gradient2(low = sinecolor_red,
                        mid = '#f7f7f7',
                        high = sinecolor_blue,
                        midpoint = 0.5) +
  scale_shape_manual(values = 23) +
  theme_bw(base_family = ff, base_size = f3_fs) +
  theme(aspect.ratio = 1,
        plot.title = element_text(size = f3_fs)) +
  labs(x = TeX("$x_1$"),
       y = TeX("$x_2$"),
       color = "Random \nForest \nProbability \nfor 'blue'",
       fill = "Random \nForest \nProbability \nfor 'blue'",
       title = "Testing Data",
       shape = "") +
  guides(shape = guide_legend(order = 1))

# Join the plots
plot_grid(sine_plot_obs, sine_plot_pred,
          nrow = 1,
          rel_widths = c(0.4825, 0.5175))



## ----sine-lime-------------------------------------------

# Apply LIME with various tuning parameters to the sine data
if (!file.exists("../data/sine_lime_explain.rds")) {

    # Apply lime with various input options
    sine_lime_explain <- apply_lime(
      train = sine_data_train %>% select(x1, x2, x3),
      test = sine_data_test %>% select(x1, x2, x3),
      model = rfsine,
      label = "blue",
      n_features = 2,
      sim_method = c('quantile_bins', "kernel_density"),
      nbins = 2:6,
      feature_select = "auto",
      dist_fun = "gower",
      kernel_width = NULL,
      gower_pow = 1,
      return_perms = TRUE,
      all_fs = TRUE,
      seed = 20190914)

    saveRDS(sine_lime_explain, "../data/sine_lime_explain.rds")

  } else {

    sine_lime_explain <- readRDS("../data/sine_lime_explain.rds")

  }



## ----sine-lime-default-----------------------------------

# Extract the explanations from the default lime application
sine_lime_default <- sine_lime_explain$explain %>% filter(nbins == 4)

# Extract the explanations from the default lime application
# for the prediction of interest only
sine_poi_lime_default <- sine_lime_default %>%
  filter(case == sine_poi %>% pull(case)) %>%
  mutate(case = c("Prediction of Interest", "Prediction of Interest"))



## ----warning = FALSE-------------------------------------

# Obtain the simulated data associated with the poi
sine_poi_perms <- sine_poi_lime_default %>%
  slice(1) %>%
  select(perms_raw, perms_pred_complex, perms_numerified, weights) %>%
  mutate(perms_numerified =
           map(perms_numerified,
               .f = function(x) rename(x,
                                       "x1num" = "x1",
                                       "x2num" = "x2",
                                       "x3num" = "x3"))) %>%
  unnest(cols = c(perms_raw, perms_pred_complex, perms_numerified, weights)) %>%
  select(-red) %>%
  rename(rfpred = blue) %>%
  mutate(case = factor(1:n())) %>%
  select(case, everything())

# Determine the bin cuts for the default lime application
sine_lime_default_bin_cuts <- sine_lime_default %>%
  select(feature, feature_desc) %>%
  unique() %>%
  separate(feature_desc, c("other", "bin_cut"), sep = " <= ") %>%
  select(-other) %>%
  na.omit() %>%
  mutate(bin_cut = as.numeric(bin_cut)) %>%
  arrange(feature) %>%
  mutate(case = rep(1:3, 2)) %>%
  spread(feature, bin_cut)



## ----sine-poi-explainer, warning = FALSE-----------------

# Determine the lime explanation cutoffs
sine_poi_bounds <- sine_poi_lime_default %>%
  select(case, feature, feature_value, feature_desc) %>%
  separate(feature_desc, c("other", "upper"), sep = " <= ") %>%
  separate(other, c("lower", "feature2"), sep = " < ") %>%
  mutate(upper = ifelse(is.na(upper), "Inf", upper)) %>%
  select(-feature2) %>%
  mutate_at(.vars = c("lower", "upper"), .funs = as.numeric)

# Extract the coefficients from the explainer
sine_b0 <- sine_poi_lime_default$model_intercept[1]
sine_b1 <- sine_poi_lime_default$feature_weight[2]
sine_b2 <- sine_poi_lime_default$feature_weight[1]

# Extract the bounds of the bins
x1_lower <- sine_poi_bounds %>% filter(feature == "x1") %>% pull(lower)
x1_upper <- sine_poi_bounds %>% filter(feature == "x1") %>% pull(upper)
x2_lower <- sine_poi_bounds %>% filter(feature == "x2") %>% pull(lower)
x2_upper <- sine_poi_bounds %>% filter(feature == "x2") %>% pull(upper)

# Function for computing the predicted value via the explainer model
sine_explainer <- function(z1, z2) sine_b0 + sine_b1 * z1 + sine_b2 * z2



## ----figure-04, out.width = '6.5in', fig.width = 12, fig.height = 5, warning = FALSE----

# Specify the figure size (for determining font size)
f4_ow = 6.5
f4_fw = 12
f4_fs = fs * (f4_fw / f4_ow)
f4_ls = 0.5 * (f4_fw / f4_ow)

# Create the lime R package explanation visualization for the sine data
sine_poi_exp <- 
  sine_poi_lime_default %>% 
  plot_features() + 
  theme(text = element_text(family = ff, size = f4_fs),
        strip.text = element_text(size = f4_fs, face = "plain"))

# Create the explanation scatterplot for the lime explanation for the sine data
exp_scatter <-
  plot_explain_scatter(
    sine_poi_lime_default,
    alpha = 0.4,
    line_size = f4_ls
  ) +
  facet_grid(switch = "both") +
  labs(
    x = TeX("x_1"),
    y = TeX("x_2"),
    size = "Weight",
    shape = "",
    fill = "Random \nForest \nProbability \nfor 'blue'",
    color = "Random \nForest \nProbability \nfor 'blue'",
    linetype = "",
    title = "Explanation Scatterplot"
  ) +
  theme_bw(base_family = ff, base_size = f4_fs) +
  guides(shape = guide_legend(order = 1),
         size = guide_legend(
           nrow = 2,
           byrow = T,
           override.aes = list(alpha = 1)
         )) +
  theme(aspect.ratio = 1,
        plot.title = element_text(size = f4_fs)) 

# Join the plots
plot_grid(sine_poi_exp, exp_scatter) 



## ----figure-05, out.width = '3.125in', fig.width = 6, fig.height = 3----

# Specify the figure size (for determining font size)
f5_ow = 3.125
f5_fw = 6
f5_fs = fs * (f5_fw / f5_ow)

# Specify the number of cases and LIME input options
ncases = 10
ninputs = 5

# Create a good case of chosen feature example data
set.seed(20191008)
heatmap_data_good <-
  tibble(
    case = factor(rep(1:ncases, each = ninputs),
                  levels = ncases:1),
    input = factor(rep(1:ninputs, ncases)),
    feature = factor(c(
      rep(1, 5),
      rep(2, 5),
      rep(3, 5),
      rep(1, 5),
      rep(1, 5),
      rep(4, 5),
      rep(1, 5),
      rep(3, 5),
      rep(1, 5),
      rep(4, 5)
    ))
  ) %>%
  mutate(input = forcats::fct_recode(
    input,
    "A" = "1",
    "B" = "2",
    "C" = "3",
    "D" = "4",
    "E" = "5"
  ))

# Create the conceptual good heatmap
heatmap_plot_good <- ggplot(data = heatmap_data_good,
                            mapping = aes(
                              x = input,
                              y = case,
                              fill = feature,
                              color = feature
                            )) +
  geom_tile() +
  labs(
    x = "Tuning Parameter Value",
    y = "Case",
    fill = "Feature",
    color = "Feature",
    title = "Situation 1",
    subtitle = "Consistent Explanations"
  ) +
  theme_bw(base_family = ff, base_size = f5_fs) +
  scale_fill_grey() +
  scale_color_grey() +
  theme(legend.position = "none", 
        plot.title = element_text(size = f5_fs))

# Create a bad case of chosen feature example data
set.seed(20190627)
heatmap_data_bad <- tibble(
  case = factor(rep(1:ncases, ninputs),
                levels = ncases:1),
  input = factor(rep(1:ninputs, each = ncases)),
  feature = factor(rep(c(1, 2, 4, 3, 2), each = 10))
) %>%
  mutate(input = forcats::fct_recode(
    input,
    "A" = "1",
    "B" = "2",
    "C" = "3",
    "D" = "4",
    "E" = "5"
  ))

# Create the conceptual bad heat map
heatmap_plot_bad <- ggplot(data = heatmap_data_bad,
                           mapping = aes(
                             x = input,
                             y = case,
                             fill = feature,
                             color = feature
                           )) +
  geom_tile() +
  labs(
    x = "Tuning Parameter Value",
    y = "Case",
    fill = "Feature",
    color = "Feature",
    title = "Situation 2",
    subtitle = "Inconsistent Explanations"
  ) +
  theme_bw(base_family = ff, base_size = f5_fs) +
  theme(plot.title = element_text(size = f5_fs)) +
  scale_fill_grey() +
  scale_color_grey()

heatmap_leg <- get_legend(heatmap_plot_bad)
heatmap_plot_bad <-
  heatmap_plot_bad + theme(legend.position = 'none')

# Join the plots
plot_grid(
  heatmap_plot_good,
  heatmap_plot_bad,
  heatmap_leg,
  nrow = 1,
  rel_widths = c(0.43, 0.43, 0.14)
)



## ----figure-06, out.width = '3.125in', fig.width = 4, fig.height = 4.5----

# Specify the figure size (for determining font size)
f6_ow = 3.125
f6_fw = 4
f6_fs = fs * (f6_fw / f6_ow)

# Create the example feature heatmap with the sine data
plot_feature_heatmap(sine_lime_explain$explain,
                     order_method = "PCA") +
  theme_bw(base_family = ff, base_size = f6_fs) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(color = "white", fill = "white")
  ) +
  labs(y = "Case")



## ----figure-07, out.width = '3.25in', fig.width = 4.5, fig.height = 3.75, warning = FALSE----

# Specify the figure size (for determining font size)
f7_ow = 3.25
f7_fw = 4.5
f7_fs = fs * (f7_fw / f7_ow)

# Create the metric plot
plot_metrics(sine_lime_explain$explain, rank_legend = "discrete") +
  theme_bw(base_family = ff, base_size = f7_fs) +
  theme(
    strip.background = element_rect(color = "white", fill = "white"),
    strip.placement = "outside",
    strip.text.y = element_text(size = f7_fs)
  )



## ----bullet-data-----------------------------------------

# Load the hamby data
bullet_train <- read.csv("../data/hamby173and252_train.csv") %>%
  mutate(case = factor(1:n())) %>%
  select(case, everything())
bullet_test <- read.csv("../data/hamby224_test.csv")

# Extract the features and order them based on feature importance
bullet_features <- rownames(bulletxtrctr::rtrees$importance)
bullet_features_ordered <-
  data.frame(
    feature = rownames(bulletxtrctr::rtrees$importance),
    MeanDecreaseGini = bulletxtrctr::rtrees$importance
  ) %>%
  arrange(desc(MeanDecreaseGini)) %>%
  mutate(
    feature = fct_recode(feature,
      "Cross Correlation Function" = "ccf",
      "Consecutively Matching Striae" = "cms",
      "Matches" = "matches",
      "Mismatches" = "mismatches",
      "Non-Consecutively Matching Striae" = "non_cms",
      "Rough Correlation" = "rough_cor",
      "Distance Standard Deviation" = "sd_D",
      "Distance" = "D",
      "Sum of Peaks" = "sum_peaks"
    )
  )

# Create an ordered version of the data
bullet_features_ordered <- bullet_features_ordered %>%
  mutate(feature = factor(feature, levels = bullet_features_ordered$feature))



## ----figure-08, out.width = '2.25in', fig.width = 6, fig.height = 3, warning = FALSE----

# Convert the bullet figure to eps (if needed)
if (!file.exists("./figure-static/figure-08-1.eps")) {
  bullets <- image_read("./figure-static/figure-08-1.png")
  image_write(bullets, path = "./figure-static/figure-08-1.eps", format = "eps")
}

# Load and print the plot
knitr::include_graphics("./figure-static/figure-08-1.eps")



## ----figure-09, out.width = '3.125in', fig.width = 6, fig.height = 3.5, warning = FALSE----

# Specify the figure size (for determining font size)
f9_ow = 3.125
f9_fw = 6
f9_fs = fs * (f9_fw / f9_ow)

# Load the signatures data
signatures <- readRDS("../data/signatures.rds")

# Create plot of the signatures
signatures %>%
  mutate(land = c("Signature 1", "Signature 2")[as.factor(source)]) %>%
  ggplot(aes(x = x/1000)) + 
  geom_line(aes(y = sig), colour = "grey30") +
  facet_grid(land~.) +
  ylim(c(-4,6)) +
  theme_bw(base_family = ff, base_size = f9_fs) +
  theme(strip.background = element_blank()) +
  ylab("Surface Measurement (in microns)") +
  xlab("Relative Location (in millimeters)") 



## ----figure-10, out.width = '6.5in', warning = FALSE-----

# Create or load the parallel coordinate plot
if (!file.exists("./figure-static/figure-10-1.png")) {

  # Specify the figure size (for determining font size)
  f10_ow = 6.5
  f10_fw = 7.5
  f10_fs = 7 * (f10_fw / f10_ow)
  f10_ls = 0.5 * (f10_fw / f10_ow)

  # Create the plot
  bullet_pcp <- bullet_train %>%
    select(all_of(bullet_features), samesource, rfscore) %>%
    bind_rows(bullet_test %>%
                select(all_of(bullet_features), samesource, rfscore),
              .id = "set") %>%
    mutate(
      set = fct_recode(set,
                       "Training Set" = "1",
                       "Testing Set" = "2"),
      samesource = fct_recode(
        factor(samesource),
        "Match" = "TRUE",
        "Non-Match" = "FALSE"
      )
    ) %>%
    rename(
      "Cross Correlation Function" = "ccf",
      "Consecutively Matching Striae" = "cms",
      "Matches" = "matches",
      "Mismatches" = "mismatches",
      "Non-Consecutively Matching Striae" = "non_cms",
      "Rough Correlation" = "rough_cor",
      "Distance Standard Deviation" = "sd_D",
      "Distance" = "D",
      "Sum of Peaks" = "sum_peaks"
    ) %>%
    na.omit() %>%
    ggplot(aes(color = rfscore)) +
    geom_pcp(
      mapping = aes(vars = vars(bullet_features_ordered$feature)),
      alpha = 0.2,
      size = f10_ls
    ) +
    facet_grid(set ~ samesource) +
    scale_color_gradient2(low = "grey50",
                          high = "darkorange",
                          midpoint = 0.5) +
    theme_bw(base_family = ff, base_size = f10_fs) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.placement = "outside",
      strip.background = element_rect(color = "white",
                                      fill = "white")
    ) +
    labs(x = "Features Ordered by Random Forest Variable Importance \n(highest to lowest from left to right)",
         y = "Scaled Feature Values",
         color = "Random \nForest \nProbability")

  # Save the plot
  ggplot2::ggsave(
    plot = bullet_pcp,
    filename = "./figure-static/figure-10-1.png",
    width = 7.5,
    height = 3.5,
    units = "in",
    dpi = 1200
  )
  
  # Load the figure and then save as an EPS file
  bullet_pcp <- image_read("./figure-static/figure-10-1.png")
  image_write(bullet_pcp, path = "./figure-static/figure-10-1.eps", format = "eps")
  
} else {
 
   # Load and print the plot
  knitr::include_graphics("./figure-static/figure-10-1.eps")

}



## ----bullet-lime-----------------------------------------

# Apply LIME to all but two cases without returning the permutations
if (file.exists("../data/hamby_lime.rds")){

  # Load the files
  bullet_explain_noperms <- readRDS("../data/hamby_explain.rds")

} else {

  # Apply lime with various input options to the hamby data
  bullet_lime_explain_noperms <- apply_lime(
    train = bullet_train %>% select(all_of(bullet_features)),
    test = bullet_test %>%
      select(all_of(bullet_features)) %>%
      na.omit(),
    model = bulletxtrctr::rtrees,
    label = as.character(TRUE),
    n_features = 3,
    sim_method = c('quantile_bins', 'equal_bins',
                   'kernel_density', 'normal_approx'),
    nbins = 2:6,
    feature_select = "auto",
    dist_fun = "gower",
    kernel_width = NULL,
    gower_pow = c(0.5, 1, 10),
    return_perms = FALSE,
    all_fs = FALSE,
    seed = 20190914)

  # Separate the lime and explain parts of the results
  bullet_lime_noperms <- bullet_lime_explain_noperms$lime
  bullet_explain_noperms <- bullet_lime_explain_noperms$explain

  # Save the output objects
  saveRDS(object = bullet_lime_noperms,
          file = "../data/hamby_lime.rds")
  saveRDS(object = bullet_explain_noperms,
          file = "../data/hamby_explain.rds")

}



## ----bullet-lime-perms-----------------------------------

# Specify two cases of interest
bullet_poi_match <- unique(bullet_explain_noperms$case)[c(325)]
bullet_poi_nonmatch <- unique(bullet_explain_noperms$case)[c(20)]

# Apply LIME to two cases with the permutations returned
bullet_lime_explain_perms <- apply_lime(
  train = bullet_train %>% select(all_of(bullet_features)),
  test = bullet_test %>%
    filter(case %in% bullet_poi_match) %>%
    bind_rows(bullet_test %>%
                filter(case %in% bullet_poi_nonmatch)) %>%
    select(all_of(bullet_features)),
  model = bulletxtrctr::rtrees,
  label = as.character(TRUE),
  n_features = 3,
  sim_method = c(
    'quantile_bins',
    'equal_bins',
    'kernel_density',
    'normal_approx'
  ),
  nbins = 3,
  feature_select = "auto",
  dist_fun = "gower",
  kernel_width = NULL,
  gower_pow = 0.5,
  return_perms = TRUE,
  all_fs = FALSE,
  seed = 20190914
)

# Separate the lime and explain parts of the results
bullet_lime_perms <- bullet_lime_explain_perms$lime
bullet_explain_perms <- bullet_lime_explain_perms$explain %>%
  mutate(case = ifelse(case == 1,
                       bullet_poi_match,
                       bullet_poi_nonmatch))



## ----bullet-lime-combined--------------------------------

# Determine the application and case number of the poi
# in the no_perms data
poi_cases_no_perms <- bullet_explain_noperms %>%
  filter(case %in% c(bullet_poi_match, bullet_poi_nonmatch),
         nbins %in% c(3, NA),
         gower_pow == 0.5) %>%
  select(case, implementation) %>%
  unique()

# Join the no perms data (with poi removed) with the
# perms data (perms removed)
bullet_explain <- bullet_explain_noperms %>%
  anti_join(poi_cases_no_perms,
            by = c("implementation", "case")) %>%
  bind_rows(
    bullet_explain_perms %>%
      mutate(
        implementation = fct_recode(
          implementation,
          "2" = "1",
          "7" = "2",
          "31" = "3",
          "32" = "4"
        )
      ) %>%
      select(
        -perms_raw,
        -perms_numerified,-perms_pred_simple,
        -perms_pred_complex,-weights
      )
  )



## ----figure-11, out.width = '6.5in', fig.width = 18, fig.height = 14----

# Specify the figure size (for determining font size)
f11_ow = 6.5
f11_fw = 18
f11_fs = fs * (f11_fw / f11_ow)

# Create a feature heatmap
plot_feature_heatmap(
  bullet_explain %>%
    mutate(
      label = as.factor(label),
      feature = fct_recode(
        feature,
        "Rough Correlation" = "rough_cor",
        "Consecutively Matching Striae" = "cms",
        "Distance" = "D",
        "Matches" = "matches",
        "Mismatches" = "mismatches",
        "Non-Consecutively Matching Striae" = "non_cms",
        "Cross Correlation Function" = "ccf",
        "Sum of Peaks" = "sum_peaks",
        "Distance Standard Deviation" = "sd_D"
      )
    ),
  facet_var = bullet_test %>%
    mutate(samesource = fct_recode(
      factor(samesource),
      "Match" = "TRUE",
      "Non-Match" = "FALSE"
    )) %>%
    pull(samesource) %>%
    na.omit(),
  order_method = "PCA"
) +
  scale_fill_gretchenalbrecht(palette = "last_rays", discrete = TRUE) +
  scale_color_gretchenalbrecht(palette = "last_rays",
                               discrete = TRUE,
                               reverse = TRUE) +
  theme_bw(base_family = ff, base_size = f11_fs) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.background = element_rect(color = "white", fill = "white"),
    strip.text.y.right = element_text(angle = 0),
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  guides(fill = guide_legend(nrow = 3)) +
  labs(y = "Case", color = "Complex Model Feature", fill = "Complex Model Feature")



## ----figure-12, out.width = '6.5in', warning = FALSE, fig.width = 10, fig.height = 4.25----

# Specify the figure size (for determining font size)
f12_ow = 6.5
f12_fw = 10
f12_fs = fs * (f12_fw / f12_ow)
f12_ls = 0.5 * (f12_fw / f12_ow)

# Create a metric comparison plot
plot_metrics(
  bullet_explain %>%
    mutate(label = as.factor(label)),
  add_lines = TRUE,
  line_alpha = 0.75,
  line_size = f12_ls
) +
  theme_bw(base_family = ff, base_size = f12_fs) +
  theme(
    strip.background = element_rect(color = "white", fill = "white"),
    strip.placement = "outside",
    strip.text.y = element_text(size = f12_fs)
  )



## ----figure-13, out.width = '6.25in', fig.width = 13, fig.height = 17.5, message = FALSE----

# Specify the figure size (for determining font size)
f13_ow = 6.25
f13_fw = 13
f13_fs = fs * (f13_fw / f13_ow)
f13_ls = 0.5 * (f13_fw / f13_ow)

# Adjust names of the cases
bullet_explain_perms_clean <- bullet_explain_perms %>%
  mutate(case = ifelse(case == bullet_poi_nonmatch, "NM", "M"))

# Create a theme for the lime explanation visualizations
bullet_explain_plot_theme <-
  list(
    scale_fill_manual(values = c("darkorange", "grey50")),
    theme(
      text = element_text(family = ff, size = f13_fs),
      strip.text.x = element_text(face = "plain", size = f13_fs)
    )
  )

# Create the lime explanation visualizations
pfM_3qb <- plot_features(bullet_explain_perms_clean[1:3,]) + bullet_explain_plot_theme
pfNM_3qb <- plot_features(bullet_explain_perms_clean[4:6,]) + bullet_explain_plot_theme
pfM_3eb <- plot_features(bullet_explain_perms_clean[7:9,]) + bullet_explain_plot_theme
pfNM_3eb <- plot_features(bullet_explain_perms_clean[10:12,]) + bullet_explain_plot_theme

# Create theme for explanation scatterplots
bullet_eoi_plot_theme <-
  list(
    scale_color_gradient2(
      low = "grey50",
      high = "darkorange",
      midpoint = 0.5,
      limits = c(0, 1)
    ),
    scale_fill_gradient2(
      low = "grey50",
      high = "darkorange",
      midpoint = 0.5,
      limits = c(0, 1)
    ),
    theme_bw(base_family = ff, base_size = f13_fs),
    theme(
      aspect.ratio = 1,
      plot.title = element_text(size = f13_fs),
      strip.text.x = element_text(size = f13_fs),
      strip.text.y = element_text(size = f13_fs),
      strip.placement = "outside",
      strip.background = element_rect(color = "white", fill = "white")
    ),
    guides(size = guide_legend(nrow = 2, byrow = T), 
           linetype = guide_legend(override.aes = list(color = c("grey50", "darkorange"))))
  )

# Create the explanation scatterplots
eoiM_3qb <-
  plot_explain_scatter(
    bullet_explain_perms_clean[1:3, ],
    alpha = 0.9,
    weights = TRUE,
    line_size = f13_ls
  ) +
  bullet_eoi_plot_theme +
  guides(linetype = guide_legend(override.aes = list(color = c("darkorange"))))
eoiNM_3qb <-
  plot_explain_scatter(
    bullet_explain_perms_clean[4:6, ],
    alpha = 0.9,
    weights = TRUE,
    line_size = f13_ls
  ) +
  bullet_eoi_plot_theme
eoiM_3eb <-
  plot_explain_scatter(
    bullet_explain_perms_clean[7:9, ],
    alpha = 0.9,
    weights = TRUE,
    line_size = f13_ls
  ) +
  bullet_eoi_plot_theme
eoiNM_3eb <-
  plot_explain_scatter(
    bullet_explain_perms_clean[10:12,],
    alpha = 0.9,
    weights = TRUE,
    line_size = f13_ls
  ) +
  bullet_eoi_plot_theme

# Join the plots
plot_grid(
  pfNM_3qb,
  pfNM_3eb,
  eoiNM_3qb,
  eoiNM_3eb,
  pfM_3qb,
  pfM_3eb,
  eoiM_3qb,
  eoiM_3eb,
  nrow = 4,
  rel_heights = c(0.2, 0.3, 0.2, 0.3)
)



## ----figure-B1, out.width = '6.5in', fig.width = 12, fig.height = 5, fig.align = "center"----

# Specify the figure size (for determining font size and line size)
fb1_ow = 6.5
fb1_fw = 12
fb1_fs = fs * (fb1_fw / fb1_ow)
fb1_ls = 0.5 * (fb1_fw / fb1_ow)

# Create the explanation scatterplot
plot_explain_scatter(
  sine_lime_explain$explain %>%
    filter(sim_method == "kernel_density", case == sine_poi %>% pull(case)),
  alpha = 0.75,
  title.opt = FALSE, 
  line_size = fb1_ls 
) +
  theme_bw(base_family = ff, base_size = fb1_fs) +
  theme(
    strip.background = element_rect(fill = "white", color = "white"),
    strip.placement = "outside",
    strip.text.x = element_text(size = fb1_fs)
  )



## ----figure-B2, out.width = '6.5in', fig.width = 12, fig.height = 10, fig.align = "center"----

# Specify the figure size (for determining font size and line size)
fb2_ow = 6.5
fb2_fw = 12
fb2_fs = fs * (fb2_fw / fb2_ow)
fb2_ls = 0.5 * (fb2_fw / fb2_ow)

# Create the explanation scatterplot: case M, kernel density
bullet_es_M <-
  plot_explain_scatter(
    bullet_explain_perms[16:18,] %>% mutate(case = "NM"),
    alpha = 0.75,
    title.opt = TRUE, 
    line_size = fb2_ls
  ) +
  theme_bw(base_family = ff, base_size = fb2_fs) +
  theme(
    aspect.ratio = 1,
    strip.background = element_rect(fill = "white", color = "white"),
    strip.placement = "outside",
    strip.text.x = element_text(size = fb2_fs),
    plot.title = element_text(size = fb2_fs)
  )

# Create the explanation scatterplot: case NM, kernel density
bullet_es_NM <-
  plot_explain_scatter(
    bullet_explain_perms[13:15, ] %>% mutate(case = "M"),
    alpha = 0.75,
    title.opt = TRUE, 
    line_size = fb2_ls
  ) +
  theme_bw(base_family = ff, base_size = fb2_fs) +
  theme(
    aspect.ratio = 1,
    strip.background = element_rect(fill = "white", color = "white"),
    strip.placement = "outside",
    strip.text.x = element_text(size = fb2_fs), 
    plot.title = element_text(size = fb2_fs)
  )

# Join the plots
plot_grid(bullet_es_NM, bullet_es_M, nrow = 2)



## ----figure-C3, out.width = '3.125in', fig.width = 5, fig.height = 4, warning = FALSE----

# Specify the figure size (for determining font size and line size)
fc3_ow = 3.125
fc3_fw = 5
fc3_fs = fs * (fc3_fw / fc3_ow)
fc3_ls = 0.5 * (fc3_fw / fc3_ow)

# Calculate the residuals from the explainer model
y <- sine_poi_perms$rfpred
sine_poi_explainer <-
  function(x1, x2)
    sine_b0 + (sine_b1 * x1) + (sine_b2 * x2)
sine_poi_perms <- sine_poi_perms %>%
  mutate(exppred = sine_poi_explainer(x1num, x2num)) %>%
  mutate(resid = exppred - rfpred)

# Create the residual plot
ggplot(sine_poi_perms, aes(x = exppred, y = resid)) +
  geom_hline(yintercept = 0, size = fc3_ls) +
  geom_jitter(alpha = 0.5, width = 0.02) +
  theme_bw(base_family = ff, base_size = fc3_fs) +
  theme(plot.title = element_text(size = fc3_fs)) +
  labs(x = "Explainer Model Predictions",
       y = "Explainer Model Residuals",
       title = "Explainer Model Residual Plot")


