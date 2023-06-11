# 0. Presets and data import ----
#options(scipen = 999)
library(dplyr)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(mvtnorm)
library(lmtest)
library(manipulate)

dataset <- readr::read_csv("data/data_cleared.csv")
dataset


# 1. Describing variables ----
make_density <- function(variable, df, plot_type = "density") {
  df %>% 
    ggplot(aes_string(x = variable)) +
    do.call(paste0("geom_", plot_type), list(fill = "darkgreen", alpha = 0.6, color = "#131313")) +
    labs(y = "", title = plot_type) +
    theme(
      panel.background = element_rect(fill = "#e4dcdc"),
      plot.background = element_rect(fill = "#e4dcdc"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#cacaca")
    )
}
make_box <- function(variable, df, variable_x = "year_month") {
  df %>% 
    ggplot(aes_string(variable_x, variable)) +
    geom_boxplot(fill = "darkgreen") +
    labs(title = "box plot") +
    theme(
      panel.background = element_rect(fill = "#e4dcdc"),
      plot.background = element_rect(fill = "#e4dcdc"),
      panel.grid.major.y = element_line(color = "#cacaca"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#cacaca"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )
}
make_density_cat <- function(variable, df) {
  df %>% 
    ggplot(aes_string(variable)) +
    geom_bar(color = "#131313", fill = "darkgreen", alpha = 0.6) +
    geom_text(stat='count', aes(label=..count..), vjust=1.2) +
    labs(y = "", title = "histogram") +
    theme(
      panel.background = element_rect(fill = "#e4dcdc"),
      plot.background = element_rect(fill = "#e4dcdc"),
      panel.grid.major = element_line(color = "#cacaca"),
      panel.grid.minor = element_blank()
    )
}
make_point <- function(variable, df=df_sel, variable_y = "price") {
  df %>% 
    ggplot(aes_string(variable, variable_y)) +
    geom_point(color = "darkgreen") +
    labs(title = "point plot")+
    theme(
      panel.background = element_rect(fill = "#e4dcdc"),
      plot.background = element_rect(fill = "#e4dcdc"),
      panel.grid.major.y = element_line(color = "#cacaca"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#cacaca")
    )
}
make_qq <- function(variable, df) {
  ggplot(df, aes_string(sample = variable)) + 
    stat_qq(color = "darkgreen") + 
    stat_qq_line(size = 1.2)+
    labs(y = "", x = variable, title = "QQ plot") +
    theme(
      panel.background = element_rect(fill = "#e4dcdc"),
      plot.background = element_rect(fill = "#e4dcdc"),
      panel.grid.major.y = element_line(color = "#cacaca"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "#cacaca")
    )
}


# price
ggarrange(
  make_density("price", dataset),
  make_density("price", dataset, "histogram"),
  make_box("price", dataset),
  make_qq("price", dataset),
  nrow = 2,
  ncol = 2
)


# age
ggarrange(
  make_density("age", dataset),
  make_density("age", dataset, "histogram"),
  make_point("age", dataset),
  nrow = 3
)


# dist_to_mrt
df_temp <- dataset %>% 
  mutate(dist_to_mrt_log = log(dist_to_mrt))
ggarrange(
  make_density("dist_to_mrt", dataset),
  make_density("dist_to_mrt", dataset, "histogram"),
  make_density("dist_to_mrt_log", df_temp),
  make_density("dist_to_mrt_log", df_temp, "histogram"),
  nrow = 2,
  ncol = 2
)
ggarrange(
  make_point("dist_to_mrt", dataset),
  make_point("dist_to_mrt_log", df_temp),
  nrow = 2
)

# no_stores
df_temp <- dataset %>% 
  mutate(
    no_stores = as.factor(no_stores)
  )

ggarrange(
  make_density("no_stores", dataset),
  make_density_cat("no_stores", dataset),
  make_box("price", df_temp, "no_stores"),
  nrow = 3
)


# data cleaning
df_sel <- dataset %>% 
  mutate(
    dist_to_mrt_log = log(dist_to_mrt)
  ) %>% 
  select(-year_month, -dist_to_mrt) %>% 
  filter(price < 100)


# correlation
cor(df_sel) %>% 
  as_tibble(rownames = "variable") %>% 
  mutate_if(is.numeric, round, 2)





# 2. OLS regression ----
model <- lm(price ~ age+no_stores+dist_to_mrt_log, df_sel)
summary(model)

bptest(model)    # HC is present
gqtest(model)    # HC is not present
dwtest(model)    # AK is not present
Box.test(df_sel$price)    # AK is not present
car::vif(model)   # No multicollinearity

N.data <- nrow(df_sel)
X <- cbind(
  as.matrix(rep(1, N.data)),
  df_sel %>% select(-price) %>% as.data.frame() %>% as.matrix()
)
Beta.ols.data <- model$coefficients
v.data <- length(model$residuals)
XTX.data <- t(X) %*% as.matrix(X)
s2.data <- sum((model$residuals) ^ 2) / v.data


# 3. Prior ----
Beta.ols.data

intercept_prior <- mean(df_sel$price) %>% round(2)
no_stores_prior <- 1.106
age_prior <- -0.05
dist_to_mrt_log_prior <- -8

Beta.prior <- c(intercept_prior, age_prior, no_stores_prior, dist_to_mrt_log_prior)
Sd.prior <- c(5, 0.01, 0.025, 3)
sm2.prior <- 44.44  # s2.prior = 0.0225, s.prior = 0.15
U.prior <- diag(Sd.prior^2)
v.prior <- 100
vs2.prior <- v.prior / sm2.prior

Beta.prior
sm2.prior
U.prior
v.prior


# 4. Posterior ----
Beta.posterior <- solve(solve(U.prior) + XTX.data) %*% (solve(U.prior) %*% Beta.prior + XTX.data %*% Beta.ols.data)
U.posterior <- solve(solve(U.prior) + XTX.data)
v.posterior <- v.prior + N.data
vs2.posterior <- v.prior / sm2.prior + v.data * s2.data + t(Beta.ols.data - Beta.prior) %*% solve(U.prior + solve(XTX.data)) %*% (Beta.ols.data - Beta.prior)
sm2.posterior <- 1 / (vs2.posterior / v.posterior)


Beta.posterior %>% 
  as_tibble(rownames = "variable") %>% 
  rename(posterior = V1) %>% 
  mutate(
    variable = ifelse(variable == "", "intersept", variable),
    model = summary(model)$coefficients[, 1],
    prior = Beta.prior
  )

# 5. Posterior plots ----
beta.space <- seq(from = -3, to = 3, length.out = 10000) #length.out zamiast by dla ulatwienia parametryzacji
n_eval_points <- length(beta.space)
n_parameters <- length(Beta.posterior)
prior.marg.dens.beta <- matrix(NA, nrow = n_parameters, ncol = n_eval_points)
posterior.marg.dens.beta <- matrix(NA, nrow = n_parameters, ncol = n_eval_points)
beta_space_from <- c(35, -0.25, 0.8, -9)
beta_space_to <- c(90, 0.025, 1.4, -6)

for (ii in 1:length(Beta.posterior)) {
  
  beta.space <- seq(from = beta_space_from[ii], to = beta_space_to[ii], length.out = 10000)
  n_eval_points <- length(beta.space)
  n_parameters <- length(Beta.posterior)
  prior.marg.dens.beta[ii, ] <- apply(as.matrix(beta.space), 1, dmvt,
                                      delta = Beta.prior[ii], sigma = as.matrix(U.prior[ii, ii] / sm2.prior), df = v.prior, log = FALSE)
  posterior.marg.dens.beta[ii, ] <- apply(as.matrix(beta.space), 1, dmvt,
                                          delta = Beta.posterior[ii], sigma = as.matrix(U.posterior[ii, ii] / sm2.posterior), df = v.posterior, log = FALSE)
}

grey_area <- rgb(160, 160, 160, 80, names = NULL, maxColorValue = 255)
grey_line <- rgb(80, 80, 80, 160, names = NULL, maxColorValue = 255)
green_area <- rgb(24, 121, 104, 80, names = NULL, maxColorValue = 255)
green_line <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)

par(mfrow = c(2, 2))
for (ii in 1:length(Beta.posterior)) {
  beta.space <- seq(from = beta_space_from[ii], to = beta_space_to[ii], length.out = 10000)
  n_eval_points <- length(beta.space)
  n_parameters <- length(Beta.posterior)
  plot(beta.space, prior.marg.dens.beta[ii, ], las = 1, lwd = 2, bty = "n", col = grey_area,
       ylim = c(0, max(c(max(prior.marg.dens.beta[ii, ]),max(posterior.marg.dens.beta[ii, ]))) + 1), type = "l", ylab = "gestosc", main = colnames(X)[ii])
  polygon(c(beta.space, rev(beta.space)), c(prior.marg.dens.beta[ii, ], 
                                            rep(0, length(beta.space))), col = grey_area, border = NA)
  abline(v = Beta.prior[ii], col = grey_line, lwd = 3)
  text(Beta.prior[ii], max(prior.marg.dens.beta[ii, ]) + 0.4, paste("E(beta) a priori = ", round(Beta.prior[ii],2)), col = grey_line)
  abline(v = Beta.ols.data[ii], col = rgb(0, 0, 0, 1), lwd = 3)
  text(Beta.ols.data[ii], max(posterior.marg.dens.beta[ii, ]) + 0.2, paste("parametr OLS = ", round(Beta.ols.data[ii], 4)), col = rgb(0, 0, 0, 1))
  lines(beta.space, posterior.marg.dens.beta[ii, ], lwd = 2, col = green_line)
  polygon(c(beta.space, rev(beta.space)), c(posterior.marg.dens.beta[ii, ], 
                                            rep(0, length(beta.space))), col = green_area, border = NA)
  abline(v = Beta.posterior[ii], col = green_line, lwd = 3)
  text(Beta.posterior[ii], max(posterior.marg.dens.beta[ii, ]) + 0.6, paste("E(beta) a posteriori = ", round(Beta.posterior[ii], digits = 4)), col = green_line)
}


# 6. BF ----
# gamma(171) - number
# gamma(172) - inf
# => use min function + log

l_P_y_M1 <- 0.5*log(det(U.posterior)) + log(log(gamma(min(v.posterior/2, 171)))) - v.posterior/2*log(vs2.posterior) -
  (N.data/2*log(pi) + 0.5*det(U.prior) + log(gamma(v.prior/2)) - v.prior/2*log(vs2.prior))


N_param <- ncol(X)
l_P_y_M2 <- rep(NA, N_param-1)
l_BF <- rep(NA, N_param-1)
BF <- rep(NA, N_param-1)
for (ii in 2:N_param) {
  
  X_2 <- X[, -c(ii)]
  eval(parse(text = paste("OLS_results_2 <- lm(price ~ ", paste(colnames(X_2), collapse = "+"), ", data = df_sel)", sep = "")))
  Beta.ols.data_2 <- OLS_results_2$coefficients
  v.data_2 <- OLS_results_2$df.residual
  XTX.data_2 <- t(X_2) %*% X_2
  s2.data_2 <- sum((OLS_results_2$residuals) ^ 2) / v.data_2
  
  Beta.prior_2 <- Beta.prior[-c(ii)]
  U.prior_2 <- U.prior[- c(ii), -c(ii)]
  sm2.prior_2 <- sm2.prior
  v.prior_2 <- v.prior
  vs2.prior_2 <- vs2.prior
  
  Beta.posterior_2 <- solve(solve(U.prior_2) + XTX.data_2) %*% (solve(U.prior_2) %*% Beta.prior_2 + XTX.data_2 %*% Beta.ols.data_2)
  U.posterior_2 <- solve(solve(U.prior_2) + XTX.data_2)
  v.posterior_2 <- v.prior_2 + N.data
  vs2.posterior_2 <- v.prior_2 / sm2.prior_2 + v.data_2 * s2.data_2 + t(Beta.ols.data_2 - Beta.prior_2) %*% solve(U.prior_2 + solve(XTX.data_2)) %*% (Beta.ols.data_2 - Beta.prior_2)
  sm2.posterior_2 <- 1 / (vs2.posterior_2 / v.posterior_2)
  
  l_P_y_M2[ii - 1] <- 0.5*log(det(U.posterior_2)) + log(gamma(min(v.posterior/2, 171))) - v.posterior_2/2*log(vs2.posterior_2) -
    (N.data/2*log(pi) + 0.5*det(U.prior_2) + log(gamma(v.prior_2/2)) - v.prior_2/2*log(vs2.prior_2))
  
  l_BF[ii - 1] <- l_P_y_M1 - l_P_y_M2[ii - 1]
  
  BF[ii - 1] <- exp(l_BF[ii - 1])
}
BF_1_2_table <- tibble(names(Beta.ols.data[2:4]),BF) %>% 
  `colnames<-`(c("variable", "BF"))
BF_1_2_table


# 7. HPDI ----
#for(ii in 1:length(Beta.posterior)) 
ii = 2
beta_space_from <- c(70, -0.30, 0.6, -8)
beta_space_to <- c(90, -0.05, 1.4, -5.5)
{
  
  beta.space <- seq(from = beta_space_from[ii], to = beta_space_to[ii], by = 0.01)#200-400-200-400
  
  n_eval_points <- length(beta.space)
  n_parameters <- length(Beta.posterior)
  prior.marg.dens.beta <- matrix(NA, nrow = n_parameters, ncol = n_eval_points)
  posterior.marg.dens.beta <- matrix(NA, nrow = n_parameters, ncol = n_eval_points)
  prior.marg.dens.beta[ii, ] <- apply(as.matrix(beta.space), 1, dmvt,
                                      delta = Beta.prior[ii], sigma = as.matrix(U.prior[ii, ii] / sm2.prior), df = v.prior, log = FALSE)
  posterior.marg.dens.beta[ii, ] <- apply(as.matrix(beta.space), 1, dmvt,
                                          delta = Beta.posterior[ii], sigma = as.matrix(U.posterior[ii, ii] / sm2.posterior), df = v.posterior, log = FALSE)
  
  
  grey_area <- rgb(160, 160, 160, 80, names = NULL, maxColorValue = 255)
  grey_line <- rgb(80, 80, 80, 160, names = NULL, maxColorValue = 255)
  green_area <- rgb(24, 121, 104, 80, names = NULL, maxColorValue = 255)
  green_line <- rgb(13, 85, 72, 160, names = NULL, maxColorValue = 255)
  red_area <- rgb(255, 100, 123, 80, names = NULL, maxColorValue = 255)
  red_line <- rgb(200, 0, 30, 160, names = NULL, maxColorValue = 255)
  par(mfrow = c(1, 1))
  manipulate( 
    {
      credible_set_indicator <- as.vector(as.integer(posterior.marg.dens.beta[ii, ] > line_level))
      credible_set_begin <- match(1, credible_set_indicator)
      credible_set_end <- length(credible_set_indicator) - match(1, rev(credible_set_indicator))
      #Lewy i prawy brzeg HPDI
      x1 <- beta.space[credible_set_begin]
      x2 <- beta.space[credible_set_end]
      #Na potrzeby wykresu tworzymy wektor, który przyjmuje wartość gęstości a posteriori w HPDI i zero poza nim
      posterior.cs <- posterior.marg.dens.beta[ii, ] * credible_set_indicator
      #Poziom ufności
      HPDI_probab <- sum(posterior.cs) * 0.01
      #Wykres gęstości a posteriori
      plot(beta.space, posterior.marg.dens.beta[ii, ], las = 1, lwd = 2, bty = "n", col = green_line,
           ylim = c(0, max(posterior.marg.dens.beta[ii, ] + 1)), type = "l", ylab = "gęstość", main = colnames(X)[ii])
      polygon(c(beta.space, rev(beta.space)), 
              c(posterior.marg.dens.beta[ii, ], rep(0, length(beta.space))), 
              col = green_area, border = NA)
      text(Beta.posterior[ii], max(posterior.marg.dens.beta[ii, ]) + 0.6, paste("E(beta) a posteriori = ", round(Beta.posterior[ii], digits = 4)), col = green_line)
      abline(v = Beta.posterior[ii], col = green_line, lwd = 3)
      #Linia pozioma odcinająca "najwyższe" gęstości a posteriori (HPD)
      abline(h = line_level, col = red_line, lwd = 3)
      #Pole oznaczaj?ce gęstość a posteriori w przedziale ufno?ci HPDI
      polygon(c(beta.space, rev(beta.space)), 
              c(posterior.cs, rep(0, length(beta.space))), 
              col = red_area, border = NA)
      
      #Wyświetl poziom ufności i granice przedziału
      text(x=mean(beta.space),
           max(posterior.marg.dens.beta[ii, ])+0.2,
           paste(round(HPDI_probab * 100, digits = 2),
                 "% przedział HPDI: (", round(x1, digits = 4), " , ", round(x2, digits = 4), ")"), col = red_line)
    },
    line_level = slider(0, max(posterior.marg.dens.beta[ii, ]) + 0.002, step = 0.001, initial = max(posterior.marg.dens.beta[ii, ]) + 0.001)
  )
  
}