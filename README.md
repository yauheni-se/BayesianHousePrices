# Description
Research based on a dataset prepared by the UC Irvine ML Repository. The goal of the research is to apply the Bayesian approach to discover factors influencing house prices in New Taipei City and check the prior assumptions on the basis of the previous papers.

# Content
- data - folder with raw and cleaned datasets as well as .rstan files with prior features' distributions.
- data_preparation.R - initial data preparation procedure stored as R script
- analysis.R - analysis, modeling and testing procedure stored as R script
- Research.pdf - main file containing full research description

# Methodology
Final estimates were achieved by applying the analytical Bayesian approach, which combined prior assumptions and estimations from the Ordinal Least Squares regression model.

# Findings
The a posteriori expected values for all factors (not counting no_stores) are closer to the OLS estimates than the a priori estimates. The expected values of the a priori and OLS parameters are consistent with respect to the direction of the impact.