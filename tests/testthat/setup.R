# Setup file for baselinenowcast tests
#
# This file runs once before all tests execute.
# It creates shared test data that is used across multiple test files.

# Create standardized COVID-19 test data
covid_data <- create_covid_test_data()
