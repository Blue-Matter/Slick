# Engineering

## Dependencies ----
## Amend DESCRIPTION with dependencies read from package code parsing
## install.packages('attachment') # if needed.
attachment::att_amend_desc()

## Add modules ----
## Create a module infrastructure in R/
golem::add_module(name = "Resources", with_test = FALSE) # Name of the module
golem::add_module(name = "About")
golem::add_module(name = "Sidebar")
golem::add_module(name = "Home")
golem::add_module(name = "toplink")
golem::add_module(name = "Metadata")
golem::add_module(name = "Filter")
golem::add_module(name = "Filter_select")

golem::add_module(name = "subtitle")


golem::add_module(name = "MP_Info")
golem::add_module(name = "OM_Info")
golem::add_module(name = "PM_Info")


golem::add_module(name = "Boxplot")
golem::add_module(name = "Boxplot_plot")
golem::add_module(name = "Boxplot_overall")
golem::add_module(name = "Boxplot_OM")

golem::add_module(name = "Quilt")
golem::add_module(name = "Quilt_plot")

golem::add_module(name = "Tradeoff")
golem::add_module(name = "TradeOff_plot")

golem::add_module(name = "Spider")
golem::add_module(name = "Spider_plot")
golem::add_module(name = "Spider_MP")
golem::add_module(name = "Spider_OM")
golem::add_module(name = "Spider_overall")


golem::add_module(name = "Kobe")

golem::add_module(name = "Timeseries")

golem::add_module(name = "Report_Page")
golem::add_module(name = "Report_Add")
golem::add_module(name = "Report_Add_Button")

golem::add_module(name = "Zigzag_plot")
## Add helper functions ----
## Creates fct_* and utils_*
golem::add_fct("wrapper_dropdownbutton")
golem::add_fct("Check")
golem::add_fct("plots")

golem::add_utils("util", with_test = FALSE)
golem::add_utils("Table", with_test = FALSE)

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file("script")
golem::add_js_handler("handlers")
golem::add_css_file("styles")
golem::add_sass_file("custom")

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw(name = "my_dataset", open = FALSE)

## Tests ----
## Add one line by test you want to create
usethis::use_test("app")

# Documentation

## Vignette ----
usethis::use_vignette("slickgolem")
devtools::build_vignettes()

## Code Coverage----
## Set the code coverage service ("codecov" or "coveralls")
usethis::use_coverage()

# Create a summary readme for the testthat subdirectory
covrpage::covrpage()

## CI ----
## Use this part of the script if you need to set up a CI
## service for your application
##
## (You'll need GitHub there)
usethis::use_github()

# GitHub Actions
usethis::use_github_action()
# Chose one of the three
# See https://usethis.r-lib.org/reference/use_github_action.html
usethis::use_github_action_check_release()
usethis::use_github_action_check_standard()
usethis::use_github_action_check_full()
# Add action for PR
usethis::use_github_action_pr_commands()

# Travis CI
usethis::use_travis()
usethis::use_travis_badge()

# AppVeyor
usethis::use_appveyor()
usethis::use_appveyor_badge()

# Circle CI
usethis::use_circleci()
usethis::use_circleci_badge()

# Jenkins
usethis::use_jenkins()

# GitLab CI
usethis::use_gitlab_ci()
