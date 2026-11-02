# flourishR
# Package development workflow


# Project

setwd(
  "~/Library/CloudStorage/GoogleDrive-ty781431@gmail.com/My Drive/Research/Well-Being Research/Meta Science Projects/Programming Wellbeing Project/flourishR"
)

list.files()


# Package metadata ------

readLines("DESCRIPTION")

readLines("NAMESPACE")


# Documentation ---------

devtools::document()

list.files("man")


# Load development version

devtools::load_all()


# Testing

devtools::test()

devtools::run_examples()


# Package check ---------

devtools::check()


# One-time package setup

# install.packages(
#   c(
#     "devtools",
#     "usethis",
#     "testthat",
#     "roxygen2",
#     "haven"
#   )
# )

# usethis::create_package(getwd())

# usethis::use_testthat(3)

# usethis::use_package("haven")

# usethis::use_build_ignore("functions")

# usethis::use_mit_license("Taylor Hill")
