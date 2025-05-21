
# Assign values to a new `Slick` object
slick <- Slick()

Title(slick) <- 'An Example Slick Object'
Subtitle(slick) <- ""
Date(slick) <- Sys.Date()
Author(slick) <- 'Adrian Hordyk'
Email(slick) <-  "[mailto:adrian@bluematterscience.com](mailto:adrian@bluematterscience.com)"
Institution(slick) <- "[Blue Matter Science](bluematterscience.com)"

Introduction(slick) <- "This is the Introduction text"

# Access values from `Slick` object
Title(slick)
Subtitle(slick)
Date(slick)
Author(slick)
Email(slick)
Institution(slick)
Introduction(slick)

