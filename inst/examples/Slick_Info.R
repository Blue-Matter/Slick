# Assign values to a new `Slick` object
slick <- Slick()

Title(slick) <- 'An Example Slick Object'
Subtitle(slick) <- "[Created for the Developer's Guide](https://slick.bluematterscience.com/articles/Building-A-Slick-Object.Rmd)"
Date(slick) <- Sys.Date()
Author(slick) <- 'Adrian Hordyk'
Email(slick) <-  "[mailto:adrian@bluematterscience.com](mailto:adrian@bluematterscience.com)"
Institution(slick) <- "[Blue Matter Science](bluematterscience.com)"

Introduction(slick) <- "
This is an example Slick object. It has been created for the purposes of demonstrating the key features of Slick.

The introduction can include paragraphs as well as Markdown such as *italics* and **bold** text and [Links](https://slick.bluematterscience.com).
"

# Access values from `Slick` object
Title(slick)
Subtitle(slick)
Date(slick)
Author(slick)
Email(slick)
Institution(slick)
Introduction(slick)
