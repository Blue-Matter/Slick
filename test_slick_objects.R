library(Slick)

# Test built in objects
example_objects <- list.files('inst', pattern='.rda')

for (i in seq_along(example_objects)) {
  slick <- readRDS(file.path('inst', example_objects[i]))
  slick = Update(slick)
  slick@MPs@Code <- c('a', 'b')
  Slick:::check_slick_file(slick)
}


# Test existing objects

dir <- 'G:/Shared drives/BM shared/4. Resources/18. Chile_MSE_Course_2023/Workshop/workshop-slick/Slick'
slick <- readRDS(file.path(dir, "Slick_anchoveta_11.02.2023.slick"))

slick <- readRDS(file.path(dir, "Slick_hake_11.02.2023.slick"))
object <- Update(slick)

object <- OMs(object)
