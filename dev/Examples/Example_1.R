# Simple example with three MPs

library(openMSE)
library(Slick)

Stock <- MSEtool::Albacore
Fleet <- MSEtool::Generic_IncE
Obs <- MSEtool::Generic_Obs
Imp <- MSEtool::Perfect_Imp

nsim <- 100

OM <- new('OM', Stock, Fleet, Obs, Imp, nsim=nsim)

all_MPs <- avail('MP')
all_MPs <- all_MPs[!grepl('ref', all_MPs)]
all_MPs <- all_MPs[!grepl('DDSS', all_MPs)]

MSE <- runMSE(OM, MPs=all_MPs, parallel = TRUE)

saveRDS(MSE, 'dev/Examples/Example_1.mse')


# ---------------------------------------------------------------------------- #

MSE <- readRDS('dev/Examples/Example_1.mse')

# Calculate Performance Metrics
PGK <- function(MSE = NULL, Ref = 1, Yrs = NULL) {
  Yrs <- ChkYrs(Yrs, MSE)
  PMobj <- new("PMobj")
  PMobj@Name <- "Probability of Green Kobe"
  PMobj@Caption <- "Probability of Green Kobe"
  PMobj@Ref <- Ref
  PMobj@Stat <- MSE@SB_SBMSY[, , Yrs[1]:Yrs[2]] > 1 & MSE@F_FMSY[, , Yrs[1]:Yrs[2]]  < 1
  PMobj@Prob <- calcProb(PMobj@Stat, MSE)
  PMobj@Mean <- calcMean(PMobj@Prob)
  PMobj@MPs <- MSE@MPs
  PMobj
}
class(PGK) <- 'PM'


pgk <- PGK(MSE)
lty <- LTY(MSE)
df <- data.frame(x=pgk@Mean, y=lty@Mean, MP=pgk@MPs)

library(ggplot2)
library(ggrepel)

ggplot(df, aes(x=x, y=y, color=MP)) +
  geom_point() +
  ggrepel::geom_label_repel(aes(label=MP)) +
  expand_limits(x=c(0,1), y=c(0,1)) +
  guides(color='none')

MP_select <- c('SP_4010', 'AvC', 'IT5')
df <- df |> dplyr::filter(MP%in% MP_select)

STY(MSE)

AAVY(MSE)


# Make Slick object

## TODO:
# - update show function for slick object and all plotting objects
# - write Check function for slick
# - test Check function in App



Check <- function(slick) {

  error <- 0
  warning <- 0

  add_error <- function(error) {
    error +1
  }
  print_error <- function(error) {
    if (error==0)
      usethis::ui_info('Errors found in Slick object:')
  }

  # title, etc
  if (length(Title(slick))<1) {
    print_error(error)
    usethis::ui_oops("Requires a Title; use {usethis::ui_code('Title(slick_object) <- \\' My Title \\' ')}")
    error <- add_error(error)
  }

  if (length(Author(slick))<1) {
    print_error(error)
    usethis::ui_oops("Requires at least one Author; use {usethis::ui_code('Author(slick_object) <- \\'Author Name\\' ')}")
    error <- add_error(error)
  }

  if (length(Email(slick))<1) {
    print_error(error)
    usethis::ui_oops("Requires at least one Email; use {usethis::ui_code('Email(slick_object) <- \\'Email Address\\' ')}")
    error <- add_error(error)
  }

  if (length(Institution(slick))<1) {
    print_error(error)
    usethis::ui_oops("Requires at least one Institution; use {usethis::ui_code('Institution(slick_object) <- \\'Institution\\'')}")
    error <- add_error(error)
  }

  if (length(Introduction(slick))<1) {
    print_error(error)
    usethis::ui_oops("Requires Introduction; use {usethis::ui_code('Introduction(slick_object) <- \\'Introduction text ...\\'')}")
    error <- add_error(error)
  }

  mps_valid <- methods::validObject(slick@MPs, test=TRUE)
  nMPs <- nrow(Metadata(slick@MPs))
  if (nMPs<2) {
    print_error(error)
    usethis::ui_oops(glue::glue("Slick object has ", nMPs, " management procedures. Requires at least two MPs. ", "See {usethis::ui_code('MPs(slick_object)')}"))
    error <- add_error(error)
  }


  if (error>0)
    stop(error, ' errors found in Slick object', call.=FALSE)
  invisible(slick)
}


slick <- Slick()


Title(slick) <- 'A simple example with three management procedures'
Author(slick) <- 'Adrian Hordyk'
Email(slick) <- 'adrian@bluematterscience.com'
Institution(slick) <- 'Blue Matter Science'

Introduction(slick) <- 'This is an example Slick object.

It is designed to demonstrate some of the features of Slick. See the [Slick Website](https://slick.bluematterscience.com/) for more information
'


# MPs
MPs <- MPs()

Metadata(MPs) <- data.frame(Code=c('SP4010', 'AvC', 'IT5'),
                                   Label=c('Surplus Production Model',
                                           'Average Catch',
                                           'Iterative Index Target'),
                                   Description=c('A surplus production assessment model, with a 40-10 control rule',
                                                 'A constant TAC set to the average historical landings',
                                                 'The TAC is iteratively adjusted by +/- 5% based on recent index level relative to a target level'))

Preset(MPs) <- list(All=1:3)

MPs(slick) <- MPs



# OMs
?OMs
?OMs()

slick

## UP TO HERE
# document OMs properly
# add validate functions for OMs
# populate and check for a single OM - remove from filters
myOMs <- OMs()


saveRDS(slick, 'dev/Examples/Example_1.slick')

Slick()
