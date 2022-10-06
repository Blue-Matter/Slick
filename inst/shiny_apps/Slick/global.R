library(dplyr)
library(DT)
library(fresh)
library(ggplot2)
library(ggrepel)
library(readxl)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(shiny.i18n)
library(shinyWidgets)




# -- multi-language support ----
i18n <- Translator$new(translation_csvs_path =  "data/translations")
i18n$set_translation_language("en")
i18n$use_js()
languages <- i18n$get_languages()

language_codes <- read.csv('data/language-codes_csv.csv')
ind <- match(languages, language_codes[,1])
lang_names <- language_codes[ind,2]

names(languages) <- lang_names

# -- dashboard functions ----
source("./Source/dashboard_functions/dashboardHeader2.R")

# -- source app functions ----
source("./Source/app_functions/Load_slick.R",local=TRUE)
source("./Source/NonTech/Filters.R")
source("./Source/NonTech/Summary_Text.R")

# -- Pages
fls <- list.files("./Source/Pages")
for (fl in fls) source(file.path("./Source/Pages", fl), local = TRUE)

fls <- list.files("./Source/Plot_Pages")
for (fl in fls) source(file.path("./Source/Plot_Pages", fl), local = TRUE)



# Global formatting parameters

MPtxt <- 14/12 # cex

axis.title <- 18
axis.text <- 16

roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}


# load example object

# obj<-readRDS("./data/SLICKobj.rda")
# obj<-readRDS("./data/ABT.slick")
#
# # ---- Default Colors ----
# nMP <- length(obj$MP$Labels)
# ncol <- length(obj$Misc$Cols$MP)
#
# if (ncol< nMP) {
#   cols <- rainbow(nMP) # dumb range of colors
#   obj$Misc$Cols$MP <- c(obj$Misc$Cols$MP, cols)[1:nMP]
# }


# Dimensions (total)




#
# source("./Source/NonTech/Pages/Page_1.R",local=TRUE)
# source("./Source/NonTech/Pages/Page_2.R",local=TRUE)
# source("./Source/NonTech/Pages/Page_3.R",local=TRUE)

# App code

# source("./Source/App/Results_Filtering.R",local=TRUE)
#source("./Source/App/Misc.R",local=TRUE)

