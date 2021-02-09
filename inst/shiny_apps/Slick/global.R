library(dplyr)
library(DT)
library(ggplot2)
library(ggrepel)
library(shinyWidgets)
library(readxl)


# App code
source("./Source/App/Load_slick.R",local=TRUE)
source("./Source/NonTech/Filters.R")
source("./Source/NonTech/Summary_Text.R")


# Non technical pages
fls <- list.files("./Source/NonTech/Pages")
for (fl in fls) source(file.path("./Source/NonTech/Pages", fl), local = TRUE)

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

