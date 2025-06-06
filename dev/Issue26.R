
# Kobe: Add option to choose the year to show in main Kobe plo
devtools::load_all()

library(Slick)

slick <- readRDS("G:/Shared drives/BM shared/1. Projects/SLICK/Example.slick")

slick <- Update(slick)

slotNames('Kobe')

# 1. Add new slot to Kobe for time to show in plot: slick@Kobe@TimePlot
# 2. Add UpdateKobe function same as UpdateTimeseries to add slot to older objects
# 3. Update Kobe page with option to select terminal year for plot
# 4. Update documentation - and examples/userguide
# 5. Test and run R checks
