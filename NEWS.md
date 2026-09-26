# Slick 1.0.3

- `plotTimeseries()` now overplots a random selection of individual simulation trajectories ("worms"), with 10 worms shown by default.
- `OMs@Preset` is now a flat named list of `Design` row indices, matching the format
  already used by `MPs@Preset` (and the performance-metric `Preset`s), instead of a
  nested list keyed by `Factor`. The old nested format is still accepted and is
  auto-converted for backward compatibility. 
- Added checks to `Preset` slot 

# Slick 1.0.2 

- Minor improvements to plot pages for different screen size/resolution
- Fix bug in boxplot: was not averaging over OMs correctly for violin plots

# Slick 1.0.1

- Added option to include simulation dimension in Quilt plot.
- Added Portuguese to the list of supported language. Many thanks to Rodrigo Sant'Ana for translating the text!
- Added a `Misc` slot to all plot objects. 
- Added option to show `mean` or `median` in the time series plots shown in the 
  Slick app.
- Fix issue with subsetting plots by Performance Indicators
- Modify `plotTimeseries` so it can handle plots with no historical values
- Fix to `plotQuilt(kable=TRUE)` for identical PI values across MPs

# Slick 1.0.0

`Slick` has been released on [CRAN](https://CRAN.R-project.org/package=Slick)
