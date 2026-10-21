# Changelog

## Slick 1.1.0

### New features

- The Quilt table can now be added to the Report in the App. The table
  is saved as an image with
  [`flextable::save_as_image()`](https://davidgohel.github.io/flextable/reference/save_as_image.html);
  the `ragg` package is now required for this (added to Suggests).
- Items can now be removed from the Report in the App.
- Accessibility improvements to the App: plots and explanatory images
  now have alternative text for screen reader.
- The example `Slick` object `WSKJ` is now shown in the README, the
  Developer’s Guide,
  [`?WSKJ`](https://slick.bluematterscience.com/reference/WSKJ.md), and
  the examples for the plotting and class functions.

### Changes

- `OMs@Preset` is now a flat named list of `Design` row indices,
  matching the format already used by `MPs@Preset` (and the
  performance-metric `Preset`s), instead of a nested list keyed by
  `Factor`. The old nested format is still accepted and is
  auto-converted for backward compatibility (by
  [`Preset()`](https://slick.bluematterscience.com/reference/Preset.md)
  and
  [`Update()`](https://slick.bluematterscience.com/reference/Update.md)).
- Added checks to the `Preset` slots: names must be unique and
  non-blank, and values must be whole numbers within range, with no
  duplicates. Slick objects with invalid `Preset`s are rejected when
  loaded in the App.
- [`Check()`](https://slick.bluematterscience.com/reference/Check.md) on
  a `Slick` object now warns when the OM or MP dimension of a plot’s
  `Value` doesn’t match the number of OMs or MPs in the object.
- The App now shows the details of the errors when an invalid Slick
  object is loaded, and no longer loads an invalid object passed to
  `App(slick=)`.
- [`FilterSlick()`](https://slick.bluematterscience.com/reference/FilterSlick.md)
  now ignores out-of-range MP, OM, and PI indices, and uses all of them
  if none are valid.
- `plotKobe(Time=TRUE)`: the stacking order of the Kobe Time bars is
  reversed, so the bottom-right (green) quadrant is now at the bottom of
  each bar. The new `BR_bottom` argument controls this
  (`BR_bottom=FALSE` restores the previous order).

### Bug fixes

- [`plotBoxplot()`](https://slick.bluematterscience.com/reference/plotBoxplot.md)
  now works when `PI` has more than one value.
- [`plotKobe()`](https://slick.bluematterscience.com/reference/plotKobe.md):
  the `OMs` argument is now used when it includes `NA` values.
- [`plotQuilt()`](https://slick.bluematterscience.com/reference/plotQuilt.md):
  fixed the lower limit of the color shading when `MinValue` was set,
  and the shading of the static (`kable=TRUE`) table now matches the
  interactive table.
- [`plotTradeoff()`](https://slick.bluematterscience.com/reference/plotTradeoff.md)
  now ignores `NA` values when averaging over OMs.
- [`Update()`](https://slick.bluematterscience.com/reference/Update.md)
  now reads the target and limit reference points correctly from legacy
  Slick objects, including for the Time Series plot.
- App: fixed the OM filter so it selects the correct OMs for presets and
  for factors whose levels are not in ascending order.
- App: fixed the Kobe plot axis limits for performance indicators other
  than the first two.
- App: the Quilt plot page now works when `Value` includes the
  simulation dimension.
- App: fixed errors when filtering MPs for a Quilt with a simulation
  dimension, and for some transient MP selections.
- App: the “deselect” button in the filters now clears the selection.

## Slick 1.0.3

- Added the `WSKJ` example `Slick` object (western Atlantic skipjack
  tuna MSE results).

## Slick 1.0.2

CRAN release: 2026-06-30

- Minor improvements to plot pages for different screen size/resolution
- Fix bug in boxplot: was not averaging over OMs correctly for violin
  plots

## Slick 1.0.1

CRAN release: 2026-02-27

- Added option to include simulation dimension in Quilt plot.
- Added Portuguese to the list of supported language. Many thanks to
  Rodrigo Sant’Ana for translating the text!
- Added a `Misc` slot to all plot objects.
- Added option to show `mean` or `median` in the time series plots shown
  in the Slick app.
- Fix issue with subsetting plots by Performance Indicators
- Modify `plotTimeseries` so it can handle plots with no historical
  values
- Fix to `plotQuilt(kable=TRUE)` for identical PI values across MPs

## Slick 1.0.0

CRAN release: 2025-09-11

`Slick` has been released on
[CRAN](https://CRAN.R-project.org/package=Slick)
