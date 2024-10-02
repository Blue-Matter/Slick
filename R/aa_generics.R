
#' Generic show method
#'
#' @param object Object to print to console
#' @export
show <- function(object) methods::show(object)

# ---- Boxplot ----

#' Methods for Creating, Accessing and Assigning `Boxplot` objects
#'
#' The `Boxplot` function is used both to create and modify an [Boxplot-class()] object.
#' and to access and assign `Boxplot` for an object of class [Slick-class()].
#' See `Details`.
#'
#' @details
#' Objects of class `Boxplot` are created with `Boxplot()`
#'
#' Use [plotBoxplot()] to create the boxplot from the console.
#'
#'
#' Use the  [Code()], [Label()], [Description()], [Value()], [Preset()] functions to
#' access and assign the values for an existing `Boxplot` object, see `Examples`
#'
#' ## Multi-Language Support
#' Text with multi-language supported can be provided as a named list. Available languages:
#' - `en`: English (default)
#' - `es`: Spanish
#' - `fr`: French
#'
#' ## Note
#' Character strings in `Code`, `Label`, and `Description` must all be same length
#' as the number of performance indicators (`nPIs`) in `Value
#'
#' ## Defaults
#' `Defaults` is used to select the plot options that are selected in the Boxplot.
#' It is a list of length 2, with the following requirements for the list elements:
#'
#' 1. A character string. Options: 'overall' (default) or 'byom'
#' 2. A character string. Options: 'boxplot' (default), 'violin', or 'both'
#'
#' If unrecognized values are entered, the defaults will be used.
#'
#' @param Code `r code_PI_param()`
#' @param Label  `r label_PI_param() `
#' @param Description `r description_PI_param()`
#' @param Value  A numeric array with the stochastic performance indicator values for each
#' simulation (sim), operating model (OM), management procedure (MP), and performance indicator (PI).
#' Dimensions: c(`nsim`, `nOM`, `nMP`, and `nPI`).
#' @param Preset `r preset_param()`
#' @param Defaults A list object with default selections for the Boxplot
#'
#'
#' @rdname Boxplot-methods
#' @docType methods
#' @example inst/examples/Boxplot.R
#' @seealso [Code()], [Label()], [Description()], [Metadata()], [Value()],
#' [Preset()], [Defaults()], [plotBoxplot()]
#' @export
setGeneric("Boxplot", function(Code='',
                               Label='',
                               Description='',
                               Value=array(),
                               Preset=list(),
                               Defaults=list('overall', 'boxplot')) standardGeneric("Boxplot"))



#' @rdname Boxplot-methods
#' @param Slick A [Slick-class()] object
#' @param value A [Boxplot-class()] object
#' @export
setGeneric("Boxplot<-", function(Slick, value) standardGeneric("Boxplot<-"))

# ---- Check ----

#' Check an object for errors or issues
#'
#' Checks S4 objects to check for warnings and errors
#'
#' @param object An object of class: [Slick-class()], [MPs-class()],
#' [OMs-class()] or the six chart types: [Boxplot-class()], [Kobe-class()],
#' [Quilt-class()], [Spider-class()], [Timeseries-class()],
#' and [Tradeoff-class()].
#' @param ... Additional arguments
#' @return Prints messages to the console
#' @export
setGeneric("Check", function(object, ...) standardGeneric("Check"))


# ---- Code ----

#' Access or assign `Code`, `Label`, and `Description` for a valid object class
#'
#'
#' @param object `r object_Code_param()`
#' @param lang `r lang_param()`
#'
#' @details
#' `Code`, `Label`, and `Description` must all be equal length.
#'
#' ## Multi-Language Support
#' Text with multi-language supported can be provided as a named list. Available languages:
#' - `en`: English (default)
#' - `es`: Spanish
#' - `fr`: French
#'
#' See `Examples`
#'
#' @example inst/examples/Code.R
#'
#' @seealso [Label()], [Description()], [MPs-class()], [Boxplot-class()],
#' [Kobe-class()], [Quilt-class()], [Spider-class()],
#' [Timeseries-class()], [Tradeoff-class()]
#' @export
setGeneric("Code", function(object, lang='en') standardGeneric("Code"))

#' @rdname Code
#' @param value A character vector or a named list for multi-language
#' @export
setGeneric("Code<-", function(object, value) standardGeneric("Code<-"))

# ---- Color ----

#' Access or assign `Color` for `MPs` and `Quilt` objects
#' @param object An [MPs-class()] or [Quilt-class()] object
#'
#' @export
setGeneric("Color", function(object) standardGeneric("Color"))

#' @param value A character vector formatted to match the class of `object`. See the documentation for
#' corresponding `object` class for more details.
#' @rdname Color
#' @export
setGeneric("Color<-", function(object, value) standardGeneric("Color<-"))

# ---- Defaults ----

#' Set default selections for the plots in the `App()`
#' @param object A plot object
#'
#' @details
#' In development.
#'
#' @export
setGeneric("Defaults", function(object) standardGeneric("Defaults"))


#' @rdname Defaults
#' @param value A list of default selections for the [App()]
#' @export
setGeneric("Defaults<-", function(object, value) standardGeneric("Defaults<-"))


# ---- Description ----

#' @rdname Code
#' @export
setGeneric("Description", function(object, lang='en') standardGeneric("Description"))


#' @rdname Code
#' @param value A character vector or a named list for multi-language support
#' @export
setGeneric("Description<-", function(object, value) standardGeneric("Description<-"))

# ---- Design -----

#' Return the Design matrix from an `OMs` object
#'
#' @param object An [OMs-class()] or [Slick-class()] object.
#' @return The Design matrix from an `OMs` object in a `Slick` object
#' @export
setGeneric("Design", function(object) standardGeneric("Design"))

#' @rdname Design
#' @param value A `data.frame` with the Design matrix
#' @export
setGeneric("Design<-", function(object, value) standardGeneric("Design<-"))




# ---- Factors ----

#' Return the Factors matrix from an `OMs` object
#'
#' @param object An [OMs-class()] or [Slick-class()] object.
#' @param lang `r lang_param()`
#' @return The Design matrix from an `OMs` object in a `Slick` object
#' @export
setGeneric("Factors", function(object, lang='en') standardGeneric("Factors"))

#' @rdname Factors
#' @param value A `data.frame` with the Factors
#' @export
setGeneric("Factors<-", function(object, value) standardGeneric("Factors<-"))





# ---- Kobe ----

#' Methods for Creating, Accessing and Assigning `Kobe` objects
#'
#' The `Kobe` function is used both to create and modify an [Kobe-class()] object.
#' and to access and assign `Kobe` for an object of class [Slick-class()].
#' See `Details`.
#'
#' Objects of class `Kobe` are created with `Kobe()`
#'
#' The Kobe plot typically shows B/BMSY (or something similar) on the x-axis, and
#' F/FMSY (or something similar) on the y-axis.
#'
#' ## Performance Indicators
#' The first PI will be on the x-axis (usually B/BMSY or something similar) and the second
#' on the y-axis (e.g., F/FMSY)
#'
#' ## Multi-Language Support
#' Text with multi-language supported can be provided as a named list. Available languages:
#' - `en`: English (default)
#' - `es`: Spanish
#' - `fr`: French
#'
#' ## Note
#' Character strings in `Code`, `Label`, and `Description` must all be same length
#' as the number of performance indicators (`nPIs`) in `Value`
#'
#' @param Code `r code_PI_param()`
#' @param Label  `r label_PI_param() `
#' @param Description `r description_PI_param()`
#' @param Time A numeric vector with values for the projection time-steps. Must
#' match length `nTS` in `Value`
#' @param TimeLab Character string length 1. Name of the time step (e.g., 'Year'). Will be used as the label in the `Kobe Time` plot. Use a named list for
#' multiple languages.
#' @param Value A numeric array with the stochastic performance indicator values for each
#' simulation (sim), operating model (OM), management procedure (MP), performance indicator (PI),
#' and projection time-steps (nTS).
#' Dimensions: c(`nsim`, `nOM`, `nMP`, `nPI`, `nTS`)
#' @param Preset `r preset_param()`
#' @param Target Numeric vector length `nPI` with the target value for the PIs. Defines the color quadrants on the Kobe plot. Defaults to c(1,1).
#' @param Limit Numeric vector length `nPI` with the limit value for the PIs. Shows as red line on Kobe plot. NULL to ignore.
#'
#' @rdname Kobe-methods
#' @docType methods
#' @example inst/examples/Kobe.R
#' @seealso [Code()], [Label()], [Description()], [Time()], [TimeLab(), [Value()], [Preset()],
#' [Target()] and [Limit()]
#' @export
setGeneric("Kobe", function(Code='',
                            Label='',
                            Description='',
                            Time=numeric(),
                            TimeLab='Year',
                            Value=array(),
                            Preset=list(),
                            Target=1,
                            Limit=NULL) standardGeneric("Kobe"))



#' @rdname Kobe-methods
#' @param Slick A [Slick-class()] object
#' @param value A [Kobe-class()] object
#' @export
setGeneric("Kobe<-", function(Slick, value) standardGeneric("Kobe<-"))


# ---- Label ----

#' @rdname Code
#' @export
setGeneric("Label", function(object, lang='en') standardGeneric("Label"))


#' @rdname Code
#' @export
setGeneric("Label<-", function(object, value) standardGeneric("Label<-"))



# ---- Limit -----

#' @rdname Target
#' @export
setGeneric("Limit", function(object) standardGeneric("Limit"))

#' @export
#' @rdname Target
setGeneric("Limit<-", function(object, value) standardGeneric("Limit<-"))



# ---- Metadata ----

#' Return `Code`, `Label`, `Description` and other information from an object
#'
#' @param object A [Slick-class()], [MPs-class()], [Boxplot-class()], [Kobe-class()],
#' [Quilt-class()], [Spider-class()], [Timeseries-class()], or [Tradeoff-class()] object
#' @param lang `r lang_param()`
#' @return `A data.frame`
#' @export
setGeneric("Metadata", function(object, lang='en')
  standardGeneric("Metadata"))

#' @rdname Metadata
#' @param value Replacement value for `Metadata()` in the corresponding `object`.
#' See help documentation for the relevant object class for details.
#' @export
setGeneric("Metadata<-", function(object, value) standardGeneric("Metadata<-"))


# ---- MPs -----

#' Methods for Creating, Accessing and Assigning `MPs` objects
#'
#' The `MPs` function is used both to create and modify an [MPs-class()] object.
#' and to access and assign `MPs` for an object of class [Slick-class()].
#' See `Details`.
#'
#' Objects of class `MPs` are created with `MPs()`
#'
#' @param Code `r code_MP_param()`
#' @param Label  `r label_MP_param() `
#' @param Description `r description_MP_param()`
#' @param Color A character vector of colors for the MPs.
#' @param Preset `r preset_param()`
#'
#'
#' Use [Code()], [Label()], [Description()], and [Preset()] to access
#' and assign the values for an existing `MPs` object, see `Examples`.
#'
#' @rdname MPs-methods
#' @docType methods
#' @example inst/examples/MPs.R
#' @seealso [Code()], [Label()], [Description()], [Color()], [Metadata()], [Preset()]
#' @export
setGeneric("MPs", function(Code='',
                           Label='',
                           Description='',
                           Color='',
                           Preset=list()) standardGeneric('MPs'))


#' @rdname MPs-methods
#' @param object A [Slick-class()] object
#' @param value A [MPs-class()] object
#' @export
setGeneric("MPs<-", function(object, value) standardGeneric("MPs<-"))


# ---- OMs -----


#' Methods for Creating, Accessing and Assigning `OMs` objects
#'
#' The `OMs` function is used both to create and modify an [OMs-class()] object.
#' and to access and assign `OMs` for an object of class [Slick-class()].
#' See `Details`.
#'
#' @param Factors A `data.frame` with column headings `Factor`, `Level`, and `Description`.
#'  See `Details`
#' @param Design A `data.frame` with `nFactor` columns
#'  (i.e., `length(unique(Factors$Factor))`), and `nOM`
#' rows. See `Details`
#' @param Preset `r preset_param()`
#'
#' @details
#'
#' ## Factors
#' `Factors` can be accessed and assigned using `Factors(myslick)` and
#' `Factors(myslick) <- data.frame()` respectively.
#'
#' The `Factor` column should be character strings with the name of each factor,
#' while the `Level` column is a `numeric` or `character` value with the level for the
#' corresponding factor.
#'
#' The `Description` column is a description for each row, i.e., a unique factor and level.
#' See `Examples`.
#'
#' ## Design
#' The `Design` matrix is `nOM` rows and `nFactor` columns. The values in each column should
#' either be `numeric` values indicating the levels for the corresponding factor,
#' or the actual level values (i.e., `Factors$Level`) that correspond to each OM. See `Examples`.
#'
#' Use [Factors()], [Design()], and [Preset()] to access
#' and assign the values for an existing `OMs` object, see `Examples`.
#'
#' @rdname OMs-methods
#' @docType methods
#' @example inst/examples/OMs.R
#' @seealso [OMs-class()], [Factors()], [Design()], [Preset()]
#' @export
setGeneric("OMs", function(Factors=data.frame(),
                           Design=data.frame(),
                           Preset=list()) standardGeneric('OMs')
)


#' @rdname OMs-methods
#' @param object A [Slick-class()] object
#' @param value A [OMs-class()] object
#' @export
setGeneric("OMs<-", function(object, value) standardGeneric("OMs<-"))


# ---- Preset ----

#' Assign or access `Preset` for a valid object class
#' @param object An object of class [Boxplot-class()], [Kobe-class()], [Quilt-class()],
#' [Spider-class()], [Timeseries-class()], or [Tradeoff-class()]
#'
#' @export
setGeneric("Preset", function(object) standardGeneric("Preset"))

#' @param value A `list`, formatted to match the class of `object`. See the documentation for
#' corresponding `object` class for more details.
#' @rdname Preset
#' @export
setGeneric("Preset<-", function(object, value) standardGeneric("Preset<-"))




# ---- Quilt ----

#' Methods for Creating, Accessing and Assigning `Quilt` objects
#'
#' The `Quilt` function is used both to create and modify an [Quilt-class()] object.
#' and to access and assign `Quilt` for an object of class [Slick-class()].
#' See `Details`.
#'
#' @details
#' Objects of class `Quilt` are created with `Quilt()`
#'
#' Use the  [Code()], [Label()], [Description()], [Value()], [Preset()], [Color()],
#' [MinValue], and [MaxValue] functions to access and assign the values for
#' an existing `Quilt` object, see `Examples`
#'
#' ## Multi-Language Support
#' Text with multi-language supported can be provided as a named list. Available languages:
#' - `en`: English (default)
#' - `es`: Spanish
#' - `fr`: French
#'
#' ## Note
#' Character strings in `Code`, `Label`, and `Description` must all be same length
#' as the number of performance indicators (`nPIs`) in `Value`
#'
#' @param Code `r code_PI_param()`
#' @param Label `r label_PI_param() `
#' @param Description `r description_PI_param()`
#' @param Value A numeric array with the stochastic performance indicator values for each
#' operating model (OM), management procedure (MP), and performance indicator (PI).
#' Dimensions: c(`nOM`, `nMP`, and `nPI`)
#' @param Preset `r preset_param()`
#' @param Color A character vector length 2 of colors for the maximum and minimum
#' values in the chart.
#' @param MinValue Numeric vector length `nPI` with the minimum possible value for the
#' respective PIs. Defaults to minimum PI value in `Value` (averaged across OMs in some cases)
#' @param MaxValue Numeric vector length `nPI` with the maximum possible value (i.e., best performance)
#' for the respective PIs. Defaults to maximum PI value in `Value` (averaged across OMs in some cases).
#'
#'
#' @rdname Quilt-methods
#' @docType methods
#' @example inst/examples/Quilt.R
#' @seealso [Code()], [Label()], [Description()], [Color()], [Metadata()], [Preset()],
#' [Color()], [MinValue()], [MaxValue()]
#' @export
setGeneric("Quilt", function(Code='',
                             Label='',
                             Description='',
                             Value=array(),
                             Preset=list(),
                             Color=c('darkblue', 'lightblue'),
                             MinValue=as.numeric(NA),
                             MaxValue=as.numeric(NA)) standardGeneric('Quilt'))


#' @rdname Quilt-methods
#' @param Slick A [Slick-class()] object
#' @param value A [Quilt-class()] object
#' @export
setGeneric("Quilt<-", function(Slick, value) standardGeneric("Quilt<-"))








# ---- Spider ----

#' Methods for Creating, Accessing and Assigning `Spider` objects
#'
#' The `Spider` function is used both to create and modify an [Spider-class()] object.
#' and to access and assign `Spider` for an object of class [Slick-class()].
#' See `Details`.
#'
#' @details
#' Objects of class `Spider` are created with `Spider()`
#'
#' Use the  [Code()], [Label()], [Description()], [Value()], [Preset()] functions to
#' access and assign the values for an existing `Spider` object, see `Examples`
#'
#' ## Multi-Language Support
#' Text with multi-language supported can be provided as a named list. Available languages:
#' - `en`: English (default)
#' - `es`: Spanish
#' - `fr`: French
#'
#' ## Note
#' Character strings in `Code`, `Label`, and `Description` must all be same length
#' as the number of performance indicators (`nPIs`) in `Value
#' @param Code `r code_PI_param()`
#' @param Label  `r label_PI_param() `
#' @param Description `r description_PI_param()`
#' @param Value  A numeric array with the stochastic performance indicator values for each
#' operating model (OM), management procedure (MP), and performance indicator (PI).
#' Dimensions: c(`nOM`, `nMP`, and `nPI`).
#' All PI values must range between 0 and 1 or 0 and 100. If all values are <= 1, they will be
#' multiplied by 100 in the plot.
#' @param Preset `r preset_param()`
#'
#'
#' @rdname Spider-methods
#' @docType methods
#' @example inst/examples/Spider
#' @seealso [Code()], [Label()], [Description()], [Metadata()], [Value()], [Preset()]
#' @export
setGeneric("Spider", function(Code='',
                              Label='',
                              Description='',
                              Value=array(),
                              Preset=list()) standardGeneric("Spider"))


#' @rdname Spider-methods
#' @param Slick A [Slick-class()] object
#' @param value A [Spider-class()] object
#' @export
setGeneric("Spider<-", function(Slick, value) standardGeneric("Spider<-"))






# ---- Target ----

#' Access or assign `Target` and `Limit` for object of class `Kobe` or `Timeseries`
#' @export
setGeneric("Target", function(object) standardGeneric("Target"))

#' @param object A [Kobe-class()] or [Timeseries()] class object
#' @param value Value to assign to `Target`
#' @describeIn Target Assign `value` to `object@Target`
#' @export
setGeneric("Target<-", function(object, value) standardGeneric("Target<-"))


# ---- Time ----

#' Access or assign `Time` for object of class `Kobe` or `Timeseries`
#' @export
setGeneric("Time", function(object) standardGeneric("Time"))

#' @param object A [Kobe-class()] or [Timeseries()] class object
#' @param value Value to assign to `Time`
#' @describeIn Time Assign `value` to `object@Time`
#' @export
setGeneric("Time<-", function(object, value) standardGeneric("Time<-"))


# ---- TimeLab ----

#' Access or assign `TimeLab` in a `Kobe` or `Timeseries` object
#'
#' @param object A [Kobe-class()] or [Timeseries-class] object
#' @export
setGeneric("TimeLab", function(object, lang='en') standardGeneric("TimeLab"))

#' @rdname TimeLab
#' @param value A character string to assign to `TimeLab` in `object`.
#' @param lang `r lang_param()`
#' @export
setGeneric("TimeLab<-", function(object, value) standardGeneric("TimeLab<-"))

# ---- Timeseries ----

#' Methods for Creating, Accessing and Assigning `Timeseries` objects
#'
#' An object of class `Timeseries` contains information for the Time Series chart.
#' The `Timeseries` function is used both to create and modify an [Timeseries-class()] object,
#' and to access and assign `Timeseries` for an object of class [Slick-class()].
#' See `Details`.
#'
#' Use [plotTimeseries()] to create the boxplot from the console.
#'
#' ## Note
#' Character strings in `Code`, `Label`, and `Description` must all be same length
#' as the number of performance indicators (`nPIs`) in `Value`
#'
#' @param Code `r code_PI_param()`
#' @param Label `r label_PI_param() `
#' @param Description `r description_PI_param()`
#' @param Time A numeric vector with values for the historical and projection time-steps. Must
#' match length `nTS` in `Value`
#' @param TimeNow A numeric value matching the last historical timestep in `Time`
#' @param TimeLab Character string length 1. Name of the time step (e.g., 'Year').
#' Will be used as the label in the `Timeseries` plot. Use a named list for
#' multiple languages.
#' @param Value A numeric array with the stochastic performance indicator values for each
#' simulation (sim), operating model (OM), management procedure (MP),  performance indicator (PI),
#' and historical + projection timestep (nTS).
#' Dimensions: c(`nsim`, `nOM`, `nMP`, `nPI`, `nTS`)
#' @param Preset `r preset_param()`
#' @param Target Numeric vector length `nPI` with the target value for the PIs.
#' @param Limit Numeric vector length `nPI` with the limit value for the PIs.
#'
#' @seealso [Timeseries-class()], [Code()], [Label()], [Description()],
#'  [Metadata()], [Value()], [Preset()], [plotTimeseries()]
#'
#'
#' @details
#' Objects of class `Timeseries` are created with `Timeseries()`
#'
#' ## Multi-Language Support
#' Text with multi-language supported can be provided as a named list. Available languages:
#' - `en`: English (default)
#' - `es`: Spanish
#' - `fr`: French
#'
#' Use the  [Code()], [Label()], [Description()], [Value()], [Preset()] functions to access and assign the values for an
#' existing `Timeseries` object, see `Examples`
#'
#' @rdname Timeseries-methods
#' @docType methods
#' @example inst/examples/Timeseries.R
#' @seealso [Code()], [Label()], [Description()], [Metadata()], [Value()], [Preset()]
#' @export
setGeneric("Timeseries", function(Code='',
                                  Label='',
                                  Description='',
                                  Time=numeric(),
                                  TimeNow=numeric(),
                                  TimeLab='Year',
                                  Value=array(),
                                  Preset=list(),
                                  Target=NULL,
                                  Limit=NULL) standardGeneric("Timeseries"))


#' @rdname Timeseries-methods
#' @param Slick A [Slick-class()] object
#' @param value A [Timeseries-class()] object
#' @export
setGeneric("Timeseries<-", function(Slick, value) standardGeneric("Timeseries<-"))

# ---- Tradeoff ----

#' Methods for Creating, Accessing and Assigning `Tradeoff` objects
#'
#' The `Tradeoff` function is used both to create and modify an [Tradeoff-class()] object.
#' and to access and assign `Tradeoff` for an object of class [Slick-class()].
#' See `Details`.
#'
#' Objects of class `Tradeoff` are created with `Tradeoff()`
#'
#'
#' ## Multi-Language Support
#' Text with multi-language supported can be provided as a named list. Available languages:
#' - `en`: English (default)
#' - `es`: Spanish
#' - `fr`: French
#'
#' ## Note
#' Character strings in `Code`, `Label`, and `Description` must all be same length
#' as the number of performance indicators (`nPIs`) in `Value`
#'
#' @param Code `r code_PI_param()`
#' @param Label `r label_PI_param() `
#' @param Description `r description_PI_param()`
#' @param Value A numeric array with the stochastic performance indicator values for each
#' operating model (OM), management procedure (MP),  and performance indicator (PI)
#' Dimensions: c(`nOM`, `nMP`, `nPI`)
#' @param Preset `r preset_param()`
#'
#' @rdname Tradeoff-methods
#' @docType methods
#' @example inst/examples/Tradeoff
#' @seealso [Code()], [Label()], [Description()], [Metadata()], [Value()], [Preset()]
#' @export
setGeneric("Tradeoff", function(Code='',
                                Label='',
                                Description='',
                                Value=array(),
                                Preset=list()) standardGeneric("Tradeoff"))


#' @rdname Tradeoff-methods
#' @param Slick A [Slick-class()] object
#' @param value A [Tradeoff-class()] object
#' @export
setGeneric("Tradeoff<-", function(Slick, value) standardGeneric("Tradeoff<-"))




# ---- Value ----

#' Assign or access `Value` for a valid object class
#' @param object An object of class [Boxplot-class()], [Kobe-class()], [Quilt-class()],
#' [Spider-class()], [Timeseries-class()], or [Tradeoff-class()]
#' @export
setGeneric("Value", function(object) standardGeneric("Value"))

#' @rdname Value
#' @param value An `array`, formatted to match the class of `object`. See the documentation for
#' corresponding `object` class for more details.
#' @export
setGeneric("Value<-", function(object, value) standardGeneric("Value<-"))













