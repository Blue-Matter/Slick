# Class `CheckList` -----
CheckList <- setClass("CheckList",
                      slots=c(object='character',
                              errors='list',
                              warnings='list',
                              messages='list',
                              complete='logical_list',
                              empty='logical_list')
)

setMethod("initialize", "CheckList", function(.Object) {
  .Object@complete <- FALSE
  .Object@empty <- TRUE
  .Object
})




# show method ----
#' @describeIn show Print a `CheckList` object
setMethod('show', 'CheckList', function(object) {

  cli::cli_h2('Checking: {.val {object@object}}')

  if (is.list(object@empty)) {
    print_list(object)
  } else {
    print_single(object)
  }
})

print_list <- function(object) {
  print_errors(object@errors$Introduction)

  nms <- names(object@empty)
  for (nm in nms) {
    cli::cli_h3('Checking: {.val {nm}}')
    if (object@empty[[nm]]) {
      cli::cli_alert_info('Object is empty')
    } else {
      print_errors(object@errors[[nm]])
      print_warnings(object@warnings[[nm]])

      if (object@complete[[nm]]) {
        cli::cli_alert_success('Complete')
      } else {
        if (length(object@errors[[nm]])>0) {
          cli::cli_alert_danger('Errors in object')
        } else if (!object@complete[[nm]]) {
          cli::cli_alert_danger('Object incomplete')
        }
      }

    }

  }

}

print_single <- function(object) {
  if (object@empty) {
    cli::cli_alert_info('Object is empty')
  } else {
    # Errors
    print_errors(object@errors)

    # Warnings
    print_warnings(object@warnings)

    # Messages

    # Status
    if (object@complete) {
      cli::cli_alert_success('Complete')
    } else {
      if (length(object@errors)>0) {
        cli::cli_alert_danger('Errors in object')
      } else if (!object@complete) {
        cli::cli_alert_danger('Object incomplete')
      }
    }
  }
}


print_errors <- function(errors) {
  if (inherits(errors, 'logical')) {
    if (length(errors)>0) {
      nms <- names(errors)
      for (i in seq_along(errors)) {
        cli::cli_alert_danger(c(nms[i], ': ', errors[[i]]))
      }
    }
  }else if (inherits(errors, 'list')) {
    for (i in seq_along(errors)) {
      object_names <- names(errors)
      if (length(errors[[i]])>0) {
        cli::cli_alert_info(' {.val {object_names[i]}}')
        nms <- names(errors[[i]])
        for (j in seq_along(errors[[i]])) {
          if (length(nms[i])>0) {
            cli::cli_alert_danger(c(nms[i], ': ', errors[[i]][[j]]))
          } else {
            cli::cli_alert_danger(errors[[i]][[j]])
          }

        }
      }
    }
  }
}

print_warnings <- function(warnings) {
  if (inherits(warnings, 'logical')) {
    if (length(warnings)>0) {
      nms <- names(warnings)
      for (i in seq_along(warnings)) {
        cli::cli_alert_danger(c(nms[i], ': ', warnings[[i]]))
      }
    }
  }else if (inherits(warnings, 'list')) {
    for (i in seq_along(warnings)) {
      object_names <- names(warnings)
      if (!is.null(object_names))
        cli::cli_alert_info(' {.val {object_names[i]}}')
      if (length(warnings[[i]])>0) {
        nms <- names(warnings[[i]])
        for (j in seq_along(warnings[[i]])) {
          cli::cli_alert_danger(c(nms[i], warnings[[i]][[j]]))
        }
      }
    }
  }
}




# check functions ----

is_empty_value <- function(value) {
  length(dim(value))==1 #  | all(is.na(value))
}

is_empty <- function(object) {
  slots <- slotNames(object)
  # defaults
  def_slots <- c('Defaults', 'TimeLab',
                 'Target',
                 'Limit',
                 'Color',
                 'MinValue',
                 'MaxValue')
  slots <- slots[!slots%in% def_slots]
  vec <- rep(TRUE, length(slots))
  for (i in seq_along(slots)) {
    sl <- slots[i]
    val <- try(slot(object, sl), silent=TRUE)
    if (inherits(val,"try-error")) {
      object <- UpdateNewSlots(object, sl)
      val <- try(slot(object, sl), silent=TRUE)
    }


    type <- class(val)
    if ('character' %in% type) {
      if(length(val)==1) {
        vec[i] <- nchar(val)<1
      } else {
        vec[i] <- length(val)<1
      }

    }
    if ('list' %in% type) {
      vec[i] <- length(val)<1
    }
    if ('array' %in% type) {
      vec[i] <- all(is.na(val))
    }
    if ('numeric' %in% type) {
      vec[i] <-  length(val)<1
    }
    if ('data.frame' %in% type) {
      vec[i] <- nrow(val)<1
    }
  }
  all(vec)
}





check_lang_list <- function(text_list) {
  error_list <- list()

  # check languages
  nms <- names(text_list)
  if (!all(nms %in% c('en', 'es', 'fr', 'pt')))
    error_list <- append(error_list,
                         'Named list must only be `en`, `es`, `fr`, or `pt`')

  # check lengths
  ll <- lapply(text_list, length)
  len_vals <- unlist(ll)
  if (!all(len_vals==len_vals[1]))
    error_list <- append(error_list, 'All list elements must be equal length')

  if (length(error_list) >0)
    return(error_list)
  NULL
}

get_len <- function(obj) {
  if (is.list(obj)) {
    length(obj[[1]])
  } else {
    length(obj)
  }
}

check_factors <- function(object) {
  out <- list()
  if (is.list(object@Factors) & !is.data.frame(object@Factors))
    out$Factors <- check_lang_list(object@Factors)
  out

}

check_design <- function(object) {
  out <- list()
  if (nrow(object@Design)<1)
    return(out)
  factors <- NULL

  if (is.data.frame(object@Factors)) {
    if (nrow(object@Factors)>0) {
      factors <- unique(object@Factors$Factor)
    }
  } else {
    if (length(object@Factors)>0) {
      factors <- unique(object@Factors[[1]]$Factor)
    }
  }
  if (!is.null(factors)) {
    if (nrow(object@Design)>0) {
      # nms <- colnames(object@Design)
      # nsm <- nms[!nms=='Name']
      if (!all(colnames(object@Design) %in% factors))
        out$Design <- "column names of `Design` must match factors in `Factors`"
    }

    # check each factor
    for (i in seq_along(factors)) {
      fac <- factors[i]
      if (is.data.frame(object@Factors)) {
        ind <- which(object@Factors$Factor %in% fac)
        levs <- object@Factors$Level[ind]
      } else {
        ind <- which(object@Factors[[1]]$Factor %in% fac)
        levs <- object@Factors[[1]]$Level[ind]
      }
      des_levs <- unique(object@Design[[factors[i]]])
      error_msg <- paste0('Error with Factor: ', factors[i], '. Each row must correspond with a level in `Factors()')

      chk <- FALSE
      if (!any(is.na(suppressWarnings(as.numeric(levs))))) {
        if (is.numeric(des_levs)) {
          chk <- (!all(seq_along(levs) %in% des_levs)) & !(all(as.numeric(levs) %in% des_levs))
        }
      } else {
        chk <-  (!all(seq_along(levs) %in% des_levs)) & !(all(levs %in% des_levs))
      }
      if (chk)
        out$Design <- append(out$Design, error_msg)

    }
  }
  out
}

check_metadata <- function(object) {

  out <- list()
  if (is.list(object@Code))
    out$Code <- check_lang_list(object@Code)


  if (is.list(object@Label))
    out$Label <- check_lang_list(object@Label)

  if (is.list(object@Description))
    out$Description <- check_lang_list(object@Description)


  lCode <- get_len(object@Code)
  lLabel <- get_len(object@Label)
  lDescription <- get_len(object@Description)

  if (sum(c(lCode, lLabel, lDescription)>1)>1) {
    if (all(!is.na(object@Description)) && any(nchar(object@Description) >0)) {
      if (any(c(lCode != lLabel,
                lLabel != lDescription,
                lCode != lDescription))) {
        out$Metadata <- '`Code`, `Label`, and `Description` must be equal length'
      }
    } else {
      if (any(c(lCode != lLabel))) {
        out$Metadata <- '`Code`, and `Label` must be equal length'
      }
    }

  }
  out
}

check_Preset_OMs <- function(object) {
  out <- list()
  Preset <- object@Preset
  if (length(Preset)<1) return(out)

  nms <- names(Preset)
  if (is.null(nms) || any(nchar(nms)<1)) {
    out$Preset_Names <- '`Preset` must be a named list, with a non-blank name for each preset'
  } else if (any(duplicated(nms))) {
    out$Preset_Names <- paste0(
      '`Preset` names must be unique - found duplicate name(s): ',
      paste(unique(nms[duplicated(nms)]), collapse=', ')
    )
  }

  if (is.data.frame(object@Factors)) {
    factors_df <- object@Factors
  } else if (length(object@Factors)>0) {
    factors_df <- object@Factors[[1]]
  } else {
    return(out)
  }
  factors <- unique(factors_df$Factor)
  nFactor <- length(factors)
  if (nFactor<1) return(out)
  nlevels <- sapply(factors, function(f) sum(factors_df$Factor==f))

  for (pn in nms) {
    p <- Preset[[pn]]
    if (!is.list(p) || length(p)!=nFactor) {
      out[[paste0('Preset_', pn)]] <- paste0(
        '`Preset$', pn, '` must be a list of length ', nFactor,
        ' (one element per Factor: ', paste(factors, collapse=', '),
        '), got length ', length(p)
      )
      next
    }
    for (i in seq_len(nFactor)) {
      vals <- suppressWarnings(as.numeric(p[[i]]))
      if (anyNA(vals) || any(vals<1) || any(vals>nlevels[i])) {
        out[[paste0('Preset_', pn, '_', factors[i])]] <- paste0(
          '`Preset$', pn, '[[', i, ']]` (factor `', factors[i],
          '`) must contain integer level indices between 1 and ', nlevels[i]
        )
      }
    }
  }
  out
}

#' Resolve which `Design` rows a single per-factor OMs preset selects.
#'
#' Mirrors the AND-across-factors matching logic used by `filterOMs()`, so
#' that validity checks see exactly what the app would select. Returns
#' `NULL` (rather than erroring) if `preset` isn't shaped like a per-factor
#' preset (that structural problem is already reported by `check_Preset_OMs`).
#' @noRd
resolve_Preset_OMs <- function(factors_df, design, preset) {
  factors <- unique(factors_df$Factor)
  nFactor <- length(factors)
  if (!is.list(preset) || length(preset)!=nFactor) return(NULL)
  if (!all(factors %in% colnames(design))) return(NULL)

  keep <- matrix(TRUE, nrow=nrow(design), ncol=nFactor)
  for (i in seq_len(nFactor)) {
    nm <- factors[i]
    lvls <- factors_df$Level[factors_df$Factor==nm]

    vals <- as.character(unlist(design[[nm]]))
    lvls_chr <- as.character(lvls)
    vals_num <- suppressWarnings(as.numeric(vals))
    lvls_num <- suppressWarnings(as.numeric(lvls_chr))
    if (!anyNA(vals_num) && !anyNA(lvls_num)) {
      codes <- match(vals_num, lvls_num)
    } else {
      codes <- match(vals, lvls_chr)
    }

    sel <- suppressWarnings(as.numeric(preset[[i]]))
    if (anyNA(sel)) return(NULL)
    keep[,i] <- codes %in% sel
  }
  which(apply(keep, 1, all))
}

#' Semantic (not just structural) checks on `OMs@Preset`: does each preset
#' actually select any OMs, and are any two presets identical? These are
#' warnings, not errors - a structurally valid `Preset` can still be
#' practically useless (e.g. an impossible combination of levels), and that
#' is easy to miss by eye once `nFactor` gets large.
#' @noRd
check_Preset_OMs_resolution <- function(object) {
  out <- list()
  Preset <- object@Preset
  if (length(Preset)<1) return(out)
  if (nrow(object@Design)<1) return(out)

  if (is.data.frame(object@Factors)) {
    factors_df <- object@Factors
  } else if (length(object@Factors)>0) {
    factors_df <- object@Factors[[1]]
  } else {
    return(out)
  }
  if (nrow(factors_df)<1) return(out)

  design <- as.data.frame(object@Design)

  resolved <- list()
  nms <- names(Preset)
  for (pn in nms) {
    sel <- tryCatch(resolve_Preset_OMs(factors_df, design, Preset[[pn]]),
                    error=function(e) NULL)
    if (is.null(sel)) next # shape problems already reported by check_Preset_OMs
    resolved[[pn]] <- sel
    if (length(sel)<1) {
      out[[paste0('Preset_', pn, '_Empty')]] <- paste0(
        '`Preset$', pn, '` does not match any rows in `Design` - the selected levels ',
        'may be an impossible combination (check they are consistent with each other)'
      )
    }
  }

  # flag presets that resolve to the identical set of OMs - likely a copy/paste mistake
  seen <- list()
  for (pn in names(resolved)) {
    key <- paste(sort(resolved[[pn]]), collapse=',')
    if (!is.null(seen[[key]])) {
      out[[paste0('Preset_', pn, '_Duplicate')]] <- paste0(
        '`Preset$', pn, '` selects the exact same OMs as `Preset$', seen[[key]],
        '` - check this is intended'
      )
    } else {
      seen[[key]] <- pn
    }
  }
  out
}

#' Check a flat (item-index) `Preset` list, as used by `MPs` and by the
#' performance-metric selectors of `Boxplot`/`Kobe`/`Quilt`/`Spider`/
#' `Timeseries`/`Tradeoff`.
#'
#' Each element of `Preset` should be a numeric vector of whole-number
#' indices into that object's own `Code` (length `max_len`).
#' @noRd
check_Preset <- function(Preset, max_len) {
  out <- list()
  if (length(Preset)<1) return(out)

  # check names
  nms <- names(Preset)
  if (is.null(nms) || any(nchar(nms)<1)) {
    out$Preset_Names <- '`Preset` must be a named list, with a non-blank name for each preset'
  } else if (any(duplicated(nms))) {
    out$Preset_Names <- paste0(
      '`Preset` names must be unique - found duplicate name(s): ',
      paste(unique(nms[duplicated(nms)]), collapse=', ')
    )
  }

  for (pn in nms) {
    p <- Preset[[pn]]
    vals <- suppressWarnings(as.numeric(p))
    is_whole <- abs(vals - round(vals)) < sqrt(.Machine$double.eps)

    if (length(p)<1 || anyNA(vals) || !all(is_whole)) {
      out[[paste0('Preset_', pn, '_Values')]] <- paste0(
        '`Preset$', pn, '` must be a vector of whole numbers, got: ',
        paste(utils::head(p, 5), collapse=', ')
      )
      next
    }
    if (any(vals<1) || any(vals>max_len)) {
      out[[paste0('Preset_', pn, '_Range')]] <- paste0(
        '`Preset$', pn, '` values must be between 1 and ', max_len,
        ' (the number of `Code` entries), got: ',
        paste(vals[vals<1 | vals>max_len], collapse=', ')
      )
    }
    if (any(duplicated(vals))) {
      out[[paste0('Preset_', pn, '_Duplicate')]] <- paste0(
        '`Preset$', pn, '` contains duplicate indices: ',
        paste(unique(vals[duplicated(vals)]), collapse=', ')
      )
    }
  }
  out
}

check_Value <- function(value, req_dimensions) {
  out <- list()
  if (is_empty_value(value))
    return(out)

  if (length(dim(value)) != length(req_dimensions)) {
    out$Value <- paste('`Value` must be a', length(req_dimensions), 'dimensionsal array')
  } else {
    dd <- dim(value)
    ind <- !is.na(req_dimensions)
    for (i in seq_along(ind)) {
      if (ind[i]) {
        if (dd[i] != req_dimensions[i])
          out$Value <- append(out$Value,
                              paste('Dimension', i, 'of `Value` must be length', req_dimensions[i])
          )

      }
    }
  }

  out
}

# print/show functions ----

print_show_heading <- function(object) {
  chk <- Check(object)
  if (is.list(chk@empty)) {
    if(all(unlist(chk@empty))) {
      cli::cli_h1('An empty object of class {.code {class(object)}}')
    } else {
      cli::cli_h1('An object of class {.code {class(object)}}')
    }
  } else if (chk@empty) {
    cli::cli_h1('An empty object of class {.code {class(object)}}')
  } else {
    cli::cli_h1('An object of class {.code {class(object)}}')
  }
  chk
}

print_show_heading_2 <- function(object) {
  chk <- Check(object)
  if (is.list(chk@empty)) {
    if(all(unlist(chk@empty))) {
      cli::cli_h2('An empty object of class {.code {class(object)}}')
    } else {
      cli::cli_h2('An object of class {.code {class(object)}}')
    }
  } else if (chk@empty) {
    cli::cli_h2('An empty object of class {.code {class(object)}}')
  } else {
    cli::cli_h2('An object of class {.code {class(object)}}')
  }
  chk
}


shorten_long_text <- function(text, maxchar=40) {
  for (i in seq_along(text)) {
    if (nchar(text[i])> maxchar) {
      text[i] <- paste0(substr(text[i], 1, maxchar), ' ...')
    }
  }
  text
}

print_value <- function(object, dim_names, mp_names=NULL) {
  value <- object@Value
  cli::cli_h2(c('{.code Value}'))
  if (!is_empty_value(value)) {
    dd <- dim(value)

    df <- data.frame(Dimension=dim_names,
                     Length=dd)
    print(df)


    ind <- which(dim_names %in% c('nMP', 'nPI'))
    mean <- round(apply(value, ind, mean, na.rm=TRUE), 2)
    if (is.null(mp_names))
      mp_names <- paste('MP', 1:dd[ind[1]])
    rownames(mean) <- mp_names

    pi_names <- Code(object)
    if (all(nchar(pi_names)<1))
      pi_names <- paste('PI', 1:dd[ind[2]])
    colnames(mean) <- pi_names
    cat('\n')
    cli::cli_alert_info('Mean across MPs and PIs')
    print(mean)
  }
}

print_metadata <- function(text, type='Code', heading='Multi-language List',
                           ol=TRUE, addNum=TRUE) {
  cli::cli_h2(c('{.code {type}}'))
  if (is.list(text)) {
    if (!is.null(heading))
      cli::cli_alert_info(heading)
    nms <- names(text)
    for (i in seq_along(text)) {
      text[[i]] <- shorten_long_text(text[[i]])
      cli::cli_h3(nms[i])
      if (ol) {
        cli::cli_ol(text[[i]])
      } else {
        if (is.list(text[[i]])) {
          for (j in 1:length(text[[i]])) {
            cli::cli_inform(text[[i]][[j]])
          }

        } else {
          cli::cli_inform(text[[i]])
        }

      }

    }
  } else {
    if (any(nchar(text)>0)) {
      if (addNum) {
        text <- paste(1:length(text), shorten_long_text(text))
      } else {
        text <- shorten_long_text(text)
      }

    }
    if (all(nchar(text))>0)
      cli::cli_inform(as.character(text)  )

  }

}


print_preset <- function(Preset) {
  if (length(Preset)<1) {
    cli::cli_h2(c('{.code {"Preset"}}'))
   } else {
    print_metadata(Preset, type='Preset', heading=NULL, ol=FALSE)
  }
}


print_factors <- function(Factors) {
  cli::cli_h2(c('{.code {"Factors"}}'))
  if (is.list(Factors) & !is.data.frame(Factors)) {
    if (length(Factors)>0) {
      cli::cli_alert_info('Multi-language List')
      nms <- names(Factors)
      for (i in seq_along(Factors)) {
        cli::cli_h3(nms[i])
        Factors[[i]]$Description <- shorten_long_text(Factors$Description[[i]])
        print(Factors[[i]])
        cat('\n')
      }
    }
  } else {
    if (nrow(Factors)>0) {
      nfact <- ncol(Factors)
      Factors$Description <- shorten_long_text(Factors$Description)
      print(Factors)
      cat('\n')
    }
  }


}

print_design <- function(Design) {
  cli::cli_h2(c('{.code {"Design"}}'))
  if (nrow(Design)>0) {
    print(Design)
    cat('\n')
  }

}







