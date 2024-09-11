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
    val <- slot(object, sl)
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
  if (!all(nms %in% c('en', 'es', 'fr')))
    error_list <- append(error_list,
                         'Named list must only be `en`, `es`, or `fr`')

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
    if (any(nchar(object@Description) >0)) {
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

check_Preset <- function(Preset, max_len) {
  out <- list()

  if (length(Preset)>0) {
    # check length
    ll <- lapply(Preset, length) |> unlist()
    if (any(ll)>max_len) {
      out$Preset_Length <- paste('Elements of `Preset` cannot be longer than', max_len)
    }
    # check names
    nms <- names(Preset)
    if (is.null(nms)) {
      out$Preset_Names <- '`Preset` must be a named list'
    } else {
      if (any(nchar(nms)<1))
        out$Preset_Names <- '`Preset` must be a named list'
    }

    #check values
    cl <- lapply(Preset, class) |> unlist()
    if (any(cl!='integer')) {
      out$Preset_Values <- '`Preset` must only be integer values'
    }
    vl <- unlist(Preset)
    if (any(vl>max_len)) {
      out$Preset_Values <- paste('`Preset` values cannot be greater than ', max_len)
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
        if (dd[i] < req_dimensions[i])
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







