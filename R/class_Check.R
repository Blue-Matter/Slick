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



shorten_long_text <- function(text, maxchar=40) {
  for (i in seq_along(text)) {
    if (nchar(text[i])> maxchar) {
      text[i] <- paste0(substr(text[i], 1, maxchar), ' ...')
    }
  }
  text
}

print_metadata <- function(text, type='Code', heading='Multi-language list') {

  usethis::ui_line(c('{usethis::ui_code(type)}'))
  if (is.list(text)) {
    if (!is.null(heading))
      usethis::ui_info(heading)
    nms <- names(text)
    for (i in seq_along(text)) {
      text[[i]] <- shorten_long_text(text[[i]])
      usethis::ui_line(c(nms[i], ':', text))
    }
  } else {
    if (any(nchar(text)>0)) {
      text <- paste(1:length(text), shorten_long_text(text))
    }
    usethis::ui_line(text)
  }

}


print_preset <- function(Preset) {
  if (length(Preset)<1) {
    usethis::ui_line(c('{usethis::ui_code("Preset")}'))

  } else {
    print_metadata(Preset, type='Preset', heading=NULL)
  }

}



print_errors <- function(object) {
  if (inherits(object@errors, 'logical')) {
    if (length(object@errors)>0) {
      nms <- names(object@errors)
      for (i in seq_along(object@errors)) {
        usethis::ui_oops(c(nms[i], object@errors[[i]]))
      }
    }
  }else if (inherits(object@errors, 'list')) {
    for (i in seq_along(object@errors)) {
      object_names <- names(object@errors)
      usethis::ui_info('{usethis::ui_value(object_names[i])}')
      if (length(object@errors[[i]])>0) {
        nms <- names(object@errors[[i]])
        for (j in seq_along(object@errors[[i]])) {
          usethis::ui_oops(c(nms[j], object@errors[[i]][j]))
        }
      }
    }
  }
}

print_warnings <- function(object) {
  if (inherits(object@warnings, 'logical')) {
    if (length(object@warnings)>0) {
      nms <- names(object@warnings)
      for (i in seq_along(object@warnings)) {
        usethis::ui_oops(c(nms[i], object@warnings[[i]]))
      }
    }
  }else if (inherits(object@warnings, 'list')) {
    object_names <- names(object@warnings)
    for (i in seq_along(object@warnings)) {
      if (length(object_names[i])>0)
        usethis::ui_info('{usethis::ui_value(object_names[i])}')
      if (length(object@warnings[[i]])>0) {
        nms <- names(object@warnings[[i]])
        for (j in seq_along(object@warnings[[i]])) {
          usethis::ui_oops(c(nms[j], object@warnings[[i]][j]))
        }
      }
    }
  }
}


# show method ----
setMethod('show', 'CheckList', function(object) {

  usethis::ui_info('Checking: {usethis::ui_value(object@object)}')
  if (object@empty) {
    usethis::ui_line('Object is empty')
  } else {
    # Errors
    print_errors(object)

    # Warnings
    print_warnings(object)

    # Messages

    # Status
    if (object@complete) {
      usethis::ui_done('Complete')
    } else {
      if (length(object@errors)>0) {
        usethis::ui_oops('Errors in object')
      } else if (!object@complete) {
        usethis::ui_oops('Object incomplete')
      }


    }


  }

})







# check functions ----


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
  }
  all(vec)
}


check_lang_list <- function(text_list) {
  nms <- names(text_list)
  if (!any(nms %in% c('en', 'es', 'fr')))
    return('Named list must only be `en`, `es`, or `fr`')

  ll <- lapply(text_list, length)
  len_vals <- unlist(ll)
  if (!all(len_vals==len_vals[1]))
    return('All list elements must be equal length')


}



check_metadata <- function(object) {

  out <- list()
  if (is.list(object@Code)) {
    out$Code <- check_lang_list(object@Code)
  }
  if (is.list(object@Label)) {
    out$Label <- check_lang_list(object@Label)
  }
  if (is.list(object@Description)) {
    out$Description <- check_lang_list(object@Description)
  }

  lCode <- length(object@Code)
  lLabel <- length(object@Label)
  lDescription <- length(object@Description)

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
