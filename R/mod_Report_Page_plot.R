# https://stackoverflow.com/questions/67941650/get-external-image-file-dimensions-with-base-r
get_image_dimensions <- function(path) {
  # Ensure file exists
  if(!file.exists(path))
    stop("No file found", call. = FALSE)

  # Ensure file ends with .png or .jpg or jpeg
  if (!grepl("\\.(png|jpg|jpeg)$", x = path, ignore.case = TRUE))
    stop("File must end with .png, .jpg, or .jpeg", call. = FALSE)

  # Get return of file system command
  s <- system(paste0("file ", path), intern = TRUE)

  # Extract width and height from string
  width <- regmatches(s, gregexpr("(?<=, )[0-9]+(?=(x| x )[0-9]+,)", s, perl = TRUE))[[1]]
  height <- regmatches(s, gregexpr(", [0-9]+(x| x )\\K[0-9]+(?=,)", s, perl = TRUE))[[1]]
  as.numeric(c(width, height))
}

#' Report_Page_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Report_Page_plot_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('showplot'))
  )
}

#' Report_Page_plot Server Functions
#'
#' @noRd
mod_Report_Page_plot_server <- function(id, i18n, PlotName='Boxplot', Report){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(Report[[PlotName]], {
      nplot <- length(Report[[PlotName]]$plot)
      if (nplot>0) {
        for (x in 1:nplot) {
          local({
            this_x <- x
            this_plot <- Report[[PlotName]]$plot[[this_x]]
            if (!prod(is.na(this_plot)))
              output[[paste(this_x, tolower(PlotName), sep="")]] <- renderImage({
                this_plot
              }, deleteFile=FALSE)
          })
        }
      }
    })

    # remove an item; the Remove buttons send the item index to input$remove
    to_remove <- reactiveVal()

    observeEvent(input$remove, {
      i18n <- i18n()
      to_remove(input$remove)
      showModal(modalDialog(
        title=i18n$t('Remove from Report'),
        i18n$t('Are you sure you want to remove this item from the Report?'),
        footer=tagList(
          div(modalButton(i18n$t('Cancel')), style='float:left;'),
          actionButton(ns('confirm_remove'), i18n$t('Remove'),
                       icon=icon('trash'), class='btn-danger')
        )
      ))
    })

    observeEvent(input$confirm_remove, {
      removeModal()
      x <- to_remove()
      this_plot <- Report[[PlotName]]$plot[[x]]
      if (all(is.na(this_plot)))
        return(NULL)
      if (file.exists(this_plot$src))
        file.remove(this_plot$src)
      section <- Report[[PlotName]]
      section$plot[[x]] <- NA
      section$caption[[x]] <- NA
      Report[[PlotName]] <- section
    })

    plotlist <- reactive({
      nplot <- length(Report[[PlotName]]$plot)
      width <- 800
      width_fun <- function(width)
        paste0(width, 'px')

      height_fun <- function(width, img_dims) {
        ratio <- img_dims[1]/img_dims[2]
        paste0(width/ ratio, 'px')
      }

      if (nplot>0) {
        i18n <- i18n()
        plot_output_list <- lapply(1:nplot, function(x) {
          # removed items are set to NA
          if (all(is.na(Report[[PlotName]]$plot[[x]])))
            return(NULL)
          plotname <- paste0(x, tolower(PlotName))
          path <- Report[[PlotName]]$plot[[x]]$src
          img_dims <- get_image_dimensions(path)

          caption <- Report[[PlotName]]$caption[[x]]
          tagList(
            hr(),
            imageOutput(ns(plotname),
                        width=width_fun(width),
                        height=height_fun(width, img_dims)),
            p(caption),
            actionButton(ns(paste0('del_', x)),
                         label=i18n$t('Remove'),
                         icon=icon('trash'),
                         class='btn-danger btn-sm',
                         onclick=sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})",
                                         ns('remove'), x)),
            hr()
          )
        })
        do.call('tagList', plot_output_list)
      }
    })

    output$showplot <- renderUI({
      chk <- lapply(Report[[PlotName]]$plot, is.na) |> unlist()
      if (!all(chk)) {
        tagList(
          h3(PlotName),
          plotlist()
        )
      }
    })
  })
}

## To be copied in the UI
# mod_Report_Page_plot_ui("Report_Page_plot_1")

## To be copied in the server
# mod_Report_Page_plot_server("Report_Page_plot_1")
