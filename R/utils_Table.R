
tableBoxplot <- function(boxplot, lang=NULL) {
  df <- Metadata(boxplot, lang)
  DT::datatable(df,
                extensions = 'Responsive',
                selection='none',
                options = list(dom = 't',
                               pageLength=100,
                               ordering=F))
}

tableMPs <- function(MPs, lang) {
  df <- Metadata(MPs, lang)
  tab_df <- df |> dplyr::select(-Color)
  cnames <- colnames(tab_df)

  DT::datatable(tab_df,
                extensions = 'Responsive',
                selection='none',
                options = list(dom = 't',
                               pageLength=100,
                               ordering=F)) |>
    DT::formatStyle(cnames[1],
                  color = DT::styleEqual(df[[cnames[1]]],
                                     df$Color)) |>
    DT::formatStyle(cnames[2],
                    color = DT::styleEqual(df[[cnames[2]]],
                                           df$Color)
    )
}

tableOMs <- function(OMs, lang, type='factor') {

  if (type=='factor') {
    return(DT::datatable(Metadata(OMs, lang),
                         extensions = 'Responsive',
                         selection='none',
                         options = list(dom = 't',
                                        pageLength=100,
                                        ordering=F))
    )
  }

  DT::datatable(Design(OMs),
                extensions = 'Responsive',
                selection='none',
                options = list(dom = 't',
                               pageLength=100,
                               ordering=F,
                               columnDefs = list(list(className = 'dt-center', targets = "_all"))))
}



tableQuilt <- function(quilt, lang=NULL) {

  df <- Metadata(quilt, lang) |>
    dplyr::select(-MinValue, -MaxValue)
  DT::datatable(df,
                extensions = 'Responsive',
                selection='none',
                options = list(dom = 't',
                               pageLength=100,
                               ordering=F))
}


tableSpider <- function(spider, lang=NULL) {

  df <- Metadata(spider, lang) |>
    dplyr::select(-MinValue, -MaxValue)
  DT::datatable(df,
                extensions = 'Responsive',
                selection='none',
                options = list(dom = 't',
                               pageLength=100,
                               ordering=F))
}
