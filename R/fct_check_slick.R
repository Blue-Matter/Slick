check_slick_file <- function(slick) {
  if (!inherits(slick, 'Slick') &  (!inherits(slick, 'SLICK'))) {
    shinyalert::shinyalert('Incorrect File Type',
                           'The loaded file is not a Slick object',
                           type='error')
    return(NULL)
  }

  # update
  if (!isS4(slick))
    slick <- Update(slick)

  # check
  # TODO update
  check <- try(Check(slick))

  if (inherits(check, 'try-error')) {
    shinyalert::shinyalert('Invalid Slick object',
                           'Use `Check(`slick_object`)` to see the errors',
                           type='error')
  }

  # set MP colors
  if (any(nchar(slick@MPs@Color)<2)) {
    nMPs <- length(slick@MPs@Code)
    Color(slick@MPs) <- default_mp_colors(nMPs)
  }
  slick


}
