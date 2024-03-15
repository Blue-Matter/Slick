
HomeDEVServer <- function(id, Object, i18n) {
  ns <- NS(id)
  moduleServer(id,
               function(input, output, session) {

                 output$welcome <- renderUI({
                   i18n <- i18n()
                   tagList(
                     box(title=h2(i18n$t('Welcome to Slick')), collapsible = FALSE,
                         width=12,
                         solidHeader=FALSE,
                         status = "primary",
                         column(5,

                         ## Welcome
                         h3(i18n$t('About Slick')),
                         p(i18n$t(
                           'Slick is a decision analysis tool that presents the outcomes of candidate harvest strategies across various states of nature, accounting for uncertainty. Slick is interactive and allows users to filter results live in order to explore robustness and performance.'
                           )),
                         p(i18n$t(
                           'While Slick can be applied to any decision analysis context it was specifically designed to investigate the performance of harvest strategies tested by management strategy evaluation (MSE). Slick can be used for any MSE process, but the current version includes a demonstration example, as well as preliminary results from the MSEs for Western Atlantic skipjack tuna and North Atlantic swordfish.'
                         ))
                         ),
                     column(4,

                         ## How to use Slick
                         h3(i18n$t('How to use Slick')),
                         tags$ol(
                           tags$li(
                             h4(i18n$t('Create a Slick Data File:')),
                             p(i18n$t('To use Slick, you will either need a Slick Data file containing your MSE results, or choose from one of the examples available in Slick. MSE Technical Developers should consult the'), a(href='https://blue-matter.github.io/openMSE/Slick-Developer-Guide.html', "Developer's Guide",  target="_blank"), i18n$t('for information on converting their MSE results into a compatible Slick Data file')
                             )

                           ),
                           tags$li(
                             h4(i18n$t('Load Slick:')),
                             p(i18n$t('Load your Slick Data File or select an example on the right.'))
                           ),
                           tags$li(
                             h4(i18n$t('Explore the Results:')),
                             p(i18n$t('After a Slick Data File is loaded, the results can be explored in one of the 12 Slick visualization plots. Click the icon with three horizontal lines in the top left corner to expand the side menu.')
                             ),
                             tags$li(
                               h4(i18n$t('Filter the Results:')),
                               p(i18n$t('The Filter button in the top right corner can be used to filter the MSE results shown in the plots. Click the Filter button to expand the Filter control menu')
                               )
                             )
                           )
                         )
                     ),
                     column(3,
                            h4(i18n$t('Load your MSE Results')),
                            fileInput("Load", accept=c("slick",".slick"),
                                      label = i18n$t("From file (.slick)"),
                                      buttonLabel=list(icon("folder",verify_fa = FALSE))
                            ),

                            h4(i18n$t('Load an Example')),
                            selectInput('example_input',
                                        label=i18n$t('Example'),
                                        choices=case_study_df$Example,
                                        selected=NULL
                            ),
                            actionButton("example_upload", i18n$t("Load"),
                                         icon("upload", verify_fa = FALSE)),
                            downloadButton("example_download", i18n$t("Download"),
                                           icon("cloud-download", verify_fa = FALSE))

                            )
                     ),




                                     box(title=h2(i18n()$t('Slick Plots')), collapsible = TRUE,
                                         width=12,
                                         solidHeader=TRUE,
                                         status = "primary",
                                         fluidRow(
                                           # deterministic
                                           box(title=h2(i18n()$t('Deterministic')),
                                               p(i18n()$t('The Deterministic plots show results that are summarized as a single value for each operating model (OM) and management procedure (MP). The Deterministic performance metrics do not include any measures of uncertainty and are scaled between 0 (worst performance) and 100 (best performance).')),
                                               tags$ol(
                                                 tags$li(
                                                   p(a(onclick='customHref("spider");', style="cursor: pointer;", "Spider:"),
                                                     i18n()$t('Also sometimes referred to as Radar charts or web diagrams, these plots show results in the form of a two-dimensional chart of three or more quantitative values, each represented on axes starting from the same center point. The results are shown as the median values of each performance metric across the operating models.')),
                                                   p(i18n()$t('You can see an example Spider plot'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-01-spider-plot.pdf', 'here.', target="_blank"))
                                                 ),
                                                 tags$li(p(a(onclick='customHref("spiderOM");', style="cursor: pointer;", "Spider OM:"),
                                                           i18n()$t('Similar to the Spider plot but show the results separately for each OM.')),
                                                         p(i18n()$t('You can see an example Spider OM plot'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-10-spider-plot.pdf', 'here.', target="_blank"))
                                                 ),

                                                 tags$li(p(a(onclick='customHref("zigzag");', style="cursor: pointer;", "Zigzag:"),
                                                           i18n()$t('Compare the individual scores for each performance metric. Points further to the left indicate worse performance. The scores are calculated as the median values of each performance metric across the operating models. An overall average score for each management procedure is calculated as the average of the individual performance metric scores.')),
                                                         p(i18n()$t('You can see an example Zigzag plot'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-03-zigzag-plot.pdf', 'here.', target="_blank"))
                                                 ),
                                                 tags$li(p(a(onclick='customHref("rail");', style="cursor: pointer;", "Rail:"),
                                                           i18n()$t('Similar to the Zigzag plot but without lines connecting the performance metrics for each management procedure.')),
                                                         p(i18n()$t('You can see an example Rail plot'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-07-rail-plot.pdf', 'here.', target="_blank"))
                                                 )
                                               )
                                           ),

                                           # stochastic
                                           box(title=h2(i18n()$t('Stochastic')),
                                               p(i18n()$t('The Stochastic plots show results that include a measure of uncertainty. The performance metrics in these plots include a single value for each simulation, OM, and MP. The results are typically summarized by MP as the median and percentiles of the performance metrics across the simulations (and sometimes across the OMs). They are scaled between 0 (worst performance) and 100 (best performance).')),
                                               tags$ol(
                                                 tags$li(p(a(onclick='customHref("boxplot");', style="cursor: pointer;", "Boxplot:"),
                                                           i18n()$t('The stochastic performance metrics for each MP are summarized across the operating models and shown as the median, 1st and 3rd quartiles, and lowest and highest values.')),
                                                         p(i18n()$t('You can see an example Boxplot plot'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-09-box-plot.pdf', 'here.', target="_blank"))
                                                 ),

                                                 tags$li(p(a(onclick='customHref("boxplotOM");', style="cursor: pointer;", "Boxplot OM:"),
                                                           i18n()$t('Similar to Boxplot, but showing the results separately for each operating model.')),
                                                         p(i18n()$t('You can see an example Boxplot OM plot'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-12-box-plot.pdf', 'here.', target="_blank"))
                                                 ),

                                                 tags$li(p(a(onclick='customHref("violin");', style="cursor: pointer;", "Violin:"),
                                                           i18n()$t('Similar to Boxplot, but the variability is shown as a density plot, where the width of the violin indicating the frequency of data points in each region of the plot.'))
                                                 )
                                               )
                                           )
                                         ),
                                         fluidRow(
                                           # projection
                                           box(title=h2(i18n()$t('Projection')),
                                               p(i18n()$t('The Projection plots show results that include a time dimension. These performance metrics include a value for each simulation, OM, MP, and projection time-step.')),
                                               tags$ol(
                                                 tags$li(p(a(onclick='customHref("kobe");', style="cursor: pointer;", "Kobe:"),
                                                           i18n()$t('A trade-off plot comparing the performance of MPs across operating models with respect to two performance metrics. By default, the plot shows the projected performance metrics from the final year in the projection period. Uncertainty is displayed using error bars with user-specified percentiles.')),
                                                         p(i18n()$t('You can see an example Kobe plot'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-05-kobe-plot.pdf', 'here.', target="_blank"))
                                                 ),
                                                 tags$li(p(a(onclick='customHref("kobetime");', style="cursor: pointer;", "Kobe Time:"),
                                                           i18n()$t('Shows the median percentage of simulations (across all operating models) that fall in each of the Kobe quadrants in each projection year.')),
                                                         p(i18n()$t('You can see an example Kobe Time plot'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-06-kobe-time-plot.pdf', 'here.', target="_blank"))
                                                 ),
                                                 tags$li(p(a(onclick='customHref("slope");', style="cursor: pointer;", "Slope:"),
                                                           i18n()$t('Compares trade-offs in two competing performance metrics. By default, the plot shows the projected performance metrics from the final year in the projection period. Uncertainty is displayed using error bars with 90th percentiles.')),
                                                         p(i18n()$t('You can see an example Slope plot'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-04-slope-plot.pdf', 'here.', target="_blank"))
                                                 )
                                               )
                                           ),
                                           # state
                                           box(title=h2(i18n()$t('Time Series')),
                                               p(i18n()$t('The Time Series plots are similar to the Projected plot, but include the historical years.  State variables are quantities that have persisted in the past as well as the future and in this way provide a historical perspective on future MP performance')),
                                               tags$ol(
                                                 tags$li(p(a(onclick='customHref("line");', style="cursor: pointer;", "Line:"),
                                                           i18n()$t('Plots the time-series of a user-specified variable during the historical period and the projection period for each management procedure. Results are summarized across all operating models. Uncertainty in the projection period is summarized as a median, 25th and 75th, and 10th and 90th percentiles.')),
                                                         p(i18n()$t('You can see an example Line plot'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-08-line-plot.pdf', 'here.', target="_blank"))
                                                 ),
                                                 tags$li(p(a(onclick='customHref("lineOM");', style="cursor: pointer;", "Line OM:"),
                                                           i18n()$t('Similar to the Line plot, but shows the results for each operating model separately.')),
                                                         p(i18n()$t('You can see an example Line OM plot'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-11-line-plot.pdf', 'here.', target="_blank"))
                                                 ),
                                                 tags$li(p(a(onclick='customHref("lineOMSim");', style="cursor: pointer;", "Line OM Sim:"),
                                                           i18n()$t('Similar to the Line OM plot, but shows the results from an individual simulation for each operating model.'))
                                                 )
                                               )
                                           )
                                         )
                                     )
                              )


                 })




               }
  )
}



HomeDEVUI <- function(id, label="home") {

  ns <- NS(id)

  tagList(
    usei18n(i18n),
    fluidRow(
      htmlOutput(ns('welcome'))
    )
  )
}


