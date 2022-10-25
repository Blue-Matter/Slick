
HomeServer <- function(id, i18n) {
  moduleServer(id,
               function(input, output, session) {

                 output$welcome <- renderUI({
                   tagList(
                     fluidRow(
                       column(12,
                              box(title=h2(i18n()$t('Welcome')), collapsible = TRUE,
                                  width=6,
                                  solidHeader=TRUE,
                                  status = "primary",
                                  h3(i18n()$t('Welcome to Slick')),
                                  p(i18n()$t('Slick is a decision analysis tool that presents the outcomes of potential policy options across various states of nature. It allows for the simultaneous presentation of various performance metrics and can account for uncertainty in the states of nature. Slick is interactive and allows users to filter results live in order to explore robustness and performance.')),
                                  p(i18n()$t('While Slick can be applied to any decision analysis context it was specifically designed to investigate the performance of harvest strategies tested by Management Strategy Evaluation (MSE)')),

                                  h3(i18n()$t('Slick Presentation of MSE Results')),
                                  p(i18n()$t('MSEs have four axes over which results are generally presented: ')),
                                  tags$ol(
                                    tags$li(i18n()$t('Operating Models (a state of nature or scenario for real system dynamics)')),
                                    tags$li(i18n()$t('Management Procedures (MP - a management option, aka. harvest strategy)')),
                                    tags$li(i18n()$t('Performance Metrics (aka. cost function, utility measure. E.g. probability of not overfishing, long-term yields)')),
                                    tags$li(i18n()$t('Uncertainty within an Operating Model (multiple simulations for each discrete state of nature)'))
                                  ),
                                  p(i18n()$t("Slick allows users to filter Operating Models, Performance Metrics and Management Procedures in order to explore robustness and characterize performance. Importantly, Slick is MSE-platform agnostic. Provided MSE practitioners format their results in a compatible Slick data file, these can be loaded to the App."))

                              ),
                              box(title=h2(i18n()$t('Using Slick')), collapsible = TRUE,
                                  boxToolSize='xs',
                                  width=6,
                                  solidHeader=TRUE,
                                  status = "primary",
                                  h3(i18n()$t('1. Create a Slick Data File')),
                                  p(i18n()$t('To use Slick, you will need your MSE results available in a Slick Data file. MSE Technical Developers should consult the'), a(href='https://blue-matter.github.io/openMSE/Slick-Developer-Guide.html', "Developer's Guide",  target="_blank"), i18n()$t('for information on converting their MSE results into a compatible Slick Data file. Slick includes several example Data files which can be used to explore the App.')),
                                  h3(i18n()$t('2. Load Slick File')),
                                  p(i18n()$t('Go to the '), a(onclick="openTab('load')", href="#", 'Load'), i18n()$t('panel on the left and upload your Slick Data file. If you do not have your MSE results in a compatible Slick Data File, you can load one of the example Slick Data files. The example Data Files can also be downloaded as R objects so you can explore how the Slick objects are constructed.')),
                                  h3(i18n()$t('3. Explore the MSE Results')),
                                  p(i18n()$t('Once you have uploaded a Slick Data File containing MSE results, you can explore these results in one of the 12 Slick visualization plots. See the Slick Plots box below for more information on the plots')),
                                  h3(i18n()$t('4. Filter the MSE Results')),
                                  p(i18n()$t('The Filter button in the top right corner can be used to filter the MSE results show in the plots. See the box below for more details.'))
                                  )
                       )
                     ),
                     fluidRow(
                       column(12,
                              box(title=h2(i18n()$t('Slick Plots')), collapsible = TRUE,
                                  width=12,
                                  solidHeader=TRUE,
                                  status = "primary",
                                  fluidRow(
                                    # deterministic
                                    box(title=h2(i18n()$t('Deterministic')),
                                        p(i18n()$t('The Deterministic plots show results that a summarized as a single value for each operating model (OM) and management procedure (MP). The Determinstic performance metrics do not include any measures of uncertainty and are scaled between 0 (worst performance) and 100 (best performance).')),
                                        tags$ol(
                                          tags$li(
                                            p(a(onclick='customHref("spider");', style="cursor: pointer;", "Spider:"),
                                              i18n()$t('Also sometimes referred to as Radar charts, these plots show multivariate data in the form of a two-dimensional chart of three or more quantitative values, each represented on an axes starting from the same point. The results are shown as the median values of each performance metric across the operating models.')),
                                            p(i18n()$t('You can see a general example of the Spider plots'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-01-spider-plot.pdf', 'here.', target="_blank"))
                                          ),
                                          tags$li(p(a(onclick='customHref("spiderOM");', style="cursor: pointer;", "Spider OM:"),
                                                    i18n()$t('Similar the to Spider plot but show the results for each OM included in the Slick data file.')),
                                                  p(i18n()$t('You can see a general example of the Spider OM plots'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-10-spider-plot.pdf', 'here.', target="_blank"))
                                          ),

                                          tags$li(p(a(onclick='customHref("zigzag");', style="cursor: pointer;", "Zigzag:"),
                                                    i18n()$t('Compare the individual and overall average scores for each performance metric. Points further to the left indicate worse performance.')),
                                                  p(i18n()$t('You can see a general example of the Zigzag plots'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-03-zigzag-plot.pdf', 'here.', target="_blank"))
                                          ),
                                          tags$li(p(a(onclick='customHref("rail");', style="cursor: pointer;", "Rail:"),
                                                    i18n()$t('Similar to the Zigzag plot but without lines connecting the performance metrics for each management procedure.')),
                                                  p(i18n()$t('You can see a general example of the Rail plots'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-07-rail-plot.pdf', 'here.', target="_blank"))
                                          )
                                        )
                                    ),

                                    # stochastic
                                    box(title=h2(i18n()$t('Stochastic')),
                                        p(i18n()$t('The Stochastic plots show results that include a meausure of uncertainty. These performance metrics have a single value for each simulation, OM, and MP. The results are typically summarized as the median and percentiles of the performance metrics across the simulations (and sometimes across the OMs). They are scaled between 0 (worst performance) and 100 (best performance).')),
                                        tags$ol(
                                          tags$li(p(a(onclick='customHref("boxplot");', style="cursor: pointer;", "Boxplot:"),
                                                    i18n()$t('The stochastic performance metrics for each MP are summarized across the operating models and shown as the median, 1st and 3rd quartiles, and lowest and highest values.')),
                                                  p(i18n()$t('You can see a general example of the Boxplot plots'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-09-box-plot.pdf', 'here.', target="_blank"))
                                          ),

                                          tags$li(p(a(onclick='customHref("boxplotOM");', style="cursor: pointer;", "Boxplot OM:"),
                                                    i18n()$t('Similar to Boxplot, but showing the results for each operating model included in the Slick data file.')),
                                                  p(i18n()$t('You can see a general example of the Boxplot OM plots'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-12-box-plot.pdf', 'here.', target="_blank"))
                                          ),

                                          tags$li(p(a(onclick='customHref("violin");', style="cursor: pointer;", "Violin:"),
                                                    i18n()$t('Similar to Boxplot, but the variability is shown as a kernel density plot.'))
                                          )
                                        )
                                    )
                                  ),
                                  fluidRow(
                                    # projection
                                    box(title=h2(i18n()$t('Projected')),
                                        p(i18n()$t('The Projected plots show results that include a time dimension. These performance metrics include a value for each simulation, OM, MP, and projection time-step.')),
                                        tags$ol(
                                          tags$li(p(a(onclick='customHref("kobe");', style="cursor: pointer;", "Kobe:"),
                                                    i18n()$t('A trade-off plot comparing the performance of MP across operating models with respect to two performance metrics. By default the plot shows the projected performance metrics from the final year in the projection period. Uncertainty is displayed using error bars with user-specified percentiles.')),
                                                  p(i18n()$t('You can see a general example of the Kobe plots'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-05-kobe-plot.pdf', 'here.', target="_blank"))
                                          ),
                                          tags$li(p(a(onclick='customHref("kobetime");', style="cursor: pointer;", "Kobe Time:"),
                                                    i18n()$t('Shows the median percentage of simulations (across all operating models) that fall in each of the Kobe quadrants in each projection year.')),
                                                  p(i18n()$t('You can see a general example of the Kobe Time plots'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-06-kobe-time-plot.pdf', 'here.', target="_blank"))
                                          ),
                                          tags$li(p(a(onclick='customHref("slope");', style="cursor: pointer;", "Slope:"),
                                                    i18n()$t('Compares trade-offs in two co-dependent performance metrics. By default the plot shows the projected performance metrics from the final year in the projection period. Uncertainty is displayed using error bars with 90th percentiles.')),
                                                  p(i18n()$t('You can see a general example of the Slope plots'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-04-slope-plot.pdf', 'here.', target="_blank"))
                                          )
                                        )
                                    ),
                                    # state
                                    box(title=h2(i18n()$t('State Variables')),
                                        p(i18n()$t('The State Variables plots are similar to the Projected plot, but include the historical years.  State variables are quantities that have persisted in the past as well as the future and in this way provide a historical perspective on future MP performance')),
                                        tags$ol(
                                          tags$li(p(a(onclick='customHref("line");', style="cursor: pointer;", "Line:"),
                                                    i18n()$t('Plots the time-series of a user-specified state varaible from the historical period and for the projection period for each management procedure. Results are summarized across all operating models. Uncertainty in the projection period is summarized as a median, 25th and 75th, and 10th and 90th percentiles.')),
                                                  p(i18n()$t('You can see a general example of the Line plots'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-08-line-plot.pdf', 'here.', target="_blank"))
                                          ),
                                          tags$li(p(a(onclick='customHref("lineOM");', style="cursor: pointer;", "Line OM:"),
                                                    i18n()$t('Similar to the Line plot, but shows the results for each operating model separately.')),
                                                  p(i18n()$t('You can see a general example of the Line OM plots'), a(href='https://harveststrategies.org/wp-content/uploads/2021/01/mse-graphic-11-line-plot.pdf', 'here.', target="_blank"))
                                          )

                                        )
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



HomeUI <- function(id, label="home") {

  ns <- NS(id)


  tagList(
    usei18n(i18n),
    fluidRow(

      htmlOutput(ns('welcome')),
      plotOutput(ns("plot_test"))
    )
  )
}


