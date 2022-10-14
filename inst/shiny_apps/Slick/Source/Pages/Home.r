
HomeServer <- function(id, i18n) {
  moduleServer(id,
               function(input, output, session) {


                 output$welcome <- renderUI({
                   tagList(
                     box(title=h2(i18n()$t('Welcome')), collapsible = TRUE,
                         width=6,
                         solidHeader=TRUE,
                         status = "primary",
                         # column(6,
                                h3(i18n()$t('Welcome to Slick')),

                                p(i18n()$t('Slick is a decision analysis tool that presents the outcomes of potential policy options across various states of nature. It allows for the simultaneous presentation of various performance metrics and can account for uncertainty in the states of nature. Slick is interactive and allows users to filter results live in order to explore robustness and performance.')),

                                p(i18n()$t('While Slick can be applied to any decision analysis context it was specifically designed to investigate the performance of harvest strategies tested by Management Strategy Evaluation (MSE)')),
                                h3(i18n()$t('Management Strategy Evaluation')),
                                p(i18n()$t('Management Strategy Evaluation (MSE) is an approach for establishing simple rules for managing a resource and then simulation testing their robustness to various hypothetical scenarios for system dynamics (Butterworth and Punt 1999; Cochrane et al. 1998).')),
                                p(i18n()$t('Often referred to as Management Procedures (MPs, aka Harvest Strategies) these rules typically use streamlined data to generate management advice such as a Total Allowable Catch (TAC).')),
                                p(i18n()$t('In fisheries, MSE differs substantially from conventional stock assessment in how models of fisheries dynamics are used to derive management advice. In conventional stock assessment, fisheries dynamics models are used to directly derive management advice. For example, setting a TAC commensurate with fishing mortality rate at maximum sustainable yield. MSEs typically use a greater number of fitted fisheries dynamics models ("operating models") that span a much wider range of uncertainties in order to test the robustness of MPs. The focus in MSE is robustness accounting for feedbacks between management options and the system rather than establishing a single "best" model of the resource.')),
                                p(i18n()$t('Consequently, MSE allows managers and stakeholders to establish a comparatively simple management rule (an MP), understand its performance and have confidence that it can perform adequately even in the face of uncertainties in system dynamics.')),
                                p(i18n()$t('See'), a(href='https://harveststrategies.org/management-strategy-evaluation-2/', 'here', target="_blank"), i18n()$t('for more information on MSE. Punt et al. (2014) also provide a comprehensive summary of the history of MSE implementations.'))
                         ,

                                h3(i18n()$t('Slick Presentation of MSE Results')),
                                p(i18n()$t('MSEs have four axes over which results are generally presented: ')),
                                tags$ol(
                                  tags$li(i18n()$t('Operating Models (a state of nature or scenario for real system dynamics)')),
                                  tags$li(i18n()$t('Management Procedures (MP - a management option, aka. harvest strategy)')),
                                  tags$li(i18n()$t('Performance Metrics (aka. cost function, utility measure. E.g. probability of not overfishing, long-term yields)')),
                                  tags$li(i18n()$t('Uncertainty within an Operating Model (multiple simulations for each discrete state of nature)'))
                                ),
                                p(i18n()$t('Slick allows users to filter Operating Models, Performance Metrics and Management Procedures in order to explore robustness and characterize performance. Importantly, Slick is MSE-platform agnostic. Provided MSE practitioners format their results in a compatible Slick data file, these can be loaded to the App.')),
                                h3(i18n()$t('About Slick')),
                                p(i18n()$t('Slick was designed and commissioned by'), a(href='https://oceanfdn.org/', 'The Ocean Foundation',  target="_blank"), i18n()$t('and developed by'), a(href='https://www.bluematterscience.com/', 'Blue Matter Science.',  target="_blank")),
                                p(i18n()$t('Slick is under going further development. All feedback is welcome. Please contact'),
                                  a(href="mailto:smiller@oceanfdn.org?&subject=Slick Development", 'Shana Miller'), i18n()$t('with any comments or suggestions for further development'))

                     ),
                     box(title=h2(i18n()$t('How to Use Slick')), collapsible = TRUE,
                         width=6,
                         solidHeader=TRUE,
                         status = "primary",
                         column(12,
                                h3(i18n()$t('Slick Data File')),
                                p(i18n()$t('To use Slick, you will need your MSE results available in a Slick Data file. MSE Technical Developers should consult the'), a(href='https://blue-matter.github.io/openMSE/Slick-Developer-Guide.html', "Developer's Guide",  target="_blank"), i18n()$t('for information on converting their MSE results into a compatible Slick Data file.')),
                                p(i18n()$t('Slick includes several Example Data files which can be used to explore the App')),

                                h3(i18n()$t('About the Slick Plots')),

                         )
                     )
                     # ),
                     # box(title=h2(i18n()$t('About the Slick Plots')), collapsible = TRUE,
                     #     width=6,
                     #     solidHeader=TRUE,
                     #     status = "primary",
                     #     column(12,
                     #            p(i18n()$t('Slick includes 11 Plots designed to inform decision making by revealing the absolute and comparative performance of candidate management procedures.'))
                     #            )
                     #
                     #
                     # )
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

      htmlOutput(ns('welcome'))
    )
  )
}


