


FiltersServer <- function(id, Object, SNkeep, MPkeep, Detkeep, Stochkeep, Projkeep,
                          Det, Stoch, Proj, i18n) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- NS(id)
                 # States of Nature Filters
                 # (may need wrapping for robustness https://stackoverflow.com/questions/24205676/r-shiny-wrapping-ui-elements)
                 output$SN_filters <- renderUI({
                   lapply(1:Object$nFac, function(i) {
                     checkboxGroupInput(session$ns(paste0("Fil_SN",i)),inline=TRUE,
                                        Object$obj$OM$Factor_Labels[i],
                                        choiceNames=Object$obj$OM$Labels[[i]],
                                        selected=1:length(Object$obj$OM$Labels[[i]]),
                                        choiceValues=1:length(Object$obj$OM$Labels[[i]]))
                   })
                 })

                 # MP filtering
                 output$MP_filters <- renderUI({
                   checkboxGroupInput(session$ns("Fil_MP"),label=NULL,inline=T,choices=Object$obj$MP$Labels,
                                      selected=Object$obj$MP$Labels)
                 })

                 # Deterministic Performance metric filtering
                 output$PM_Det_filters <- renderUI({

                   checkboxGroupInput(session$ns("Fil_PM_Det"),
                                      label="Deterministic",
                                      inline=T,choices=Object$obj$Perf$Det$Codes,
                                      selected=Object$obj$Perf$Det$Codes)
                 })

                 # Stochastic Performance metric filtering
                 output$PM_Stoch_filters <- renderUI({
                   checkboxGroupInput(session$ns("Fil_PM_Stoch"),label="Stochastic",inline=T,
                                      choices=Object$obj$Perf$Stoch$Codes,
                                      selected=Object$obj$Perf$Stoch$Codes)
                 })

                 # Projection performance metric filtering
                 output$PM_Proj_filters <- renderUI({
                   checkboxGroupInput(session$ns("Fil_PM_Proj"),label="Projected",
                                      inline=T,
                                      choices=Object$obj$Perf$Proj$Codes,
                                      selected=Object$obj$Perf$Proj$Codes)
                 })


                 output$show_filters <- renderUI({
                   if (!Object$Loaded) {
                     # no object loaded
                     return(
                       tagList(br(),
                               box(status = 'warning', width=12,
                                   solidHeader =FALSE,
                                   title=h4(i18n()$t('Slick object not loaded')),
                                   p(i18n()$t('Please go to '), a(onclick='customHref("load");', style="cursor: pointer;", "Load"), i18n()$t('and load a Slick object.'))
                               )

                       )
                     )
                   }

                   if (Object$Loaded>=1) {
                     tagList(
                       conditionalPanel('input.NonTech=="home" || input.NonTech=="load"',
                                        column(12, align = 'left', class='multicol',
                                               h3(i18n()$t('Filters')),
                                               p(i18n()$t('The filters will appear here when one of the plots on the menu on the left are selected.'))
                                        )
                       ),
                       conditionalPanel('input.NonTech!="home" && input.NonTech!="load"',
                                        column(12, align = 'left', class='multicol',
                                               h3(i18n()$t('Filters')),
                                               p(i18n()$t('Use the checkboxes to select the Operating Models, Management Procedures, and (where applicable) Performance Metrics.')),
                                               p(i18n()$t('Then click the FILTER button to apply the filter.')),
                                               # h3(Object$obj$Misc$App_axes[2]),
                                               h3(i18n()$t('Operating Models (OM)')),
                                               a(i18n()$t('OM Details'), onclick='customHref("load"); customHref("Operating Model"); customHref("Design");',
                                                 style="cursor: pointer;"),

                                               uiOutput(session$ns('SN_filters')),
                                               hr(),
                                               # h3(Object$obj$Misc$App_axes[3]),
                                               h3(i18n()$t('Management Procedures (MP)')),
                                               a(i18n()$t('MP Details'), onclick='customHref("load"); customHref("Management Procedures")',
                                                 style="cursor: pointer;"),
                                               uiOutput(session$ns('MP_filters')),
                                               hr(),

                                               # Page Specific Filters
                                               conditionalPanel('input.NonTech=="det"',
                                                                class='multicol2',
                                                                # h3(Object$obj$Misc$App_axes[1]),
                                                                h3(i18n()$t('Performance Metric (PM)')),
                                                                a(i18n()$t('PM Details'), onclick='customHref("load"); customHref("Performance Metrics"); customHref("Deterministic")',
                                                                  style="cursor: pointer;"),
                                                                uiOutput(session$ns('PM_Det_filters')),
                                                                hr()
                                               ),
                                               conditionalPanel('input.NonTech=="stoch"',
                                                                class='multicol2',
                                                                h3(i18n()$t('Performance Metric (PM)')),
                                                                a(i18n()$t('PM Details'), onclick='customHref("load"); customHref("Performance Metrics"); customHref("Stochastic")',
                                                                  style="cursor: pointer;"),
                                                                uiOutput(session$ns('PM_Stoch_filters')),
                                                                hr()
                                               ),
                                               # conditionalPanel('input.NonTech=="proj"',
                                               #                  class='multicol2',
                                               #                  h3('Performance Metric'),
                                               #                  a('PM Glossary', onclick='customHref("load"); customHref("Performance Metrics"); customHref("Projection")',
                                               #                    style="cursor: pointer;"),
                                               #                  # uiOutput(session$ns('PM_Proj_filters')),
                                               #                  hr()
                                               # ),
                                               conditionalPanel("output.Filt",
                                                                actionBttn( session$ns("Filt"),"FILTER",icon("cogs"),block=T, style="fill",
                                                                            color='danger',size='sm'))

                                               # h5("log/debugging"),
                                               #verbatimTextOutput("Log",placeholder=T)
                                        )
                       )
                     )
                   }

               })

                 #observeEvent(input$Filt,{
                  # FilterOMs()
                 #})

                 observeEvent(Object$Loaded, {

                   observeEvent(input$Filt,{

                     FilterOMs(Object, input, SNkeep, MPkeep, Detkeep, Stochkeep, Projkeep, Det, Stoch, Proj)

                     Object$Filt<-FALSE
                   })

                 })

                 observe({
                   FilterNames<-c(paste0("Fil_SN",1:Object$nFac),"Fil_MP","Fil_PM_Det","Fil_PM_Stoch","Fil_PM_Proj")
                   observeEvent(sapply(FilterNames, function(x) input[[x]]),{
                     Object$Filt<-TRUE
                   })
                 })

               }
  )
}




FiltersUI <- function(id, label="filters", i18n) {

  ns <- NS(id)
  tagList(
    usei18n(i18n),
    fluidRow(
     column(12,
            htmlOutput(ns('show_filters'))
     )
    )
  )
}


FilterOMs<-function(Object, input,
                    SNkeep, MPkeep, Detkeep, Stochkeep, Projkeep,
                    Det, Stoch, Proj){


  # update checkboxes that lead to zero selected results
  keep <- array(T,dim(Object$obj$OM$Design))
  for(fac in 1:Object$nFac) keep[,fac] <- Object$obj$OM$Design[,fac]%in%input[[paste0("Fil_SN",fac)]]
  SNkeep$selected<-apply(keep,1,all)

  if(all(!SNkeep$selected)){
    for(i in 1:Object$nFac){
      updateCheckboxGroupInput(getDefaultReactiveDomain(), paste0("Fil_SN",i),selected= 1:length(Object$obj$OM$Labels[[i]]))
    }
  }


  # get vals

  if (Object$Loaded) {
    obj <- Object$obj

    keep <- array(T,dim(obj$OM$Design))
    for(fac in 1:Object$nFac) keep[,fac] <- obj$OM$Design[,fac]%in%input[[paste0("Fil_SN",fac)]]
    SNkeep$selected<-apply(keep,1,all)
    if(all(!SNkeep$selected))SNkeep$selected[]<-TRUE

    MPkeep$selected<-obj$MP$Labels%in%input$Fil_MP
    if(all(!MPkeep$selected)) updateCheckboxGroupInput(getDefaultReactiveDomain(),'Fil_MP',selected=Object$obj$MP$Labels)
    if(all(!MPkeep$selected))MPkeep$selected[]<-TRUE

    Detkeep$selected<-obj$Perf$Det$Codes%in%input$Fil_PM_Det
    if(all(!Detkeep$selected)) updateCheckboxGroupInput(getDefaultReactiveDomain(),'Fil_PM_Det',selected=Object$obj$Perf$Det$Codes)
    if(all(!Detkeep$selected))Detkeep$selected[]<-TRUE

    Stochkeep$selected<-obj$Perf$Stoch$Codes%in%input$Fil_PM_Stoch
    if(all(!Stochkeep$selected)) updateCheckboxGroupInput(getDefaultReactiveDomain(),'Fil_PM_Stoch',selected=Object$obj$Perf$Stoch$Codes)
    if(all(!Stochkeep$selected))Stochkeep$selected[]<-TRUE

    Projkeep$selected<-obj$Perf$Proj$Codes%in%input$Fil_PM_Proj
    if(all(!Projkeep$selected)) updateCheckboxGroupInput(getDefaultReactiveDomain(),'Fil_PM_Proj',selected=Object$obj$Perf$Proj$Codes)
    if(all(!Projkeep$selected))Projkeep$selected[]<-TRUE

    temparray<-array(T,dim(obj$Perf$Det$Values)); temparray[!SNkeep$selected,,]<-F; temparray[,!MPkeep$selected,]<-F; temparray[,,!Detkeep$selected]<-F
    Det$mat<-obj$Perf$Det$Values
    Det$mat[!temparray]<-NA # SN, MP, PM

    temparray<-array(T,dim(obj$Perf$Stoch$Values)); temparray[,!SNkeep$selected,,]<-F; temparray[,,!MPkeep$selected,]<-F; temparray[,,,!Stochkeep$selected]<-F
    Stoch$mat<-obj$Perf$Stoch$Values
    Stoch$mat[!temparray]<-NA # sim, SN, MP, PM

    temparray<-array(T,dim(obj$Perf$Proj$Values)); temparray[,!SNkeep$selected,,,]<-F; temparray[,,!MPkeep$selected,,]<-F; temparray[,,,!Projkeep$selected,]<-F
    Proj$mat<-obj$Perf$Proj$Values
    Proj$mat[!temparray]<-NA# sim, SN, MP, PM, Time

    Object$Filt<-FALSE

  }



}


