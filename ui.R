if(!require(shinydashboard)) { install.packages("shinydashboard"); require(shinydashboard)}
if(!require(tidyverse)) { install.packages("tidyverse"); require(tidyverse)}
if(!require(knitr)) { install.packages("knitr"); require(knitr)}
if(!require(magrittr)) { install.packages("magrittr"); require(magrittr)}
if(!require(DT)) { install.packages("DT"); require(DT)}
if(!require(DiagrammeR)) { install.packages("DiagrammeR"); require(DiagrammeR)}

dashboardPage(skin="black",
  dashboardHeader(title="Sensitivity Analyses"),
  dashboardSidebar(
      sidebarMenu(
        menuItem("Instructions", tabName="instructions"),
        menuItem("Selection bias", tabName="selectionbias"),
        menuItem("Confounding", tabName="confounding")
      )
  ),
  dashboardBody(
    tabItems(
      tabItem("instructions",
              fluidRow(
                box(width = 12, status = "info", solidHeader = TRUE,
                    title ="Instructions",
                  box(width = 6, 
                    box(width = 12, status = "info",
                      title = "Instructions",
                      p("1. Enter the inputs below based on your study data"),
                      p("2. Choose the Selection Bias tab or the Confounding tab"),
                      p("3. Specify or guess the additional inputs required for your chosen calculator"),
                      p("4a. Compare unadjusted RR with weighted RR estimate to assess degree of selection bias."),
                      p("4b. The bias-adjusted RR is the estimated RR corrected for the chosen degree of unmeasured confounding"),
                      p("4c. The e-value estimates the minimum risk ratio between an unmeasured confounder and both the exposure and outcome in order to fully explain the observed risk ratio")
                    ),
                    box(width = 12, status = "info",
                        title ="References",
                        p("This app is an online supplement to:"),
                        p("1. Caniglia EC, et al. 2018. Methodological challenges when studying distance to care as an exposure in health research. Am J Epi, under review."),
                        p("E-value & bias-adjusted risk ratio:"),
                        p("2. VanderWeele TJ, Ding P. Sensitivity Analysis in Observational Research: Introducing the E-Value. Annals of internal medicine 2017; 167(4):268-274.")
                    )
                  ),
                  box(width = 6, status = "info",
                      title = "Inputs for all calculators",
                      tags$h4("Specify the inputs below from your study data"),
                      numericInput(paste0("sampSize"), "Total # of people recruited", min = 1, max = 100000, value = 51558),
                      numericInput(paste0("pA0S1"), "Pr[A=0|S=1]", min = 0, max = 1, value = 0.916599),
                      numericInput(paste0("pLA0S1"), "Pr[U=1|A=0,S=1]", min = 0, max = 1, value =0.069),
                      numericInput(paste0("pLA1S1"), "Pr[U=1|A=1,S=1]", min = 0, max = 1, value =0.1026),
                      numericInput(paste0("pYA0L0S1"), "Pr[Y=1|A=0,U=0,S=1]", min = 0, max = 1, value =0.0196),
                      numericInput(paste0("pYA0L1S1"), "Pr[Y=1|A=0,U=1,S=1]", min = 0, max = 1, value =0.057),
                      numericInput(paste0("pYA1L0S1"), "Pr[Y=1|A=1,U=0,S=1]", min = 0, max = 1, value =0.0233),
                      numericInput(paste0("pYA1L1S1"), "Pr[Y=1|A=1,U=1,S=1]", min = 0, max = 1, value =0.0658)
                  )
                )
              )
          ),
    tabItem("selectionbias",
            fluidRow(
              box(width = 12, status = "info", solidHeader = TRUE,
                  title ="Correcting for selection bias",
                box(            
                  width=6, status = "info",
                  title ="Choose the inputs below based on best guess or external data",
                  numericInput(paste0("pS1A0L0"), "Pr[S=1|A=0,U=0]", min = 0, max = 1, value =0.55),
                  numericInput(paste0("pS1A0L1"), "Pr[S=1|A=0,U=1]", min = 0, max = 1, value =0.65),
                  numericInput(paste0("pS1A1L0"), "Pr[S=1|A=1,U=0]", min = 0, max = 1, value =0.15),
                  numericInput(paste0("pS1A1L1"), "Pr[S=1|A=1,U=1]", min = 0, max = 1, value =0.60)
                ),
                box(
                  width=6, status = "info",
                  title="Estimated Risk Ratios",
                  tableOutput("table1") 
                ),
                box(
                  width = 12, status = "info", 
                  title ="Directed acyclic  graph",
                  grVizOutput("trueDAG2")
                )
              )
            )             
       ),
    tabItem("confounding",
            fluidRow(
              box(width = 12, status = "info", solidHeader = TRUE,
                  title ="Sensitivity analysis for unmeasured confounding",
                box(width = 6, 
                  box(width=6,status ="info", 
                    title = "Specify the inputs below from your study data",
                      numericInput(paste0("rrAY"), "Risk ratio for A->Y", min = 0, max = 100, value = 1.25),
                      numericInput(paste0("lbAY"), "95% lower bound for A->Y", min = 0, max = 100, value = 1.03),
                      numericInput(paste0("ubAY"), "95% upper bound for A->Y", min = 0, max = 100, value = 1.52)
                  ),
                  box(width = 6, status = "info",
                      title = "Choose based on best guess or external data",
                      numericInput(paste0("rrUD"), "Risk ratio for U->Y", min = 1, max = 100, value = 2),
                      numericInput(paste0("rrEU"), "Risk ratio for U->A", min = 0, max = 100, value = 2)
                  )
                ),
              box(
                width=6, status = "info",
                title="Estimated Bias-adjusted Risk Ratio and E-value",
                tableOutput("table2")  
              ),
              box(
                  width = 12, status = "info", 
                  title ="Directed acyclic  graph",
                  grVizOutput("trueDAG3")
                )
            )
          )
      )
    )
  )
)

    