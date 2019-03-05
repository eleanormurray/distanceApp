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
                  box(width = 12, 
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
                        p("This app is an online supplement to Caniglia EC, et al. Methodological challenges when studying distance to care as an exposure in health research. American Journal of Epidemiology, 2019 [in press]. An archived version of the app code is available at zenodo.org or to download the most recent version, please visit https://github.com/eleanormurray/distanceApp "),
                        p(" "),
                        p("For more information about e-values & the bias-adjusted risk ratio, see VanderWeele TJ, Ding P. Sensitivity Analysis in Observational Research: Introducing the E-Value. Annals of Internal Medicine, 2017; 167(4):268-274.")
                    )
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
                  box( width = 6, status = "info",
                       title = "Specify the inputs below from your study data",
                       numericInput(paste0("sampSize"), "Total # of people recruited", min = 1, max = 100000, value = 51558),
                       numericInput(paste0("pA0S1"), "Pr[E=0|S=1]", min = 0, max = 1, value = 0.916599),
                       numericInput(paste0("pLA0S1"), "Pr[U=1|E=0,S=1]", min = 0, max = 1, value =0.069),
                       numericInput(paste0("pLA1S1"), "Pr[U=1|E=1,S=1]", min = 0, max = 1, value =0.1026),
                       numericInput(paste0("pYA0L0S1"), "Pr[D=1|E=0,U=0,S=1]", min = 0, max = 1, value =0.0196),
                       numericInput(paste0("pYA0L1S1"), "Pr[D=1|E=0,U=1,S=1]", min = 0, max = 1, value =0.057),
                       numericInput(paste0("pYA1L0S1"), "Pr[D=1|E=1,U=0,S=1]", min = 0, max = 1, value =0.0233),
                       numericInput(paste0("pYA1L1S1"), "Pr[D=1|E=1,U=1,S=1]", min = 0, max = 1, value =0.0658)
                  ),
                  box( width = 6, status = "info",
                  title ="Choose based on best guess or external data",
                  numericInput(paste0("pS1A0L0"), "Pr[S=1|E=0,U=0]", min = 0, max = 1, value =0.55),
                  numericInput(paste0("pS1A0L1"), "Pr[S=1|E=0,U=1]", min = 0, max = 1, value =0.65),
                  numericInput(paste0("pS1A1L0"), "Pr[S=1|E=1,U=0]", min = 0, max = 1, value =0.15),
                  numericInput(paste0("pS1A1L1"), "Pr[S=1|E=1,U=1]", min = 0, max = 1, value =0.60)
                  )
                ),
                box(width=6, status = "info",
                  box(
                    width=12, status = "info",
                    title="Estimated Risk Ratios",
                    tableOutput("table1"),
                    p("Note, to obtain 95% confidence intervals for the weighted RR use robust standard errors.")
                  ),
                  box(
                    width = 12, status = "info", 
                    title ="Directed acyclic  graph",
                    grVizOutput("trueDAG2")
                  )
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
                      numericInput(paste0("rrAY"), "Risk ratio: RRed", min = 0, max = 100, value = 1.25),
                      numericInput(paste0("lbAY"), "95% lower bound for RRed", min = 0, max = 100, value = 1.03),
                      numericInput(paste0("ubAY"), "95% upper bound for RRed", min = 0, max = 100, value = 1.52)
                  ),
                  box(width = 6, status = "info",
                      title = "Choose based on best guess or external data",
                      numericInput(paste0("rrUD"), "Risk ratio: RRud", min = 1, max = 100, value = 2),
                      numericInput(paste0("rrEU"), "Risk ratio: RReu", min = 0, max = 100, value = 2)
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

    
