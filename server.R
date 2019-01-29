##Sensitivity analyses for selection bias and unmeasured confounding when using distance to care as an exposure in epidemiologic research##
#Nov 7, 2018#

if(!require(shinydashboard)) { install.packages("shinydashboard"); require(shinydashboard)}
if(!require(tidyverse)) { install.packages("tidyverse"); require(tidyverse)}
if(!require(knitr)) { install.packages("knitr"); require(knitr)}
if(!require(magrittr)) { install.packages("magrittr"); require(magrittr)}
if(!require(DT)) { install.packages("DT"); require(DT)}
if(!require(DiagrammeR)) { install.packages("DiagrammeR"); require(DiagrammeR)}


######################################
##1. Calculate crude RR##########
#####exposure (A)#####################
#####outcome (Y)######################
######################################
set.seed(1234)
paramNames<-c( "sampSize", "pA0S1", "pLA0S1", "pLA1S1", "pYA0L0S1", "pYA0L1S1", 
                "pYA1L0S1", "pYA1L1S1", "pS1A0L0", "pS1A0L1", "pS1A1L0","pS1A1L1", "rrUD", "rrEU", "rrAY", "lbAY", "ubAY")

estimation<-function(sampSize=51588, pA0S1 =0.916599, pLA0S1 =0.069, pLA1S1 =0.1026, pYA0L0S1 =0.0196, pYA0L1S1 =0.057, 
                pYA1L0S1 =0.0233, pYA1L1S1 =0.0658, pS1A0L0 =0.55, pS1A0L1 =0.65, pS1A1L0 =0.15, pS1A1L1 =0.6, rrUD=2, rrEU=2, rrAY=2, lbAY=2, ubAY=2){
  
  #Inputs
  N.samp     = sampSize
  pA0.S1     = pA0S1
  pL1.A0S1   = pLA0S1
  pL1.A1S1   = pLA1S1
  pY1.A0L0S1 = pYA0L0S1
  pY1.A0L1S1 = pYA0L1S1
  pY1.A1L0S1 = pYA1L0S1
  pY1.A1L1S1 = pYA1L1S1
  pS1.A0L0   = pS1A0L0
  pS1.A0L1   = pS1A0L1
  pS1.A1L0   = pS1A1L0
  pS1.A1L1   = pS1A1L1
  rr.UD      = rrUD
  rr.EU      = rrEU
  rr.AY      = rrAY
  lb.AY      = lbAY
  ub.AY      = ubAY
  
  #Crude RR

  crudeRR <-((((N.samp*(1-pA0.S1)*(1-pL1.A1S1)*pY1.A1L0S1)
                +(N.samp*(1-pA0.S1)*pL1.A1S1*pY1.A1L1S1))/(N.samp*(1-pA0.S1)))/(((
                  N.samp*(pA0.S1)*pL1.A0S1*pY1.A0L1S1)+(N.samp*pA0.S1*(1-pL1.A0S1)*pY1.A0L0S1))/(N.samp*pA0.S1)))

  #Weighted RR
  
  w.A0L0<-1/pS1.A0L0
  w.A0L1<-1/pS1.A0L1
  w.A1L0<-1/pS1.A1L0
  w.A1L1<-1/pS1.A1L1
  
  n.A0L0<-((1-pL1.A0S1)*N.samp*pA0.S1)
  n.A0L1<-(pL1.A0S1*N.samp*pA0.S1)
  n.A1L0<-((1-pL1.A1S1)*N.samp*pA0.S1)
  n.A1L1<-(pL1.A1S1*N.samp*pA0.S1)
  
  pseudoN.A0L0 <-w.A0L0*n.A0L0
  pseudoN.A0L1 <-w.A0L1*n.A0L1
  pseudoN.A1L0 <-w.A1L0*n.A1L0
  pseudoN.A1L1 <-w.A1L1*n.A1L1
  
  nY1.pseudoA0L0<-pY1.A0L0S1*pseudoN.A0L0
  nY1.pseudoA0L1<-pY1.A0L1S1*pseudoN.A0L1
  nY1.pseudoA1L0<-pY1.A1L0S1*pseudoN.A1L0
  nY1.pseudoA1L1<-pY1.A1L1S1*pseudoN.A1L1
  
  weightedRR<-(((nY1.pseudoA1L0+nY1.pseudoA1L1)/(pseudoN.A1L0+pseudoN.A1L1))/
                 ((nY1.pseudoA0L0+nY1.pseudoA0L1)/(pseudoN.A0L0+pseudoN.A0L1)))
  
  #Selection bias results table
  #table1<-data.frame(c(round(crudeRR,3), round(weightedRR,3)), 
  #                  row.names=c('Crude RR','Weighted RR'))
  table1<-data.frame(
      Names = c("Unadjusted RR", "Weighted RR"),
      Value = c(round(crudeRR, 3), round(weightedRR,3))
  )
  
  colnames(table1)<-c("Estimate", "Value")
  
  #biasadjusted RR calculator
  bias<-(rr.UD*rr.EU)/(rr.UD+rr.EU-1)
  biasadjRR <-rr.AY/bias
  lb.biasadjRR <-lb.AY/bias
  ub.biasadjRR <-ub.AY/bias
  
  #evalue calculator
    evalue <-rr.AY + sqrt(rr.AY*(rr.AY-1))
  
  #bias-adjusted RR & evalue results table
  table2<-data.frame(Names=c('Bias-adjusted RR','95% CI lower bound', '95% CI upper bound','Bias factor', 'Evalue'),
                     Value =c(round(biasadjRR,3), round(lb.biasadjRR,3), round(ub.biasadjRR,3),round(bias,3), round(evalue,3)) 
                     )
  
  colnames(table2)<-c("Estimate", "Value")
  


  #Directed Acyclic Graph
  #"true" DAG
  #nodes
  nodes1 =create_node_df(4,type = "center", label = c('Distance (A)','Outcome (Y)', 'Selection (S)', 'Unmeasured Confounder (U)'), x = c(2.5, 6, 4, 3), y = c(-0.8, -1.5,-1.5,-2.5), 
                         color = c('none','none','black', 'none'),
                         fillcolor = c('none','none','none', 'none'),
                         shape = c('square','square','rectangle', 'square'),
                         width = c(0.6,0.6,0.7, 0.6),
                         fontsize = c(8,8,8,8))
  #create dag for front page
  trueDAG1<-
    create_graph(nodes_df=nodes1)
  #add edges
  trueDAG1<-
    add_edge(
      trueDAG1,
      from = 1,
      to = 2
    )%>%
    add_edge(
      from = 1,
      to = 3
    )%>%
    add_edge(
      from = 1,
      to = 4
    )%>%
    add_edge(
      from = 4,
      to = 2
    )%>%
    add_edge(
      from = 4,
      to = 3
    )%>%
    render_graph()
  
  
  nodes2 =create_node_df(4,type = "center", label = c('Distance (A)','Outcome (Y)', 'Selection (S)', 'Unmeasured Common Cause (U)'), x = c(2.5, 6, 4, 3), y = c( -0.8, -1.5,-1.5, -2.5), 
                         color = c('none','none','black','none'),
                         fillcolor = c('none','none','none','none'),
                         shape = c('square','square','rectangle','square'),
                         width = c(0.6,0.6,0.7,0.6),
                         fontsize = c(8,8,8,8))
  #create dag for selection bias
  trueDAG2<-
    create_graph(nodes_df=nodes2)
  #add edges
  trueDAG2<-
    add_edge(
      trueDAG2,
      from = 1,
      to = 3
    )%>%
    add_edge(
      from = 4,
      to = 3
    )%>%
    add_edge(
      from = 4,
      to = 2
    )%>%
    add_edge(
      from = 1,
      to = 2
    )%>%
    add_edge(
      from = 1,
      to = 4
    )%>%
    render_graph()
 
  nodes3 =create_node_df(3,type = "center", label = c('Distance (A)','Outcome (Y)','Unmeasured Confounder (U)'), x = c(4, 6, 3), y = c(0, 0, 1), 
                         color = c('none', 'none','none'),
                         fillcolor = c('none','none','none'),
                         shape = c('square','square','square'),
                         width = c(0.6,0.6,0.6),
                         fontsize = c(8,8,8)) 
  #create dag for confounding
  trueDAG3<-
    create_graph(nodes_df=nodes3)
  #add edges
  trueDAG3<-
    add_edge(
      trueDAG3,
      from = 1,
      to = 2
    )%>%
    add_edge(
      from = 3,
      to = 2
    )%>%

    add_edge(
      from = 3,
      to = 1
    )%>%
    render_graph()
  
  outlist <-list("table1" = table1, "table2"=table2, "trueDAG1" =trueDAG1, "trueDAG2" =trueDAG2, "trueDAG3" =trueDAG3)
  return(outlist)   
}



function(input, output, session) {
  
  getParams <- function() {
    params <- lapply(paramNames, function(p) {
      input[[p]]
    })
    names(params) <- paramNames
    params
  }
  
  results_tab1 <-reactive(do.call(estimation, getParams()))
  
  
  output$table1<-renderTable({
    (results_tab1()$table1)
  })    
  output$table2<-renderTable({
    (results_tab1()$table2)
  })
  output$trueDAG1<-renderGrViz({
    results_tab1()$trueDAG2
  })
  output$trueDAG2<-renderGrViz({
    results_tab1()$trueDAG2
  })
  output$trueDAG3<-renderGrViz({
    results_tab1()$trueDAG3
  })
}
