library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Power Analysis with Crossed Random Effects"),
  p("Westfall, J., Kenny, D. A., & Judd, C. M. (2014). Statistical power and optimal design in experiments in which samples of participants respond to samples of stimuli. ",em("Journal of Experimental Psychology: General, 143"),"(5), 2020-2045."),
  fluidRow(
    column(1, div(a("Article", href="http://jakewestfall.org/publications/crossed_power_JEPG.pdf"))),
    column(3, div(a("Link to SAS simulation code (open as text file)", href="http://jakewestfall.org/resources/WKJ_crossed_designs_simulations.sas"))),
    column(3, div(a("Link to SAS simulation output (open as text file)", href="http://jakewestfall.org/resources/WKJ_crossed_designs_simulations.lst"))),
    column(3, div(a("Code for this app (using package 'shiny' in R)", href="https://github.com/jake-westfall/crossedpower"))),
    column(2, div(a("Back to JakeWestfall.org", href="http://jakewestfall.org")))
  ),
  helpText("Note: when sharing the link to this app, please use the stable redirecting page at",
           a("jakewestfall.org/power/,", href="http://jakewestfall.org/power/"),
           "as the app's current URL is possibly subject to change."),

  sidebarPanel(
    selectInput("design", "Choose a design:", 
                choices = c("Fully crossed", "Counterbalanced",
                            "Stimuli-within-Condition",
                            "Participants-within-Condition",
                            "Stimuli and Participants within Condition")),
    selectInput("type", "Standardized or Unstandardized input:", 
                choices = c("Standardized", "Unstandardized")),
    conditionalPanel(condition = "input.type == 'Standardized'",
                     helpText("Note: with Standardized input, all of the Variance Partitioning Coefficients (VPCs) must sum to 1.")),
    
    p("-------------------------------------------"),
    
    p("Enter the design parameters below."),
    p("To compute power estimates, enter an X for the variable you wish to solve for, then click the 'Solve for X' button."),
    actionButton("solve", "Solve for X"),
    p(),
    
    htmlOutput("get_d"),
    conditionalPanel(condition = "input.type == 'Unstandardized'",
                     textInput("code", "Contrast codes: (plus or minus)", 1)),
    htmlOutput("get_E"),
    htmlOutput("get_P"),
    htmlOutput("get_S"),
    htmlOutput("get_PS"),
    htmlOutput("get_PC"),
    htmlOutput("get_SC"),
    textInput("p", value=20, label="Total number of Participants:"),
    textInput("q", value=20, label="Total number of Stimuli:"),
    textInput("power", "Power:", "X")
  ),
  
  mainPanel(
    h4("Design schematic"),
    helpText("(The interpretation of this design schematic is explained in the accompanying paper.)"),
#     p("If a cell in the table below is empty, it means that this Participant never responds to this Stimulus. If a cell contains an 'A', it means that this Participant responds to this Stimulus only under Condition A. If a cell contains an 'B', it means that this Participant responds to this Stimulus only under Condition B. If a cell contains an 'AB', it means that this Participant responds to this Stimulus under both Conditions."),
    tableOutput("designTab"),
#     conditionalPanel(condition = "input.design == 'Fully crossed' || input.design == 'Counterbalanced'",
#                      p("In this design, all of the variance components listed can be estimated from the data, however the participant and stimulus intercept variances do not actually contribute anything to the standard error of the Condition difference (see paper for details). In this app we ask the user to specify the values of the participant and stimulus intercept variances/VPCs simply to encourage the user to think thoroughly about all possible sources of random variation in the study.")),
#     conditionalPanel(condition = "input.design == 'Stimuli-within-Condition'",
#                      p("In this design, stimulus slope variance is not estimable, and is implicitly added to the estimable stimulus intercept variance component. Consequently, the power analysis for this design adds the specified stimulus slope variance/VPC to the specified stimulus intercept variance/VPC and uses this combined estimate in computing power. See paper for more details.")),
#     conditionalPanel(condition = "input.design == 'Participants-within-Condition'",
#                      p("In this design, participant slope variance is not estimable, and is implicitly added to the estimable participant intercept variance component. Consequently, the power analysis for this design adds the specified participant stimulus slope variance/VPC to the specified participant intercept variance/VPC and uses this combined estimate in computing power. See paper for more details.")),
#     conditionalPanel(condition = "input.design == 'Stimuli and Participants within Condition'",
#                      p("In this design, neither the stimulus slope variance nor participant slope variance are estimable, and are implicitly added to the estimable stimulus intercept variance component and participant intercept variance component, respectively. Consequently, the power analysis for this design adds the specified stimulus slope variance/VPC to the specified stimulus intercept variance/VPC, and the specified participant stimulus slope variance/VPC to the specified participant intercept variance/VPC, and uses these combined estimates in computing power. See paper for more details.")),
    h4("Solution from power analysis"),
    verbatimTextOutput("ans"),
    h4("Additional power analysis information"),
    verbatimTextOutput("info"),
    h4("Technical output (for troubleshooting)"),
    verbatimTextOutput("debug"),
    h4("R/SAS/SPSS code for estimating the mixed model"),
    helpText("(Note: 'condition' is assumed to be entered as a numeric variable, manually contrast coded, and NOT as a factor (in R), class (in SAS), or string (in SPSS) variable. See paper for details.)"),
    p("R code:"),
    helpText("(Note: the 'lme4' and 'pbkrtest' packages must be installed and loaded.)"),
    verbatimTextOutput("R_code"),
    p("SAS code:"),
    verbatimTextOutput("SAS_code"),
    p("SPSS code:"),
    verbatimTextOutput("SPSS_code")
  )
))