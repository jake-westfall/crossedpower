library(shiny)
library(minqa)


# design schematics -------------------------------------------------------

shinyServer(function(input, output) {

  # function to display design schematic
  getTable <- reactive({
    if(input$design=="Fully crossed"){
      tab <- matrix("AB", nrow=6, ncol=6)
      rownames(tab) <- paste0("Participant", 1:6)
      colnames(tab) <- paste0("Stim", 1:6)
    } else if(input$design=="Counterbalanced"){
      A <- matrix("A", nrow=3, ncol=3)
      B <- matrix("B", nrow=3, ncol=3)
      tab <- rbind(cbind(A, B), cbind(B, A))
      rownames(tab) <- paste0("Participant", 1:6)
      colnames(tab) <- paste0("Stim", 1:6)
    } else if(input$design=="Stimuli-within-Condition"){
      tab <- cbind(matrix("A", nrow=6, ncol=3), matrix("B", nrow=6, ncol=3))
      rownames(tab) <- paste0("Participant", 1:6)
      colnames(tab) <- paste0("Stim", 1:6)
    } else if(input$design=="Participants-within-Condition"){
      tab <- rbind(matrix("A", nrow=3, ncol=6), matrix("B", nrow=3, ncol=6))
      rownames(tab) <- paste0("Participant", 1:6)
      colnames(tab) <- paste0("Stim", 1:6)
    } else if(input$design=="Stimuli and Participants within Condition"){
      A <- matrix("A", nrow=3, ncol=3)
      B <- matrix("B", nrow=3, ncol=3)
      mat <- matrix("", nrow=3, ncol=3)
      tab <- rbind(cbind(A, mat), cbind(mat, B))
      rownames(tab) <- paste0("Participant", 1:6)
      colnames(tab) <- paste0("Stim", 1:6)
    }
    data.frame(tab)
  })
  
  # produce design schematic output
  output$designTab <- renderTable({
    tab <- getTable()
    tab
  })


# output model fitting code -----------------------------------------------

  output$R_code <- renderPrint({
    if(input$design=="Fully crossed"){
      cat(
"model <- lmer(y ~ condition + (condition|participant) + (condition|stim) + (1|participant:stim), data=myData)
summary(model)
restrictedModel <- update(model, . ~ . -condition)
KRmodcomp(model, restrictedModel)")
    } else if(input$design=="Counterbalanced"){
      cat(
"model <- lmer(y ~ condition + (condition|participant) + (condition|stim), data=myData)
summary(model)
restrictedModel <- update(model, . ~ . -condition)
KRmodcomp(model, restrictedModel)")
    } else if(input$design=="Stimuli-within-Condition"){
      cat(
"model <- lmer(y ~ condition + (condition|participant) + (1|stim), data=myData)
summary(model)
restrictedModel <- update(model, . ~ . -condition)
KRmodcomp(model, restrictedModel)")
    } else if(input$design=="Participants-within-Condition"){
      cat(
"model <- lmer(y ~ condition + (1|participant) + (condition|stim), data=myData)
summary(model)
restrictedModel <- update(model, . ~ . -condition)
KRmodcomp(model, restrictedModel)")
    } else if(input$design=="Stimuli and Participants within Condition"){
      cat(
"model <- lmer(y ~ condition + (1|participant) + (1|stim), data=myData)
summary(model)
restrictedModel <- update(model, . ~ . -condition)
KRmodcomp(model, restrictedModel)")
    }
  })
    
  output$SAS_code <- renderPrint({
    if(input$design=="Fully crossed"){
      cat(
"proc mixed covtest data=mydata;
class participant stim;
model y=condition/solution ddfm=kr;
random intercept condition/sub=participant type=un;
random intercept condition/sub=stim type=un;
random intercept/sub=participant*stim;
run;")
    } else if(input$design=="Counterbalanced"){
      cat(
"proc mixed covtest data=mydata;
class participant stim;
model y=condition/solution ddfm=kr;
random intercept condition/sub=participant type=un;
random intercept condition/sub=stim type=un;
run;")
    } else if(input$design=="Stimuli-within-Condition"){
      cat(
"proc mixed covtest data=mydata;
class participant stim;
model y=condition/solution ddfm=kr;
random intercept condition/sub=participant type=un;
random intercept/sub=stim;
run;")
    } else if(input$design=="Participants-within-Condition"){
      cat(
"proc mixed covtest data=mydata;
class participant stim;
model y=condition/solution ddfm=kr;
random intercept/sub=participant;
random intercept condition/sub=stim type=un;
run;")
    } else if(input$design=="Stimuli and Participants within Condition"){
      cat(
"proc mixed covtest data=mydata;
class participant stim;
model y=condition/solution ddfm=kr;
random intercept/sub=participant;
random intercept/sub=stim;
run;")
    }
  })
    
  output$SPSS_code <- renderPrint({
    if(input$design=="Fully crossed"){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept condition | subject(participant) covtype(un)
/random=intercept condition | subject(stim) covtype(un)
/random=intercept | subject(participant*stim).")
    } else if(input$design=="Counterbalanced"){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept condition | subject(participant) covtype(un)
/random=intercept condition | subject(stim) covtype(un).")
    } else if(input$design=="Stimuli-within-Condition"){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept condition | subject(participant) covtype(un)
/random=intercept | subject(stim).")
    } else if(input$design=="Participants-within-Condition"){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept | subject(participant)
/random=intercept condition | subject(stim) covtype(un).")
    } else if(input$design=="Stimuli and Participants within Condition"){
      cat(
"mixed y with condition
/fixed=condition
/print=solution testcov
/random=intercept | subject(participant)
/random=intercept | subject(stim).")
    }
  })

# write text labels for input fields --------------------------------------


  output$get_d <- renderUI({ 
    textInput("d", value=.5, label=ifelse(input$type=="Standardized",
              "Effect size d:", "Mean difference:"))
  })
  output$get_E <- renderUI({ 
    textInput("E", value=.3, label=ifelse(input$type=="Standardized",
              "Residual VPC:", "Residual variance:"))
  })
  output$get_S <- renderUI({ 
    textInput("S", value=.2, label=ifelse(input$type=="Standardized",
              "Stimulus intercept VPC:", "Stimulus intercept variance:"))
  })
  output$get_P <- renderUI({ 
    textInput("P", value=.2, label=ifelse(input$type=="Standardized",
              "Participant intercept VPC:", "Participant intercept variance:"))
  })
  output$get_PS <- renderUI({ 
    textInput("PS", value=.1, label=ifelse(input$type=="Standardized",
              "Participant-by-Stimulus VPC:", "Participant-by-Stimulus variance:"))
  })
  output$get_SC <- renderUI({ 
    textInput("SC", value=.1, label=ifelse(input$type=="Standardized",
              "Stimulus slope VPC:", "Stimulus slope variance:"))
  })
  output$get_PC <- renderUI({ 
    textInput("PC", value=.1, label=ifelse(input$type=="Standardized",
              "Participant slope VPC:", "Participant slope variance:"))
  })
  

# grab input, define power functions --------------------------------------

  output$ans <- renderPrint({
    if(input$solve > 0){
      
      # grab input values without updating form!
      design <- isolate(input$design)
      type <- isolate(input$type)
      d <- isolate(input$d)
      code <- isolate(input$code)
      E <- isolate(input$E)
      S <- isolate(input$S)
      P <- isolate(input$P)
      PS <- isolate(input$PS)
      SC <- isolate(input$SC)
      PC <- isolate(input$PC)
      p <- isolate(input$p)
      q <- isolate(input$q)
      power <- isolate(input$power)
      
      # define function to return power for chosen design
      if(design=="Fully crossed"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q){
          if(type=="Standardized") code <- 1
          t <- d/sqrt(2*(E/p/q + 2*code^2*SC/q + 2*code^2*PC/p))
          MS <- c(e = E, sc = E + p*SC*code^2, pc = E + q*PC*code^2)
          DF <- (MS["pc"] + MS["sc"] - MS["e"])^2/
            (MS["e"]^2/(p-1)/(q-1) + MS["sc"]^2/(q-1) + MS["pc"]^2/(p-1))
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + PS + S + P + code^2*SC + code^2*PC
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
              E=E/v, P=P/v, S=S/v, PC=code^2*PC/v, SC=code^2*SC/v, PS=PS/v)
          return(ans)
        }
      } else if(design=="Counterbalanced"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q){
          if(type=="Standardized") code <- 1
          E <- E + PS
          t <- d/2/sqrt(E/p/q + code^2*SC/q + code^2*PC/p)
          MS <- c(e = E, sc = E + p/2*SC*code^2, pc = E + q/2*PC*code^2)
          DF <- (MS["pc"] + MS["sc"] - MS["e"])^2/
            (MS["e"]^2/(p-2)/(q-2) + MS["sc"]^2/(q-2) + MS["pc"]^2/(p-2))
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + S + P + code^2*SC + code^2*PC
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
              E=E/v, P=P/v, S=S/v, PC=code^2*PC/v, SC=code^2*SC/v)
          return(ans)
        }
      } else if(design=="Stimuli-within-Condition"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q){
          if(type=="Standardized") code <- 1
          E <- E + PS
          S <- S + SC
          t <- d/2/sqrt(E/p/q + S/q + code^2*PC/p)
          MS <- c(e = E, s = E + p*S, pc = E + q/2*PC*code^2)
          DF <- (MS["pc"] + MS["s"] - MS["e"])^2/
            (MS["e"]^2/(p-1)/(q-2) + MS["s"]^2/(q-2) + MS["pc"]^2/(p-1))
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + S + P + code^2*PC
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
              E=E/v, P=P/v, S=S/v, PC=code^2*PC/v)
          return(ans)
        }
      } else if(design=="Participants-within-Condition"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q){
          if(type=="Standardized") code <- 1
          E <- E + PS
          P <- P + PC
          t <- d/2/sqrt(E/p/q + code^2*SC/q + P/p)
          MS <- c(e = E, sc = E + p/2*SC*code^2, p = E + q*P)
          DF <- (MS["p"] + MS["sc"] - MS["e"])^2/
            (MS["e"]^2/(p-2)/(q-1) + MS["sc"]^2/(q-1) + MS["p"]^2/(p-2))
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + S + P + code^2*SC
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
              E=E/v, P=P/v, S=S/v, SC=code^2*SC/v)
          return(ans)
        }
      } else if(design=="Stimuli and Participants within Condition"){
        powFunc <- function(d, code, E, S, P, PS, SC, PC, p, q){
          if(type=="Standardized") code <- 1
          E <- E + PS
          S <- S + SC
          P <- P + PC
          t <- d/2/sqrt(2*E/p/q + S/q + P/p)
          MS <- c(e = E, s = E + p/2*S, p = E + q/2*P)
          DF <- (MS["p"] + MS["s"] - MS["e"])^2/
            (MS["e"]^2/(p-2)/(q-2) + MS["s"]^2/(q-2) + MS["p"]^2/(p-2))
          ans <- pt(qt(.975, DF), DF, ncp=t, lower.tail=F) + 
            pt(qt(.025, DF), DF, ncp=t)
          v <- E + S + P
          attr(ans, "extra") <- c(ncp=unname(t), df=unname(DF), d=d/sqrt(v),
              E=E/v, P=P/v, S=S/v)
          return(ans)
        }
      } # end if-else chain

# perform error checking on user input ------------------------------------
      
      # verify that only 1 variable is selected to solve for
      vals <- list(d=d,code=code,E=E,S=S,P=P,PS=PS,SC=SC,PC=PC,p=p,q=q,power=power)
      choice <- unlist(lapply(toupper(vals), "==", "X"))
      if(sum(choice) < 1) stop("No variable selected to solve for.")
      if(sum(choice) > 1) stop("Multiple variables selected to solve for.")
      
      # convert all the other variables to numeric
      choice <- names(vals)[choice]
      vals <- vals[-which(names(vals)==choice)]
      if(any(is.na(lapply(vals, function(x) as.numeric(x))))){
        stop("Non-numeric input in at least one of the provided parameters.")
      } 
      vals <- lapply(vals, as.numeric)
      
      # check that power is in [.05, 1)
      if("power" %in% names(vals)){
        if(vals$power < .05 | vals$power >= 1){
          stop("Power must be greater than or equal to .05 and less than 1.")
        }
      }
      
      # check that variance components are non-negative
      VPCs <- intersect(names(vals), c("E", "S", "P", "PS", "SC", "PC"))
      if(any(unlist(lapply(vals[VPCs], function(x) x < 0)))){
        stop("At least one variance component is negative.")
      }
      
      # check that no VPCs exceed 1
      if(type=="Standardized"){
        if(any(unlist(lapply(vals[VPCs], function(x) x > 1)))){
          stop("At least one VPC exceeds 1.")
        }
      }
      
      # check that all VPCs sum to 1 (approximately)
      if(type=="Standardized" & !(choice %in% c("E", "S", "P", "PS", "SC", "PC"))){
        total <- sum(unlist(vals[VPCs]))
        if(total < .995 | total > 1.005){
          stop(paste("With standardized input, all of the VPCs must sum to 1.
VPCs currently sum to", total))
        }
      }

# solve for selected parameter --------------------------------------------

      # solve
      if(choice=="d"){
        # define squared-error cost function
        cost <- function(test_d){
          (powFunc(test_d, vals$code, vals$E, vals$S, vals$P, vals$PS,
                   vals$SC, vals$PC, vals$p, vals$q) - vals$power)^2
        }
        # find d by minimizing cost function
        result <- bobyqa(.02, cost, lower=0, upper=Inf)
        # get answer at parameter estimate
        ans <- powFunc(result$par,vals$code,vals$E,vals$S,vals$P,vals$PS,
                       vals$SC,vals$PC,vals$p,vals$q)
        # display additional info
        info <- attr(ans, "extra")
        class(info) <- "info"
        output$info <- renderPrint({info})
        # display technical output
        class(result) <- "debug"
        output$debug <- renderPrint({result})
        # return solution
        if(type=="Standardized") {
          return(round(c("Minimum effect size d:" = result$par),3))
        } else return(round(c("Minimum mean difference:" = result$par),3))
      } else if(choice=="code"){ 
        stop("Cannot solve for contrast codes.")
      } else if(choice=="E"){
        stop("Cannot solve for variance components.")
      } else if(choice=="S"){
        stop("Cannot solve for variance components.")
      } else if(choice=="P"){
        stop("Cannot solve for variance components.")
      } else if(choice=="PS"){
        stop("Cannot solve for variance components.")
      } else if(choice=="SC"){
        stop("Cannot solve for variance components.")
      } else if(choice=="PC"){
        stop("Cannot solve for variance components.")
      } else if(choice=="p"){
        # define squared-error cost function
        cost <- function(test_p){
          (powFunc(vals$d, vals$code, vals$E, vals$S, vals$P, vals$PS,
                   vals$SC, vals$PC, test_p, vals$q) - vals$power)^2
        }
        # find p by minimizing cost function
        result <- bobyqa(4, cost, lower=1, upper=Inf)
        # display technical output
        class(result) <- "debug"
        output$debug <- renderPrint({result})
        # check for bad estimates
        if(result$ierr > 0) {
          output$info <- renderText({NULL})
          return(noquote(c(" "=
            "Power level not attainable even with infinite participants.")))
        }
        else {
          # get answer at parameter estimate
          ans <- powFunc(vals$d,vals$code,vals$E,vals$S,vals$P,vals$PS,
                         vals$SC,vals$PC,result$par,vals$q)
          # display additional info
          info <- attr(ans, "extra")
          class(info) <- "info"
          output$info <- renderPrint({info})
          # return solution
          return(round(c("Minimum number of participants:"=result$par),1))
        }
      } else if(choice=="q"){
        # define squared-error cost function
        cost <- function(test_q){
          (powFunc(vals$d, vals$code, vals$E, vals$S, vals$P, vals$PS,
                   vals$SC, vals$PC, vals$p, test_q) - vals$power)^2
        }
        # find q by minimizing cost function
        result <- bobyqa(4, cost, lower=1, upper=Inf)
        # display technical output
        class(result) <- "debug"
        output$debug <- renderPrint({result})
        # check for bad estimates
        if(result$ierr > 0) {
          output$info <- renderText({NULL})
          return(noquote(c(" "=
            "Power level not attainable even with infinite participants.")))
        }
        else {
          # get answer at parameter estimate
          ans <- powFunc(vals$d,vals$code,vals$E,vals$S,vals$P,vals$PS,
                         vals$SC,vals$PC,vals$p,result$par)
          # display additional info
          info <- attr(ans, "extra")
          class(info) <- "info"
          output$info <- renderPrint({info})
          # return solution
          return(round(c("Minimum number of stimuli:"=result$par),1))
        }
      } else if(choice=="power"){
        ans <- powFunc(vals$d,vals$code,vals$E,vals$S,vals$P,vals$PS,
                       vals$SC,vals$PC,vals$p,vals$q)
        # display additional info
        info <- attr(ans, "extra")
        class(info) <- "info"
        output$info <- renderPrint({info})
        # display technical output
        output$debug <- renderText({NULL})
        # return solution
        return(round(c("Power:"= ans),3))
      }
    } # end if(input$solve>0)
  }) # end assignment to output$ans and renderPrint()
}) # end call to shinyServer()
