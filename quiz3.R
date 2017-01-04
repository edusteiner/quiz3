setwd("/Users/eduardo.steiner/Documents/CursoDataScience/quiz3")

best <- function(state,outcome) {
    setwd("/Users/eduardo.steiner/Documents/CursoDataScience/quiz3")
    possible_outcomes <- c("heart attack", "heart failure", "pneumonia") 
    if (! outcome %in% possible_outcomes) {
        print("outcome invalido")
    } else {
        us_states_df <- read.csv("state_table.csv")
        us_states <- as.vector(us_states_df[,3])
        if (! state %in% us_states) {
          print("estado invalido")
        } else {
              ## Outcome recebido é valido
              ## Estado recebido é valido
              print("calculating ...")
              outcome_df<-read.csv("outcome-of-care-measures.csv")[,c(1,2,6,7,11,17,23)]
              # 1=Provider Number
              # 2=Hospital Name
              # 6=City
              # 7=State
              # 11=Hospital 30-Day Death (Mortality) Rates from Heart Attack
              # 17=Hospital 30-Day Death (Mortality) Rates from Heart Failure
              # 23=Hospital 30-Day Death (Mortality) Rates from Pneumonia
              if (outcome == "heart attack") {
                filtro <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
              } else if (outcome == "heart failure") {
                filtro <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
              } else {
                filtro <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
              }
              cat(paste("filtro = ", filtro, "\n"))
              
              outcome_por_estado <- subset(outcome_df, State==state)
              
              outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(as.character(outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
              outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(as.character(outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
              outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(as.character(outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
              
              outcome_por_estado <- outcome_por_estado[with(outcome_por_estado, order(outcome_por_estado[filtro],outcome_por_estado$Hospital.Name)),]
            
              cat(paste("ESTADO =", state, "   OUTCOME =", outcome, "   RESPOSTA =", outcome_por_estado[1,2]  ,"\n"))
          
        }
        
      }
}









rankhospital <- function(state,outcome,num = "best") {
  possible_outcomes <- c("heart attack", "heart failure", "pneumonia") 
  if (! outcome %in% possible_outcomes) {
    print("outcome invalido")
  } else {
    us_states_df <- read.csv("state_table.csv")
    us_states <- as.vector(us_states_df[,3])
    if (! state %in% us_states) {
      print("estado invalido")
    } else {
      ## Outcome recebido é valido
      ## Estado recebido é valido
      print("calculating ...")
      outcome_df<-read.csv("outcome-of-care-measures.csv")[,c(1,2,6,7,11,17,23)]
      # 1=Provider Number
      # 2=Hospital Name
      # 6=City
      # 7=State
      # 11=Hospital 30-Day Death (Mortality) Rates from Heart Attack
      # 17=Hospital 30-Day Death (Mortality) Rates from Heart Failure
      # 23=Hospital 30-Day Death (Mortality) Rates from Pneumonia
      if (outcome == "heart attack") {
        filtro <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
      } else if (outcome == "heart failure") {
        filtro <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
      } else {
        filtro <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
      }
      cat(paste("filtro = ", filtro, "\n"))
      
      outcome_por_estado <- subset(outcome_df, State==state)
      
      outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(as.character(outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
      outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(as.character(outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(as.character(outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
      
      if (num == "worst") {
          outcome_por_estado <- outcome_por_estado[with(outcome_por_estado, order(-outcome_por_estado[filtro],outcome_por_estado$Hospital.Name)),]
      } else {
        cat(paste("ordenação=",num,"\n"))
        outcome_por_estado <- outcome_por_estado[with(outcome_por_estado, order(outcome_por_estado[filtro],outcome_por_estado$Hospital.Name)),]
      }
      
      # remove os NA's
      outcome_por_estado <- outcome_por_estado[!is.na(outcome_por_estado[filtro]),]
      head(outcome_por_estado)
      
      if (num %in% c("best","worst")) {
          num <- 1
      }
      
      if(num>nrow(outcome_por_estado)) {
          cat(paste("parametro NUM muito grande","\n"))   
      } else {
        cat(paste("ESTADO =", state, "   OUTCOME =", outcome, "   RESPOSTA =", outcome_por_estado[num,2]  ,"\n")) 
          }
      
       
      
      #cat(paste("ESTADO =", state, "   OUTCOME =", outcome, "   RESPOSTA =", outcome_por_estado[num,2]  ,"\n"))
      
      
      
      
      
    }
    
  }
}







rankall <- function(outcome, num = "best") {
  setwd("/Users/eduardo.steiner/Documents/CursoDataScience/quiz3")
  possible_outcomes <- c("heart attack", "heart failure", "pneumonia") 
  if (! outcome %in% possible_outcomes) {
    print("outcome invalido")
  } 
      ## Outcome recebido é valido
      ## Estado recebido é valido
      print("calculating ...")
      outcome_df<-read.csv("outcome-of-care-measures.csv")[,c(1,2,6,7,11,17,23)]
      # 1=Provider Number
      # 2=Hospital Name
      # 6=City
      # 7=State
      # 11=Hospital 30-Day Death (Mortality) Rates from Heart Attack
      # 17=Hospital 30-Day Death (Mortality) Rates from Heart Failure
      # 23=Hospital 30-Day Death (Mortality) Rates from Pneumonia
      if (outcome == "heart attack") {
        filtro <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
      } else if (outcome == "heart failure") {
        filtro <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
      } else {
        filtro <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
      }
      cat(paste("filtro = ", filtro, "\n"))
      
      outcome_por_estado <- outcome_df
      outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(as.character(outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack))
      outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(as.character(outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure))
      outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(as.character(outcome_por_estado$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia))
      
      
      if (num == "worst") {
        outcome_por_estado <- outcome_por_estado[with(outcome_por_estado, order(-outcome_por_estado[filtro],outcome_por_estado$Hospital.Name)),]
      } else {
        cat(paste("ordenação=",num,"\n"))
        outcome_por_estado <- outcome_por_estado[with(outcome_por_estado, order(outcome_por_estado[filtro],outcome_por_estado$Hospital.Name)),]
      }
      
      us_states_df <- read.csv("state_table.csv")
      us_states <- as.vector(us_states_df[,3])
      
      if (num %in% c("best","worst")) {
        num <- 1
      }
      
      for (estado in us_states) {
        outcome_deste_estado <- subset(outcome_por_estado, State==estado)
        cat(paste(estado, filtro, outcome_deste_estado[num,2],"\n"))
      }
      
      
      #outcome_por_estado <- outcome_por_estado[with(outcome_por_estado, order(outcome_por_estado[filtro],outcome_por_estado$Hospital.Name)),]
      
      
      cat(paste("ESTADO =", state, "   OUTCOME =", outcome, "   RESPOSTA =", outcome_por_estado[1,2]  ,"\n"))
      
    
    
  }



















