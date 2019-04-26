rm(list=ls())
source("./Codes/My-Functions/my.library.loader.R")

sampled.path <- "../Emotion Dynamics 1/ED 1 - Codes/3. mirt-Model-Comp/Processed-457/1persons.0days.0beeps/"
sampled.path <- "../Emotion Dynamics 1/ED 1 - Codes/3. mirt-Model-Comp/Processed-457/0persons.0days.1beeps/"

sampled.names <- list.files(path = sampled.path, pattern = "*.RData")

list.of.names <- list('CLINICAL ESM','Cogito','ELISE ESM14', 'JULIAN EGON',
                      'KATHLEEN STRESSKLINIEK', 'Laura ESM 2014','Laura ESM 2016',
                      'LONGITUDINAL W1', 'LONGITUDINAL W2', 'LONGITUDINAL W3','MARLIES BPD',
                      'MDD BPD TRULL', 'MDD GOTLIB', 'MTURK DAILY DIARY', 
                      'PETEMADDY')

def.colnames <- list(def.A = c("Communal", "h2"),
                     def.B.bare = c("Pos", "Neg", "h2"),
                     def.B.cov = c("Pos", "Neg", "h2"),
                     def.C.bare = c("Communal", "Pos", "Neg", "h2"),
                     def.C.cov = c("Communal", "Pos", "Neg", "h2"),
                     def.D.bare = c("F1", "F2", "h2"),
                     def.D.cov = c("F1", "F2", "h2"),
                     def.E.bare = c("F1", "F2", "F3", "h2"),
                     def.E.cov = c("F1", "F2", "F3", "h2")
                     )

loadings <- def.colnames %>%
              llply(function(x){
                      df <- matrix(nrow = 0, ncol = (4+length(x))) %>% data.frame()
                      colnames(df) <- c("name.dataset", "name.model", "item", "seed", x)
                      })

extract.append <- function(prev.loadings, model, name.model, name.dataset){
  
  ls.and.h2 <- model@Fit$F %>% cbind(model@Fit$h2)
  colnames(ls.and.h2)[ncol(ls.and.h2)] <- "h2"
  
  name.model <- name.model %>% rep(nrow(ls.and.h2))
  item <- rownames(ls.and.h2)
  seed <- seed %>% rep(nrow(ls.and.h2))
  
  name.dataset <- name.dataset %>% rep(nrow(ls.and.h2))
  
  single.case <- cbind(name.dataset, name.model, item, seed, ls.and.h2)
  rownames(single.case) <- c()
  
  rownames(prev.loadings) <- c()
  out <- rbind(prev.loadings, single.case)
  rownames(out) <- c()
  out %>% return()
}


# sampled <- sampled.names[10]

t <- Sys.time()
j <- 1

def.C.cov <- matrix(nrow = 0, ncol = (4+4)) %>% data.frame()
for(sampled in sampled.names){
  load(paste(sampled.path, sampled, sep = ""))
  name.dataset <- to.be.saved$name
  list.of.models <- to.be.saved$model
  seed <- to.be.saved$parameters$seed
  print(j)
  j <- j + 1
  
  # for(i in 1:2){ #length(list.of.models)

    def.C.cov <- extract.append(prev.loadings = def.C.cov, model = list.of.models[[5]],
                   name.model = names(list.of.models)[5],
                   name.dataset)
  }
Sys.time() - t


