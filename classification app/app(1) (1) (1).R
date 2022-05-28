library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(fastDummies)
library(DT)
library(dplyr)

library(xgboost)
library(e1071)
library(MASS)
library(xtable)
library(ROCR)
library(caret)

library(shinyWidgets)

not_sel <- "Not Selected"

#Cleaning part
Page_1 <-tabPanel(title = "Data",
                  titlePanel("Cleaning"),
                  sidebarLayout(
                      
                      sidebarPanel(
                        fileInput("MonFichier", "Choissisez un data set", accept = c(".csv") ),
                          radioButtons("dman_Preview", "visualisation :",c("Preview", "Str",'summary'),inline = TRUE
                          ),
                          radioButtons("dman_Preview1", "nettoyage : ",c("Incorrect", "Missing Data", "Outliers"),inline = TRUE),
                          radioButtons("dman_Preview2", "data transformation",c("Dummification", "Norrmalisation"),
                                       selected = "Preview", inline = TRUE),
                        radioButtons( "dman_Preview3", "Exploration",c("Correlation", 'Contingency table', "Visualisation"), inline = TRUE),
                      
                          
                          ),
                      mainPanel(
                          tabsetPanel(
                              tabPanel(
                                  title = "visualisation",
                                  uiOutput("myoutput")
                              ),tabPanel(
                                  title = "nettoyage",
                                  uiOutput("myoutput1")
                              ),
                              tabPanel(
                                  title = "data transformation",
                                  uiOutput("myoutput2")),
                              
                              
                              
                             tabPanel(title = "Exploration",
                                     
                                      uiOutput("myoutputt4")
                                 #selectInput("numvar", "Numerical Variable", choices = c(not_sel)),
                                 #selectInput("factvar", "Factor Variable", choices = c(not_sel))
                                          )
                              ))))


#Analysing data Part
Page_3 <-tabPanel(
  "Training (ROC)",
  fluidRow(
    column(8,
           htmlOutput("text1")),
    
  ),
  fluidRow(
    column(3,
           selectInput("out","Label", choices = c(not_sel))),
    column(3,
           pickerInput("inn","Features", choices = c(not_sel), options = list(`actions-box` = TRUE),multiple = T)),
    column(2,  sliderInput(
      "Slider1",
      label = "Train/Test Split",
      min = 0,
      max =100,
      value = 75
    )),
    column(2,
           selectInput("deseq","Appliquer le reequilibrage des classes", choices = c("non","oui"))),
    column(2, actionButton("butt1", "save"))
    
    
    
  ),
  fluidRow(
    column(8,
           htmlOutput("desequilibre")),
    
  ),
  fluidRow( textOutput("cntTrain"),
            textOutput("cntTest")),
  fluidRow(
    column(
      width = 7,
      class = "well",
      h4("ROC Curve"),
      plotOutput("plot_roc"),
      style = "background-color:white;",
      sliderInput(
        "thresh_roc",
        label = "",
        min = 0,
        max = 1,
        value = c(0.5)
      )
    ),
    uiOutput("myoutput22")
  )
)
ui <-fluidPage(
  theme = shinytheme("cerulean"),
  navbarPage(
    title = 'Data Analyser',
    Page_1,
   # Page_2,
    Page_3
  )
)


#Predifined function to use in :
draw_plot_1 <- function(data_input, num_var_1, num_var_2, fact_var){
    if(fact_var!=not_sel){
        data_input[,(fact_var):= as.factor(data_input[,get(fact_var)])]
    }
    if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var != not_sel){
        ggplot(data = data_input,
               aes_string(x = num_var_1, y = num_var_2, color = fact_var)) +
            geom_point()
    }
    else if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var == not_sel){
        ggplot(data = data_input,
               aes_string(x = num_var_1, y = num_var_2)) +
            geom_point()
    }
    else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var != not_sel){
        ggplot(data = data_input,
               aes_string(x = fact_var, y = num_var_1)) +
            geom_violin()
    }
    else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var != not_sel){
        ggplot(data = data_input,
               aes_string(x = fact_var, y = num_var_2)) +
            geom_violin()
    }
    else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var == not_sel){
        ggplot(data = data_input,
               aes_string(x = num_var_1)) +
            geom_histogram()
    }
    else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var == not_sel){
        ggplot(data = data_input,
               aes_string(x = num_var_2)) +
            geom_histogram()
    }
    else if(num_var_1 == not_sel & num_var_2 == not_sel & fact_var != not_sel){
        ggplot(data = data_input,
               aes_string(x = fact_var)) +
            geom_bar()
    }
}

create_num_var_table <- function(data_input, num_var){
    if(num_var != not_sel){
        col <- data_input[,get(num_var)]
        if (length(col)>5000) col_norm <- sample(col,5000) else col_norm <- col
        norm_test <- shapiro.test(col_norm)
        statistic <- c("mean", "median", "5th percentile", "95th percentile",
                       "Shapiro statistic", "Shapiro p-value")
        value <- c(round(mean(col),2), round(median(col),2),
                   round(quantile(col, 0.05),2), round(quantile(col, 0.95),2),
                   norm_test$statistic, norm_test$p.value)
        data.table(statistic, value)
    }
}

create_fact_var_table <- function(data_input, fact_var){
    if(fact_var != not_sel){
        freq_tbl <- data_input[,.N, by = get(fact_var)]
        freq_tbl <- setnames(freq_tbl,c("factor_value", "count"))
        freq_tbl
    }
}

create_combined_table <- function(data_input, num_var_1, num_var_2, fact_var){
    if(fact_var != not_sel){
        if(num_var_1 != not_sel & num_var_2 != not_sel){
            res_tbl <- data_input[,.(correlation = cor(get(num_var_1), get(num_var_2))), by = fact_var]
        }
        else if(num_var_1 != not_sel & num_var_2 == not_sel){
            res_tbl <- data_input[,.(mean = mean(get(num_var_1))), by = fact_var]
        }
        else if(num_var_1 == not_sel & num_var_2 != not_sel){
            res_tbl <- data_input[,.(mean = mean(get(num_var_2))), by = fact_var]
        }
    }
    else if(num_var_1 != not_sel & num_var_2 != not_sel){
        res_tbl <- data.table(
            statistic = c("correlation"),
            value = c(cor(
                data_input[,get(num_var_1)],
                data_input[,get(num_var_2)])))
    }
    return(res_tbl)
}

min_max_norm <- function(x) {(x - min(x)) / (max(x) - min(x))}

# *************************************************************************************
# ************************************************************************************

server <- function(input, output,session){
    
    options(shiny.maxRequestSize=10*1024^2) 
   #1-Cleaning Part :  
    #data for cleaning
  data <- reactiveVal(NULL)
  observeEvent(input$MonFichier, {
    print("dataaa")
    req(input$MonFichier)
    
    ext <- tools::file_ext(input$MonFichier$name)
    t<-switch(ext,
              csv = vroom::vroom(input$MonFichier$datapath, delim = ","),
              validate("Invalid file Please upload a .csv")
    )
    inFile <- isolate(input$MonFichier)
    t <- read.csv(inFile$datapath, header = TRUE)
    
    col_names <- sapply(t, function(col) length(unique(col)) < 10)
    t[ , col_names] <- lapply(t[ , col_names] , factor)
    
    data(t)
    Type<-sapply(data(), class)
    lestypes$types<- type_stat_f(Type)
    
    
  })
  
  
  
    #####
  
    
    ######
     observe({
         updateSelectInput(session, "colum", choices = sort(as.character(colnames(data_input()))))})
     
     observe({
         updateSelectInput(session, "rowvar", choices = sort(as.character(colnames(data_input()))))
    })
    
    output$foo <- renderPrint({CrossTable((data_input()[,c(input$rowvar)]), (data_input()[,c(input$colum)]), expected = TRUE)})
    output$myTableOutput1 <- renderDataTable({data()})
    
    
    
    output$myoutput <- renderUI({
        switch(input$dman_Preview, 
               "Preview" = 
                 tabsetPanel(
                   id = "PREV",
                   tabPanel("le DATASET",
                            #afficher le data set
                            dataTableOutput("contents"),
                   ),
                   tabPanel("TYPES DES VARIABLES",
                           dataTableOutput('myTableOutput'),
                           htmlOutput("mySelection"),
                           actionButton(inputId = "sauvegarde", label = "save")
                   )
                 ),
                 
                 
              
               
               "Str" = verbatimTextOutput("Str"),
                "summary"= verbatimTextOutput("summary"))})
    output$summary <- renderPrint({
      #Multiple prints !!!
      print('Summarize numeric variables:')
      numeric_var <- names(data())[sapply(data(), is.numeric)]
      print( setDT(data())[, summary(.SD), .SDcols = numeric_var] )
      
      print('Summarize factor variables:')
      fac_var <- names(data())[sapply(data(), is.factor)]
      print(lapply(data()[, .SD, .SDcols = fac_var], table))
      
      
      
    })
    
    output$Str <- renderPrint({str(data()) })
    output$contents <- renderDataTable({data()})
    
    #Dumification + Normalisation :
    output$myoutput2 <- renderUI({
        switch(input$dman_Preview2, 
               "Dummification" = dataTableOutput("Dummification"),
               "Norrmalisation" = dataTableOutput("Norrmalisation"))
    })
    
    min_max_norm <- function(x) {
        (x - min(x)) / (max(x) - min(x))
    }
    #appliquer la dummification sur les variables qualitatif
    Dum<-reactive({
        dumfi <- dummy_cols(as.data.frame((data())),remove_first_dummy = TRUE,remove_selected_columns = TRUE)
        dumfi})
    output$Dummification <- renderDataTable({Dum()} , options = list(pageLength = 5))
    
    #appliquer la normalisation :
    
    
    norm<-reactive({
        res <- as.data.frame(data())
        vec <- c()
        #appliquer la normalisation
        
        k = 1
        for (i in as.data.frame((data()))){
            
            if(is.numeric(i)== TRUE){
                vec<-c(vec,colnames(data())[k]) 
            }
            k = k+1
        }
        
        for (i in vec){
            res[i]<-as.data.frame(lapply(as.data.frame((data()))[i], min_max_norm))
            print(res[i])
        }
        #print(vec)
        res 
    })
    
    
    output$Norrmalisation <- renderDataTable({norm()}, options = list(pageLength = 5))
    #outliers + missing data 
    #####OUTLIER AFFICHER LEUR PRESENCE ET LEUR VALEURS SI Y EN A
    presoutlier<- reactive({
        #compteur<-0
        #LISTE pour la presence d'outliers
        mylist<-c()
        #liste pour specifier la valeur des outliers
        mylist2<-c()
        liste_pourcentage<-c()
        
        
        #on stocke dans table.tmp la liste des nom de colonne
        table.tmp<-as.data.frame(colnames(data()))
        
        for(i in seq_len(length(colnames(data())))){
            pourcentage<-0.0
            #----> df1 de type dataframe
            df1<-data()[colnames(data())[i]]
            if(sapply(df1,class)=="numeric"){
                #---> firstlist de type list
                firstlist<-suppressWarnings(lapply(df1,as.numeric))
                
                
                
                #---> num de type numeric
                num <- as.numeric(unlist(firstlist))
                total<-length(num)
                
                #POUR LES OUTLIERS
                b<-boxplot.stats(num)$out
                rep<-""
                if(length(b)==0){
                    rep<-"non"
                    mylist <- c(mylist, rep)
                    mylist2<-c(mylist2,"aucun")
                }else{
                    rep<-"oui"
                    c=""
                    for(k in b){
                        c=paste(k,c,sep=" ") 
                    }
                    mylist <- c(mylist, rep)
                    mylist2<-c(mylist2,c)
                    pourcentage<-length(b)/total
                    pourcentage<-pourcentage*100
                }
            } else {
                rep<-"non"
                mylist <- c(mylist, rep)
                mylist2<-c(mylist2,"aucun")
            }
            liste_pourcentage<-c(liste_pourcentage,pourcentage)
        }
        table.tmp <- cbind(table.tmp, mylist)
        table.tmp <- cbind(table.tmp, mylist2,liste_pourcentage)
        
        colnames(table.tmp) <- c("Variables", "Presence d'outliers", "Valeurs des outliers","Proportion des outliers (en %)")
        
        
        table.tmp
        #data()
        
    })
    
    
    #####VAL MANQUANTES AFFICHER LEUR PRESENCE ET LEUR LIGNE SI Y EN A
    val_manquantes <- reactive({
        #compteur<-0
        liste_pourcentage<-c()
        valeur_manquante<-c()
        
        #on stocke dans table.tmp la liste des nom de colonne
        table.tmp<-as.data.frame(colnames(data()))
        
        for(i in seq_len(length(colnames(data())))){
            #----> df1 de type dataframe
            df1<-data()[colnames(data())[i]]
            
            total=as.numeric(count(df1)[1,"n"])
            pourcentage=0.0
            #POUR LES VALEURS MANQUANTES
            pos_val_manquantes<-""
            if(length(which(is.na(df1),arr.ind=TRUE))!=0){
                for(k in which(is.na(df1),arr.ind=TRUE)[,'row']){
                    pos_val_manquantes<-paste(pos_val_manquantes,k,sep=" ") 
                }
                print(length(which(is.na(df1),arr.ind=TRUE)[,'row']))
                pourcentage<-length(which(is.na(df1),arr.ind=TRUE)[,'row'])/total
                pourcentage<-pourcentage*100
            } else {
                pos_val_manquantes<-"pas de valeurs manquantes"
            }
            liste_pourcentage<-c(liste_pourcentage,pourcentage)
            valeur_manquante<-c(valeur_manquante, pos_val_manquantes)
        }
        
        table.tmp <- cbind(table.tmp,!!colSums(is.na(data())), colSums(is.na(data())),valeur_manquante,liste_pourcentage)
        colnames(table.tmp) <- c("Variables","presence de valeur manquantes", "nombre de valeurs manquantes","indices lignes des valeurs manquantes","proportion des valeurs manquantes (en %)")
        
        
        table.tmp
        
    })
    
    output$Outliers<-renderTable({presoutlier()})
    output$MissingData<-renderTable({val_manquantes()})
    output$Incorrect <- renderDT(req(data()))
    ### 1-MANIPULATION SUR LES OUTLIERS  (SUPP ET REMPLACEMENT)
    ##1-1-SUPPRIMER LES OUTLIERS
    rvs = reactiveValues(buttons = list(), observers = list()) 
    
    observeEvent(input$supp_outliers,{
        ok<-as.numeric(input$variable_outliers)
        print("ICIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII")
        okok<-input$variable_outliers
        print(okok)
        print("ICIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII")
        #seq_len(length(colnames(data()))) 
        rvs$observers = lapply(
            1, 
            function(i) {
                observeEvent(input[["supp_outliers"]], 
                             supp_outliers_f(ok)
                )
            }
        )
        #l = seq_len(length(colnames(data()))) 
    }
    )
    
    
    supp_outliers_f <- function(x) {
        print("ici")
        df2 <- data()
        
        #----> df1 de type dataframe
        df1<-data()[colnames(data())[x]]
        if(sapply(df1,class)=="numeric"){
            #---> firstlist de type list
            firstlist<-suppressWarnings(lapply(df1,as.numeric))
            
            #---> num de type numeric
            num <- as.numeric(unlist(firstlist))
            # nouveau_calcul<-caclul_outliers(num)
            #POUR LES OUTLIERS
            b<-boxplot.stats(num)$out
            print("ici2")
            print(num)
            if(length(b)!=0){
                print("ici3")
                for(k in b){
                    print("ici4")
                    ligne=as.numeric(which(df1==k,arr.ind=TRUE)[,'row'])
                    print("ici5")
                    print(ligne)
                    df2 <- df2[-(ligne:ligne),]
                    print("ici6")
                    #df2 <- df2[-(k:k),]
                    
                }
                
            }
            #print(nouveau_calcul)
            print(data()[colnames(data())[x]])
            data(df2) 
        }
    }
    
    
    
    #######
    ##1-2-REMPLACER les outliers par qlq chose d'autre
    rvss = reactiveValues(buttons = list(), observers = list()) 
    
    observeEvent(input$remplacer_outliers_s,{
        #l = length(rvss$buttons) + 1
        
        # for(i in l:(l+1)) {
        rvss$buttons[[1]] = selectInput("remplacement1", "Remplacer par:", 
                                        get_min_max_moy(as.numeric(input$remplacer_outliers_s)))})
    
    
    observeEvent(input$remplacer_outliers_b,{
        ok<-as.numeric(input$remplacer_outliers_s)
        print("ICIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII")
        okok<-input$remplacer_outliers_s
        print(okok)
        print("ICIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII")
        rvs$observers = lapply(
            1, 
            function(i) {
                observeEvent(input[["remplacer_outliers_b"]], 
                             remplacer_outliers(as.numeric(input$remplacement1))
                )
            }
        )
        #l = seq_len(length(colnames(data()))) 
    }
    )
    
    output$more_buttons = renderUI({
        do.call(fluidRow, rvss$buttons) # Add the dynamic buttons into a single fluidRow
    })
    
    
    
    remplacer_outliers<-function(val){
        
        x=as.numeric(input$remplacer_outliers_s)
        df1<-data()[colnames(data())[x]]
        if(sapply(df1,class)=="numeric"){
            print(val)
            
            
            df2 <- data()
            name_col=colnames(data())[x]
            
            #---> firstlist de type list
            firstlist<-suppressWarnings(lapply(df1,as.numeric))
            
            #---> num de type numeric
            num <- as.numeric(unlist(firstlist))
            
            #POUR LES OUTLIERS
            b<-boxplot.stats(num)$out
            
            
            if(length(b)!=0){
                for(k in b){
                    ligne=which(df1==k,arr.ind=TRUE)[,'row']
                    if(length(ligne)>1){
                        for(j in ligne){
                            df2[j,x] <- as.numeric(val)
                        }
                    }else{
                        df2[ligne,x] <- as.numeric(val)
                    }
                    #df2 <- df2[-(k:k),]
                    
                }
                
            }
            
            data(df2)
            
            print(data()[colnames(data())[x]])
            
            print(sprintf("You clicked button nummmmmber %d",x))
        }
        
    }
    
    
    
    moyenne<-function(x){
        #---> firstlist de type list
        firstlist<-suppressWarnings(lapply(x,as.numeric))
        #---> num de type numeric
        x <- as.numeric(unlist(firstlist))
        print("on est dans la moyenne:")
        print(x)
        print("on est dans la moyenne!!!!!")
        moy=0
        if(length(x)!=0){
            sum=0
            print("icimoy1")
            total=0
            for(i in x){
                if(!is.na(i)){
                    sum=sum+i
                    total=total+1
                }
            }
            if(total!=0){
                moy=sum/total
            }
            
        } 
        
        moy
    }
    
    
    
    get_min_max_moy<-function(x){
        col=x
        minimum<-min(data()[colnames(data())[col]])
        maximum<- max(data()[colnames(data())[col]])
        moy<-moyenne(data()[colnames(data())[col]])
        valeurs<-c(moy,minimum,maximum)
        min_c=paste('minimum: ', minimum)
        max_c=paste('maximum:' ,maximum)
        moy_c=paste('moyenne: ',moy)
        x<-setNames(as.list(valeurs),c(moy_c,min_c,max_c))
        x
        
    }
    
    
    #creation d'une fct qui calcul le min et le max manuellement en ignorant les NA
    min_max_moy_f<-function(col){
        x<-data()[colnames(data())[col]]
        entrer<-x
        print("fonctionnJSKHF")
        print(x)
        retour<-c()
        firstlist<-suppressWarnings(lapply(x,as.numeric))
        #---> num de type numeric
        x <- as.numeric(unlist(firstlist))
        max<-x[1]
        min<-x[1]
        print("CREAAATION DE FCTTT")
        print(max)
        print(length(x))
        print("CREAAATION!!!")
        for(i in length(x)){
            print(i)
            if(max<x[i]){
                max<-x[i]
            }
            if(min>x[i]){
                min<-x[i]
            }
        }
        moy=moyenne(entrer)
        retour<-c(retour,max)
        retour<-c(retour,min)
        retour<-c(retour,moy)
        print(retour)
        min_c=paste('minimum: ', min)
        max_c=paste('maximum:' ,max)
        moy_c=paste('moyenne: ',moy)
        x<-setNames(as.list(retour),c(moy_c,min_c,max_c))
        print("OKKK")
        x
        
        
    }
    
    
    
    
    
    
    
    
    
    
    
    
    ###################
    ####2-VALEURS MANQUANTES
    ##2-1-sUPPRIMER LES VALEURS MANQUANTES
    rvs = reactiveValues(buttons = list(), observers = list()) 
    
    observeEvent(input$supp_val_m_b,{
        ok<-as.numeric(input$variable)
        rvs$observers = lapply(
            seq_len(length(colnames(data()))) , 
            function(i) {
                observeEvent(input[["supp_val_m_b"]], 
                             supp(ok)
                )})} )
    
    
    #supprimer les valeurs manquantes
    supp <- function(x) {
        df1<-data()[colnames(data())[x]]
        df2 <- data()
        if(length(which(is.na(df1),arr.ind=TRUE))!=0){
            for(k in which(is.na(df1),arr.ind=TRUE)[,'row']){
                df2 <- df2[-(k:k),]
                
            }
            
            data(df2)
        } 
        
        print(data()[colnames(data())[x]])
        
    }
    
    
    
    
    ##2-2 REMPLACER LES COLONNE NUMERIQUE PAR MIN MAX OU MOY
    find_NA_num_var<-reactive({
        valeur_manquante<-c()
        id_val<-c()
        
        #on stocke dans table.tmp la liste des nom de colonne
        table.tmp<-as.data.frame(colnames(data()))
        
        for(i in seq_len(length(colnames(data())))){
            #----> df1 de type dataframe
            df1<-data()[colnames(data())[i]]
            if(sapply(df1,class)=="numeric"){
                #POUR LES VALEURS MANQUANTES
                
                if(length(which(is.na(df1),arr.ind=TRUE))!=0){
                    valeur_manquante<-c(valeur_manquante,colnames(data())[i])
                    id_val<-c(id_val,i)
                }
            }
        }
        x=setNames(as.list(id_val),valeur_manquante)
        x
        
    })
    rvss = reactiveValues(buttons = list(), observers = list()) 
    
    observeEvent(input$remplacer_valm_num_s,{
        l = length(rvss$buttons) + 1
        
        # for(i in l:(l+1)) {
        rvss$buttons[[1]] = selectInput("remplacement2", "Remplacer par:", 
                                        min_max_moy_f(as.numeric(input$remplacer_valm_num_s)))
       
    }
    )
    
    
    observeEvent(input$remplacer_valm_b,{
        ok<-as.numeric(input$remplacer_outliers_s)
        print("ICIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII")
        okok<-input$remplacer_outliers_s
        print(okok)
        print("ICIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIIII")
        rvs$observers = lapply(
            1, 
            function(i) {
                observeEvent(input[["remplacer_valm_b"]], 
                             remplacer_val_manq_num(as.numeric(input$remplacement2))
                )
            }
        )
        #l = seq_len(length(colnames(data()))) 
    }
    )
    
    output$more_buttons2 = renderUI({
        do.call(fluidRow, rvss$buttons) # Add the dynamic buttons into a single fluidRow
    })
    
    
    
    remplacer_val_manq_num <- function(val) {
        print("icii1")
        x<-as.numeric(input$remplacer_valm_num_s)
        df1<-data()[colnames(data())[x]]
        print("icii2")
        
        print("icii3")
        df2 <- data()
        print("icii4")
        if(length(which(is.na(df1),arr.ind=TRUE))!=0){
            print("ICIII44!!!")
            # max<-max(which(is.na(df1),arr.ind=TRUE)[,'row'])
            # min<-min(which(is.na(df1),arr.ind=TRUE)[,'row'])
            for(k in which(is.na(df1),arr.ind=TRUE)[,'row']){
                df2[k,x] <- as.numeric(val)
                
            }
            
            data(df2)
        } 
        
        print(data()[colnames(data())[x]])
        print("par laaakdj")
        
    }
    
    
    
    
    
    
    
    
    
    ################### 
    ### 3-LISTES POUR LES BOUTONS SELECT
    ##!!trouver les valeurs manquantes POUR LE SELECT
    find_NA_var<-reactive({
        valeur_manquante<-c()
        id_val<-c()
        
        #on stocke dans table.tmp la liste des nom de colonne
        table.tmp<-as.data.frame(colnames(data()))
        
        for(i in seq_len(length(colnames(data())))){
            #----> df1 de type dataframe
            df1<-data()[colnames(data())[i]]
            
            #POUR LES VALEURS MANQUANTES
            
            if(length(which(is.na(df1),arr.ind=TRUE))!=0){
                valeur_manquante<-c(valeur_manquante,colnames(data())[i])
                id_val<-c(id_val,i)
            }
        }
        x=setNames(as.list(id_val),valeur_manquante)
        x
        
    })
    
    # !!!! TROUVER LES OUTLIER pour le SELECT
    find_outliers_var<-reactive({
        mylist<-c()
        id_val<-c()
        #on stocke dans table.tmp la liste des nom de colonne
        
        print(seq_len(length(colnames(data()))) )
        for(i in seq_len(length(colnames(data())))){
            df1<-data()[colnames(data())[i]]
            if(sapply(df1,class)=="numeric"){
                firstlist<-suppressWarnings(lapply(df1,as.numeric))
                num <- as.numeric(unlist(firstlist))
                b<-boxplot.stats(num)$out
                rep<-""
                if(length(b)!=0){
                    mylist <- c(mylist, colnames(data())[i])
                    id_val<-c(id_val,i)}}}
        x=setNames(as.list(id_val),mylist)
        x })
    
    
    
    varTarget<-reactive({
      choices <- c()
      for(i in names(data())){
        if(length(levels(data()[[i]]))==2){
          choices<-c(choices,i)
        }
      }
      choices
    })
    
    choixTarget=reactiveValues(choix = character)
    
    
    observeEvent(input$bvartarget1,{
      choixTarget$choix<-input$choixTarget1
      #<-c(choixTarget$choix,input$choixTarget1)
      
      updateSelectInput(inputId = "tar1", choices = choixTarget$choix)
      
      
      
    })
    
    
    
    
    output$myoutput1 <- renderUI({
      switch(input$dman_Preview1, 
             "Outliers" = fluidPage(
               if(length(find_outliers_var())!=0){
                 fluidRow(
                   column(5, selectInput("variable_outliers", "Variable:", find_outliers_var()),
                          actionButton(inputId = "supp_outliers", label = "Supprimer les outliers")),
                   column(5,selectInput("remplacer_outliers_s", "Variable:", find_outliers_var() ),
                          uiOutput("more_buttons"),
                          actionButton(inputId = "remplacer_outliers_b", label = "Remplacer"),    
                   ),)},
               
               fluidRow(column(10,tableOutput("Outliers")),)),
             "Missing Data" = fluidPage(
               if(length(find_NA_var())!=0){
                 fluidRow(
                   column(5, selectInput("variable", "Variable:",find_NA_var()),
                          actionButton(inputId = "supp_val_m_b", label = "Supprimer")),
                   if(length(find_NA_num_var())!=0){
                     column(5, selectInput("remplacer_valm_num_s", "Variable:",find_NA_num_var()),
                            uiOutput("more_buttons2"),
                            actionButton(inputId = "remplacer_valm_b", label = "Remplacer"),
                            renderText("  "))},)},
               
               fluidRow(column(10, tableOutput("MissingData")),)),
             "Incorrect" = fluidPage(
               
               fluidRow(
                 column(4,selectInput("col", "Column to search:",  choices = c(not_sel)),
                        textInput("old", "Replace:"),
                        textInput("new", "By:"),
                        actionButton(inputId = "replace",label = "Replace!"),
                        actionButton(inputId ="bvartarget1", label ="save"))
                 
               ),
               fluidRow(column(10,DTOutput("Incorrect")))
             ),
             
             
             
      )})
    
    
    observeEvent(data(),{
      choices <- c(not_sel,names(data()))
      updateSelectInput(session, "col", choices = choices)})
    
    observeEvent(input$replace, {
      req(input$col)
      dat <- req(data())
      traf <- if (is.numeric(dat[[input$col]])) as.numeric else identity
      data(dat %>%
             mutate(!!rlang::sym(input$col) := 
                      replace(!!rlang::sym(input$col),
                              as.character(!!rlang::sym(input$col)) == input$old,
                              input$new) %>% 
                      traf()))
    })
    
    #2-Visualisation Part :
    #data for visualisation
    # data_input <- reactive({
    #     req(input$csv_input)
    #     fread(input$csv_input$datapath)})
    
    data_input <- eventReactive(input$csv_input, {
      inFile <- isolate(input$csv_input)
      if (is.null(inFile)) return(NULL)
      t <- read.csv(inFile$datapath, header = TRUE)
      col_names <- sapply(t, function(col) length(unique(col)) < 10)
      t[ , col_names] <- lapply(t[ , col_names] , factor)
      t})
    ####
    
    observeEvent(data(),{
      choices <- c(not_sel)
      for(i in names(data())){
        if(length(levels(data()[[i]]))==2){
          choices<-c(choices,i)
        }
      }
      updateSelectInput(inputId = "out", choices = choices)
      updatePickerInput(session, inputId = "inn", choices = c(not_sel,names(data())))
      
    })
    
    observeEvent(input$butt1,{
      print(input$Slider1)
      print(input$inn)
      print("UUUUUUUUUUUUUUUUUUUUUUUU8888888888888888!!!!!!")
      #test<-c("bonjour","aurevoir","salut","cava")
      #test <- test[ test != "aurevoir" ]
      #print(test)
    })
    
    observeEvent(input$out,{
      choixTarget$choix<-input$out
      choices <- c(not_sel,names(data()))
      if(!is.null(input$out)){
        choices<-choices[ choices != input$out]
      }
      updatePickerInput(session, inputId = "inn", choices = choices)
    })
    
    out <- eventReactive(input$run_button,input$out)
    inn <- eventReactive(input$run_button,input$inn)
    
    tabl <- eventReactive(input$inn,{
      d<-data()
      subset_table <- d[, input$inn, drop = F]
      indx <- sapply(subset_table, is.factor)
      subset_table[indx] <- lapply(subset_table[indx], function(x) as.integer(as.factor(x)))
      subset_table
    })
    
    splitSlider <- reactive({
      input$Slider1 / 100
    })
    
    set.seed(100)  # setting seed to reproduce results of random sampling
    trainingRowIndex <-
      reactive({
        sample(1:nrow(tabl()),
               splitSlider() * nrow(tabl()))
      })# row indices for training data
    
    trainingData <- reactive({
      tmptraindt <- tabl()
      tmptraindt[trainingRowIndex(), ]
    })
    
    testData <- reactive({
      tmptestdt <- tabl()
      tmptestdt[-trainingRowIndex(),]
    })
    
    f <- reactive({
      as.formula(paste(input$out, "~."))
    })
    
    #  Xtrain <- reactive({
    #      # split dataset
    #      dd <- trainingData()
    #      pp <- input$out
    #      dd$pp<-NULL
    #      dd
    #  })
    #  Ytrain <-reactive({
    #      dd <- trainingData()
    #      pp <- input$out
    #      dd$pp
    #  }) 
    #  
    #  Xtest <- reactive({
    #      # split dataset
    #      dd <-  testData
    #      pp <- input$out
    #      dd$pp<-NULL
    #      dd
    #  })
    #  Ytest <-reactive({
    #      dd <-  testData
    #      pp <- input$out
    #      dd$pp
    #  }) 
    #  
    output$cntTrain <-
      renderText(paste("Train Data:", NROW(trainingData()), "records"))
    output$cntTest <-
      renderText(paste("Test Data:", NROW(testData()), "records"))
    #  
    #  # Construct xgb.DMatrix object from training matrix
    #  dtrain <-reactive({ 
    #    traint <- Xtrain()
    #    traint[] <- lapply(traint, as.numeric)
    #    xgb.DMatrix(as.matrix(traint, label = Ytrain()))})
    #  
    #  # Create a list of parameters for XGBoost model
    #  param <- list(max_depth=3, 
    #                silent=1, 
    #                eta = 0.3,
    #                objective='binary:logistic',
    #                eval_metric = 'auc')
    #  
    #  # Training a XGBoost model using training dataset and chosen parameters
    #  bst <- reactive({xgb.train(param, nrounds = 82, dtrain()) }) ####Am here
    #  # 
    #  # # Predicting the results using testing dataset
    #  predxgb <- reactive({
    #      pred_xgb <- predict(bst(), as.matrix(testData()))
    #      pred_xgb 
    #  })
    #  
    #  train1<- reactive({
    #      dd <- Xtrain()
    #      pp <- input$out
    #      dd$pp<-Ytrain()
    #      dd })
    #  
    #  svm_model<- reactive({
    #                 Attrition <- input$out
    #                 svm(Attrition~.,                #set model formula
    #                 type="C-classification",   #set classification machine
    #                 gamma=0.001668101,         #set gamma parameter
    #                 cost=35.93814,             #set cost parameter
    #                 data=train1(),
    #                 cross=3,                   #3-fold cross validation
    #                 probability = TRUE        #allow for probability prediction
    #  )})
    #  
    #  svm_modelpredict<-reactive({predict(svm_model(), test, probability=TRUE)}) 
    #  
    #  # Obtain the predicted probability for class 0/1
    #  svm_modelprob <- reactive({attr(svm_modelpredict(),"probabilities")})
    #  
    #  # Training a logistic regression model
    #  LR_model <- reactive({
    #     Attrition <- input$out
    #     glm(Attrition ~.,family=binomial(link='logit'),data=train1())})
    #  
    #  # Predicting the results using testing dataset
    #  LR_modelpredict <- reactive({predict(LR_model(), testData(), type = "response")}) 
    #  
    #  # Create a prediction object using previously saved results
    #   ROCRpred_xgb <- reactive({prediction(predxgb(), Ytest())})
    #   ROCRpred_svm <- reactive({prediction(svm_modelprob(), Ytest())})
    #   ROCRpred_lr <- reactive({prediction(LR_modelpredict(), Ytest())})
    #  # 
    #   get_cutoff_point <- function(perf, threshold)
    #   {
    #     cutoffs <- data.frame(fpr=perf@x.values[[1]], tpr=perf@y.values[[1]])
    #     cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
    #     cutoffs <- subset(cutoffs, fpr <= threshold)
    #     if(nrow(cutoffs) == 0){ return(1.0)}
    #     else return(cutoffs[1, 1])
    #   }
    #   
    #   draw_confusion_matrix <- function(cm, auc, color) {
    #     
    #     layout(matrix(c(1,1,2)))
    #     par(mar=c(0,0.1,1,0.1))
    #     plot(c(125, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
    #     
    #     # create the matrix 
    #     rect(150, 430, 240, 370, col=color)
    #     text(195, 435, '0', cex=1.2)
    #     rect(250, 430, 340, 370, col='white')
    #     text(295, 435, '1', cex=1.2)
    #     text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
    #     text(245, 450, 'Actual', cex=1.3, font=2)
    #     rect(150, 305, 240, 365, col='white')
    #     rect(250, 305, 340, 365, col=color)
    #     text(140, 400, '0', cex=1.2, srt=90)
    #     text(140, 335, '1', cex=1.2, srt=90)
    #     
    #     # add in the cm results 
    #     res <- as.numeric(cm$table)
    #     text(195, 400, res[1], cex=1.6, font=2, col='white')
    #     text(195, 335, res[2], cex=1.6, font=2, col='black')
    #     text(295, 400, res[3], cex=1.6, font=2, col='black')
    #     text(295, 335, res[4], cex=1.6, font=2, col='white')
    #     
    #     # add in the specifics 
    #     plot(c(0, 100), c(0, 50), type = "n", xlab="", ylab="", main = "", xaxt='n', yaxt='n')
    #     
    #     # add in the accuracy information 
    #     
    #     text(25, 30, "AUC", cex=1.8, font=2)
    #     text(25, 20, round(as.numeric(auc), 3), cex=1.8)
    #     text(75, 30, names(cm$overall[1]), cex=1.8, font=2)
    #     text(75, 20, round(as.numeric(cm$overall[1]), 3), cex=1.8)
    #   }
    #   
    #   # Create roc plot
    #   #-------------------------------------------------------
    #   #XGBoost roc data
    #   roc_perf_xgb <- reactive({performance(ROCRpred_xgb(), 'tpr','fpr')  })                
    #   roc_xgbdata <- reactive({
    #     roc_perf_xgb <- roc_perf_xgb()
    #     data.frame(fpr=unlist(roc_perf_xgb@x.values),
    #                              tpr=unlist(roc_perf_xgb@y.values), model="XGBoost")})
    #   
    #   #SVM roc data
    #   roc_perf_svm <- reactive({performance(ROCRpred_svm(), 'tpr','fpr') })                 
    #   roc_svmdata <- reactive({
    #     roc_perf_svm <-roc_perf_svm()
    #     data.frame(fpr=unlist(roc_perf_svm@x.values),
    #                              tpr=unlist(roc_perf_svm@y.values), model="SVM")})
    #   
    #   #Logistic Regression roc data
    #   roc_perf_lr <- reactive({performance(ROCRpred_lr(), 'tpr','fpr')  })                  
    #   roc_lrdata <- reactive({ 
    #     roc_perf_lr <-roc_perf_lr()
    #     data.frame(fpr=unlist(roc_perf_lr@x.values),
    #                             tpr=unlist(roc_perf_lr@y.values), model="LR")})
    #   
    #   output$plot_roc<-renderPlot({ggplot() + 
    #       geom_line(data = roc_xgbdata(), aes(x=fpr, y=tpr, colour = "XGBoost")) + #set XGBoost roc curve
    #       geom_line(data = roc_svmdata(), aes(x = fpr, y=tpr, colour = "SVM")) + #set SVM roc curve
    #       geom_line(data = roc_lrdata(), aes(x = fpr, y=tpr, colour = "Logistic Regression")) + 
    #       
    #       #set LR roc curve
    #       geom_vline(xintercept = xintercept_roc(), color = "red", linetype=2) + theme_bw() + #set themes
    #       scale_colour_manual(name = "Models", values = cols) + 
    #       xlab("False Positive Rate") +
    #       ylab("True Positive Rate") +
    #       theme(legend.position = c(0.8, 0.2), 
    #             legend.text = element_text(size = 15), 
    #             legend.title = element_text(size = 15))
    #   })
    #   
    #   # draw XGBoosting confusion matrix
    #   output$confusionMatrix_roc_xgb<-renderPlot({
    #     roc_auc_xgb <- performance(ROCRpred_xgb(), measure = "auc")  #obtain auc from @performance
    #     roc_perf_xgb <- performance(ROCRpred_xgb(), 'tpr','fpr')  #obtain tpr and fpr from @performance                   
    #     roc_cut <- get_cutoff_point(roc_perf_xgb(), xintercept_roc()) #obtain the cutoff probability
    #     roc_pred_values_xgb <- ifelse(predxgb() > roc_cut,1,0) #classify using cutoff probability
    #     roc_cm_xgb <- confusionMatrix(data = factor(roc_pred_values_xgb), reference = factor(Ytest()))#obtain confusion matrix
    #     draw_confusion_matrix(roc_cm_xgb, roc_auc_xgb@y.values, "#3DB7E4")  #Draw confusion matrix plot
    #   })
    #   
    #  ##ty
    #   # draw SVM confusion matrix
    #   output$confusionMatrix_roc_svm<-renderPlot({
    #     roc_auc_svm <- performance(ROCRpred_svm(), measure = "auc")
    #     roc_perf_svm <- performance(ROCRpred_svm(), 'tpr','fpr')                  
    #     roc_cut <- get_cutoff_point(roc_perf_svm(), xintercept_roc())
    #     roc_pred_values_svm <- ifelse(svm_modelprob() > roc_cut,1,0)
    #     roc_cm_svm <- confusionMatrix(data = factor(roc_pred_values_svm), reference = factor(Ytest()))
    #     draw_confusion_matrix(roc_cm_svm, roc_auc_svm@y.values, "#FF8849")
    #   })
    #   
    #   # draw Logistic regression confusion matrix
    #   output$confusionMatrix_roc_lr<-renderPlot({
    #     roc_auc_lr <- performance(ROCRpred_lr(), measure = "auc")
    #     roc_perf_lr <- performance(ROCRpred_lr(), 'tpr','fpr')                    
    #     roc_cut <- get_cutoff_point(roc_perf_lr, xintercept_roc())
    #     roc_pred_values_lr <- ifelse(LR_modelpredict() > roc_cut,1,0)
    #     roc_cm_lr <- confusionMatrix(data = factor(roc_pred_values_lr), reference = factor(Ytest()))
    #     draw_confusion_matrix(roc_cm_lr, roc_auc_lr@y.values, "#69BE28")
    #   })
    #   
    #   #-------------------------------------------------------
    #   
    #   #Create precision plot
    #   #-------------------------------------------------------
    #   #XGBoost
    #   precision_perf_xgb <-reactive({performance(ROCRpred_xgb(), "prec", "cutoff")})  #use 'prec' and 'cutoff' as measurements                 
    #   precision_xgbdata <- reactive({
    #     precision_perf_xgb <-precision_perf_xgb()
    #     data.frame(x=unlist(precision_perf_xgb@x.values), y=unlist(precision_perf_xgb@y.values),
    #                                    model="XGBoost")})
    #   
    #   #SVM
    #   precision_perf_svm <- reactive({performance(ROCRpred_svm(), "prec", "cutoff")})                
    #   precision_svmdata <- reactive({
    #     precision_perf_svm <-precision_perf_svm()
    #     data.frame(x=unlist(precision_perf_svm@x.values), y=unlist(precision_perf_svm@y.values),
    #                                    model="SVM")})
    #   
    #   #Logistic Regression
    #   precision_perf_lr <- reactive({performance(ROCRpred_lr(), "prec", "cutoff")  })                  
    #   precision_lrdata <- reactive({
    #     precision_perf_lr <-  precision_perf_lr()
    #     data.frame(x=unlist(precision_perf_lr@x.values), y=unlist(precision_perf_lr@y.values),
    #                                   model="LR")})
    #   
    #   
    #   cols <- c("XGBoost" = "#3DB7E4", "SVM" = "#FF8849", "Logistic Regression" = "#69BE28")
    #   
    #   output$plot_precision<-renderPlot({ggplot() +
    #       geom_line(data = precision_xgbdata(), aes(x=x, y=y, colour = "XGBoost")) + 
    #       geom_line(data = precision_svmdata(), aes(x =x, y=y, colour = "SVM")) + 
    #       geom_line(data = precision_lrdata(), aes(x =x, y=y, colour = "Logistic Regression")) + 
    #       scale_colour_manual(name = "Models", values = cols) + 
    #       xlab("Cutoff") +
    #       ylab("Precision") +
    #       geom_vline(xintercept = xintercept_precision(), color = "red", linetype=2) + theme_bw() +
    #       theme(legend.position = c(0.8, 0.2), 
    #             legend.text = element_text(size = 15), 
    #             legend.title = element_text(size = 15))
    #   })
    #   
    #   
    #   # draw XGBoosting confusion matrix
    #   output$confusionMatrix_precision_xgb<-renderPlot({
    #     precision_auc_xgb <- performance(ROCRpred_xgb(), measure = "auc")  #obtain auc from @performance
    #     precision_perf_xgb <- performance(ROCRpred_xgb(), "prec", "cutoff") #use 'prec' and 'cutoff' as measurements                 
    #     precision_cut <- get_cutoff_point(precision_perf_xgb(), xintercept_precision()) #obtain the cutoff probability
    #     precision_pred_values_xgb <- ifelse(predxgb() >  precision_cut,1,0) #classify using cutoff probability
    #     precision_cm_xgb <- confusionMatrix(data = factor(precision_pred_values_xgb), reference = factor(Ytest())) #obtain confusion matrix
    #     draw_confusion_matrix(precision_cm_xgb, precision_auc_xgb@y.values, "#3DB7E4")  #Draw confusion matrix plot
    #   })
    #   
    #   # draw SVM confusion matrix
    #   output$confusionMatrix_precision_svm<-renderPlot({
    #     precision_auc_svm <- performance(ROCRpred_svm(), measure = "auc")
    #     precision_perf_svm <- performance(ROCRpred_svm(), "prec", "cutoff")                  
    #     precision_cut <- get_cutoff_point(precision_perf_svm(), xintercept_precision())
    #     precision_pred_values_svm <- ifelse(svm_modelprob() >  precision_cut,1,0)
    #     precision_cm_svm <- confusionMatrix(data = factor(precision_pred_values_svm), reference = factor(Ytest()))
    #     draw_confusion_matrix(precision_cm_svm, precision_auc_svm@y.values, "#FF8849")
    #   })
    #   
    #   # draw Logistic regression confusion matrix
    #   output$confusionMatrix_precision_lr<-renderPlot({
    #     precision_auc_lr <- performance(ROCRpred_lr(), measure = "auc")
    #     precision_perf_lr <- performance(ROCRpred_lr(), "prec", "cutoff")                    
    #     precision_cut <- get_cutoff_point(precision_perf_lr(), xintercept_precision())
    #     precision_pred_values_lr <- ifelse(LR_modelpredict() >  precision_cut,1,0)
    #     precision_cm_lr <- confusionMatrix(data = factor(precision_pred_values_lr), reference = factor(Ytest()))
    #     draw_confusion_matrix(precision_cm_lr, precision_auc_lr@y.values, "#69BE28")
    #   })
    #   
    #   
    #   
    #   
    #   
    #   
    #   
    #   #-------------------------------------------------------
    #   
    #   #Create recall plot
    #   #-------------------------------------------------------
    #   #XGBoost
    #   recall_perf_xgb <- reactive({performance(ROCRpred_xgb(), "rec", "cutoff")}) #use 'rec' and 'cutoff' as measurements                 
    #   recall_xgbdata <- reactive({
    #     recall_perf_xgb <- recall_perf_xgb()
    #     data.frame(x=unlist(recall_perf_xgb@x.values), y=unlist(recall_perf_xgb@y.values),
    #                                 model="XGBoost") })
    #   
    #   #SVM
    #   recall_perf_svm <- reactive({performance(ROCRpred_svm(), "rec", "cutoff")})              
    #   recall_svmdata <- reactive({
    #     recall_perf_svm <- recall_perf_svm()
    #     data.frame(x=unlist(recall_perf_svm@x.values), y=unlist(recall_perf_svm@y.values),
    #                                 model="SVM")})
    #   
    #   #Logistic Regression
    #   recall_perf_lr <- reactive({performance(ROCRpred_lr(), "rec", "cutoff")  })                  
    #   recall_lrdata <- reactive({
    #     recall_perf_lr <-  recall_perf_lr()
    #     data.frame(x=unlist(recall_perf_lr@x.values), y=unlist(recall_perf_lr@y.values),
    #                                model="LR")})
    #   
    #   
    #   cols <- c("XGBoost" = "#3DB7E4", "SVM" = "#FF8849", "Logistic Regression" = "#69BE28")
    #   
    #   
    #   output$plot_recall<-renderPlot({ggplot() +
    #       geom_line(data = recall_xgbdata(), aes(x=x, y=y, colour = "XGBoost")) + 
    #       geom_line(data = recall_svmdata(), aes(x =x, y=y, colour = "SVM")) + 
    #       geom_line(data = recall_lrdata(), aes(x =x, y=y, colour = "Logistic Regression")) + 
    #       scale_colour_manual(name = "Models", values = cols) + 
    #       xlab("Cutoff") +
    #       ylab("recall") +
    #       geom_vline(xintercept = xintercept_recall(), color = "red", linetype=2) + theme_bw() +
    #       theme(legend.position = c(0.8, 0.8), 
    #             legend.text = element_text(size = 15), 
    #             legend.title = element_text(size = 15))
    #   })
    #   
    #   
    #   # draw XGBoosting confusion matrix
    #   output$confusionMatrix_recall_xgb<-renderPlot({
    #     recall_auc_xgb <- performance(ROCRpred_xgb(), measure = "auc")  #obtain auc from @performance
    #     recall_perf_xgb <- performance(ROCRpred_xgb(), "prec", "cutoff") #use 'prec' and 'cutoff' as measurements                 
    #     recall_cut <- get_cutoff_point(recall_perf_xgb(), xintercept_recall()) #obtain the cutoff probability
    #     recall_pred_values_xgb <- ifelse(predxgb() > recall_cut,1,0) #classify using cutoff probability
    #     recall_cm_xgb <- confusionMatrix(data = factor(recall_pred_values_xgb), reference = factor(Ytest())) #obtain confusion matrix
    #     draw_confusion_matrix(recall_cm_xgb, recall_auc_xgb@y.values, "#3DB7E4")  #Draw confusion matrix plot
    #   })
    #   
    #   # draw SVM confusion matrix
    #   output$confusionMatrix_recall_svm<-renderPlot({
    #     recall_auc_svm <- performance(ROCRpred_svm(), measure = "auc")
    #     recall_perf_svm <- performance(ROCRpred_svm(), "prec", "cutoff")                  
    #     recall_cut <- get_cutoff_point(recall_perf_svm(), xintercept_recall())
    #     recall_pred_values_svm <- ifelse(svm_modelprob() > recall_cut,1,0)
    #     recall_cm_svm <- confusionMatrix(data = factor(recall_pred_values_svm), reference = factor(Ytest()))
    #     draw_confusion_matrix(recall_cm_svm, recall_auc_svm@y.values, "#FF8849")
    #   })
    #   
    #   # draw Logistic regression confusion matrix
    #   output$confusionMatrix_recall_lr<-renderPlot({
    #     recall_auc_lr <- performance(ROCRpred_lr(), measure = "auc")
    #     recall_perf_lr <- performance(ROCRpred_lr(), "prec", "cutoff")                    
    #     recall_cut <- get_cutoff_point(recall_perf_lr(), xintercept_recall())
    #     recall_pred_values_lr <- ifelse(LR_modelpredict() > recall_cut,1,0)
    #     recall_cm_lr <- confusionMatrix(data = factor(recall_pred_values_lr), reference = factor(Ytest()))
    #     draw_confusion_matrix(recall_cm_lr, recall_auc_lr@y.values, "#69BE28")
    #   })
    
    # output$table2 <- DT::renderDataTable({
    #       subset_table <- tabl()
    #       datatable(subset_table)
    #   })
    ####
    
    observeEvent(data_input(),{
      choices <- c(not_sel,names(data_input()))
      updateSelectInput(inputId = "num_var_1", choices = choices)
      updateSelectInput(inputId = "num_var_2", choices = choices)
      updateSelectInput(inputId = "fact_var", choices = choices)
    })
    
    num_var_1 <- eventReactive(input$run_button,input$num_var_1)
    num_var_2 <- eventReactive(input$run_button,input$num_var_2)
    fact_var <- eventReactive(input$run_button,input$fact_var)
    
    # plot
    
    plot_1 <- eventReactive(input$run_button,{
      draw_plot_1(data_input(), num_var_1(), num_var_2(), fact_var())
    })
    
    output$plot_1 <- renderPlot(plot_1())
    
    # 1-d summary tables
    
    output$num_var_1_title <- renderText(paste("Num Var 1:",num_var_1()))
    
    num_var_1_summary_table <- eventReactive(input$run_button,{
      create_num_var_table(data_input(), num_var_1())
    })
    
    output$num_var_1_summary_table <- renderTable(num_var_1_summary_table(),colnames = FALSE)
    
    output$num_var_2_title <- renderText(paste("Num Var 2:",num_var_2()))
    
    num_var_2_summary_table <- eventReactive(input$run_button,{
      create_num_var_table(data_input(), num_var_2())
    })
    
    output$num_var_2_summary_table <- renderTable(num_var_2_summary_table(),colnames = FALSE)
    
    output$fact_var_title <- renderText(paste("Factor Var:",fact_var()))
    
    fact_var_summary_table <- eventReactive(input$run_button,{
      create_fact_var_table(data_input(), fact_var())
    })
    
    output$fact_var_summary_table <- renderTable(fact_var_summary_table(),colnames = FALSE)
    
    # multi-d summary table
    
    combined_summary_table <- eventReactive(input$run_button,{
      create_combined_table(data_input(), num_var_1(), num_var_2(), fact_var())
    })
    
    output$combined_summary_table <- renderTable(combined_summary_table())
    
    output$myoutput3 <- renderUI({
      switch(input$dman_Preview3, 
             "Summary"  = verbatimTextOutput("Summary"),
             "Correlation"= verbatimTextOutput("Correlation"),
             "Pvalue"= verbatimTextOutput("Pvalue") )})
    
    output$Summary <- renderPrint({
      #Multiple prints !!!
      print('Summarize numeric variables:')
      numeric_var <- names(data_input())[sapply(data_input(), is.numeric)]
      print( setDT(data_input())[, summary(.SD), .SDcols = numeric_var] )
      
      print('Summarize factor variables:')
      copy <- data.table(data_input())
      print(lapply(copy[, .SD, .SDcols = sapply(copy, is.factor)], table))
      
    })
    
    
    output$Correlation <- renderPrint({
      print('Correlation for all numeric values :')
      numeric_var <- names(data_input())[sapply(data_input(), is.numeric)]
      print( setDT(data_input())[, cor(.SD), .SDcols = numeric_var]) })
    
    output$Pvalue <- renderPrint({})
    
    
    
    
    
    
    output$myoutput22 <- renderUI({
      
      column(
        width = 5,
        class = "well",
        tabsetPanel(
          tabPanel(
            "XGBoost",
            h4("Confusion Matrix (XGBoost)"),
            plotOutput("confusionMatrix_roc_xgb"),
            style = "background-color:white;"
          ),
          tabPanel(
            "SVM",
            h4("Confusion Matrix (SVM)"),
            plotOutput("confusionMatrix_roc_svm"),
            style = "background-color:white;"
          ),
          tabPanel(
            "Logistic Regression",
            h4("Confusion Matrix (Logistic Regression)"),
            plotOutput("confusionMatrix_roc_lr"),
            style = "background-color:white;"
          )
        ),
        style = "background-color:white;"
      )
      
    })
    
    observeEvent(input$inn,{
      choixTarget$choix<-input$out
      
      if(input$inn!="Not Selected"){
        
        observeEvent(input$Slider1,{
          train_test_calcul()
          
          train=train_test$train
          test=train_test$test
          #print(input$Slider1)
          
          agr=aggregate(train[[choixTarget$choix]], by=list(train[[choixTarget$choix]]), FUN=length)
          # print(agr)
          
          print(nrow(agr))
          print('********************************************************************')
          s=""
          #La proportion de la classe 0 est:  70% et la classe 1 contient 30%
          for(i in seq(nrow(agr))){
            s=paste0(s, " La proportion de la classe ", agr[i,1])
            s=paste0(s," dans le train est ", round(  (agr[i,2]/nrow(train) *100),1), "%")
          }
          print("4444444444444")
          print(s)
          
          output$desequilibre <- renderText(HTML(s))
          
        })
        
      }
    })
    
    
    
    
    
    train_test<-reactiveValues(train=data.frame,test=data.frame)
    train_test_calcul<-reactive({
      
      dd<-as.data.frame(data())
      # d$Over18 <- NULL
      #  d$EmployeeCount <- NULL
      # d$StandardHours <- NULL
      #d$EmployeeNumber <- NULL
      
      d<-as.data.frame(data()[[choixTarget$choix]])
      nom_col<-c(choixTarget$choix)
      for(k in input$inn){
        d<-cbind(d,as.data.frame(data()[[k]]))
        nom_col<-c(nom_col,k)
      }
      colnames(d) <- nom_col
      
      #print(d)
      # Next, change all the categorical variables to number as fllows,
      
      d[[choixTarget$choix]]<-as.integer(as.factor(d[[choixTarget$choix]])) - 1
      #var<-colnames(dd)
      
      for(k in input$inn){
        d[[k]]<-as.integer(as.factor(d[[k]]))
        
      }
      
      if(input$Slider1<30){
        nb=30*0.01
      } else if(input$Slider1>90){
        nb=80*0.01
      } else{
        nb=input$Slider1*0.01
      }
      
      # Create data for training and test
      set.seed(0)
      tr.number<-sample(nrow(d),nrow(d)*nb)  
      
      
      # we split whole dataset into nb % training data and 1-nb % testing data
      train<-d[tr.number,]
      test<-d[-tr.number,]
      
      print(class(train))
      train_test$train<-train
      train_test$test<-test
      
      c(train,test)
      
    })
    
    
    
    
    ROCRpred_lr<-reactive({
      
      if(choixTarget$choix!="Not Selected" && input$inn!="Not Selected"){
        
        #resultat=train_test_calcul()
        train=train_test$train
        test=train_test$test
        print(class(train))
        print(class(test))
        agr=aggregate(train[[choixTarget$choix]], by=list(train[[choixTarget$choix]]), FUN=length)
        # print(agr)
        print(nrow(agr))
        print("JJJJJJJJJJJJJJJJJJ")
        print('********************************************************************')
        s=""
        for(i in seq(nrow(agr))){
          print(agr[i,2])
        }
        
        print("__________________________________________o")
        print(train)
        print("__________________________________________o")
        # output$desequilibre <- renderText(HTML("Veuillez selectionner des variables\n"))
        
        
        
        # split dataset
        train_Y = as.numeric(train[[choixTarget$choix]])
        train[[choixTarget$choix]]<-NULL
        test_Y = test[[choixTarget$choix]]
        test[[choixTarget$choix]]<-NULL
        
        # numericize training and testing data
        train[] <- lapply(train, as.numeric)
        test[] <- lapply(test, as.numeric)
        
        
        
        train[[choixTarget$choix]]<-train_Y
        formulaa=as.formula(paste(choixTarget$choix,"~."))
        # Training a SVM 
        svm_model<-svm(formulaa,        
                       type="C-classification",   
                       gamma=0.001668101,        
                       cost=35.93814,             
                       data=train,
                       cross=3,                   
                       probability = TRUE       
        )
        
        # Predicting the results using testing dataset
        
        print("11111111122222")
        print(test_Y)
        
        
        # Training a logistic regression model
        LR_model <- glm(formulaa,family=binomial(link='logit'),data=train)
        
        # Predicting the results using testing dataset
        LR_model.predict <- predict(LR_model, test, type = "response")
        
        # Create a prediction object using previously saved results
        
        ROCRpred_lr <- prediction(LR_model.predict, test_Y)
        print("lfffffffffffffff")
        print(levels(factor(test_Y)))
        
        
        # draw Logistic regression confusion matrix
        output$confusionMatrix_roc_lr<-renderPlot({
          roc_auc_lr <- performance(ROCRpred_lr, measure = "auc")
          roc_perf_lr <- performance(ROCRpred_lr, 'tpr','fpr')                    
          roc_cut <- get_cutoff_point(roc_perf_lr, xintercept_roc())
          roc_pred_values_lr <- ifelse(LR_model.predict > roc_cut,1,0)
          roc_cm_lr <- suppressWarnings(confusionMatrix(data = factor(roc_pred_values_lr), reference = factor(test_Y)))
          draw_confusion_matrix(roc_cm_lr, roc_auc_lr@y.values, "#69BE28")
          
          
        })
        
        ROCRpred_lr}
    })
    
    
    
    ROCRpred_svm<-reactive({
      #d <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
      if(choixTarget$choix!="Not Selected" && input$inn!="Not Selected"){d<-as.data.frame(data())
      
      # resultat=train_test_calcul()
      train=train_test$train
      test=train_test$test
      # split dataset
      train_Y = as.numeric(train[[choixTarget$choix]])
      train[[choixTarget$choix]]<-NULL
      test_Y = test[[choixTarget$choix]]
      test[[choixTarget$choix]]<-NULL
      
      # numericize training and testing data
      train[] <- lapply(train, as.numeric)
      test[] <- lapply(test, as.numeric)
      
      
      
      
      
      
      train[[choixTarget$choix]]<-train_Y
      formulaa=as.formula(paste(choixTarget$choix,"~."))
      # Training a SVM 
      svm_model<-svm(formulaa,        
                     type="C-classification",   
                     gamma=0.001668101,        
                     cost=35.93814,             
                     data=train,
                     cross=3,                   
                     probability = TRUE       
      )
      
      # Predicting the results using testing dataset
      # Obtain the predicted class 0/1
      svm_model.predict<-predict(svm_model, test, probability=TRUE) 
      
      # Obtain the predicted probability for class 0/1
      svm_model.prob <-attr(svm_model.predict,"probabilities")
      
      
      
      # Create a prediction object using previously saved results
      
      ROCRpred_svm <- prediction(svm_model.prob[,2], test_Y)
      
      
      
      # draw SVM confusion matrix
      output$confusionMatrix_roc_svm<-renderPlot({
        roc_auc_svm <- performance(ROCRpred_svm, measure = "auc")
        roc_perf_svm <- performance(ROCRpred_svm, 'tpr','fpr')                  
        roc_cut <- get_cutoff_point(roc_perf_svm, xintercept_roc())
        roc_pred_values_svm <- ifelse(svm_model.prob[,2] > roc_cut,1,0)
        roc_cm_svm <- suppressWarnings(confusionMatrix(data = factor(roc_pred_values_svm), reference = factor(test_Y)))
        draw_confusion_matrix(roc_cm_svm, roc_auc_svm@y.values, "#FF8849")
      })
      
      print("YYYyYYYYYYYYy")
      print(levels(factor(test_Y)))
      
      ROCRpred_svm
      }
    })
    
    
    
    
    
    
    ROCRpred_xgb<-reactive({
      
      if(choixTarget$choix!="Not Selected" && input$inn!="Not Selected"){ #d <- read.csv("WA_Fn-UseC_-HR-Employee-Attrition.csv")
        
        train=train_test$train
        test=train_test$test
        
        
        
        # split dataset
        train_Y = as.numeric(train[[choixTarget$choix]])
        train[[choixTarget$choix]]<-NULL
        test_Y = test[[choixTarget$choix]]
        test[[choixTarget$choix]]<-NULL
        
        # numericize training and testing data
        train[] <- lapply(train, as.numeric)
        test[] <- lapply(test, as.numeric)
        
        
        
        
        
        
        # Construct xgb.DMatrix object from training matrix
        dtrain <- xgb.DMatrix(as.matrix(train), label = train_Y)
        
        # Create a list of parameters for XGBoost model
        param <- list(max_depth=3, 
                      
                      eta = 0.3,
                      objective='binary:logistic',
                      eval_metric = 'auc')
        
        # Training a XGBoost model using training dataset and chosen parameters
        bst <- xgb.train(param, nrounds = 82, dtrain)
        
        # Predicting the results using testing dataset
        pred.xgb <- predict(bst, as.matrix(test))
        
        train[[choixTarget$choix]]<-train_Y
        formulaa=as.formula(paste(choixTarget$choix,"~."))
        
        
        
        
        # Create a prediction object using previously saved results
        ROCRpred_xgb <- prediction(pred.xgb, test_Y)
        
        
        # draw XGBoosting confusion matrix
        output$confusionMatrix_roc_xgb<-renderPlot({
          roc_auc_xgb <- performance(ROCRpred_xgb, measure = "auc")  #obtain auc from @performance
          roc_perf_xgb <- performance(ROCRpred_xgb, 'tpr','fpr')  #obtain tpr and fpr from @performance                   
          roc_cut <- get_cutoff_point(roc_perf_xgb, xintercept_roc()) #obtain the cutoff probability
          roc_pred_values_xgb <- ifelse(pred.xgb > roc_cut,1,0) #classify using cutoff probability
          roc_cm_xgb <- suppressWarnings(confusionMatrix(data = factor(roc_pred_values_xgb), reference = factor(test_Y))) #obtain confusion matrix
          draw_confusion_matrix(roc_cm_xgb, roc_auc_xgb@y.values, "#3DB7E4")  #Draw confusion matrix plot
        })
        ROCRpred_xgb}
    })
    
    
    
    
    
    
    
    
    
    
    #XGBoost roc data
    roc_perf_xgb <- reactive({performance(ROCRpred_xgb(), 'tpr','fpr')})                  
    roc_xgb.data <- reactive({
      data.frame(fpr=unlist(roc_perf_xgb()@x.values),
                 tpr=unlist(roc_perf_xgb()@y.values), model="XGBoost")
    })
    
    #SVM roc data
    roc_perf_svm <- reactive({performance(ROCRpred_svm(), 'tpr','fpr')})                  
    roc_svm.data <- reactive({
      data.frame(fpr=unlist(roc_perf_svm()@x.values),
                 tpr=unlist(roc_perf_svm()@y.values), model="SVM")
    })
    
    
    #Logistic Regression roc data
    roc_perf_lr <-reactive({performance(ROCRpred_lr(), 'tpr','fpr')})                  
    roc_lr.data <- reactive({
      data.frame(fpr=unlist(roc_perf_lr()@x.values),
                 tpr=unlist(roc_perf_lr()@y.values), model="LR")
    })
    
    
    cols <- c("XGBoost" = "#3DB7E4", "SVM" = "#FF8849", "Logistic Regression" = "#69BE28")
    
    xintercept_roc <- reactive({
      input$thresh_roc
    })
    
    xintercept_precision <- reactive({
      input$thresh_precision
    })
    
    xintercept_recall <- reactive({
      input$thresh_recall
    })
    
    fct_reeq<-reactive({
      prop_v<-c()
      prop_n<-c()
      train=train_test$train
      agr=aggregate(train[[choixTarget$choix]], by=list(train[[choixTarget$choix]]), FUN=length)
      #La proportion de la classe 0 est:  70% et la classe 1 contient 30%
      max<-0
      max_n<-""
      
      #0->20% et 1->70% du coup on aura prop
      for(i in seq(nrow(agr))){
        prop_n<-c(prop_n,agr[i,1])
        prop_v<-c(prop_v,agr[i,2])
        if(max<agr[i,2]){
          max<-agr[i,2]
          max_n<-agr[i,1]
        }
        
      }
      min_n<-""
      min<-0
      if(prop_n[1]==max_n){
        min_n<-prop_n[2]
        min<-prop_v[2]
      }else{
        min_n<-prop_n[1]
        min<-prop_v[1]
      }
      
      p_max<-round(  (agr[i,2]/nrow(train[choixTarget$choix]) *100),1)
      
      print(p_max)
      if(p_max>60){
        n=round(nrow(train[choixTarget$choix])*(p_max-60)*0.01,0)
        print(n)
        for(i in seq(nrow(train[choixTarget$choix]))){
          
          if(n>0){
            if(train[i,choixTarget$choix]==as.numeric(max_n)){
              
              print( train[i,choixTarget$choix])
              train[i,choixTarget$choix]<-as.numeric(min_n)
              print( train[i,choixTarget$choix])
              n=n-1
            }
            
          }
        }
      }
      train_test$train<-train
      
      
    })
    
    
    
    observeEvent(input$butt1,{
      print("7777777777777777777777777777777777777777!!!!!!")
    
      if(length(which(is.na(data()),arr.ind=TRUE))!=0){
        observe(output$text1 <- renderText(HTML("Presence de valeurs manquantes: Veuillez les supprimer dans l'onglet ")))
      }else if(choixTarget$choix=="Not Selected" || input$inn=="Not Selected"){
        observe(output$text1 <- renderText(HTML("Veuillez selectionner des variables")))
      } else{
        
        if(input$deseq=="oui"){
          
          #temporaire<- ovun.sample(formula=okok, data=train, N=nrow(train), seed=1, method="both")$data
          
          fct_reeq()
          
          #train_test$test <- ovun.sample(as.formula(paste0(ch,"~ .")), data=test,
          #                                 N=nrow(test), 
          #                               seed=1, method="both")$data
          
          
        }
        
        
        train=train_test$train
        test=train_test$test
        agr=aggregate(train[[choixTarget$choix]], by=list(train[[choixTarget$choix]]), FUN=length)
        # print(agr)
        
        s=""
        #La proportion de la classe 0 est:  70% et la classe 1 contient 30%
        for(i in seq(nrow(agr))){
          s=paste0(s, " La proportion de la classe ", agr[i,1])
          s=paste0(s," dans le train est ", round(  (agr[i,2]/nrow(train[choixTarget$choix]) *100),1), "%")
        }
        
        
        output$desequilibre <- renderText(HTML(s))
        #
        

        observe(output$text1 <- renderText(HTML(" ")))
        output$plot_roc<-renderPlot({ggplot() + 
            geom_line(data = roc_xgb.data(), aes(x=fpr, y=tpr, colour = "XGBoost")) + #set XGBoost roc curve
            geom_line(data = roc_svm.data(), aes(x = fpr, y=tpr, colour = "SVM")) + #set SVM roc curve
            geom_line(data = roc_lr.data(), aes(x = fpr, y=tpr, colour = "Logistic Regression")) + 
            #set LR roc curve
            geom_vline(xintercept = xintercept_roc(), color = "red", linetype=2) + theme_bw() + #set themes
            scale_colour_manual(name = "Models", values = cols) + 
            xlab("False Positive Rate") +
            ylab("True Positive Rate") +
            theme(legend.position = c(0.8, 0.2), 
                  legend.text = element_text(size = 15), 
                  legend.title = element_text(size = 15))
        })
      }
    })
    
    
    
    
    
    
    
    
    
    
    
    # Define a function to obtain the cutoff probability
    # @perf is a S4 object gotten from @performance function
    # @threshold is the targeted fpr
    # In the ShinyApp, users can adjust the threshold by themselves and
    # obtain different confusion matrix accordingly. Here, we always set
    # threshold = 0.5 just for illustration.
    get_cutoff_point <- function(perf, threshold)
    {
      cutoffs <- data.frame(fpr=perf@x.values[[1]], tpr=perf@y.values[[1]])
      cutoffs <- cutoffs[order(cutoffs$tpr, decreasing=TRUE),]
      cutoffs <- subset(cutoffs, fpr <= threshold)
      if(nrow(cutoffs) == 0){ return(1.0)}
      else return(cutoffs[1, 1])
    }
    
    # ROCRpred_xgb@cutoffs[[1]]
    
    #-------------------------------------------------------
    # Define a function to draw a confusion matrix plot
    # @cm is a confusion matrix obtained from @confusionMatrix function
    # @auc is the auc value obtained from @performance function
    # @color is the kind of color you want for true positive and true negative areas
    # In this function, we also add in accuracy information which calculates the
    # overall performance of model
    draw_confusion_matrix <- function(cm, auc, color) {
      
      layout(matrix(c(1,1,2)))
      par(mar=c(0,0.1,1,0.1))
      plot(c(125, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
      
      # create the matrix 
      rect(150, 430, 240, 370, col=color)
      text(195, 435, '0', cex=1.2)
      rect(250, 430, 340, 370, col='white')
      text(295, 435, '1', cex=1.2)
      text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
      text(245, 450, 'Actual', cex=1.3, font=2)
      rect(150, 305, 240, 365, col='white')
      rect(250, 305, 340, 365, col=color)
      text(140, 400, '0', cex=1.2, srt=90)
      text(140, 335, '1', cex=1.2, srt=90)
      
      # add in the cm results 
      res <- as.numeric(cm$table)
      text(195, 400, res[1], cex=1.6, font=2, col='white')
      text(195, 335, res[2], cex=1.6, font=2, col='black')
      text(295, 400, res[3], cex=1.6, font=2, col='black')
      text(295, 335, res[4], cex=1.6, font=2, col='white')
      
      # add in the specifics 
      plot(c(0, 100), c(0, 50), type = "n", xlab="", ylab="", main = "", xaxt='n', yaxt='n')
      
      # add in the accuracy information 
      
      text(25, 30, "AUC", cex=1.8, font=2)
      text(25, 20, round(as.numeric(auc), 3), cex=1.8)
      
      text(75, 30, names(cm$overall[1]), cex=1.8, font=2)
      text(75, 20, round(as.numeric(cm$overall[1]), 3), cex=1.8)
      recall1=(res[1]+res[4])/(res[1]+res[3]+res[4]+res[2])
      
      text(25, 10, "Recall", cex=1.8, font=2)
      text(25, 2,recall1 , cex=1.8)
      
      f1score=2*( round(as.numeric(cm$overall[1]))*recall1)/( round(as.numeric(cm$overall[1]))+recall1)
      
      text(75, 10, "F-score", cex=1.8, font=2)
      text(75, 2,f1score, cex=1.8)
    }
    #-------------------------------------------------------
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    type_stat_f<-function(x){
      newtype<-c()
      k=1
      for(i in x){
        if(i=="numeric"){
          oko$types[k]="quantitative continue"
          newtype<-c(newtype,"quantitative continue")
          
        } else {
          oko$types[k]="qualitative nominale"
          newtype<-c(newtype,"qualitative nominale")
        }
        k=k+1
      }
      newtype
    }
    
    
    nomvar<-reactiveValues(names=c())
    Types<-reactiveVal({NULL})
    observeEvent(input$MonFichier, {
      req(input$MonFichier)#si j'ai choisi mon fichier
      
      d1<-as.data.frame(data())
      Nom_variable<-c()
      Type<-sapply(d1, class)
      for(i in colnames(d1)){
        Nom_variable<-c(Nom_variable,i)
      }
      typest<-lestypes$types
      # for(i in seq_len(11)){
      #  typest<-c(typest,"quantitative discrete")
      # }
      selecttype<-c()
      k=1
      for(ii in typest){
        selecttype<-c(selecttype,as.character(selectInput(inputId=paste0("row_select_", k), label=NULL,selected = ii, choices=c("Qualitative nominale"="qualitative nominale", "qualitative ordinal" = "Qualitative ordinal","quantitative discrete" = "quantitative discrete","quantitative continue" = "quantitative continue"))))
        k=k+1
      }
      d2<-data.frame(Nom_variable,Type)
      setDT(d2)  
      # d2[, row_id := paste0("row_select_", .I)][, select := as.character(selectInput(inputId=row_id, label=NULL, choices=c(yes=TRUE, no=FALSE))), by = row_id]
      # d2[, row_id := paste0("row_select_", .I)][, select := as.character(selectInput(inputId=row_id, label=NULL, choices=c("Qualitative nominale"="qualitative nominale", "qualitative ordinal" = "Qualitative ordinal","quantitative discrete" = "quantitative discrete","quantitative continue" = "quantitative continue"))), by = row_id]
      
      d2<-data.frame(d2,lestypes$types,selecttype)
      print("$$$$$$$$$$$$")
      print(lestypes$types)
      print("$$$$$$$$$$$$")
      Types(d2)
    })
    
    observeEvent(input$sauvegarde, {
      rvs$observers = lapply(
        1, 
        function(i) {
          observeEvent(input[["sauvegarde"]], 
                       changerr()
          )
        }
      )
    })
    
    ##########################################################################################
    
    
    
    
    
    lestypes = reactiveValues(types = c()) 
    
    
    
    
    
    ##########################################################################################
    output$monTexte <- renderText({ "LE DATASET:"})
    output$files <- renderTable({head(data())})
    output$tabType<-renderTable({Types()})
    
    
    
    
    output$myTableOutput <- DT::renderDataTable({
      datatable(as.data.table(Types()), escape = FALSE, options = list(
        preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'),
        drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); }')
      ))
    })
    
    # output$mySelection <- renderUI({
    #   HTML(paste0(as.data.table(Types())$row_id, ": ", lapply(as.data.table(Types())$row_id, function(x){input[[x]]}), collapse = "<br>"))
    # })
    
    rvs = reactiveValues(buttons = list(), observers = list())
    
    observeEvent(input$sauvesgarde,{
      print("-------------")
      print(input$row_select_1)
      print("-------------")
      # lestypes$types<-newtypes
      #rvs$observers = lapply(
      # 1, 
      #function(i) {
      # observeEvent(input[["sauvegarde"]], 
      #             changer()
      #)
      #}
      #)
      
      # #l = seq_len(length(colnames(data()))) 
    }
    )
    
    
    
    changerr<-reactive({
      
      d1<-as.data.frame(data())
      d2<-data()
      nom_col<-colnames(data())
      Nom_variable<-c()
      Type<-sapply(d1, class)
      for(i in colnames(d1)){
        Nom_variable<-c(Nom_variable,i)
      }
      
      compteur=1
      for(o in seq_len(length(lestypes$types))){
      
        #print("kdjfkdjfkdjfkdjfkkfdjf-----------")
        #print(nom_col[compteur])
        #print(d2[[nom_col[compteur]]])
        
      #  print("kdjfkdjfkdjfkdjfkkfdjf-----------")
        observeEvent(input[[paste0("row_select_", compteur)]],{
         
          
          print(input[[paste0("row_select_", compteur)]])
          print("IIIIIIIIIIIIIII")
          print(is.null(input[[paste0("row_select_", compteur)]]))
          if(is.null(input[[paste0("row_select_", compteur)]])==FALSE){
            #lestypes$types[compteur]<-input[[paste0("row_select_", compteur)]]
            oko$types[[compteur]]=input[[paste0("row_select_", compteur)]]
            lestypes$types[[compteur]]<-input[[paste0("row_select_", compteur)]]
            
            typesAjour$types[[compteur]]=input[[paste0("row_select_", compteur)]]
            print("OOOOOOOOOOOOOOOKKK222")
            if(input[[paste0("row_select_", compteur)]]=="qualitative nominale" || input[[paste0("row_select_", compteur)]]=="qualitative ordinal" ){
                as.factor(d2[[nom_col[compteur]]])
              print("DDDDDDDDDDDDDDDDDDDDDDDDDDDD888154654")
              print("DDDDDDDDDDDDDDDDDDDDDDDDDDDD888154654")
            }
            
            
          }
        })
        print(compteur)
        compteur=compteur+1
      }
      data(d2)
      
      typest<-lestypes$types
      # for(i in seq_len(11)){
      #  typest<-c(typest,"quantitative discrete")
      # }
      selecttype<-c()
      k=1
      for(ii in typest){
        selecttype<-c(selecttype,as.character(selectInput(inputId=paste0("row_select_", k), label=NULL,selected = ii, choices=c("Qualitative nominale"="qualitative nominale", "qualitative ordinal" = "Qualitative ordinal","quantitative discrete" = "quantitative discrete","quantitative continue" = "quantitative continue"))))
        k=k+1
      }
      d2<-data.frame(Nom_variable,Type)
      setDT(d2)  
      # d2[, row_id := paste0("row_select_", .I)][, select := as.character(selectInput(inputId=row_id, label=NULL, choices=c(yes=TRUE, no=FALSE))), by = row_id]
      # d2[, row_id := paste0("row_select_", .I)][, select := as.character(selectInput(inputId=row_id, label=NULL, choices=c("Qualitative nominale"="qualitative nominale", "qualitative ordinal" = "Qualitative ordinal","quantitative discrete" = "quantitative discrete","quantitative continue" = "quantitative continue"))), by = row_id]
      
      d2<-data.frame(d2,lestypes$types,selecttype)
      print("$$$$$$$$$$$$")
      print(lestypes$types)
      print("$$$$$$$$$$$$")
      Types(d2)
    })
    
    
    oko= reactiveValues(types = list())
    typesAjour=reactiveValues(types=list())
    
    observeEvent(input$sauvegarde,{
      print(oko$types)
      nom_col<-colnames(data())
      d2<-as.data.frame(data())
      compteur=1
      for(i in seq_len(length(lestypes$types))){
        print(input[[paste0("row_select_", compteur)]])
        if(is.null(input[[paste0("row_select_", compteur)]])==FALSE){
          
          if( lestypes$types[[compteur]]!=input[[paste0("row_select_", compteur)]]){
            if(input[[paste0("row_select_", compteur)]]=="qualitative nominale" || input[[paste0("row_select_", compteur)]]=="Qualitative ordinal" ){
              d2[ , nom_col[1]] <- lapply(d2[ , nom_col[1]] , factor)
              print("DDDDDDDDDDDDDDDDDDDDDDDDDDDD888154654")
              print("DDDDDDDDDDDDDDDDDDDDDDDDDDDD888154654")
            } 
            
            if(input[[paste0("row_select_", compteur)]]=="quantitative continue" || input[[paste0("row_select_", compteur)]]=="quantitative discrete" ){
              d2[ , nom_col[1]] <- lapply(d2[ , nom_col[1]] , numeric)
              print("DDDDDDDDDDDDDDDDDDDDDDDDDDDD888154654")
              print("DDDDDDDDDDDDDDDDDDDDDDDDDDDD888154654")
            } 
            
          }
          
          #lestypes$types[compteur]<-input[[paste0("row_select_", compteur)]]
          oko$types[[compteur]]=input[[paste0("row_select_", compteur)]]
          lestypes$types[[compteur]]<-input[[paste0("row_select_", compteur)]]
          typesAjour$types[[compteur]]=input[[paste0("row_select_", compteur)]]
          
          #if(input[[paste0("row_select_", compteur)]]=="qualitative nominale" || input[[paste0("row_select_", compteur)]]=="qualitative ordinale" ){
            
          #}
         
           
        
          
        }
        compteur=compteur+1
      }
      print("------uuuuuuu-------")
      #as.factor(d2[nom_col[1]])
    
      #d2<-transform(d2, d2[nom_col[1]] = as.factor(d2[nom_col[1]]))
      #d2 <- d2[, d2[nom_col[1]] := as.numeric(d2[nom_col[1]])]
     
      data(d2)
      print("------uuuuuuu-------")
      #print("-------------")
      print(oko$types)
      print("--- types ---")
      print(lestypes$types)
      print("-------------")
      print(typesAjour$types)
      
      
      
      rvs$observers = lapply(
        1, 
        function(i) {
          observeEvent(input[["sauvegarde"]], 
                       changerr()
          )
        }
      )
      
      
      # #l = seq_len(length(colnames(data()))) 
    }
    )
    
    
    
    
    
    
    
    
    
    
    ################Correlation + Contingency######################
    output$Correlation <- renderPlot({
      numeric_var <- names(data())[sapply(data(), is.numeric)]
      a = setDT(data())[, cor(.SD), .SDcols = numeric_var]
      corr <- round(cor(a), 1)
      ggcorrplot(corr, hc.order = TRUE, type = "lower",lab = TRUE)
    })
    output$myoutputt4 <- renderUI({
      switch(input$dman_Preview3, 
             "Correlation" = fluidPage( plotOutput("Correlation")),
             
             "Contingency table" = fluidPage(
               fluidRow(
                 column(4,selectInput("colum", "Column Variable", choices = c(names(data())[sapply(data(), is.factor)]))),
                 column(4,selectInput("rowvar", label = "Row Variable", choices = c(names(data())[sapply(data(), is.factor)]))),
                 column(4, actionButton("run_butt", "Run Analysis", icon = icon("play")))),
               
               fluidRow(column(12,verbatimTextOutput('conting')))),
             "Visualisation" = fluidPage(
               fluidRow(
                 
                 column(4,selectInput("num_var_1", "Numerical Variable 1", choices = c(not_sel,names(data())[sapply(data(), is.numeric)]))),
                 column(4,selectInput("num_var_2", "Numerical Variable 2", choices = c(not_sel,names(data())[sapply(data(), is.numeric)]))),
                 column(4,selectInput("fact_var", "Factor Variable", choices = c(not_sel,names(data())[sapply(data(), is.factor)]))),
                 fluidRow( column(4, actionButton("run_button", "Run Analysis", icon = icon("play"))),
                           column(12, plotOutput("plot_1"))),
                 fluidRow(
                   column(width = 4, strong(textOutput("num_var_1_title"))),
                   column(width = 4, strong(textOutput("num_var_2_title"))),
                   column(width = 4, strong(textOutput("fact_var_title")))
                 ),
                 fluidRow(
                   column(width = 4, tableOutput("num_var_1_summary_table")),
                   column(width = 4, tableOutput("num_var_2_summary_table")),
                   column(width = 4, tableOutput("fact_var_summary_table"))
                 )
                 
               ) ))})
    
    
    # output$conting <- renderPrint({
    #     ligne <- (data()[,c(input$rowvar)])
    #     column <- (data()[,c(input$colum)])
    #     CrossTable(ligne, column, expected = TRUE)})
    
    # funct <- function(data, row, col){
    #   ligne <- (data()[,c(input$rowvar)])
    #   column <- (data()[,c(input$colum)])
    #   CrossTable(ligne, column, expected = TRUE)
    #   
    # }
    
    contingence <- eventReactive(input$run_butt,{
      ligne <- (data()[,c(input$rowvar)])
      column <- (data()[,c(input$colum)])
      CrossTable(ligne, column, expected = TRUE)
    })
    output$conting <- renderPrint(contingence())
    
    
    num_var_1 <- eventReactive(input$run_button,input$num_var_1)
    num_var_2 <- eventReactive(input$run_button,input$num_var_2)
    fact_var <- eventReactive(input$run_button,input$fact_var)
    
    create_num_var_table <- function(data, num_var){
      if(num_var != not_sel){
        col <- data()[,c(num_var)]
        col = as.numeric(col)
        statistic <- c("Min","mean", "median", "5th percentile", "95th percentile",
                       "Max")
        value <- c(round(min(col),2),round(mean(col),2), round(median(col),2),
                   round(quantile(col, 0.05),2), round(quantile(col, 0.95),2),
                   round(max(col),2))
        data.table(statistic, value)
      }
    }
    
    create_fact_var_table <- function(data, fact_var){
      if(fact_var != not_sel){
        freq_tbl <- lapply(setDT(data())[, .SD, .SDcols = fact_var], levels)
        freq_tbl1 <- lapply(setDT(data())[, .SD, .SDcols = fact_var], table)
        value <- c(freq_tbl, freq_tbl1 ) 
        freq_tbl <- setnames(setDT(value),c("factor_value",'names'))
        freq_tbl
      }
    }
    
    
    
    draw_plot_1 <- function(data, num_var_1, num_var_2, fact_var){
      if(fact_var!=not_sel){
        setDT(data)[,(fact_var):= as.factor(setDT(data)[,get(fact_var)])]
      }
      if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var != not_sel){
        ggplot(data = data,
               aes_string(x = num_var_1, y = num_var_2, color = fact_var)) +
          geom_point()
      }
      else if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var == not_sel){
        ggplot(data = data,
               aes_string(x = num_var_1, y = num_var_2)) +
          geom_point()
      }
      else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var != not_sel){
        ggplot(data = data,
               aes_string(x = fact_var, y = num_var_1)) +
          geom_violin()
      }
      else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var != not_sel){
        ggplot(data = data,
               aes_string(x = fact_var, y = num_var_2)) +
          geom_violin()
      }
      else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var == not_sel){
        ggplot(data = data,
               aes_string(x = num_var_1)) +
          geom_histogram()
      }
      else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var == not_sel){
        ggplot(data = data,
               aes_string(x = num_var_2)) +
          geom_histogram()
      }
      else if(num_var_1 == not_sel & num_var_2 == not_sel & fact_var != not_sel){
        ggplot(data = data,
               aes_string(x = fact_var)) +
          geom_bar()
      }
    }
    
    plot_1 <- eventReactive(input$run_button,{
      draw_plot_1(data(), num_var_1(), num_var_2(), fact_var())
    })
    output$plot_1 <- renderPlot(plot_1())
    
    # 1-d summary tables
    
    output$num_var_1_title <- renderText(paste("Num Var 1:",num_var_1()))
    
    num_var_1_summary_table <- eventReactive(input$run_button,{
      create_num_var_table(data(), num_var_1())
    })
    
    output$num_var_1_summary_table <- renderTable(num_var_1_summary_table(),colnames = FALSE)
    
    output$num_var_2_title <- renderText(paste("Num Var 2:",num_var_2()))
    
    num_var_2_summary_table <- eventReactive(input$run_button,{
      create_num_var_table(data(), num_var_2())
    })
    
    output$num_var_2_summary_table <- renderTable(num_var_2_summary_table(),colnames = FALSE)
    
    output$fact_var_title <- renderText(paste("Factor Var:",fact_var()))
    
    fact_var_summary_table <- eventReactive(input$run_button,{
      create_fact_var_table(data(), fact_var())
    })
    
    output$fact_var_summary_table <- renderTable(fact_var_summary_table(),colnames = FALSE)
    
    
    
    
    
    
    
}

shinyApp(ui = ui, server = server)