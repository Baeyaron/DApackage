#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#' @import car
#' @import datasets
#' @import agricolae
#' @import gplots
#' @import DT
#' @noRd
app_server <- function(input, output) {
  options(shiny.maxRequestSize=100*1024^2)
  scan <- function(string, sep)
  {
    re<-c()
    if(string=="")
      return(re)
    else
    {
      b <-strsplit(string,sep,fixed = T)
      for (i in 1:length(b[[1]]))
      {
        if( gregexpr(":",b[[1]][i])[[1]][1]==-1 )
        {
          re_t <- as.numeric(b[[1]][i])
          re <- c(re,re_t)
        }
        else
        {
          cut <- gregexpr(":",b[[1]][i])[[1]][1]
          re_f <- as.numeric(substr(b[[1]][i],1,cut-1))
          re_b <- as.numeric(substr(b[[1]][i],cut+1,nchar(b[[1]][i])))
          re_t <- re_f:re_b
          re <- c(re,re_t)
        }
      }
      return(re)
    }
  }

  #var view#
  var_view <- function(df)
  {
    sec <- rep(1:5,ceiling(ncol(df)/5))[1:ncol(df)]
    re<-c()
    if(ncol(df)<5)
      for(i in 1:ncol(df))
      {
        re_t <- cbind(Num = i,Var = colnames(df)[i])
        re <- cbind(re,re_t)
      }
    else
      for(i in 1:5)
      {
        Var <- colnames(df)[sec==i]
        re_t <- cbind(Num = seq(i,ncol(df),5),Var)
        if(length(seq(i,ncol(df),5))<ceiling(ncol(df)/5))
          re_t <- rbind(re_t,matrix("",nrow = 1,ncol = 2))
        re <- cbind(re,re_t)
      }
    return(re)
  }

  ####Tab Settings####
  ntabs <- 3
  tabnames <- c("setting", "results", "plots")
  tablabels <- c("Setting", "Download results", "Download plots")

  output$Sidebar <- renderUI({
    Menus <- vector("list", 2)
    Menus[[1]] <-   menuItem(tablabels[1], tabName = tabnames[1], icon = icon("gear"),selected = T)
    #    Menus[[2]] <-   menuItem(tablabels[2], tabName = tabnames[2], icon = icon("th"))
    #    Menus[[3]] <-   menuItem(tablabels[3], tabName = tabnames[3], icon = icon("dashboard"))

    Menus[[2]] <-   menuItem(c("Analysis and Download"), icon = icon("bar-chart-o"),
                             menuSubItem("Result and Download", tabName = "results"),
                             menuSubItem("Plots and Download ", tabName = "plots"))


    do.call(function(...) sidebarMenu(id = 'sidebarMenu', ...), Menus)
  })

  output$TABUI <- renderUI({
    Tabs <- vector("list", 3)
    Tabs[[1]] <- tabItem(tabName = tabnames[1],
                         fluidRow(
                           tabBox(
                             id = "method1",
                             title = "Design Types and Multiple Comparison Methods",
                             side = "right", height = "300px",
                             selectInput("method",
                                         "Choose the design type",
                                         choices = c("Completely randomized design"="Completely randomized design",
                                                     "Randomized block design"="Randomized block design"),
                                         selected = 1),
                             selectInput("method2",
                                         "Choose a multiple comparison method:",
                                         choices = c("SNK-q"="SNK-q",
                                                     "Bonferroni"="Bonferroni",
                                                     "LSD-t"="LSD-t"))

                           ),
                           tabBox(
                             title = "Variable Settings",
                             side = "right", height = "300px",
                             textInput(inputId="variable",
                                       label="Analyzing variable (input the column number):"),
                             textInput(inputId = "group",
                                       label = "Grouping variable (input the column number):"),
                             textInput(inputId="quzu",
                                       label="Block variable (input the column number):"))
                         ),
                         actionButton("act","Submit"),
                         h3(c("Column Numbers of  Variables")),
                         DTOutput('view_var',width = "50%",height = "auto"))
    Tabs[[2]] <- tabItem("results",
                         h3(c("Summary")),
                         tableOutput(outputId ="view1"),
                         h3("Multiple Comparisons"),
                         tableOutput(outputId ="view2"),
                         downloadButton(outputId ="downloadTable"),
                         width = 7)
    Tabs[[3]] <- tabItem("plots",
                         h3(c("Plots and Download")),
                         plotOutput(outputId = "Boxplot"),
                         downloadButton(outputId ="downloadPlot"),
                         width = 7)

    do.call(tabItems, Tabs)


  })

  output$title <- renderText(c("Summary"))
  observeEvent(input$file$datapath, {
    output$title <- renderText(c("Column numbers of variables"))
    output$view_var <- DT::renderDataTable(
      {
        if (is.null(input$file)) {
          return(NULL)
        } else {
          df <- read.csv(input$file$datapath, header = TRUE, sep = ",")
          return(var_view(df))
        }
      },
      options = list(lengthChange = FALSE),
      rownames = F
    )

    output$view1 <- renderTable(NULL)
    output$view2 <- renderTable(NULL)
    output$Boxplot <- renderPlot(NULL, width = 500, height = 600)
  })

  observeEvent(input$act,{
    df <- read.csv(input$file$datapath, header = TRUE, sep = ",")

    g <- input$group
    var <- input$variable
    quzu <- input$quzu

    g1 <- scan(g,",")
    var1 <- scan(var,",")
    quzu1 <- scan(quzu,",")

    group1 <- df[,g1]
    var2 <- df[,var1]
    var2 <-  as.matrix(var2,ncol = length(var1))
    quzu2 <- df[,quzu1]

    xtitle <- c()
    for (i in 1:ncol(var2)) {
      xtitle[i] <- paste(colnames(df[var1])[i])
    }
    ytitle <- paste(colnames(df[g1]))

    if(input$method == "Completely randomized design"){

      jieguo_anova <- list()
      jieguoF <- list()
      jieguoP <- list()

      for (i in 1:ncol(var2)) {
        jieguo_anova[[i]] <- aov(var2[,i]~as.factor(group1))
        jieguoF[[i]] <- summary(jieguo_anova[[i]])[[1]][1,4]
        jieguoP[[i]] <- summary(jieguo_anova[[i]])[[1]][1,5]
      }

      jieguoF1 <- unlist(jieguoF)
      jieguoP1 <- unlist(jieguoP)

      jieguoF2 <- c()
      jieguoP2 <- c()

      for (i in 1:length(jieguoF1)) {
        jieguoF2[i] <- sprintf("%0.4f",jieguoF1[i])
        jieguoP2[i] <- sprintf("%0.4f",jieguoP1[i])
      }

      jieguoF3 <- as.matrix(jieguoF2,ncol=1)
      jieguoP3 <- as.matrix(jieguoP2,ncol=1)


      jieguoDF1 <- summary(jieguo_anova[[1]])[[1]][1,1]+summary(jieguo_anova[[1]])[[1]][2,1]
      jieguoDF11 <- sprintf("%0.0f",jieguoDF1)
      jieguoDF2 <- summary(jieguo_anova[[1]])[[1]][1,1]
      jieguoDF21 <- sprintf("%0.0f",jieguoDF2)
      jieguoDF3 <- summary(jieguo_anova[[1]])[[1]][2,1]
      jieguoDF31 <- sprintf("%0.0f",jieguoDF3)

      jieguoSS1 <- summary(jieguo_anova[[1]])[[1]][1,2]+summary(jieguo_anova[[1]])[[1]][2,2]
      jieguoSS11 <- sprintf("%0.4f",jieguoSS1)
      jieguoSS2 <- summary(jieguo_anova[[1]])[[1]][1,2]
      jieguoSS21 <- sprintf("%0.4f",jieguoSS2)
      jieguoSS3 <- summary(jieguo_anova[[1]])[[1]][2,2]
      jieguoSS31 <- sprintf("%0.4f",jieguoSS3)

      jieguoMS2 <- summary(jieguo_anova[[1]])[[1]][1,3]
      jieguoMS21 <- sprintf("%0.4f",jieguoMS2)
      jieguoMS3 <- summary(jieguo_anova[[1]])[[1]][2,3]
      jieguoMS31 <- sprintf("%0.4f",jieguoMS3)

      fangcha <- list()
      fangcha1 <- list()
      for (i in 1:ncol(var2)) {
        fangcha[[i]] <- leveneTest(var2[,i] ~ as.factor(group1))
        fangcha1[[i]] <- fangcha[[i]]$`Pr(>F)`[1]
      }
      fangcha2 <- unlist(fangcha1)
      fangcha3 <- sprintf("%0.4f", fangcha2)
      fangcha4 <- matrix(fangcha3, ncol = 1)

      colnames <- c("Source","DF","Sum of Square","Mean Square","F value","P value","homogeneity of variance (Levene's test based on median)")
      rownames <- c("Model","Error","Corrected Total")

      k3 <- c(jieguoDF11,jieguoSS11,NA,NA,NA,NA)
      k1 <- c(jieguoDF21, jieguoSS21, jieguoMS21,jieguoF2[1],jieguoP2[1],fangcha4)
      k2 <- c(jieguoDF31, jieguoSS31, jieguoMS31,NA,NA,NA)
      k4 <- rbind(k1,k2,k3)
      k <- cbind(rownames,k4)
      colnames(k) <- colnames

      rownames11 <- colnames(df[var1])
      k11 <- cbind(rownames11,jieguoF3,jieguoP3)
      colnames(k11) <- c("Variable","F","P")
    }


    if(input$method == "Randomized block design"){

      jieguo_anova <- list()
      jieguoF1 <- list()
      jieguoP1 <- list()
      jieguoF2 <- list()
      jieguoP2 <- list()

      for (i in 1:ncol(var2)) {
        jieguo_anova[[i]] <- aov(var2[,i]~as.factor(group1)+as.factor(quzu2))
        jieguoF1[[i]] <- summary(jieguo_anova[[i]])[[1]][1,4]
        jieguoF2[[i]] <- summary(jieguo_anova[[i]])[[1]][2,4]
        jieguoP1[[i]] <- summary(jieguo_anova[[i]])[[1]][1,5]
        jieguoP2[[i]] <- summary(jieguo_anova[[i]])[[1]][2,5]
      }

      jieguoF11 <- unlist(jieguoF1)
      jieguoF21 <- unlist(jieguoF2)
      jieguoP11 <- unlist(jieguoP1)
      jieguoP21 <- unlist(jieguoP2)

      jieguoF12 <- c()
      jieguoF22 <- c()
      jieguoP12 <- c()
      jieguoP22 <- c()

      for (i in 1:length(jieguoF1)) {
        jieguoF12[i] <- sprintf("%0.4f",jieguoF11[i])
        jieguoF22[i] <- sprintf("%0.4f",jieguoF21[i])
        jieguoP12[i] <- sprintf("%0.4f",jieguoP11[i])
        jieguoP22[i] <- sprintf("%0.4f",jieguoP21[i])
      }

      jieguoF13 <- as.matrix(jieguoF12,ncol=1)
      jieguoF23 <- as.matrix(jieguoF22,ncol=1)
      jieguoP13 <- as.matrix(jieguoP12,ncol=1)
      jieguoP23 <- as.matrix(jieguoP22,ncol=1)

      jieguoDF1 <- summary(jieguo_anova[[1]])[[1]][1,1]
      jieguoDF1 <- sprintf("%0.0f",jieguoDF1)
      jieguoDF2 <- summary(jieguo_anova[[1]])[[1]][2,1]
      jieguoDF2 <- sprintf("%0.0f",jieguoDF2)
      jieguoDF3 <- summary(jieguo_anova[[1]])[[1]][3,1]
      jieguoDF3 <- sprintf("%0.0f",jieguoDF3)
      jieguoDF4 <- summary(jieguo_anova[[1]])[[1]][1,1]+summary(jieguo_anova[[1]])[[1]][2,1]+summary(jieguo_anova[[1]])[[1]][3,1]
      jieguoDF4 <- sprintf("%0.0f",jieguoDF4)

      jieguoSS1 <- summary(jieguo_anova[[1]])[[1]][1,2]
      jieguoSS1 <- sprintf("%0.4f",jieguoSS1)
      jieguoSS2 <- summary(jieguo_anova[[1]])[[1]][2,2]
      jieguoSS2 <- sprintf("%0.4f",jieguoSS2)
      jieguoSS3 <- summary(jieguo_anova[[1]])[[1]][3,2]
      jieguoSS3 <- sprintf("%0.4f",jieguoSS3)
      jieguoSS4 <- summary(jieguo_anova[[1]])[[1]][1,2]+summary(jieguo_anova[[1]])[[1]][2,2]+summary(jieguo_anova[[1]])[[1]][3,2]
      jieguoSS4 <- sprintf("%0.4f",jieguoSS4)


      jieguoMS1 <- summary(jieguo_anova[[1]])[[1]][1,3]
      jieguoMS1 <- sprintf("%0.4f",jieguoMS1)
      jieguoMS2 <- summary(jieguo_anova[[1]])[[1]][2,3]
      jieguoMS2 <- sprintf("%0.4f",jieguoMS2)
      jieguoMS3 <- summary(jieguo_anova[[1]])[[1]][3,3]
      jieguoMS3 <- sprintf("%0.4f",jieguoMS3)

      fangcha <- list()
      fangcha1 <- list()
      for (i in 1:ncol(var2)) {
        fangcha[[i]] <- leveneTest(var2[,i] ~ as.factor(group1))
        fangcha1[[i]] <- fangcha[[i]]$`Pr(>F)`[1]
      }
      fangcha2 <- unlist(fangcha1)
      fangcha3 <- sprintf("%0.4f", fangcha2)
      fangcha4 <- matrix(fangcha3, ncol = 1)

      colnames <- c("Source","DF","Sum of Square","Mean Square","F value","P value","homogeneity of variance (Levene's test based on median)")
      rownames <- c("Treat","Block","Error","Corrected Total")

      k1 <- c(jieguoDF1, jieguoSS1, jieguoMS1,jieguoF12[1],jieguoP12[1],fangcha4)
      k2 <- c(jieguoDF2, jieguoSS2, jieguoMS2,jieguoF22[1],jieguoP22[1],NA)
      k3 <- c(jieguoDF3,jieguoSS3,jieguoMS3,NA,NA,NA)
      k4 <- c(jieguoDF4,jieguoSS4,NA,NA,NA,NA)

      k5 <- rbind(k1,k2,k3,k4)
      k <- cbind(rownames,k5)

      colnames(k) <- colnames

      rownames11 <- colnames(df[var1])
      k11 <- cbind(rownames11,jieguoF13,jieguoF23,jieguoP13,jieguoP23)
      colnames(k11) <- c("Variable","F (treat)","P (treat)","F (block)","P (block)")
    }


    output$title <- renderText(c("Summary"))
    output$view_var <- DT::renderDataTable(NULL,options = list(lengthChange = FALSE))


    output$view1 <- renderTable(k, rownames = F)
    output$view2 <- renderTable(
      { group1 <- as.factor(group1)
      jieguo_anova111 <- aov(var2[,1]~group1)

      if(input$method2 == "SNK-q"){
        out <- SNK.test(jieguo_anova111,"group1",console = T)
        aa1 <- print(SNK.test(jieguo_anova111,"group1",group = FALSE))
        d <- aa1$comparison
        Groups  <- rownames(d)
        d2 <- cbind(Groups,d)
      }

      if(input$method2 == "Bonferroni"){
        out <- LSD.test(jieguo_anova111,"group1",p.adj = "bonferroni")
        d <- out$groups
        Groups  <- rownames(d)
        d2 <- cbind(Groups,d)
        colnames <- c("Group","Mean","Bonferroni Grouping")
        colnames(d2) <- colnames
      }

      if(input$method2 == "LSD-t"){
        out <- LSD.test(jieguo_anova111,"group1",p.adj = "none")
        d <- out$groups
        Groups  <- rownames(d)
        d2 <- cbind(Groups,d)
        colnames <- c("Group","Mean","LSD Grouping")
        colnames(d2) <- colnames
      }
      d2
      })

    output$Boxplot <- renderPlot({

      par(no.readonly = TRUE)
      par(mfrow=c(1,2))

      if (input$variable == ""){
        plot <- "Please input the correct column number"
      }

      if (input$variable != ""){
        for (i in 1:ncol(var2)) {
          boxplot(var2[,i]~as.factor(group1),data=df,xlab = ytitle,ylab =xtitle,col=c("green"))
        }
      }
    })

    output$downloadTable <- downloadHandler(
      filename = function() {"ANOVA.xlsx"},
      content = function(file) {
        re_tab <- list("ANOVA" = k11)
        openxlsx::write.xlsx(re_tab, file = file, rowNames=T)
      }
    )

    output$downloadPlot <- downloadHandler(
      filename = function() {"boxplot.pdf"},
      content = function(file) {
        pdf(file, onefile = TRUE)
        par(no.readonly = TRUE)
        par(mfrow=c(2,2))

        if (input$variable == ""){
          plot <- "Please input the correct column number"
        }

        if (input$variable != ""){
          for (i in 1:ncol(var2)) {
            plot <- boxplot(var2[,i]~as.factor(group1),data=df,xlab = ytitle,ylab =xtitle,col=c("green"))
          }
        }

        dev.off()
      })
  })
}
