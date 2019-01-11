library(shiny)
library(colourpicker)
library(tidyverse)
library(patchwork)

server <- function(input, output) {
    
    userdata <- reactive({
      infile <- input$datafile
      if (is.null(infile)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      read.csv(infile$datapath) ## we actually want to force factors, all we do is summary!
    })

    ### Reactive variables for conditionalPanel interaction in ui.R
    output$colornodetype <- reactive( input$colornode == "type" )
    outputOptions(output, "colornodetype", suspendWhenHidden = FALSE)
    


#    ## Simulate, plot, summarize reactive to "Run Simulation!"
#    observeEvent(input$go,  {
#        
#        
#        
#    })
#  
#    
#
#    ## UI: Which variables to plot?
#    output$selectvars <- renderUI({
#    
#        
#        input$dataviz
#        if (is.null(userdata())) return(NULL)
#        dfnames <- names(userdata())
#        
#        switch(input$dataviz, 
#                "scatter" = list(
#                                selectInput("xvar", "Select the X-axis (aka independent/predictor) variable:",dfnames),
#                                selectInput("yvar", "Select the Y-axis (aka dependent/response) variable:",dfnames),
#                                checkboxInput(inputId = "bestfit",
#                                    label = "Show the line of best fit (i.e. regression line)?",
#                                    value = TRUE,
#                                    width = NULL
#                                )),         
#                 "scatter2" = list(
#                                selectInput("xvar", "Select the X-axis (aka independent/predictor) variable:",dfnames),
#                                selectInput("yvar", "Select the Y-axis (aka dependent/response) variable:",dfnames),
#                                selectInput("catvar", "Select the categorical variable to show in the scatterplot:",dfnames)                                
#                                ),         
#                "quant"   = selectInput("quantvar", "Select the quantitative variable to visualize:",dfnames),
#                "multquant" = list(
#                                    selectInput("quantvar", "Select the quantitative variable to visualize:",dfnames),
#                                    selectInput("quantvar_cat", "Select the categorical variable to visualize the quantitative variable across:",dfnames)
#                                ),
#                "multquant2" = list(
#                                    selectInput("quantvar", "Select the quantitative variable to visualize:",dfnames),
#                                    selectInput("quantvar_cat", "Select the first categorical variable to visualize across:",dfnames),
#                                    selectInput("quantvar_cat2", "Select the second categorical variable to visualize across:",dfnames)
#                                ),
#                "counts"   = selectInput("countvar", "Select the categorical variable to visualize its count:",dfnames),
#                "counts2"  = list(
#                                    selectInput("countvar", "Select the independent (aka explanatory) categorical variable:",dfnames),
#                                    selectInput("countvar2", "Selection the dependent (aka response) categorical variable:",dfnames)
#                                 )                              
#            )
#    })
# 
# 
# 
#    ## UI: Summary table of *full* upload
#    output$summary <- renderPrint({
#        req(userdata())
#        if (is.null(userdata())) return(NULL)
#         summary(userdata())
#    })
#
#
#
#
#    ###############################################################################################
#    ############################## Functions for making individual plots ##########################
#    isolate_data <- function(){
#        isolate(userdata()) 
#    }        
#
#
#    isolate_color <- function(){
#        isolate(input$yaycolor)
#    }
#        plot_histogram <- function()
#    {
#        finaldata <- isolate_data()
#        thecolor  <- isolate_color()
#        quantvar2 <- as.symbol( isolate(input$quantvar) )
#        p <- ggplot(finaldata, aes(x = !!quantvar2)) + geom_histogram(color = "black", fill = thecolor)
#        p    
#    }
#
#    plot_density <- function()
#    {
#        finaldata <- isolate_data()
#        thecolor  <- isolate_color()
#        quantvar2 <- as.symbol( isolate(input$quantvar) )
#        p <- ggplot(finaldata, aes(x = !!quantvar2)) + geom_density(color = "black", fill = thecolor, alpha=0.8)
#        p    
#    }
#    plot_boxplot <- function()
#    {
#        finaldata <- isolate_data()
#        thecolor  <- isolate_color()
#        quantvar2 <- as.symbol( isolate(input$quantvar) )
#        p <- ggplot(finaldata, aes(y = !!quantvar2)) + geom_boxplot(fill = thecolor) + theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
#        p    
#    }
#
#    plot_jitter <- function()
#    {
#        finaldata <- isolate_data()
#        thecolor  <- isolate_color()
#        quantvar2 <- as.symbol( isolate(input$quantvar) )
#        p <- ggplot(finaldata, aes(y = !!quantvar2)) + geom_jitter(size=3, aes(x=""), color = thecolor, width=0.075) + xlab("")
#        p    
#    }
#    plot_barquant <- function()
#    {
#        finaldata <- isolate_data()
#        thecolor  <- isolate_color()
#        quantvar2 <- as.symbol( isolate(input$quantvar) )
#        p <- ggplot(finaldata, aes(x="")) + stat_summary(aes(y = !!quantvar2), fun.y = "mean", geom = "bar", width=0.1, fill = thecolor) + stat_summary(aes(y = !!quantvar2), fun.data = "mean_se", geom = "errorbar", width = 0.05) + xlab("") + scale_y_continuous(expand = c(0,0))
#        p    
#    }
#
#
#    plot_multbox <- function()
#    {
#        finaldata <- isolate_data()
#        quantvar2 <-  as.symbol(isolate(input$quantvar))
#        quantvar_cat2 <-  as.symbol(isolate(input$quantvar_cat))
#        p <- ggplot(finaldata, aes(x = !!quantvar_cat2, y = !!quantvar2, fill = factor(!!quantvar_cat2))) + geom_boxplot() + scale_fill_hue(name = quantvar_cat2, l=45)
#        p
#    }
#    plot_multbox2 <- function()
#    {
#        finaldata <- isolate_data()
#        quantvar2 <-  as.symbol(isolate(input$quantvar))
#        quantvar_cat2 <-  as.symbol(isolate(input$quantvar_cat))
#        quantvar_cat22 <-  as.symbol(isolate(input$quantvar_cat2))
#        p <- ggplot(finaldata, aes(x = !!quantvar_cat2, y = !!quantvar2, fill = factor(!!quantvar_cat22))) + geom_boxplot() + scale_fill_hue(name = quantvar_cat22, l=45)
#        p
#    }
#    plot_multdensity <- function()
#    {
#        finaldata <- isolate_data()
#        quantvar2 <-  as.symbol(isolate(input$quantvar))
#        quantvar_cat2 <-  as.symbol(isolate(input$quantvar_cat))
#        p <- ggplot(finaldata, aes(x = !!quantvar2, fill = factor(!!quantvar_cat2))) + geom_density(color = "black", alpha=0.5) + scale_fill_hue(name = quantvar_cat2, l=45)
#        p
#    }
#    plot_multjitter <- function()
#    {
#        finaldata <- isolate_data()
#        quantvar2 <-  as.symbol(isolate(input$quantvar))
#        quantvar_cat2 <-  as.symbol(isolate(input$quantvar_cat))
#        p <- ggplot(finaldata, aes(x = !!quantvar_cat2, y = !!quantvar2, color = factor(!!quantvar_cat2))) + geom_jitter(size=3, width = 0.1) + scale_color_hue(name = quantvar_cat2, l=45)
#        p
#    }
#    plot_multjitter2 <- function()
#    {
#        finaldata <- isolate_data()
#        quantvar2 <-  as.symbol(isolate(input$quantvar))
#        quantvar_cat2 <-  as.symbol(isolate(input$quantvar_cat))
#        quantvar_cat22 <-  as.symbol(isolate(input$quantvar_cat2))
#        p <- ggplot(finaldata, aes(x = !!quantvar_cat2, y = !!quantvar2, color = factor(!!quantvar_cat22))) + geom_jitter(size=3, position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.8)) + scale_color_hue(name = quantvar_cat22, l=45)
#        p
#    }
#    plot_multbarquant <- function()
#    {
#        finaldata <- isolate_data()
#        quantvar2 <-  as.symbol(isolate(input$quantvar))
#        quantvar_cat2 <-  as.symbol(isolate(input$quantvar_cat))
#        p <- ggplot(finaldata) + stat_summary_bin(aes(x = factor(!!quantvar_cat2), y = !!quantvar2, fill = factor(!!quantvar_cat2)), fun.y = "mean", geom = "bar") + stat_summary_bin(aes(factor(!!quantvar_cat2), y = !!quantvar2), fun.data = "mean_se", geom = "linerange", size=3) + xlab(quantvar_cat2) +  scale_fill_hue(name = quantvar_cat2, l=45) + scale_y_continuous(expand = c(0,0))
#        p
#    }
#    plot_multbarquant2 <- function()
#    {
#        finaldata <- isolate_data()
#        quantvar2 <-  as.symbol(isolate(input$quantvar))
#        quantvar_cat2 <-  as.symbol(isolate(input$quantvar_cat))
#        quantvar_cat22 <-  as.symbol(isolate(input$quantvar_cat2))
#        position_bar <- position_dodge(preserve = "total") ## will have a single category one fill up whole x
#        position_error <- position_dodge(0.9)
#        p <- ggplot(finaldata) + stat_summary_bin(aes(x = factor(!!quantvar_cat2), y = !!quantvar2, fill = factor(!!quantvar_cat22)), fun.y = "mean", geom = "bar", position = position_bar) + stat_summary_bin(aes(x = factor(!!quantvar_cat2), y = !!quantvar2, group = factor(!!quantvar_cat22)), fun.data = "mean_se", geom = "linerange", size=4, position = position_error) + xlab(quantvar_cat2) +  scale_fill_hue(name = quantvar_cat22, l=45) + scale_y_continuous(expand = c(0,0))
#        p
#    }
#    plot_line <- function()
#    {
#        finaldata <- isolate_data()
#        quantvar2 <-  as.symbol(isolate(input$quantvar))
#        quantvar_cat2 <-  as.symbol(isolate(input$quantvar_cat))
#        p <- ggplot(finaldata) + 
#                stat_summary_bin(aes(x = factor(!!quantvar_cat2), y = !!quantvar2, color = factor(!!quantvar_cat2)), fun.y = "mean", geom = "point", size=5) +
#                stat_summary_bin(aes(x = factor(!!quantvar_cat2), y = !!quantvar2), group=1, fun.y = "mean", geom = "line") +
#                stat_summary_bin(aes(factor(!!quantvar_cat2), y = !!quantvar2), fun.data = "mean_se", geom = "linerange") + xlab(quantvar_cat2) +  scale_color_hue(name = quantvar_cat2, l=45) 
#        p
#    }
#
#    plot_barcount <- function()
#    {
#        finaldata <- isolate_data()
#        thecolor  <- isolate_color()
#        countvar <-  as.symbol(isolate(input$countvar))
#        p <- ggplot(finaldata, aes(x = factor(!!countvar))) + geom_bar(fill = thecolor) + xlab(countvar) + scale_y_continuous(expand = c(0,0))
#        p
#    }
#    plot_barcount2 <- function()
#    {
#        finaldata <- isolate_data()
#        thecolor  <- isolate_color()
#        countvar <-  as.symbol(isolate(input$countvar))
#        countvar2 <-  as.symbol(isolate(input$countvar2))
#        p <- ggplot(finaldata, aes(x = factor(!!countvar), fill = factor(!!countvar2))) + geom_bar(position = "dodge2") + xlab(countvar) + scale_fill_hue(name = countvar2, l=45) + scale_y_continuous(expand = c(0,0))
#        p
#    }    
#    
#    plot_scatter <- function()
#    {
#        finaldata <- isolate_data()
#        thecolor  <- isolate_color()
#        x <- as.symbol(isolate(input$xvar))
#        y <- as.symbol(isolate(input$yvar))
#        regression <- isolate(input$bestfit)
#        p <- ggplot(finaldata, aes(x = !!x, y = !!y)) + geom_point(size=2)
#        if (regression == TRUE) 
#        {
#            f.str <- paste(y, "~", x)
#            fit <- lm(as.formula(f.str), data = finaldata)
#            r2 <- round( summary(fit)$r.squared, 4 )
#            slope <- round(fit$coefficients[2], 3)
#
#            p <- p + geom_smooth(color = thecolor, method = "lm") + ggtitle(paste0("R^2 = ", r2, ".   Slope = ", slope)) + theme(plot.title = element_text(face = "bold.italic", size=20, hjust =0.1))
#        }
#        p
#    }
#    plot_scatter2 <- function()
#    {
#        finaldata <- isolate_data()
#        thecolor  <- isolate_color()
#        x <- as.symbol(isolate(input$xvar))
#        y <- as.symbol(isolate(input$yvar))
#        catvar <- as.symbol(isolate(input$catvar))
#        p <- ggplot(finaldata, aes(x = !!x, y = !!y, color = factor(!!catvar))) + geom_point(size=3) + scale_color_hue(name = catvar, l=45) 
#		p
#    }
#
#
#    ###############################################################################################
#    ###############################################################################################
#    
#
#    
#    ## "Regular" function which returns the plot
#    printplot <- function() {
#
#        viz <- isolate(input$dataviz)
#        geom_quant <- isolate(input$whichquant)
#
#        ############### Single distribution ##################
#        if (viz == "quant" & geom_quant == "Histogram") p <- plot_histogram()
#
#        if (viz == "quant" & geom_quant == "Density plot") p <- plot_density()
#
#        if (viz == "quant" & geom_quant == "Boxplot") p <- plot_boxplot()
#
#        if (viz == "quant" & geom_quant == "Jitter plot") p <- plot_jitter()
#
#        if (viz == "quant" & geom_quant == "Bar plot") p <- plot_barquant()
#
#        if (viz == "quant" & geom_quant == "Make all")
#        {
#            p1 <- plot_histogram()
#            p2 <- plot_density()
#            p3 <- plot_boxplot()
#            p4 <- plot_jitter()
#            p5 <- plot_barquant()
#            
#            p <- (p1 + p2 + p3) / (p4 + p5)
#        }
#        #######################################################
#
#        ############### Multiple distributions with single cat ##################
#        if (viz == "multquant" & geom_quant == "Boxplot") p <- plot_multbox()
#
#        if (viz == "multquant" & geom_quant == "Density plot") p <- plot_multdensity()
#
#        if (viz == "multquant" & geom_quant == "Jitter plot") p <- plot_multjitter()
#
#        if (viz == "multquant" & geom_quant == "Bar plot") p <- plot_multbarquant()
#
#        if (viz == "multquant" & geom_quant == "Line plot") p <- plot_line()
#
#        if (viz == "multquant" & geom_quant == "Make all")
#        {
#            p1 <- plot_multbox() + theme(legend.position = "none")
#            p2 <- plot_multdensity() + theme(legend.position = "none")
#            p3 <- plot_multjitter() + theme(legend.position = "none")
#            p4 <- plot_multbarquant() + theme(legend.position = "bottom")
#            p5 <- plot_line() + theme(legend.position = "none")
#            
#            p <- (p1+p2+p3)/(p4+p5)
#        }
#        #######################################################
#
#        ############### Multiple distributions with two cat ##################
#        if (viz == "multquant2" & geom_quant == "Boxplot") p <- plot_multbox2()
#
#        if (viz == "multquant2" & geom_quant == "Jitter plot") p <- plot_multjitter2()
#
#        if (viz == "multquant2" & geom_quant == "Bar plot") p <- plot_multbarquant2()
#
#        if (viz == "multquant2" & geom_quant == "Make all")
#        {
#            p1 <- plot_multbox2() + theme(legend.position = "none")
#            p2 <- plot_multjitter2() + theme(legend.position = "bottom")
#            p3 <- plot_multbarquant2() + theme(legend.position = "none")
#           
#            p <- p1+p2+p3
#        }
#        #######################################################
#
#
#        ############### Barplot for counts ####################
#        if (viz == "counts") p <- plot_barcount()
#        if (viz == "counts2") p <- plot_barcount2()
#        #######################################################
#
#        ############### Scatterplot ###########################
#        if (viz == "scatter") p <- plot_scatter()
#        if (viz == "scatter2") p <- plot_scatter2()
#        #######################################################
#        
#        print(p)
#    }
#
#
#    ## Render plot and download button, reactive to "Go" button
#    observeEvent(input$go,  {
#
#    
#        output$mahplot <- renderPlot( { 
#            printplot()
#        })
#
#        output$download <- renderUI({
#            downloadButton('download_plot', 'Download plot')
#        })
#
#
#       output$download_plot <- downloadHandler(
#            filename = function() {
#                "download.png"
#            },
#            content = function(file) {
#                ggsave(file, printplot(), width = 6, height = 4)
#            }
#        )
#    })  
}

