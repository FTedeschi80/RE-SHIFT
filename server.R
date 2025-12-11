library(shiny)
library(shinyjs)
library(dplyr)
library(stringr)
library(tidyselect)
library(waiter)
library(xtable)
library(ggplot2)
library(shinyWidgets)
library(knitr)
library(ids)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                 #
#   Define JS functions for shinyjs                               #
#                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

jsCode <- "

shinyjs.reload = function(){
    location.reload();
}

shinyjs.replaceImage = function() {
 document.getElementById('effect-plot').style.display = 'block';
 document.getElementById('filler-plot').style.display = 'none';
}

shinyjs.setAppVersion = function(appVersion) {
    document.getElementById('app-version-badge').src = 'https://img.shields.io/badge/App%20Version-' + appVersion + '-blue?style=for-the-badge';
}
"

appVersion <- "1.0.0"



# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#                                                                 #
#   Server function                                               #
#                                                                 #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

function(input, output, session) {

    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    #   App setup                                                     #
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

    # Disable warnings
    options(warn = -1)

    # Include shinyjs
    useShinyjs(html = TRUE)

    # Load util functions & initial data
    source("utils.R")

    # Define first load
    firstLoad <- TRUE

    # Insert header with shinyjs imports
    insertUI(
        "head", "beforeEnd",
        extendShinyjs(
            text = jsCode,
            functions = c("reload", "replaceImage")
        )
    )

    # Set app version
    runjs(paste0("shinyjs.setAppVersion('", appVersion, "');"))
    
    # Add custom CSS to remove modal backdrop only for Shiny modals
    insertUI(
        "head", "beforeEnd",
        HTML('<style>
            /* Remove all modal backdrops for Shiny modals */
            .shiny-modal .modal-backdrop,
            .shiny-modal + .modal-backdrop,
            .modal-backdrop.show {
                display: none !important;
                opacity: 0 !important;
                background: transparent !important;
            }
            .shiny-modal {
                background: transparent !important;
            }
            .shiny-modal .modal-dialog {
                background: transparent !important;
            }
            .shiny-modal .modal-content {
                background: white !important;
                border: 1px solid lightgray !important;
            }
            .shiny-modal .modal-body {
                border: none !important;
            }
            .shiny-modal .modal-footer {
                border-top: none !important;
            }
            /* Keep Bootstrap modals in HTML with white background */
            .modal:not(.shiny-modal) {
                background: white !important;
            }
            .modal:not(.shiny-modal) .modal-dialog {
                background: white !important;
            }
            .modal:not(.shiny-modal) .modal-content {
                background: white !important;
            }
            .modal:not(.shiny-modal) .modal-header {
                background: white !important;
            }
            .modal:not(.shiny-modal) .modal-body {
                background: white !important;
            }
            .modal:not(.shiny-modal) .modal-footer {
                background: white !important;
            }
            .modal:not(.shiny-modal) .modal-backdrop {
                background-color: rgba(0,0,0,0.5) !important;
            }
        </style>')
    )

    # Define reactiveValues
    RV <- shiny::reactiveValues()


    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
    #   Buttons & Event Listeners                                     #
    # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

    # Observe new filter input --------------------------------------------
    observe({
        # Show waiter only on first load
        if (firstLoad) {
            # Hide initial JavaScript spinner
            shinyjs::runjs("
                const initialSpinner = document.getElementById('initial-spinner');
                if (initialSpinner) {
                    initialSpinner.remove();
                }
            ")
            
            waiter::waiter_show(
                html = waiter::spin_atebits(),
                color = "rgba(255, 255, 255, 0.8)"
            )
        }

        # Define reactive values
        as.numeric(comp.names %in% input$`control-input`) -> control
        as.numeric(comp.names %in% input$`treatment-input`) -> treatment
        input$`depression-input` %>% as.numeric() -> depression
        input$`age-input` %>% as.numeric() -> age
        input$`gender-input` %>% as.numeric() -> gender
        input$`rel-input` %>% as.numeric() -> rel
        input$`employ-input` %>% as.numeric() -> employ
        input$`educ-input` %>% as.numeric() -> educ

        # Create prediction
        Y <- createPrediction(
            treat1 = control, treat2 = treatment, baseline = depression,
            age = age, gender = gender, rel = rel, employ = employ,
            educ = educ
        )
        
        # Add small delay to make waiter visible (only on first load)
        if (firstLoad) {
            Sys.sleep(0.5)
        }

        # Data for gradient rectangles
        plotdat <- data.frame(x = -6000:6000 / 100, y = 100)
        ggplot(data = plotdat, aes(x, y, color = x)) + 
            geom_rect(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = "lightgray", color = "gray", size = 1) +
            geom_segment(aes(xend = x, yend = 0), size = 10) +
            scale_color_gradient2(low = "#c5e8e8", mid = "white", high = "#d9d9d9", midpoint = 0) +
            annotate("rect",
                xmin = Y[, "lower"], xmax = Y[, "upper"],
                ymin = -Inf, ymax = Inf,
                fill = "#0372b7", alpha = 0.15) +
            geom_vline(xintercept = Y[, "md"], size = 3, color = "#0372b7") +
            # geom_vline(xintercept = Y[, "lower"], size = .5, color = "#0372b7", linetype = "dotted") +
            # geom_vline(xintercept = Y[, "upper"], size = .5, color = "#0372b7", linetype = "dotted") +
            geom_vline(xintercept = 0, size = .5, color = "black", linetype = "dotted", alpha = 0.5) +
            theme_classic() +
            coord_cartesian(xlim = c(-62, 62), ylim = c(-1, 101), expand = FALSE) +
            scale_x_continuous(breaks = c(-50, -25, 0, 25, 50), labels = function(x) paste0(x, "%")) +
            theme(
                axis.title.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.line = element_blank(),
                axis.ticks.x = element_blank(),
                legend.position = "none",
                plot.margin = unit(c(0, 0, 0, 0), "cm")
            ) -> p

        # Render plot
        output$effect.plot = renderPlot({p}, height = 70)
        output$lower.or.higher = renderText(ifelse(Y[, "md"] > 0, "higher", "lower"))
        output$effect = renderText(paste0(format(round(abs(Y[, "md"]), 2), nsmall = 2), "%"))
        output$effect.ci = renderText(paste0("(95%CI: ", format(round(Y[, "lower"], 2), nsmall = 2), "% to ", format(round(Y[, "upper"], 2), nsmall = 2), "%)"))
        output$significance = renderText(ifelse(abs(Y[, "md"]/Y[, "se"]) > qnorm(.975), "significant", "not significant"))
        if (is.na(abs(Y[, "md"]/Y[, "se"]) > qnorm(.975))) { output$significance = renderText("not significant")}

        # Replace image
        shinyjs::js$replaceImage()
        
        # Hide waiter when calculation is complete (only if it was shown)
        if (firstLoad) {
            waiter::waiter_hide()
            
            # Show honeycomb and infotext when first load is complete
            shinyjs::runjs("
                document.getElementById('honeycomb').style.display = 'block';
                document.getElementById('infotext').style.display = 'block';
            ")
            
            # Set firstLoad to FALSE after first calculation
            firstLoad <<- FALSE
        }
    })

    # Handle report button click
    observeEvent(input$`report-btn`, {
        
        # Show waiter for report generation
        waiter::waiter_show(
            html = waiter::spin_atebits(),
            color = "rgba(255, 255, 255, 0.8)"
        )
        
        # Show progress
        showModal(modalDialog(
            title = "Generating Report",
            "Please wait while the PDF report is being generated...",
            footer = NULL,
            easyClose = FALSE
        ))
        
        # Add shiny-modal class to the modal and remove backdrop
        shinyjs::runjs("
            setTimeout(function() {
                $('.modal').addClass('shiny-modal');
                $('.modal-backdrop').remove();
                $('body').removeClass('modal-open');
            }, 100);
        ")
        
        # Generate report
        tryCatch({
            # Create report data
            rout <- list()
            
            # Generate unique report ID
            rout$id <- paste0("MP-", ids::random_id(1,4))
            
            # Get current input values and recode using labels from template.html
            rout$treatment_package <- paste(sapply(input$`control-input`, function(x) {
                switch(x,
                    "TAU" = "Treatment As Usual",
                    "PE" = "Psychoeducation",
                    "CR" = "Cognitive Restructuring",
                    "ACC" = "Acceptance Treatment",
                    "REL" = "Relationship",
                    "PM" = "Problem Management",
                    "BA" = "Behavioral Activation",
                    "SSS" = "Social Support",
                    "IF" = "Interpersonal Focus",
                    x)
            }), collapse = ", ")
            
            rout$comparator <- paste(sapply(input$`treatment-input`, function(x) {
                switch(x,
                    "TAU" = "Treatment As Usual",
                    "PE" = "Psychoeducation",
                    "CR" = "Cognitive Restructuring",
                    "ACC" = "Acceptance Treatment",
                    "REL" = "Relationship",
                    "PM" = "Problem Management",
                    "BA" = "Behavioral Activation",
                    "SSS" = "Social Support",
                    "IF" = "Interpersonal Focus",
                    x)
            }), collapse = ", ")
            
            # Convert numeric inputs to descriptive labels
            rout$depression.severity <- paste0(input$`depression-input`, "\\%")
            rout$age <- paste0(input$`age-input`, " years")
            
            # Recode gender (1 = Female, 0 = Male)
            rout$gender <- ifelse(input$`gender-input` == 1, "Female", "Male")
            
            # Recode relationship status (1 = Married, 0 = Unmarried)
            rout$relationship.status <- ifelse(input$`rel-input` == 1, "Married", "Unmarried")
            
            # Recode employment status (1 = Paid work/employment, 0 = Other)
            rout$employment.status <- ifelse(input$`employ-input` == 1, "Paid work/employment", "Other")
            
            # Recode education level (1 = High school, 0 = No high school)
            rout$education.level <- ifelse(input$`educ-input` == 1, "High school", "No high school")

            # Add app version
            rout$app.version <- appVersion
            
            # Get predicted effect from current plot data
            # We need to recalculate the prediction with current inputs
            as.numeric(comp.names %in% input$`control-input`) -> control
            as.numeric(comp.names %in% input$`treatment-input`) -> treatment
            input$`depression-input` %>% as.numeric() -> depression
            input$`age-input` %>% as.numeric() -> age
            input$`gender-input` %>% as.numeric() -> gender
            input$`rel-input` %>% as.numeric() -> rel
            input$`employ-input` %>% as.numeric() -> employ
            input$`educ-input` %>% as.numeric() -> educ
            
            # Create prediction
            Y <- createPrediction(
                treat1 = control, treat2 = treatment, baseline = depression,
                age = age, gender = gender, rel = rel, employ = employ,
                educ = educ
            )
            
            # Add effect results to rout for the report
            rout$predicted.effect <- Y[, "md"]
            rout$lower.or.higher <- ifelse(Y[, "md"] > 0, "higher", "lower")
            rout$effect.ci <- paste0("(95\\%CI: ", format(round(Y[, "lower"], 2), nsmall = 2), "\\% to ", format(round(Y[, "upper"], 2), nsmall = 2), "\\%)")
            rout$significance <- ifelse(abs(Y[, "md"]/Y[, "se"]) > qnorm(.975), "significant", "not significant")
            if (is.na(abs(Y[, "md"]/Y[, "se"]) > qnorm(.975))) { rout$significance <- "not significant"}
            
            # Add Y matrix to rout for the plot in Rnw
            rout$Y <- Y

            # Make rout available globally for the report
            assign("rout", rout, envir = .GlobalEnv)
            
            # Generate PDF report
            report_file <- knitr::knit2pdf('input.Rnw', clean = TRUE, quiet = TRUE)
            
            # Hide waiter
            waiter::waiter_hide()
            
            # Remove modal
            removeModal()
            
            # Show success message
            showModal(modalDialog(
                title = "Download Report",
                tagList(
                    downloadButton("downloadReport", "Download Report", class = "btn btn-default btn-outline-information px-3 w-100")
                ),
                footer = NULL,
                easyClose = TRUE,
                style = "border: 1px solid lightgray;",
                id = "download-modal"
            ))
            
            # Add shiny-modal class to the success modal and remove backdrop
            shinyjs::runjs("
                setTimeout(function() {
                    $('.modal').addClass('shiny-modal');
                    $('.modal-backdrop').remove();
                    $('body').removeClass('modal-open');
                }, 100);
            ")
            # Store report file path for download
            RV$report_file <- report_file
            
        }, error = function(e) {
            # Hide waiter
            waiter::waiter_hide()
            
            removeModal()
            showModal(modalDialog(
                title = "Error Generating Report",
                paste("An error occurred while generating the report:", e$message),
                footer = modalButton("Close"),
                easyClose = TRUE
            ))
            
            # Add shiny-modal class to the error modal and remove backdrop
            shinyjs::runjs("
                setTimeout(function() {
                    $('.modal').addClass('shiny-modal');
                    $('.modal-backdrop').remove();
                    $('body').removeClass('modal-open');
                }, 100);
            ")
        })
    })
    
    # Download handler for the report
    output$downloadReport <- downloadHandler(
        filename = function() {
            paste0("report-", Sys.Date(), ".pdf")
        },
        content = function(file) {
            if (!is.null(RV$report_file) && file.exists(RV$report_file)) {
                file.copy(RV$report_file, file)
            }
        },
        contentType = "application/pdf"
    )
}
