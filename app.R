# Set up environment -----------------------------------------------------------

## Custom functions
source("0 - prerequisites.R")

## Libraries
library(shiny)
library(lme4)
library(ggplot2)
library(reshape2)
library(extrafont)
library(plotly)
# "prettyunits" %>=>% libInstall %=>% library(.., char = T)

## Helper functions
"/prediction.R" %=>% paste0(getwd(), ..) %=>% source
"/animation.R" %=>% paste0(getwd(), ..) %=>% source
"/threeD.R" %=>% paste0(getwd(), ..) %=>% source

## Import fonts for plots
if (!"Open Sans" %in% fonts()) {
    font_import(
        pattern = "OpenSans",
        prompt = F
    )
}

## Import prediction model
load(paste0(getwd(), "/Data/model.rda"), envir = globalenv())
load(paste0(getwd(), "/Data/weatherScale.rda"), envir = globalenv())

## Some constants
cols = c("forestgreen", "yellowgreen", "orange", "red")
spp = "Xanthomonas perforans"
gene = "XopJ2"
genelabels = c("wt", "mut")
names(genelabels) = paste0(gene, c("+", "−"))
rh = "Humidity.mean"
temp = "Temp.60cm.mean"
rain = "Rain.sum"

# add model.rda to Out folder?
#add opensans to root folder. Update other script to use this?

# Shiny UI ---------------------------------------------------------------------

ui = fluidPage(
    tags$head(HTML("
        <title>Xanthomonas perforans dispersal prediction</title>
        <link rel='icon' type='image/png' href='data:image/png;base64,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'>
    ")),
    # App title
    headerPanel(
        div(
            align = "center",
            style = "padding-bottom: 20px; font-size = 24px",
                "Bacterial Dispersal Prediction"
        )
    ),
    # Input sidebar (or topbar)
    fluidRow(
        style = "border-bottom: #aaa 2px groove",
        column(3,
            # Plot type dropdown
            selectInput(
                "chart",
                "Chart type:",
                c(
                    "3d surface" = "3d",
                    "Animation" = "anim",
                    "2d tile" = "2d",
                    "Time graph" = "time"
                ),
                selected = "3d"
            ),
            radioButtons(
                "resp",
                "Response:",
                c("Pathogen" = 0, "Disease" = 1),
                selected = 0,
                inline = T
            ),
            tags$style(
                type = "text/css", "#resp{opacity: 0.5; pointer-events: none}"
            ),
            conditionalPanel(
                "input.resp == 0",
                checkboxGroupInput(
                    inputId = "gene",
                    label = "Genotype",
                    choiceNames = paste0(gene, c("+", "-")),
                    choiceValues = c("wt", "mut"),
                    selected = "wt",
                    inline = T
                )
            )
        ),
        column(8,
            column(4,
                sliderInput(
                    inputId = "week",
                    label = "Time:",
                    min = 3,
                    max = 15,
                    value = 10,
                    step = 1,
                    post = " weeks"
                ),
                sliderInput(
                    inputId = "dis",
                    label = "Distance:",
                    min = 0,
                    max = 15,
                    value = 6,
                    step = 1,
                    post = " m"
                )
            ),
            column(4,
                sliderInput(
                    inputId = "rh",
                    label = "Relative humidity:",
                    min = 65,
                    max = 95,
                    value = 78,
                    step = 1,
                    post = "%"
                ),
                sliderInput(
                    inputId = "temp",
                    label = "Average weekly temperature (@ 60cm):",
                    min = 18,
                    max = 28,
                    value = 23.5,
                    step = 0.5,
                    post = " °C"
                )
            ),
            column(4,
                sliderInput(
                    inputId = "rain",
                    label = "Total weekly precipitation:",
                    min = 0,
                    max = 40,
                    value = 3,
                    step = 0.25,
                    post = " mm"
                )
            )
        ),
        column(1,
            align = "center",
            style = "margin-top:80px",
            actionButton("go", "Plot!"),
            tags$style(
                type = "text/css",
                "#go{height: 40px; background-color: #428bca; 
                color: white; line-height: 30px}"
            ),
            checkboxInput(
                "help",
                "Help",
                value = F
            )
        )
    ),
    conditionalPanel(
        condition = "input.help",
        div(
            p("This is help text"),
            p("This is para 2")
        )
    ),
    # Display area for plot outputs.
    mainPanel(
        style = "margin-top: 20px; width: 100%;
            max-width: 1200; max-height: 640",
        conditionalPanel(
            "input.chart == '3d' & ! input.help",
            plotlyOutput("plot1")
        ),
        conditionalPanel(
            "input.chart == 'anim'",
            imageOutput("plot2")
        ),
        conditionalPanel(
            "input.chart == '2d'",
            plotOutput("plot3")
        ),
        conditionalPanel(
            "input.chart == 'time'",
            plotOutput("plot4")
        )
    )
)

server = function(input, output, session) {

    observeEvent(input$go, {

        # Set error if no genotype selected.
        error.data = reactive({
            shiny::validate(need(
                !is.null(input$gene),
                "Please select a genotype"
            ));
            " "
        })

        #Render error message
        output$error = renderText({ print(error.data()) })
        validate(need(input$gene, "Select one"))
        
        if (input$chart == "3d") { # 3D plot render
            .data = data.frame(
                expand.grid(
                    week = 0:input$week,
                    dis = 0:input$dis,
                    gene = input$gene
                ),
                Rain.sum = (input$rain - ws[rain,'mean']) / ws[rain,'sd'],
                Temp.60cm.mean = (input$temp - ws[temp,'mean']) / ws[temp,'sd'],
                Humidity.mean = (input$rh - ws[rh,'mean']) / ws[rh,'sd']
            )
            output$plot1 = renderPlotly({
                plot3d( 
                    fit, 
                    .data, 
                    pos = c("c", "m"), 
                    spp = spp,
                    gene = gene 
                )
            })
        
        } else if (input$chart == "anim") { # Animation render
            .data = list(
                grid = c(15, 15),
                week = seq(0, input$week, 0.25),
                gene = input$gene
            )
            Rain.sum = (input$rain - ws[rain,"mean"]) / ws[rain,"sd"]
            Temp.60cm.mean = (input$temp - ws[temp,"mean"]) / ws[temp,"sd"]
            Humidity.mean = (input$rh - ws[rh,"mean"]) / ws[rh,"sd"]

            output$plot2 = renderImage({
                outfile = tempfile(fileext = "gif")
                progress = shiny::Progress$new(max = length(.data$week))
                on.exit(progress$close())
                animation(
                    fit,
                    .data,
                    out = outfile,
                    spp = spp,
                    gene = gene,
                    view = F,
                    delay = 20,
                    shinyProgress = progress,
                    Rain.sum = Rain.sum,
                    Temp.60cm.mean = Temp.60cm.mean,
                    Humidity.mean = Humidity.mean
                )
                list(
                    src = outfile,
                    contentType = "image/gif",
                    width = 960,
                    height = 480
                )
            }, deleteFile = T)
        
        } else if (input$chart == "2d") { # 2D plot render
            source("prediction.R")
            .data = data.frame(
                expand.grid(
                    week = 0:input$week,
                    dis = 0:input$dis,
                    gene = input$gene
                ),
                Rain.sum = (input$rain - ws[rain,"mean"]) / ws[rain,"sd"],
                Temp.60cm.mean = (input$temp - ws[temp,"mean"]) / ws[temp,"sd"],
                Humidity.mean = (input$rh - ws[rh,"mean"]) / ws[rh,"sd"]
            )
            output$plot3 = renderPlot({
                dispersal(
                    fit,
                    .data,
                    spp = spp,
                    gene = gene,
                )
            }, width = length(input$gene) * 560 + 160, height = 600)
        
        } else { # confint plot render
            source("prediction.R")
            .data = data.frame(
                expand.grid(
                    week = 0:input$week,
                    dis = input$dis,
                    gene = input$gene
                ),
                Rain.sum = (input$rain - ws[rain,"mean"]) / ws[rain,"sd"],
                Temp.60cm.mean = (input$temp - ws[temp,"mean"]) / ws[temp,"sd"],
                Humidity.mean = (input$rh - ws[rh,"mean"]) / ws[rh,"sd"]
            )
            output$plot4 = renderPlot({
                dispersalPlus(
                    fit,
                    .data,
                    spp = spp,
                    gene = gene,
                )
            }, width = length(input$gene) * 540 + 100, height = 600)
        }
   })
}

# Run app locally
shinyApp(ui = ui, server = server)

#Deploy app to shinyapps.io
# deployApp(appFiles = c(
#     "0 - prerequisites.R",
#     "app.R",
#     "prediction.R",
#     "animation.R",
#     "threeD.R",
#     "Data/model.rda",
#     "/Data/weatherScale.rda"
# ))