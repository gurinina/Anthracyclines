# may29 2024 april19 covid
# sunday_2024_COVID19 works
# SIGNATURE WORKS!!!
#
# git config --global user.name "Your Name"
# git config --global user.email you@example.com

library(shiny)
library(dplyr)
library(shinyWidgets)
library(DT)
library(shinydashboard)
library(visNetwork)
library(shinycssloaders)
library(igraph)
library(ggplot2)
library(ggrepel)
library(shinyjs)
library(shinyBS)
library(stringr)
library(shinyjqui)

options(rsconnect.max.bundle.size = 3145728000)

#
# source(file.path("modules","2024_April11_visNetwork.R"))
source(file.path("modules", "2022_March13_GOENRICH.R"))
source(file.path("modules", "2022_March13_functions.R"))
# source(file.path("modules" ,"2024_June3_signature.R"))

fdat <- readRDS("data/2021_July19_fdat.RDS")
#
# xsig      <-     readRDS("data/x21.RDS")

phop <- readRDS("data/phop.RDS")
phiphop <- readRDS("data/phiphop.RDS")

d3 <- readRDS("data/d3.RDS")
de3 <- readRDS("data/de3.RDS")
dn3 <- readRDS("data/dn3.RDS")
dint <- readRDS("data/dint.RDS")

xcoinh <- readRDS("data/xcoinh.RDS")
xpval <- readRDS("data/xcoinh_pval.RDS")
xcofit <- readRDS("data/xcofit.RDS")
xpfit <- readRDS("data/xcofit_pval.RDS")

xcohh <- readRDS("data/xhiphop_coinh.RDS")
xpvhh <- readRDS("data/xhiphop_coinh_pval.RDS")
xfithh <- readRDS("data/xhiphop_cofit.RDS")
xpithh <- readRDS("data/xhiphop_cofit_pval.RDS")

d3 <- d3[, phop$name]
de3 <- de3[, phiphop$name]
dn3 <- dn3[, phiphop$nam]
dint <- dint[, phiphop$name]

noness <- fdat %>% filter(essential == "noness")
ess <- fdat %>% filter(essential == "ess")

st <- rev(seq(0, 1, 0.05))

sq <- rev(seq(0, 1, 0.001))

##########################

header <- dashboardHeader(title = span("Chemogenomic Profiling  COVID19",
  style = "font-weight:bold;font-size:18px;
             text-align: right;"
), titleWidth = 1200)

sidebar <- dashboardSidebar(
  width = 220,
  tags$style(HTML(".main-sidebar {
  background-color: #000066; color: #fff; font-weight:bold}
  .treeview-menu>li>a {font-weight:bold; color: #fff;}")),
  sidebarMenu(sidebarMenuOutput(("sidebarPanel")))
)

body <- dashboardBody(
  tags$style(HTML(".main-sidebar {font-size:16px;
  background-color:#000066;color:#fff;font-weight:bold}
 .treeview-menu>li>a{font-size:16px;font-weight:bold;color: #fff ;}")),
  tags$style("#controls {
  background-color: #dcd0ff;opacity: 1;}
  #controls:hover{opacity: 1;}
  #econtrols {
  background-color: #dcd0ff;opacity: 1;};  }"),
  tags$style("#controls {
  background-color: #dcd0ff;opacity: 1;}
  #econtrols:hover{opacity: 1;}
  #econtrols {
  background-color: #dcd0ff;opacity: 1;};  }"),
  tags$head(tags$style(HTML("#point_einfo tr.selected
  {background-color:white},
  #point_info tr.selected {background-color:white}"))),

  # makes pngs same size as window after rescaling
  tags$script("$(document).on('shiny:connected', function(event) {
  var myWidth = $(window).width();
  Shiny.onInputChange('shiny_width',myWidth)});"),
  useShinyjs(),
  tags$script("$(document).on('shiny:connected', function(event) {
  var myHeight = $(window).height();
  Shiny.onInputChange('shiny_height',myHeight)});"),
  tags$script(func <- JS("function(event, ui){return $(event.target).offset();}")),
  tags$script(HTML("
  //Get mouse coordinates
  var mouseX, mouseY;$(document).mousemove(function(e) {
  #mouseX = e.pageX;
  #mouseY = e.pageY;}).mouseover();
  //Function to position draggable, place on current mouse coordinates
  Shiny.addCustomMessageHandler ('placeDraggable',function (message) {
  var element = $('#click_info').parent();
  #element.css({'top': mouseY + 'px', 'left' : mouseX + 'px'})});

  //Show or hide draggable
  Shiny.addCustomMessageHandler ('hideDraggable',function (message) {
  if(message.hide == true){
  $('#click_info').parent().hide();} else{
  $('#click_info').parent().show();}});

  //Get mouse coordinates
  var mouseX, mouseY;$(document).mousemove(function(e) {
  #emouseX = e.pageX;
  #emouseY = e.pageY;}).mouseover();
  //Function to position draggable, place on current mouse coordinates
  Shiny.addCustomMessageHandler ('placeDraggable',function (message) {
  var element = $('#click_info').parent();
  #element.css({'top': mouseY + 'px', 'left' : mouseX + 'px'})});

  //Show or hide draggable
  Shiny.addCustomMessageHandler ('hideDraggable',function (message) {
  if(message.hide == true){
  $('#eclick_info').parent().hide();} else{
  $('#eclick_info').parent().show();}});

  //Show or hide draggable
  Shiny.addCustomMessageHandler ('ehideDraggable',function (message) {
  if(message.hide == true){
  $('#eclick_info').parent().hide();} else{
  $('#eclick_info').parent().show();}}),")),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "chemogenomics.css")),
  shinyjs::useShinyjs(),
  tags$style(HTML(".selectize-input{white-space: nowrap;
  font-size: 14px; font-weight:bold!important;")),

  #########################################################################
  ########################### START TABITEMS ##############################
  #########################################################################

  tabItems(
    tabItem(
      tabName = "genebyscreen",

      # BY RANK

      fluidRow(
        column(
          width = 3,
          box(
            title = "Enter gene of interest:",
            textInput(inputId = "gene", label = NULL, value = "ARG4"),
            status = "primary", solidHeader = TRUE, width = "100%", height = 250
          )
        ),
        column(
          width = 9,
          box(
            title = "Gene descriptor and function:",
            dataTableOutput("geneinfo"),
            status = "primary", solidHeader = TRUE,
            width = "100%", height = 250
          )
        )
      ), # fluidRow ok

      fluidRow(
        column(
          width = 12,
          box(
            title = "Fitness defect score across conditions:",
            h5("click OR drag to select screens of interest;
      corresponding profiles can be foundin compound menu
      on hiphop tab; cyan: p-value < 0.001; triangle:
      screens identifying drug target candidates"),
            plotOutput("genebydrug",
              click = clickOpts(id = "clickpts"),
              brush = brushOpts(id = "brushpts"), height = 800
            ),
            status = "primary", solidHeader = TRUE, width = "100%"
          )
        )
      ), # ok

      fluidRow(
        box(
          title = "Experimental detail; select row to view corresponding
    chemogenomic profile",
          dataTableOutput("tabpts"),
          status = "primary", solidHeader = TRUE, width = 12,
          height = 250
        )
      ), # ok

      fluidRow(
        box(
          title = "Cofit genes, select row to view corresponding
    gene fitness profile",
          dataTableOutput("coFitNess"),
          status = "primary", solidHeader = TRUE, width = 12
        )
      ),
      fluidRow(
        column(
          width = 6,
          uiOutput("mxFIT")
        ),
        column(
          width = 6,
          uiOutput("fiToutput")
        )
      ),

      # geneinfo tabpts,vofiyt mxfit Goterms Gotable leadingedge EnrichTable download CoFit

      fluidRow(
        box(
          title = "GO enrichment network: select a node to view details;
    right click to save image",
          uiOutput("GOterms"),
          visNetworkOutput("network_Proxy", width = "85%", height = 652),
          width = 8, status = "primary", solidHeader = TRUE, height = 800
        ),
        box(
          title = "GO term set enrichment details:",
          uiOutput("GoTable"),
          solidHeader = TRUE, background = "navy",
          width = 4, status = "primary", height = 200
        ),
        box(
          title = "Top-contributing genes:",
          br(),
          uiOutput("LeadingEdge"),
          solidHeader = TRUE, status = "primary", height = 580, width = 4
        ), # ok

        box(
          title = "GO enrichment table:",
          DT::dataTableOutput("EnrichTable"),
          status = "primary", solidHeader = TRUE, width = 12
        )
      ), # fluidRow ok

      fluidRow(
        width = 3,
        box(
          align = "center", title = "Download CoFitness:",
          br(),
          downloadButton("downloadcofitness", "HOP CoFitness"),
          status = "primary", solidHeader = TRUE, width = "100%",
          height = 150
        ), # box ok

        column(
          width = 3,
          box(
            align = "center", title = "Download EnRichments:",
            br(),
            downloadButton("enrichdownload", "GO EnRichments"),
            status = "primary", solidHeader = TRUE,
            width = "100%", height = 150
          )
        )
      )
    ), # tab ok

    tabItem("hiphop",
      class = "active",
      fluidRow(
        column(
          width = 3,
          box(
            title = "Select input datasets:",
            br(),
            prettyRadioButtons("site",
              label = NULL,
              choices = c(
                "2021:2024 HIPHOP" = "phiphop",
                "2020:2024 HOP" = "phop"
              ),
              outline = TRUE, fill = FALSE, thick = TRUE, shape = "square",
              bigger = FALSE, selected = "phop", inline = FALSE
            ),
            status = "primary", solidHeader = TRUE, width = "100%", height = 160
          )
        ),
        column(
          width = 9,
          box(
            title = "Select screen:",
            selectizeInput("cmpSERV",
              label = NULL,
              choices = phop$name, multiple = FALSE, selected = "camptothecin_360uM",
              options = list("plugins" = list("autofill_disable"))
            ),
            status = "primary", solidHeader = TRUE,
            width = "100%", height = 160
          )
        )
      ), # fluidRow ok

      fluidRow(
        column(
          width = 3,
          box(
            align = "center", title = "Reset compound menu:",
            br(),
            actionButton(inputId = "resetCmp", label = "Reset cmpMenu"),
            status = "primary", solidHeader = TRUE,
            width = "100%", height = 200
          )
        ), # column ok

        column(
          width = 3,
          box(
            title = "Fitness score threshold:",
            sliderInput("scorethreshSERV",
              label = NULL, min = 0, value = 1.0,
              step = 0.1, max = 5.0
            ),
            fluidRow(column(
              width = 12, align = "center",
              actionLink(
                inputId = "resetScore", label = "reset",
                style = "height: 100px;width: 150px;font-size:120%;
        text-align:center", size = "lg"
              )
            )),
            status = "primary", solidHeader = TRUE, width = "100%", height = 200
          )
        ),
        uiOutput("ylimits")
      ), # fluidRow ok

      uiOutput("hiphoppanel")
    ), # tab ok

    tabItem(
      "signature",
      fluidRow(sigNetworkModuleUI("sigNetworkModule1")),
      fluidRow(
        box(
          title = "HOP profiles in response signature:",
          dataTableOutput("screenResp"),
          status = "primary", solidHeader = TRUE, width = "100%", height = 12
        )
      )
    ),
    tabItem(
      "goenrich",
      fluidRow(visNetworkModuleUI("visNetwork1"))
    )
  )
)

#####################################################################
########################### END TABITEMS ############################
#####################################################################

ui <- dashboardPage(header, sidebar, body,
  skin = "blue",
  setBackgroundColor(color = "#e6e6ff", shinydashboard = TRUE)
)

server <- function(input, output, session) {
  observeEvent(input$site,
    {
      req(input$site)

      if (input$site %in% "phop") {
        output$sidebarPanel <- renderMenu({
          sidebarMenu(
            id = "tabs", selected = "HOmozygous Profiles",
            menuItem("HOmozygous Profiles",
              tabName = "hiphop",
              icon = icon("bullseye"), selected = 1
            ),
            menuItem("GO enrichments",
              tabName = "goenrich",
              icon = icon("dna")
            ),
            menuItem("Signatures",
              tabName = "signature",
              icon = icon("database")
            ),
            menuItem("Gene fitness profiles",
              tabName = "genebyscreen",
              icon = icon("chart-line")
            )
          )
        })
      } else {
        output$sidebarPanel <- renderMenu({
          sidebarMenu(
            id = "tabs", selected = HTML("HaploInsufficiency
  Profiles<br/>&nbsp&nbsp&nbsp&nbsp&nbsp&nbspHomozygous Profiles"),
            menuItem(HTML("HaploInsufficiency Profiles<br
  />&nbsp&nbsp&nbsp&nbsp&nbsp&nbspHomozygous Profiles"),
              tabName = "hiphop", icon = icon("bullseye"), selected = 1
            ),
            menuItem("GO enrichments",
              tabName = "goenrich",
              icon = icon("dna")
            ),
            menuItem("Signatures",
              tabName = "signature",
              icon = icon("database")
            ),
            menuItem("Gene fitness profiles",
              tabName = "genebyscreen",
              icon = icon("chart-line")
            )
          )
        })
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ###########################################################################
  ########################### START HIPHOPPANEL #############################
  ###########################################################################

  output$hiphoppanel <- renderUI({
    wboth <- which(colnames(dint) %in% input$cmpSERV)
    whop <- which(colnames(d3) %in% input$cmpSERV)


    options <- list(shiny = list(abs_position = list(
      dragcreate = func, # send returned value back to shiny when interaction is created.
      drag = func
    ))) # send returned value to shiny when dragging.

    if (length(whop) > 0) {
      tabItem(
        "HOP",
        fluidRow(
          box(
            title = "Signature & compound information:",
            HTML("<h5><b>Click datatable row to view response signature:</b></h5>"),
            DT::dataTableOutput("targethip"),
            status = "primary", solidHeader = TRUE, width = 12
          )
        ), # fluidRow ok

        fluidRow(
          box(
            title = "HOP profile: mouse click on points to view gene description",
            plotOutput("nfd",
              width = "90%", height = 800,
              click = clickOpts(id = "plot_click")
            ),
            status = "primary", solidHeader = TRUE, width = 12
          ),
          jqui_draggable(
            absolutePanel(
              id = "controls", width = 600,
              draggable = TRUE, uiOutput("click_info")
            ),
            options = list(cancel = ".shiny-input-container")
          )
        ), # fluidRow ok

        fluidRow(
          box(
            title = "HOP genes: mouse click on row links to gene fitness profile;
    mouse click on genename links to SGD",
            dataTableOutput("hoptab"),
            status = "primary", solidHeader = TRUE, width = 12
          ), # fluidRow ok

          box(
            title = "download HOP:",
            column(
              width = 6,
              downloadButton("downloadhop", "HOPdownload")
            ),
            column(
              width = 6,
              downloadButton("ExportnonPlot", "HOPfitness plot")
            ),
            status = "primary", solidHeader = TRUE, width = 12
          )
        ),
        fluidRow(
          box(
            title = "Compound information: CoInhibitory (CoI) value,
    pvalue, PubChem ID (CID) link, Mechanism of Action (MoA) link,
    indication and mechanism",
            h5(strong("Mouse click on row to view CoInhibitory (CoI)
      Homozygous Profile (HOP) and corresponding GO
      enrichments in next tab")),
            br(),
            dataTableOutput("coInhib"),
            status = "primary", solidHeader = TRUE, width = 12
          )
        ), # fluidRow ok

        fluidRow(
          align = "center",
          box(
            title = "download CoInhibition:",
            downloadButton("downloadcoinhib", "CoInhibition"),
            status = "primary", solidHeader = TRUE, width = 3
          )
        )
      ) # tabItem ok
    } else if (length(wboth) > 0) {
      tabItem(
        "HIPHOP",
        fluidRow(
          box(
            title = "Signature & compound information:",
            HTML("<h5><b>Click datatable row to view response signature:</b></h5>"),
            DT::dataTableOutput("targethip"),
            status = "primary", solidHeader = TRUE, width = 12
          )
        ),
        fluidRow(
          box(
            title = "HIP fitness profile:",
            plotOutput("efd",
              width = "90%", height = 800,
              click = clickOpts(id = "plot_eclick")
            ),
            status = "primary", solidHeader = TRUE, width = 6
          ),
          box(
            title = "HOP fitness profile:
    mouse click on points to view gene description",
            plotOutput("nfd",
              width = "90%", height = 800,
              click = clickOpts(id = "plot_click")
            ),
            status = "primary", solidHeader = TRUE, width = 6
          ),
          jqui_draggable(
            absolutePanel(
              id = "controls", width = 600,
              draggable = TRUE, uiOutput("click_info")
            ),
            options = list(cancel = ".shiny-input-container")
          )
        ), # fluidRow oK

        fluidRow(
          box(
            title = "HIP genes: mouse click on row links to gene fitness profile",
            dataTableOutput("hiptab", width = "100%"),
            status = "primary", solidHeader = TRUE, width = 6
          ),
          box(
            title = "HOP genes: mouse click on genename links to SGD",
            dataTableOutput("hoptab", width = "100%"),
            status = "primary", solidHeader = TRUE, width = 6
          )
        ), # fluidRow oK

        fluidRow(
          box(
            title = "download HIP:",
            column(
              width = 6,
              downloadButton("downloadhip", "HIPdownload")
            ),
            column(
              width = 6,
              downloadButton("ExportessPlot", "HIPfitness plot")
            ),
            status = "primary", solidHeader = TRUE, width = 6
          ),
          box(
            title = "download HOP:",
            column(
              width = 6,
              downloadButton("downloadhop", "HOPdownload")
            ),
            column(
              width = 6,
              downloadButton("ExportnonPlot", "HOPfitness plot")
            ),
            status = "primary", solidHeader = TRUE, width = 12
          )
        ), # fluidRow oK

        fluidRow(
          box(
            title = "Compound information: CoInhibitory (CoI) value,
    pvalue, PubChem ID (CID) link, Mechanism of Action (MoA) link,
    indication and mechanism",
            h5(strong("Mouse click on row to view CoInhibitory
      (CoI) Homozygous Profile (HOP) and corresponding GO enrichments in next tab")),
            br(),
            dataTableOutput("coInhib"),
            status = "primary", solidHeader = TRUE, width = 12
          )
        ), # fluidRow ok

        fluidRow(
          align = "center",
          box(
            title = "download CoInhibition:",
            downloadButton("downloadcoinhib", "CoInhibition"),
            status = "primary", solidHeader = TRUE, width = 3
          )
        )
      ) # tabItem ok
    }
  })

  ##############################################################################
  ########################### END HIPHOPPANEL ##################################
  ##############################################################################

  ##############################################################################
  ##########################  START CALL MODULES ##############################@
  ##############################################################################

  tabSEND <- reactiveValues(
    tab = NULL
  )

  xinput <- reactive({
    req(input$site)

    site <- input$site

    if (site == "phop") {
      xinput <- d3[, phop$name]
    } else if (site == "phiphop") {
      xinput <- dint
    }
  })

  pdata <- reactive({
    req(input$site)

    site <- input$site

    if (site == "phop") {
      pdata <- phop
    } else if (site == "phiphop") {
      pdata <- phiphop
    }
  })

  respSEND <- reactiveValues(resp = NULL)

  threshSEND <- reactiveValues(thresh = NULL)

  cmpSEND <- reactiveValues(cmp = NULL)

  observeEvent(input$tabs,
    {
      tabSEND$tab <- input$tabs
      cmpSEND$cmp <- input$cmpSERV
    },
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  returnedMOD <- callModule(visNetworkModule, "visNetwork1",
    xinput = xinput,
    cmpInput = reactive(cmpSEND$cmp),
    threshInput = reactive(threshSEND$thresh),
    siteInput = reactive(input$site),
    tabsInput = reactive(tabSEND$tab)
  )

  cmpRETURN <- reactive({
    # print(paste("cmpRETURNED",returnedMOD[[1]]()))
    returnedMOD[[2]]()
  })

  threshRETURN <- reactive({
    # print(paste("threshRETURNED",returnedMOD[[2]]()))
    returnedMOD[[2]]()
  })

  returnedSIG <- callModule(sigNetworkModule, "sigNetworkModule1",
    xRespDat = reactive(xsig),
    xRespInput = reactive(respSEND$resp),
    inputTab = reactive(tabSEND$tab)
  )

  sigRETURN <- reactive({
    print(paste("sigRETURN", returnedSIG[[1]]()))
    returnedSIG[[1]]()
  })

  tabRETURN <- reactive({
    print(paste("tabservRETURN", returnedSIG[[2]]()))
    returnedSIG[[2]]()
  })

  ########################################################################
  ########################## END CALL MODULES ############################
  ########################################################################

  ########################################################################
  ########################## START OBSERVEEVENT ##########################
  ########################################################################

  observeEvent(cmpRETURN(),
    {
      req(cmpRETURN())
      #
      #   req(input$site)
      #
      #   site = input$site

      tabNOThop <- input$tabs != "hiphop"

      threshMOD <- threshRETURN()

      cmpMOD <- cmpRETURN()

      req(cmpMOD)

      req(cmpMOD != "")

      req(!is.null(input$cmpSERV))

      cmpDIFF <- cmpMOD != input$cmpSERV

      req(tabNOThop == TRUE | tabNOThop == FALSE)

      if (cmpMOD %in% phiphop$name) {
        choices <- phiphop$name
      } else {
        choices <- phop$name
      }

      if (cmpMOD %in% phiphop$name) {
        site <- "phiphop"
      } else {
        site <- "phop"
      }

      # req(site)

      updatePrettyRadioButtons(session, "site",
        label = NULL,
        c(
          "2021:2024 HIPHOP" = "phiphop",
          "2020:2024 HOP" = "phop"
        ), selected = site
      )

      if (cmpMOD != input$cmpSERV) {
        updateSelectizeInput(session, "cmpSERV",
          label = NULL, choices = choices,
          selected = cmpRETURN(), server = TRUE
        )
      }
    },
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  observeEvent(threshRETURN(),
    {
      req(threshRETURN())

      tabNOThop <- input$tabs != "hiphop"

      threshMOD <- threshRETURN()

      req(threshMOD)

      req(threshMOD != "")

      req(!is.null(input$cmpSERV))

      threshDIFF <- threshMOD != input$scorethreshSERV

      if (threshMOD != input$scorethreshSERV) {
        updateSliderInput(session, "scorethreshSERV",
          label = NULL,
          value = threshMOD, step = 0.1, min = 0, max = 5.0
        )
      }
    },
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  observeEvent(input$cmpSERV,
    {
      req(input$cmpSERV)

      req(!is.null(input$tabs))

      req(input$tabs != "")

      updateSliderInput(session, "scorethreshSERV",
        label = NULL,
        value = 1.0, step = 0.1, min = 0, max = 5.0
      )

      cmpSEND$cmp <- input$cmpSERV

      threshSEND$thresh <- input$scorethreshSERV

      tabSEND$tabs <- input$tabs
    },
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  observeEvent(input$scorethreshSERV,
    {
      req(input$cmpSERV)

      cmpSEND$cmp <- input$cmpSERV

      threshSEND$thresh <- input$scorethreshSERV

      tabSEND$tabs <- input$tabs
    },
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  observeEvent(input$site,
    {
      req(input$site)

      site <- input$site

      req(site)

      if (site == "phop") {
        choices <- phop$name
      } else if (site == "phiphop") {
        choices <- phiphop$name
      }

      if (site == "phop") {
        selected <- "camptothecin_360uM"
      } else if (site == "phiphop") {
        selected <- "camptothecin_360uM"
      }

      updateSelectizeInput(session, "cmpSERV",
        label = NULL,
        choices = choices, selected = selected, server = TRUE
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  #######################################################
  ################## END OBSERVEEVENTS ##################
  #######################################################

  #######################################################
  ############# START SIGNATURE EVENTS ##################
  #######################################################

  output$screenResp <- renderDataTable({
    w <- which(phop$signature %in% sigRETURN())

    validate(need(length(w) > 0, message = "please enter a valid signature"))

    df <- data.frame(
      screen = phop$name[w],
      mechanism = phop$mechanism[w],
      PCID = phop$PCID[w],
      compound = phop$cmp[w],
      stringsAsFactors = FALSE
    )

    opts <- list(
      pageLength = 10, autoWidth = FALSE, scrollX = FALSE,
      columnDefs = list(
        list(className = "dt-left", targets = c(0, 1, 2, 3)),
        list(width = c("100px"), targets = c(2)),
        list(width = c("500px"), targets = c(1)),
        list(width = c("300px"), targets = 0)
      )
    )

    df <- df %>% dplyr::arrange(compound)

    df <- DT::datatable(df,
      options = opts, rownames = FALSE, escape = FALSE,
      selection = "single"
    ) %>%
      formatStyle(c(1:4), fontWeight = "bold", fontSize = "14px")
  })

  observeEvent(input$screenResp_rows_selected, {
    w <- which(phop$signature %in% sigRETURN())

    validate(need(length(w) > 0, message = "please enter a valid signature"))

    d <- phop$name[w]

    df <- data.frame(
      screen = phop$name[w],
      mechanism = phop$mechanism[w],
      PCID = phop$PCID[w],
      compound = phop$cmp[w],
      stringsAsFactors = FALSE
    )

    df <- df %>% dplyr::arrange(compound)

    row <- input$screenResp_rows_selected

    w <- which(df$screen %in% df$screen[row])

    validate(need(length(w) > 0, message = "please enter a valid signature"))

    selected <- df$screen[row]

    cmpSEND$cmp <- selected

    updateSelectizeInput(session, "cmpSERV",
      label = NULL,
      choices = phop$name, selected = selected, server = TRUE
    )

    newtab <- switch(input$tabs,
      "signature" = "hiphop",
      "hiphop" = "signature"
    )

    updateTabItems(session, "tabs", newtab)
  })

  output$targethip <- DT::renderDataTable({
    if (input$site == "phiphop") {
      w <- which(phiphop$name %in% input$cmpSERV)
      pdata <- phiphop
    } else {
      w <- which(phop$name %in% input$cmpSERV)
      pdata <- phop
    }

    d <- pdata[w, c(
      "name",
      "cmp",
      #  "signature",
      "PCID",
      "images",
      "mechanism",
      "MoA"
    )]

    PCID <- '<a href="https://pubchem.ncbi.nlm.nih.gov" target="_blank">PCID</a>'

    names(d)[4] <- "structure"

    names(d)[1] <- "screen"

    names(d)[3] <- PCID

    names(d)[2] <- "compound"

    opts <- list(
      dom = "Bfrtip", paging = FALSE, target = "cell", searching = FALSE,
      info = FALSE, autowidth = TRUE, scrollX = TRUE, ordering = FALSE,
      columnDefs = list(
        list(className = "dt-left", targets = c(0, 1, 4)),
        list(className = "dt-center", targets = c(2, 3)),
        list(width = c("200px"), targets = c(0, 2)),
        list(width = c("100px"), targets = c(1, 3, 6)),
        list(width = c("400px"), targets = c(5))
      )
    )

    datatable(d,
      options = opts, escape = FALSE,
      class = "table-bordered stripe table-condensed",
      rownames = FALSE
    )
  })

  observeEvent(input$targethip_rows_selected, {
    w <- which(phop$name %in% input$cmpSERV)

    row <- input$targethip_rows_selected

    respSEND$resp <- phop$signature[w][row]

    newtab <- switch(input$tabs,
      "hiphop" = "signature",
      "signature" = "hiphop"
    )

    updateTabItems(session, "tabs", newtab)
  })

  #######################################################
  ############# END SIGNATURE EVENTS ####################
  #######################################################

  ###################################################
  ################## RESET EVENTS ###################
  ###################################################

  observeEvent(input$resetscore,
    {
      updateSliderInput(session, "scorethreshSERV",
        label = NULL,
        value = 1.0, step = 0.1, min = 0, max = 5.0
      )
    },
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  observeEvent(input$resetCmp,
    {
      site <- input$site

      updateSelectizeInput(session, "cmpSERV",
        label = NULL, choices = NULL,
        selected = NULL
      )

      if (site == "phop") {
        updateSelectizeInput(session, "cmpSERV",
          label = NULL, choices = phop$name,
          selected = "camptothecin_360uM", server = TRUE
        )
      } else if (site == "phiphop") {
        updateSelectizeInput(session, "cmpSERV",
          label = NULL,
          choices = phiphop$name,
          selected = "camptothecin_360uM", server = TRUE
        )
      }
    },
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )

  ##################################################
  ################ START MOUSE-OVERS ###############
  ##################################################

  show_click <- reactiveVal(NULL)
  print_click <- reactiveVal(NULL)
  output$click_info <- renderUI(show_click())
  output$point_info <- DT::renderDataTable({
    df <- print_click()

    opts <- list(
      dom = "Bfrtip", paging = FALSE, target = "cell", searching = FALSE,
      info = FALSE, autowidth = TRUE, scrollX = TRUE, ordering = FALSE,
      columnDefs = list(
        list(width = c("10%"), targets = c(0, 1, 2)),
        list(width = c("70%"), targets = 3)
      )
    )

    df <- DT::datatable(df, options = opts, rownames = FALSE, escape = FALSE, selection = "single") %>%
      formatStyle(c(1:4), fontWeight = "bold", fontSize = "12px", target = "row")
  })

  observeEvent(input$plot_click, {
    whop <- which(colnames(d3) %in% input$cmpSERV)
    wboth <- which(colnames(dn3) %in% input$cmpSERV)

    if (length(whop) > 0) {
      x <- d3
    } else if (length(wboth) > 0) {
      x <- dn3
    }

    req(input$cmpSERV)
    w1 <- which(colnames(x) == input$cmpSERV)

    req(length(w1) > 0)

    validate(need(length(w1) == 1, message = "please enter a valid compound"))

    hop <- geneAnno(mat = x, cmp = colnames(x)[w1], fdat = fdat, xvar = FALSE)

    pclick <- nearPoints(
      df = hop, xvar = "gene", yvar = "FD",
      coordinfo = input$plot_click, threshold = 10
    )

    hideTooltip <- function(hide) {
      session$sendCustomMessage(type = "hideDraggable", message = list("hide" = hide))
    }

    g <- grep("FD", names(pclick))

    if (length(g) > 0) pclick[, g] <- format(round(pclick[, g], 2), nsmall = 1, scientific = FALSE)

    g <- which(names(pclick) %in% c("xvar", "gene"))

    if (length(g) > 0) pclick <- pclick[, -g]

    if (nrow(pclick) == 0) {
      show_click(NULL)

      hideTooltip(TRUE) # Hide tooltip if there's no info to show

      return()
    } else {
      session$sendCustomMessage(type = "placeDraggable", message = list())

      show_click(tagList({
        DT::dataTableOutput("point_info", width = "100%")
      }))

      print_click({
        pclick
      })
    }
  })

  show_eclick <- reactiveVal(NULL)
  print_eclick <- reactiveVal(NULL)
  output$eclick_info <- renderUI(show_eclick())
  output$point_einfo <- DT::renderDataTable({
    df <- print_eclick()

    opts <- list(
      dom = "Bfrtip", paging = FALSE, target = "cell", searching = FALSE,
      info = FALSE, autowidth = TRUE, scrollX = TRUE, ordering = FALSE,
      columnDefs = list(
        list(width = c("10%"), targets = c(0, 1, 2)),
        list(width = c("70%"), targets = 3)
      )
    )

    df <- DT::datatable(df, options = opts, rownames = FALSE, escape = FALSE, selection = "single") %>%
      formatStyle(c(1:4), fontWeight = "bold", fontSize = "12px", target = "row")
  })

  observeEvent(input$plot_eclick, {
    x <- de3

    req(input$cmpSERV)

    w1 <- which(colnames(x) == input$cmpSERV)

    req(length(w1) > 0)

    validate(need(length(w1) == 1, message = "please enter a valid compound"))

    hideTooltip <- function(hide) {
      session$sendCustomMessage(type = "ehideDraggable", message = list("hide" = hide))
    }

    hop <- geneAnno(mat = x, cmp = colnames(x)[w1], fdat = fdat, xvar = FALSE)

    peclick <- nearPoints(
      df = hop, xvar = "gene", yvar = "FD",
      coordinfo = input$plot_eclick, threshold = 5
    )

    g <- grep("FD", names(peclick))

    if (length(g) > 0) peclick[, g] <- format(round(peclick[, g], 2), nsmall = 1, scientific = FALSE)

    g <- which(names(peclick) %in% c("xvar", "gene"))

    if (length(g) > 0) peclick <- peclick[, -g]

    if (nrow(peclick) == 0) {
      show_eclick(NULL)

      hideTooltip(TRUE) # Hide tooltip if there's no info to show

      return()
    } else {
      session$sendCustomMessage(type = "eplaceDraggable", message = list())

      show_click(tagList({
        DT::dataTableOutput("point_info", width = "100%")
      }))

      print_click({
        peclick
      })
    }
  })

  ####################################################################### #######################################################################
  ####################### START GENEBYSCREEN ############################
  #######################################################################
  #######################################################################

  data_for_genebydrug <- reactive({
    req(input$gene)

    gene <- toupper(input$gene)

    xdat <- xinput()

    df <- pdata()

    w <- which(rownames(xdat) == gene)

    validate(need(length(w) == 1, message = "please enter a valid gene"))

    if (length(w) > 0) {
      mx2 <- mymeltdf(xdat, row = gene, df = df)
    }

    mx2
  })

  ################################################
  ################ GENEBYSCREEN PLOT #############
  ################################################

  output$genebydrug <- renderPlot({
    mx2 <- data_for_genebydrug()

    med <- median(mx2$fitness_defect)

    tit <- toupper(input$gene)

    mx2$fitness_defect <- round(mx2$fitness_defect, 2)

    mx2$sig <- 0

    mx2$shape <- 19

    g <- which(mx2$fitness_defect >= 15)

    if (length(g) > 0) mx2$sig[g] <- 17

    wx <- which(mx2$fitness_defect >= 1)

    if (length(wx) > 0) mx2$sig[wx] <- 1

    g <- ggplot(mx2, aes(
      x = screen, y = fitness_defect,
      col = factor(sig)
    )) +
      theme_bw() +
      geom_point(aes(col = factor(sig), shape = factor(shape), size = 15)) +
      scale_shape_manual(values = c(17, 19))

    g1 <- g + theme(legend.position = "none") +
      theme(panel.grid.minor = element_blank()) +
      theme(panel.grid.major = element_blank()) +
      theme(axis.ticks = element_blank(), axis.text.x = element_blank())


    g1 <- g1 + labs(y = "fitness defect score") + labs(x = "compound") +
      geom_hline(
        yintercept = median(mx2$fitness_defect), color = "black",
        linetype = "dashed", size = 1
      ) +
      ggtitle(tit)

    g2 <- g1 + geom_text_repel(
      size = 6,
      data = subset(mx2, sig == TRUE),
      aes(x = screen, y = fitness_defect, label = screen),
      point.padding = 0.25,
      segment.alpha = 0.2, col = "black"
    )

    g2 <- g2 + theme(
      axis.text = element_text(size = 18),
      axis.title = element_text(size = 24, face = "bold"),
      plot.title = element_text(size = 24, face = "bold")
    )

    g2
  })

  ####################################################################### #######################################################################
  ####################### START BRUSH PTS ###############################
  #######################################################################
  #######################################################################

  observeEvent(input$brushpts,
    {
      mx2 <- data_for_genebydrug()

      mx2$fitness_defect <- round(mx2$fitness_defect, 2)

      nam <- names(mx2)

      g <- grep("cmp", nam)

      pts <- brushedPoints(mx2, input$brushpts,
        xvar = "screen", yvar = "fitness_defect"
      )

      output$tabpts <- DT::renderDataTable(
        {
          pts
        },
        escape = FALSE,
        rownames = FALSE,
        selection = "single",
        server = FALSE
      )


      choices <- pts$screen
    },
    ignoreInit = TRUE,
    ignoreNULL = FALSE
  )


  observeEvent(input$tabpts_rows_selected, {
    row <- input$tabpts_rows_selected

    req(data_for_genebydrug())

    req(input$brushpts)

    mx2 <- data_for_genebydrug()

    mx2$fitness_defect <- round(mx2$fitness_defect, 2)

    nam <- names(mx2)

    g <- grep("cmp", nam)

    pts <- brushedPoints(mx2, input$brushpts, xvar = "screen", yvar = "fitness_defect")

    selected <- pts$screen[row]

    site <- input$site

    w <- which(rownames(d3) == toupper(input$gene))

    whop <- which(colnames(d3) %in% selected)

    wboth <- which(colnames(dint) %in% selected)

    if (length(whop) > 0) {
      site <- "phop"
    }

    if (length(wboth) > 0) {
      site <- "phiphop"
    }

    req(site)

    updatePrettyRadioButtons(session, "site",
      label = NULL,
      c(
        "2021:2024 HIPHOP" = "phiphop",
        "2020:2024 HOP" = "phop"
      ), selected = site
    )

    if (selected %in% phop$name) {
      choices <- phop$name
    } else if (selected %in% phiphop$name) {
      choices <- phiphop$name
    }

    updateSelectizeInput(session, "cmpSERV",
      label = NULL,
      choices = choices, selected = selected, server = TRUE
    )

    newtab <- switch(input$tabs,
      "genebyscreen" = "hiphop",
      "hiphop" = "genebyscreen"
    )

    updateTabItems(session, "tabs", newtab)
  })

  ####################################################################### #######################################################################
  ####################### END BRUSH PTS #################################
  #######################################################################
  #######################################################################

  output$geneinfo <- renderDataTable(
    {
      req(xinput())

      xdat <- xinput()

      w <- which(fdat$sgd_gene %in% rownames(xdat))

      fd <- fdat[w, ] %>% distinct(sgd_gene, .keep_all = TRUE)

      w1 <- which(fd$sgd_gene %in% toupper(input$gene))

      validate(need(length(w1) == 1, message = "please enter a valid gene"))

      f <- fd[w1, c("sgd_orf", "sgd_gene", "link", "descriptor")]

      names(f)[1] <- c("ORF")

      names(f)[2] <- c("GENE")

      f
    },
    escape = FALSE,
    options = list(
      paging = FALSE, searching = FALSE,
      info = FALSE, scrollX = TRUE, scrollY = TRUE, scrollCollapse = TRUE, autowidth = TRUE
    ),
    rownames = FALSE
  )

  #######################################################################
  ######################### END GENEBYSCREEN ############################
  #######################################################################

  ##################################################
  ########### START GO ENRICHMENT BY RANK ##########
  ##################################################

  ##################################################
  ############### START outFit outMit ##############
  ##################################################

  outFit <- reactiveValues(dFit = NULL)

  outMit <- reactiveValues(mFit = NULL)

  observeEvent(input$gene, {
    req(input$gene)

    req(input$site)

    if (input$site == "phiphop") {
      xfit <- xfithh
    }
    if (input$site == "phop") {
      xfit <- xcofit
    }

    if (input$site == "phiphop") {
      xpv <- xpithh
    }
    if (input$site == "phop") {
      xpv <- xpfit
    }

    ww <- which(colnames(xfit) == toupper(input$gene))

    validate(need(length(ww) == 1, message = "please enter a valid gene"))

    if (length(ww) != 0) {
      p <- xpv[, ww, drop = F]
      d <- xfit[, ww, drop = F]
    }

    d <- d[order(d[, 1], decreasing = TRUE), , drop = F]

    df <- data.frame(
      gene = rownames(d), cofit = d[rownames(d), 1],
      pvalue = p[rownames(d), 1], stringsAsFactors = FALSE
    )

    w <- which(df$cofit >= 0)

    if (length(w) > 0) df <- df[w, ]

    xfit <- df

    xfit$cofit <- as.numeric(xfit$cofit)

    xfit$pvalue <- as.numeric(xfit$pvalue)

    xfit$pvalue <- formatC(xfit$pvalue, format = "e", digits = 2)

    xfit$cofit <- format(round(xfit$cofit, 6), nsmall = 2)

    mfit <- as.matrix(xfit[, 2:3])

    mfit[, 2] <- 1:nrow(mfit)

    colnames(mfit)[2] <- "rank"

    m <- mfit

    sss <- rev(seq(0, 1, length.out = nrow(mfit)))

    ss <- round(sss, 4)

    m <- cbind(m, ss)

    mm <- apply(m, 2, as.numeric)

    rownames(mm) <- rownames(m)

    outFit$dFit <- xfit

    outMit$mFit <- mm
  })

  ##################################################
  ############### END outFit outMit ################
  ##################################################

  output$fiToutput <- renderUI({
    tagList(
      fluidRow(
        box(
          title = "Set cofitness rank threshold:",
          HTML("<h5><b>Top: Genes ranked by cofitness,
      left to right;

   Bottom: Corresponding cofitness 0
   Click on arrow to animate GO enrichment
   dependency on cofitness rank.</b></h5>"),
          br(), br(),
          HTML("<h5><b>Cofitness values:</b></h5>"),
          st = rev(seq(0, 1, 0.05)),
          sliderTextInput(
            inputId = "corrFIT", label = NULL,
            choices = st, grid = TRUE, animate = TRUE
          ),
          HTML("<h5><b>Corresponding cofitness rank:</b></h5>"),
          sliderInput("scoreFIT",
            label = NULL,
            value = 1, min = 1, max = 1000, step = 1, animate = TRUE
          ),
          HTML("<h5><b>Set FDR threshold for GO enrichment by RANK:</b></h5>"),
          sliderInput("fdrSERV",
            label = NULL,
            min = 0, max = 0.5, value = 0.1, step = 0.05
          ),
          status = "primary", solidHeader = TRUE, width = "100%", height = 650
        )
      )
    )
  })


  rankFIT <- debounce(reactive(input$scoreFIT), 500)
  coFIT <- debounce(reactive(input$corrFIT), 500)

  scoreVal <- reactiveValues(
    previous = NULL,
    current  = NULL
  )

  corrVal <- reactiveValues(
    previous = NULL,
    current  = NULL
  )

  observeEvent(input$corrFITRUE,
    {
      req(input$scoreFIT)

      req(input$corrFIT)

      print(c("input$scoreFIT", input$scoreFIT))
      print(c("input$corrFIT", input$corrFIT))

      corr <- input$corrFIT

      mfit <- outMit$mFit

      sss <- rev(seq(0, 1, length.out = nrow(mfit)))

      ss <- round(sss, 4)

      w <- which(mfit[, 3] == corr)

      req(length(w) > 0)

      score <- mfit[w, "rank"]

      w1 <- slid(x = input$corrFIT, y = input$scoreFIT, mfit = mfit)

      score1 <- w1$corr[2]

      test <- c(as.numeric(input$scoreFIT) != scoreVal$current)

      test2 <- as.numeric(input$scoreFIT) != score

      cond <- is.null(scoreVal$current)

      if (is.null(scoreVal$current)) {
        updateSliderInput(
          session = session,
          inputId = "scoreFIT", value = score
        )
      } else if (!is.null(scoreVal$current) &
        as.numeric(input$scoreFIT) != score) {
        updateSliderInput(
          session = session,
          inputId = "scoreFIT", value = score
        )
      }

      scoreVal$previous <- scoreVal$current

      scoreVal$current <- score
    },
    ignoreNULL = FALSE,
    ignoreInit = TRUE
  )

  observeEvent(input$scoreFIT,
    {
      mfit <- outMit$mFit

      req(input$scoreFIT)
      req(input$corrFIT)

      w1 <- slid(x = input$corrFIT, y = input$scoreFIT, mfit = mfit)

      score1 <- w1$score[1]

      rank <- input$scoreFIT
      w <- which(mfit[, 2] == rank)
      req(length(w) > 0)
      score <- mfit[w, 3]

      cest <- as.numeric(input$corrFIT) != score

      cest2 <- as.numeric(input$corrFIT) != corrVal$current

      req(cest == TRUE | cest == FALSE)

      print(c("cest", cest))

      sq <- rev(seq(0, 1, 0.001))
      sss <- rev(seq(0, 1, length.out = nrow(mfit)))

      ss <- round(sss, 4)

      if (is.null(corrVal$current)) {
        updateSliderTextInput(session, "corrFIT",
          label = NULL,
          choices = ss, selected = score
        )
      } else if (as.numeric(input$corrFIT) != score) {
        updateSliderTextInput(session, "corrFIT",
          label = NULL,
          choices = ss, selected = score
        )
      }

      corrVal$previous <- corrVal$current

      corrVal$current <- score
    },
    ignoreNULL = FALSE,
    ignoreInit = FALSE
  )

  output$mxFIT <- renderUI({
    tagList(
      box(
        title = "Optimizing GO enrichment:",
        tags$head(tags$style(HTML("<h5><b>label(font-family:Arial;
      font-weight:bold;font-size;20)<h5><b>"))),
        HTML("<h5><b>Finds the cofitness/cofitness rank that
      maximizes the number of GO enrichment terms for the
      top 1000 cofit strains for a given FDR value. Stepsize
      is the increment between ranks tested.<h5><b>"),
        HTML("<h5><b>Choose a stepsize and FDR threshold,
      then hit GO to run.<h5><b>"),
        HTML("<h5><b>Warning: calculation time for stepsize 1: ~ 3min.<h5><b>"),
        br(),
        fluidRow(
          column(
            width = 12, align = "center",
            prettyRadioButtons(
              inputId = "stepsize", label = "stepsize",
              choices = c(1, 5, 20, 50), outline = TRUE, fill = FALSE, thick = TRUE,
              status = "primary", shape = "round", bigger = TRUE,
              width = "100%", selected = 50, inline = TRUE
            )
          ),
          HTML("<h5><b>&nbsp&nbsp&nbsp&nbsp&nbspSet FDR threshold</b></h5>"),
          br(),
          column(
            width = 12, align = "center",
            sliderInput("fdrRANK",
              label = NULL, min = 0, max = 0.5,
              value = 0.2, step = 0.1
            )
          ),
          column(
            width = 12, align = "left",
            actionButton(inputId = "rankGO", label = "GO!", class = "btn-success")
          ),
          column(
            width = 12, align = "left",
            tags$head(tags$style(HTML("<h5><b>label(font-family:Arial;
       ,font-weight:bold;font-size;20)<h5><b>"))),
            htmlOutput("finishTEXT"),
            htmlOutput("stepTEXT"),
            withSpinner(htmlOutput("mxCOFITvalue"),
              type = 5,
              color = "#4d88ff", hide.ui = FALSE, size = 0.5
            )
          )
        ),
        status = "primary", solidHeader = TRUE, width = "100%", height = 650
      )
    )
  })

  gseaMAX <- reactiveValues(
    rank = NULL,
    cofit = NULL,
    goterms = NULL
  )

  observeEvent(outFit$dFit,
    {
      useShinyjs()
      click("rankGO")
      reset("scoreFIT")
      reset("fdrRANK")
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE,
    once = TRUE
  )

  observeEvent(input$gene,
    {
      useShinyjs()
      reset("scoreFIT")
      reset("fdrRANK")
      shinyjs::hide(id = "finishTEXT")
      shinyjs::hide(id = "stepTEXT")
      shinyjs::hide(id = "mxCOFITvalue")
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )


  observeEvent(input$fdrRANK,
    {
      useShinyjs()
      reset("scoreFIT")
      shinyjs::hide(id = "finishTEXT")
      shinyjs::hide(id = "stepTEXT")
      shinyjs::hide(id = "mxCOFITvalue")
      reset("corrFIT")
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  observeEvent(input$stepsize,
    {
      useShinyjs()
      # click("rankGO")
      reset("scoreFIT")
      reset("fdrRANK")
      shinyjs::hide(id = "finishTEXT")
      shinyjs::hide(id = "stepTEXT")
      shinyjs::hide(id = "mxCOFITvalue")
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE,
    once = FALSE
  )


  observeEvent(input$rankGO,
    {
      req(outMit$mFit)

      req(input$fdrSERV)

      gseaMAX$rank <- NULL
      gseaMAX$cofit <- NULL
      gseaMAX$goterms <- NULL
      shinyjs::hide(id = "finishTEXT")
      shinyjs::hide(id = "stepTEXT")
      shinyjs::hide(id = "mxCOFITvalue")

      mfit <- outMit$mFit

      FDR <- input$fdrRANK

      mn <- 1

      mx <- 1000

      steps <- pretty(c(mn, mx), n = 100)

      range <- max(steps) - min(steps)

      step <- range / (length(steps) - 1)


      if (as.numeric(input$stepsize) == 1) {
        steps <- 1:1000
      } else if (as.numeric(input$stepsize) == 5) {
        steps <- seq(0, 1000, 5)
      } else if (as.numeric(input$stepsize) == 20) {
        steps <- seq(0, 1000, 20)
      } else if (as.numeric(input$stepsize) == 50) {
        steps <- seq(0, 1000, 50)
      }

      dFIT <- NULL

      for (i in 1:length(steps)) {
        dFIT[[i]] <- compRANK(mfit, coln = 2, rank = steps[i])
      }

      names(dFIT) <- steps

      net <- lapply(dFIT, runGOENRICH,
        fdrThresh = FDR, minSetSize = 5,
        curr_exp = "tst", bp_path = "2023_January30_GOBP.RDS",
        go_path = "2023_January30_GOID_GOBP_SGD.txt"
      )

      enrich <- lapply(net, function(x) x <- x[[1]])

      isnull <- sapply(enrich, is.null)

      wisnull <- which(isnull == TRUE)

      nrw <- sapply(enrich, nrow)

      if (length(wisnull) > 0) isnull <- lapply(nrw[wisnull], function(x) x <- 0)

      nrw[wisnull] <- isnull

      unrw <- unlist(nrw)

      mx <- which.max(unrw)

      nmx <- as.numeric(names(mx))

      cofit <- mfit[nmx, 1]

      goterms <- unrw[mx]

      if (length(nmx) > 0) gseaMAX$rank <- nmx

      if (length(nmx) > 0) gseaMAX$cofit <- cofit

      if (!is.null(unrw[mx])) gseaMAX$goterms <- goterms

      shinyjs::show(id = "finishTEXT")

      shinyjs::show(id = "stepTEXT")

      shinyjs::show(id = "mxCOFITvalue")

      req(!is.null(gseaMAX$cofit))

      output$finishTEXT <- renderText({
        paste("<h5><b>", "Results:", "</b></h5>")
      })

      output$stepTEXT <- renderText({
        paste("<h5><b>", "STEPSIZE = ", input$stepsize, ";", "FDR = ", input$fdrSERV, "</b></h5>")
      })

      output$mxCOFITvalue <- renderText({
        req(!is.null(gseaMAX$cofit))

        paste(
          "<h5><b>", "COFITNESS = ", round(as.numeric(isolate(gseaMAX$cofit)), 3),
          ";", "RANK = ", isolate(gseaMAX$rank), "</b></h5>"
        )
      })
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )


  output$mxCOFITvalue <- renderText({
    NULL
  })

  output$GOterms <- renderUI({
    htmlOutput("countGO")
  })

  observeEvent(gseaMAX$goterms, {
    output$countGO <- renderText({
      paste(
        "<h5><b>", "no. GO terms =",
        gseaMAX$goterms, "</b></h5>"
      )
    })
  })

  observeEvent(gseaMAX$rank,
    {
      req(outMit$mFit)

      req(!is.null(gseaMAX$rank))

      mfit <- outMit$mFit

      mx <- nrow(mfit)

      value <- gseaMAX$rank

      scoreVal$previous <- input$scoreFIT

      scoreVal$current <- gseaMAX$rank

      updateSliderInput(session, "scoreFIT",
        label = NULL, min = 1,
        max = 1000, value = gseaMAX$rank, step = 1
      )
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  observeEvent(rankFIT(),
    {
      if (is.null(scoreVal$current)) scoreVal$current <- rankFIT()

      if (rankFIT() != scoreVal$current) {
        scoreVal$previous <- scoreVal$current

        scoreVal$current <- rankFIT()
      }

      mfit <- outMit$mFit

      mx <- nrow(mfit)

      w <- which(mfit[, 2] == rankFIT())

      req(length(w) != 0)

      corel <- mfit[w, 1]

      if (length(corel) > 1) corel <- corel[1]

      updateSliderInput(session, "corrFIT",
        label = NULL,
        min = 0, max = 1.0, value = corel, step = 0.1
      )

      corrVal$previous <- corrVal$current

      corrVal$current <- corel # }
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  ###########################################################
  ###################### END COFITNESS ######################
  ###########################################################

  ##################################################
  ########### START GO ENRICHMENT BY RANK ##########
  ##################################################

  goEnrich <- reactive({
    req(input$fdrSERV)

    req(outMit$mFit)

    req(input$scoreFIT)

    mfit <- outMit$mFit

    gseaMAX$goterms <- NULL

    wfit <- which(mfit[, 2] <= input$scoreFIT)

    validate(need(length(wfit) != 0, message = "No CoFitness scores above threshold"))

    req(length(wfit) != 0)

    qFIT <- compRANK(mfit, coln = 2, rank = input$scoreFIT)

    curr_exp <- "cofit"

    FDR <- input$fdrSERV

    network <- runGOENRICH(
      fdrThresh = FDR, minSetSize = 5,
      curr_exp = curr_exp, score = qFIT, bp_path = "2023_January30_GOBP.RDS",
      go_path = "2023_January30_GOID_GOBP_SGD.txt"
    )

    validate(need(!is.null(network$enrichInfo), message = "No GO enrichment,
  try relaxing the FDR or scorethreshold"))

    enrichInfo <- network$enrichInfo

    req(!(is.null(enrichInfo)))

    if (!is.null(enrichInfo)) gseaMAX$goterms <- nrow(enrichInfo)

    edgeMat <- network$edgeMat

    if (!is.null(enrichInfo)) {
      return(network)
    }
  })

  ##################################################
  ########### END GO ENRICHMENT BY RANK ############
  ##################################################

  ##################################################
  ################### START VISNET #################
  ##################################################

  visNet <- reactive({
    req(!is.null(goEnrich()$enrichInfo))

    enrich <- goEnrich()$enrichInfo

    edge <- goEnrich()$edgeMat

    vis <- visSetup(enrichInfo = enrich, edgeMat = edge, fontsize = 20, fontface = "Courier")

    vis
  })

  output$GoTable <- renderUI({
    req(input$network_Proxy_selected)

    DT::dataTableOutput("gotermTable")
  })

  EnrichReact <- reactive({
    req(!is.null(visNet()$nodes))

    enrich <- visNet()$nodes

    req(length(nrow(enrich)) > 0)

    row <- input$EnrichTable_rows_selected

    out <- OutEnrich()

    id <- out$id[row]

    id
  })

  observeEvent(EnrichReact(),
    {
      req(EnrichReact())

      id <- EnrichReact()

      visNetworkProxy("network_Proxy") %>%
        visSelectNodes(id = id)
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )

  OutEnrich <- reactive({
    req(goEnrich()$enrichInfo)

    enrich <- goEnrich()$enrichInfo

    enrich[, c("querySetFraction", "geneSetFraction", "foldEnrichment")] <-
      format(round(enrich[, c(
        "querySetFraction", "geneSetFraction",
        "foldEnrichment"
      )], 2), nsmall = 1, scientific = FALSE)

    enrich[, c("P", "FDR")] <-
      format(signif(enrich[, c("P", "FDR")], 2), nsmall = 1, scientific = T)

    w <- which(names(enrich) %in% c(
      "formattedLabel", "cluster", "size",
      "filename", "maxOverlapGeneScore"
    ))

    enrich <- enrich[, -w]

    enrich <- enrich[, c(
      "id", "GOID", "term", "nGenes",
      "nQuery", "nOverlap", "querySetFraction", "geneSetFraction",
      "foldEnrichment", "P", "FDR", "overlapGenes"
    )]

    enrich
  })

  output$EnrichTable <- DT::renderDataTable({
    out <- OutEnrich()

    out <- out[, c(
      "GOID", "term", "querySetFraction",
      "geneSetFraction", "foldEnrichment", "FDR", "overlapGenes"
    )]

    opts <- list(
      pageLength = 5, paging = FALSE,
      info = FALSE, autowidth = TRUE, scrollX = TRUE, ordering = FALSE,
      columnDefs = list(
        list(width = c("10%"), targets = c(0, 2, 3, 4, 5)),
        list(width = c("70%"), targets = c(1))
      )
    )

    df <- DT::datatable(out,
      options = opts, rownames = FALSE,
      escape = FALSE, selection = "single"
    ) %>%
      formatStyle(c(1:7), fontWeight = "bold")
  })

  output$enrichdownload <- downloadHandler(
    filename = function() {
      paste0("enrich:", input$gene, "_", Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(OutEnrich(), file, row.names = FALSE, sep = "\t")
    }
  )

  ##################################################
  ################### END VISNET ###################
  ##################################################

  ##################################################
  ################### START COFITNESS ##############
  ##################################################

  output$coFitNess <- DT::renderDataTable(
    {
      outFit$dFit
    },
    escape = FALSE,
    rownames = FALSE,
    options =
      list(pageLength = 10),
    selection = "single",
    server = FALSE
  )

observeEvent(input$coFitNess_rows_selected, {

row <- input$coFitNess_rows_selected

xfit <- outFit$dFit

w <- xfit$gene[row]

validate(need(length(w) == 1, message = "please enter a valid gene"))

updateTextInput(session,
  inputId = "gene",
  label = NULL, value = xfit$gene[row]
)
  })
  ####################################################

  output$downloadcofitness <- downloadHandler(
filename = function() {
  paste0("CoFit:", input$gene, "_", Sys.Date(), ".txt")
},
content = function(file) {
  write.table(as.data.frame(outFit$dFit),
file,
row.names = FALSE, sep = "\t", quote = FALSE
  )
}
  )

  ##################################################
  ###################### END COFITNESS #############
  ##################################################

  #######################################################
  #######################################################
  ################## START COINHIBITION #################
  #######################################################
  #######################################################

outcoInhib <- reactive({

req(input$cmpSERV)

if (input$cmpSERV %in% phiphop$name) {
  xcov <- xcohh
  xpv <- xpvhh
  pdata <- phiphop
} else {
  xcov <- xcoinh
  xpv <- xpval
  pdata <- phop
}

w <- which(colnames(xcov) == input$cmpSERV)

validate(need(length(w) == 1, message = "please enter a valid condition"))

p <- xpv[, w, drop = F]
d <- xcov[, w, drop = F]

d <- d[order(d[, 1], decreasing = TRUE), , drop = F]

df <- data.frame(
  screen = rownames(d)[1:nrow(d)],
  CoI = d[1:nrow(d), 1], pvalue = p[rownames(d), 1],
  stringsAsFactors = FALSE
)

df$pvalue <- formatC(df$pvalue, format = "e", digits = 2)

w <- which(df$CoI >= 0)

if (length(w) > 0) df <- df[w, ]

m <- match(df$screen, pdata$name)

df$CoI <- format(round(df$CoI, 2), nsmall = 1, scientific = FALSE)

df$CID <- pdata$PCID[m]

df$struture <- pdata$image[m]

w <- which(names(df) == "CID")

names(df)[w] <- '<a href="https://pubchem.ncbi.nlm.nih.gov" target="_blank">CID</a>'

df$mechanism <- pdata$mechanism[m]

df

})

output$coInhib <- renderDataTable({

xcoinhib <- outcoInhib()

opts <- list(
  dom = "Bfrtip",
  pageLength = 5,
  autoWidth = FALSE,
  scrollX = TRUE
)

DT::datatable(xcoinhib,
  options = opts,
  escape = FALSE, rownames = FALSE, selection = "single")

  })

observeEvent(input$coInhib_rows_selected, {
row <- input$coInhib_rows_selected

df <- outcoInhib()

updateSelectizeInput(session, "cmpSERV",
  label = NULL,
  choices = df$screen, selected = df$screen[row][1], server = TRUE
)
  })

  output$downloadcoinhib <- downloadHandler(
filename = function() {
  paste0(
"HOP:",
input$cmpSERV,
"_", Sys.Date(), ".txt"
  )
},
content = function(file) {
  write.table(as.data.frame(outcoInhib()), file, row.names = FALSE, sep = "\t", quote = FALSE)
}
  )

  ##################################################
  ################# END COINHIBITION ###############
  ##################################################

  ##################################################
  ################# START PROXY ####################
  ##################################################

output$network_Proxy <- renderVisNetwork({

req(visNet()$nodes)

vis <- visNet()

n <- visNet()$nodes
req(n)

w <- nrow(n)
n <- n %>% arrange(term)

names <- n$id

if (nrow(vis$edges) == 0) {
  visNetwork(vis$nodes, width = "100%") %>%

    visNodes(shadow = list(enabled = T, size = 25), borderWidth = 1) %>%
      visOptions(
          highlightNearest = list(enabled = T, degree = 5, hover = TRUE),
          nodesIdSelection = list(
            enabled = TRUE, values = names,
            style = "width: 500px; height = 31px; font-size: 18px;
   color: #000066;border: 3px solid #4d88ff;"
          ),
          selectedBy = list(
            variable = "FDR",
            style = "width: 500px; height = 31px; font-size: 18px;
     color: #000066;border: 3px solid #4d88ff;"
          )
        ) %>%
        visEvents(select = "function(nodes) {
  Shiny.onInputChange('current_node_id', nodes.nodes);;}")
    } else {
      visNetwork(vis$nodes, vis$edges, width = "100%") %>%
        visNodes(shadow = list(enabled = TRUE, size = 25)) %>%
        visOptions(
          highlightNearest = list(enabled = TRUE, degree = 5, hover = TRUE),
          nodesIdSelection = list(
            enabled = TRUE, values = names,
            style = "width: 500px; height = 31px; font-size: 18px;
      color: #000066;border: 3px solid #4d88ff;"
          ),
          selectedBy = list(
            variable = "FDR",
            style = "width: 500px; height = 31px; font-size: 18px;
      color: #000066;border: 3px solid #4d88ff;"
          )
        ) %>%
        visIgraphLayout(type = "full") %>%
        visEvents(select = "function(nodes) {
    Shiny.onInputChange('current_node_id', nodes.nodes);;}")
    }
  })

  observeEvent(input$network_Proxy_selectedBy,
    {
      req(input$network_Proxy_selectedBy)

      n <- visNet()$nodes

      w <- which(n$FDR %in% as.numeric(input$network_Proxy_selectedBy))

      id <- n$id[w]


      visNetworkProxy("network_Proxy") %>%
        visSelectNodes(id = id)
    },
    ignoreNULL = TRUE,
    ignoreInit = FALSE
  )


  ##################################################
  ################### END PROXY ####################
  ##################################################

  ######################################################################
  ######################### START BARPLOTS #############################
  ######################################################################

  output$gotermTable <- DT::renderDataTable({
    req(input$network_Proxy_selected)

    vis <- visNet()

    n <- visNet()$nodes

    w <- which(vis$nodes$id %in% c(input$network_Proxy_selected))

    req(length(w) > 0)

    term <- vis$nodes$label[w]

    nam <- c("term", "nGenes", "geneSetFraction", "FDR")

    m <- match(nam, names(vis$nodes))

    n <- vis$nodes[w, m]

    term <- vis$nodes$label[w]

    nam <- c("term", "nGenes", "geneSetFraction", "FDR")

    m <- match(nam, names(vis$nodes))

    names(vis$nodes)[m] <- c("GO term", "geneSet size", "% of geneSet", "FDR")

    n <- vis$nodes[w, m]

    req(nrow(n) != 0)

    t <- t(n[, 2:4])

    datatable(t,
      width = 220, caption = htmltools::tags$caption(term,
        style = "caption-side: top; text-align: center; color:black;b
  background:white;font-weight:bold;"
      ),
      options = list(
        paging = FALSE, scrollY = FALSE, dom = "t", scroller = FALSE,
        searching = FALSE, ordering = FALSE, rowCallback = JS(
          "function(row, data) {",
          "for (i = 1; i < data.length; i++) {",
          "if (data[i]>1000 | data[i]<1){",
          "$('td:eq('+i+')', row).html(data[i].toExponential(1));", "}", "}", "}"
        )
      ),
      height = 400, colnames = NULL
    ) %>%
      formatStyle(
        target = "row", color = "black", backgroundColor = "white",
        columns = c(1, 2), fontWeight = "bold"
      )
  })

  output$LeadingEdge <- renderUI({
    req(input$network_Proxy_selected)

    plotOutput("BarPlot",
      width = input$shiny_width / 300,
      height = input$shiny_height / 300
    )
  })

  output$BarPlot <- renderPlot({
    req(input$network_Proxy_selected)

    vis <- visNet()

    n <- visNet()$nodes

    w <- which(vis$nodes$id %in% c(input$network_Proxy_selected))

    req(length(w) > 0)

    n <- vis$nodes[w, ]

    req(nrow(n) != 0)

    s6 <- geneBARPLOT(n$overlapGenes)

    d <- s6

    if (nrow(d) > 10) d <- d[1:10, ]

    barplot(d$score,
      names.arg = d$gene, las = 1, horiz = TRUE, cex.axis = 0.8,
      cex.lab = 1.1, col = "navy", xlab = "median fitness defect score"
    )
  })

  ######################################################################
  ######################### END BARPLOTS ###############################
  ######################################################################

  ####################################################
  ################ START HIPHOP TABLES ###############
  ####################################################

  output$hiptab <- DT::renderDataTable(
    {
      req(input$cmpSERV)

      mat <- de3

      hip <- geneAnno(mat,
        fdat = fdat, sgdlink = TRUE,
        cmp = input$cmpSERV, arrange = TRUE, xvar = FALSE
      )

      hip <- hip[, c("FD", "ORF", "GENE", "descriptor")]

      hip$FD <- format(round(hip$FD, 2), nsmall = 1, scientific = FALSE)

      opts <- list(
        pageLength = 5, autoWidth = TRUE, scrollX = TRUE,
        columnDefs = list(list(width = "80px", targets = c(-2, -3)))
      )

      hip
    },
    escape = FALSE,
    options = opts,
    rownames = FALSE
  )

  outhip <- reactive({
    req(input$cmpSERV)

    hip <- geneAnno(mat = de3, fdat = fdat, cmp = input$cmpSERV, xvar = FALSE)

    hip <- hip[, c("gene", "GENE", "FD", "descriptor")]

    hip
  })

  observeEvent(input$hiptab_rows_selected, {
    row <- input$hiptab_rows_selected

    df <- outhip()

    updateTextInput(session,
      inputId = "gene", label = NULL,
      value = df$gene[row]
    )

    newtab <- switch(input$tabs,
      "hiphop" = "genebyscreen",
      "genebyscreen" = "hiphop"
    )

    updateTabItems(session, "tabs", newtab)
  })

  output$downloadhip <- downloadHandler(
    filename = function() {
      paste0("HIP:", input$cmpSERV, "_", Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(as.data.frame(outhip()), file,
        row.names = FALSE,
        sep = "\t", quote = FALSE
      )
    }
  )

  output$efd <- renderPlot({
    req(input$cmpSERV)
    validate(need(input$cmpSERV,
      message =
        "please select condition"
    ))

    cond1 <- input$cmpSERV

    x <- de3

    wx <- which(colnames(x) %in% cond1)

    req(length(wx) != 0)
    req(input$site == "phiphop")
    colnames(x)[wx] <- paste0("HIP|", colnames(x)[wx])
    coln <- colnames(x)[wx]
    coln <- str_wrap(coln, width = 30, whitespace_only = FALSE)
    colnames(x)[wx] <- coln
    p10(x, wx, pch = 17, ylim = input$elim, sig = input$scorethreshSERV)
  })

  essPlot <- function() {
    validate(
      need(input$cmpSERV,
        message =
          "please select condition"
      )
    )
    cond1 <- input$cmpSERV
    if (input$site == "phiphop") {
      x <- de3
    }

    wx <- which(colnames(x) %in% cond1)

    colnames(x)[wx] <- paste0("HIP|", colnames(x)[wx])

    p10(x, wx, pch = 17, ylim = input$elim, sig = input$scorethreshSERV)
  }
  ##########################
  output$ExportessPlot <- downloadHandler(
    filename = function() {
      paste0("HIPplot:", input$cmpSERV, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file,
        width = (input$shiny_width / 12) * 6,
        height = 1000
      )

      essPlot()
      dev.off()
    }
  )

  output$hoptab <- DT::renderDataTable(
    {
      req(input$cmpSERV)

      if (input$site == "phop") {
        mat <- d3[, phop$name]
      } else if (input$site == "phiphop") {
        mat <- dn3
      }

      hop <- geneAnno(mat,
        fdat = fdat, sgdlink = TRUE, cmp = input$cmpSERV,
        arrange = TRUE, xvar = FALSE
      )

      hop <- hop[, c("FD", "ORF", "GENE", "descriptor")]

      hop$FD <- format(round(hop$FD, 2), nsmall = 1, scientific = FALSE)

      opts <- list(
        pageLength = 5, autoWidth = TRUE, scrollX = TRUE,
        columnDefs = list(list(width = "80px", targets = c(-2, -3)))
      )

      hop
    },
    escape = FALSE,
    options = opts,
    rownames = FALSE
  )

  outhop <- reactive({
    req(input$cmpSERV)

    if (input$site == "phop") {
      mat <- d3[, phop$name]
    } else if (
      input$site == "phiphop") {
      mat <- dn3
    }

    hop <- geneAnno(mat = mat, cmp = input$cmpSERV, fdat = fdat, xvar = FALSE)
    hop <- hop[, c("gene", "GENE", "FD", "descriptor")]
    hop$FD <- format(round(hop$FD, 2), nsmall = 1, scientific = FALSE)

    hop
  })

  observeEvent(input$hoptab_rows_selected, {
    row <- input$hoptab_rows_selected

    df <- outhop()

    updateTextInput(session, inputId = "gene", label = NULL, value = df$gene[row])
  })

  output$downloadhop <- downloadHandler(
    filename = function() {
      paste0("HOP:", input$cmpSERV, "_", Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(as.data.frame(outhop()), file,
        row.names = FALSE,
        sep = "\t", quote = FALSE
      )
    }
  )

  output$nfd <- renderPlot({
    req(input$site)
    req(input$cmpSERV)

    validate(need(input$cmpSERV, message = "please select condition"))

    cond1 <- input$cmpSERV

    if (input$site == "phiphop") {
      x <- dn3
    }
    if (input$site == "phop") {
      x <- d3
    }

    wx <- which(colnames(x) %in% cond1)

    colnames(x)[wx] <- paste0("HOP|", colnames(x)[wx])

    if (input$site == "phiphop") {
      coln <- colnames(x)[wx]
      coln <- str_wrap(coln, width = 30, whitespace_only = FALSE)
      colnames(x)[wx] <- coln
    }

    req(length(wx) > 0)
    p10(x, wx, pch = 17, ylim = input$ylim, sig = input$scorethreshSERV)
  })
  ##########################
  nonPlot <- function() {
    validate(
      need(input$cmpSERV, message = "please select condition")
    )

    cond1 <- input$cmpSERV

    if (input$site == "phop") {
      x <- d3[, phop$name]
    } else if (input$site == "phiphop") {
      x <- dn3
    }

    wx <- which(colnames(x) %in% cond1)

    colnames(x)[wx] <- paste0("HOP|", colnames(x)[wx])

    p10(x, wx, pch = 17, ylim = input$ylim, sig = input$scorethreshSERV)
  }
  ##########################
  output$ExportnonPlot <- downloadHandler(
    filename = function() {
      paste0("HOPplot:", input$cmpSERV, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      png(file,
        width = (input$shiny_width / 12) * 6,
        height = 1000
      )

      nonPlot()
      dev.off()
    }
  )

  #######################################################
  #######################################################
  ############# END HIPHOPTABLES  AND PLOTS #############
  #######################################################
  #######################################################

  #######################################################
  #######################################################
  ################## START Y-AXIS LIMITS ################
  #######################################################
  #######################################################

  output$ylimits <- renderUI({
    x <- xinput()

    req(input$cmpSERV)

    coln <- colnames(x)

    wcoln <- which(coln %in% input$cmpSERV)

    req(length(wcoln) > 0)

    req(xinput())
    req(input$cmpSERV)

    x <- xinput()

    wne <- which(rownames(x) %in% noness$sgd_gene)

    we <- which(rownames(x) %in% ess$sgd_gene)

    if (length(wne) > 0) {
      ylims <- c(
        floor(min(x[wne, wcoln, drop = F], na.rm = TRUE)),
        ceiling(max(x[wne, wcoln, drop = F], na.rm = TRUE))
      )
    }

    req(length(wne) > 0)

    if (length(we) > 0) {
      elims <- c(
        floor(min(x[we, wcoln, drop = F], na.rm = TRUE)),
        ceiling(max(x[we, wcoln, drop = F], na.rm = TRUE))
      )
    }

    tagList(
      if (length(we) > 0) {
        column(
          width = 3,
          box(
            title = "HIP y-axis range:",
            fluidRow(column(
              width = 12,
              sliderInput("elim",
                label = NULL, min = elims[1],
                max = elims[2], value = c(-0.5, elims[2])
              )
            )),
            fluidRow(column(
              width = 12, align = "center",
              actionLink(
                inputId = "eorig",
                label = "reset",
                style = "height: 100px;width: 150px;font-size:120%;text-align:center",
                size = "lg"
              )
            )),
            status = "primary", solidHeader = TRUE, width = "100%", height = 200
          )
        )
      },
      if (length(ylims) == 2) {
        column(
          width = 3,
          box(
            title = "HOP y-axis range:",
            fluidRow(column(
              width = 12,
              sliderInput("ylim",
                label = NULL, min = ylims[1],
                max = ylims[2], value = c(-0.5, ylims[2])
              )
            )),
            fluidRow(column(
              width = 12, align = "center",
              actionLink(
                inputId = "yorig",
                label = "reset",
                style = "height: 100px;width: 150px;font-size:120%;text-align:center",
                size = "lg"
              )
            )),
            status = "primary", solidHeader = TRUE, width = "100%", height = 200
          )
        )
      }
    )
  })


 max=$(git log --oneline|wc -l); for i in $(seq $max -500 1); do echo $i; git push origin main ~${i}:refs/heads/master; done; git push origin main –



  observeEvent(input$yorig,
    {
      x <- xinput()
      coln <- colnames(xinput())

      wcoln <- which(coln %in% input$cmpSERV)

      wne <- which(rownames(x) %in% noness$sgd_gene)

      if (length(wne) > 0) {
        xne <- xinput()
        ylims <- c(
          floor(min(xne[wne, wcoln, drop = F], na.rm = TRUE)),
          ceiling(max(xne[wne, wcoln, drop = F], na.rm = TRUE))
        )
      }

      updateNumericRangeInput(session, "ylim", label = NULL, value = ylims)
    },
    ignoreInit = FALSE,
    ignoreNULL = TRUE
  )


  observeEvent(input$eorig, {
    cole <- colnames(xinput())
    xe <- xinput()
    wcole <- which(cole %in% input$cmpSERV)

    we <- which(rownames(xe) %in% ess$sgd_gene)

    if (length(we) > 0) {
      elims <- c(
        floor(min(xe[we, wcole, drop = F], na.rm = TRUE)),
        ceiling(max(xe[we, wcole, drop = F], na.rm = TRUE))
      )
    }

    updateNumericRangeInput(session, "elim", label = NULL, value = elims)
  })

  #######################################################
  #######################################################
  ################## END Y-AXIS LIMITS ##################
  #######################################################
  #######################################################
}
##########################
shinyApp(ui, server)
