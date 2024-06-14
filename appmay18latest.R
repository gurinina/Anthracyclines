library(htmltools)
library(shiny)
library(dplyr)
library(shinyWidgets)
library(DT)
library(shinydashboard)
library(visNetwork)
library(igraph)
library(ggplot2)
library(ggrepel)
library(shinyjs)
library(shinycssloaders)
library(shinyjqui)

options(rsconnect.max.bundle.size=3145728000)

### Response network of signatures
source(file.path("modules", "2024_April27_signature.R"))


### GO enrichments for each screen
source(file.path("modules", "2024_April27_visNetwork.R"))

### functions
source(file.path("modules", "2021_May18_functions.R"))
source(file.path("modules", "2022_March13_GOENRICH.R"))

### read gene annotation file
fdat    <- read.delim("data/2021_July19_fdata.txt", stringsAsFactors = F, check.names = F)
noness  <-  fdat  %>% filter(essential == "noness")
ess     <-  fdat  %>% filter(essential == "ess")

### read matrix with signatures
xsig    <-  readRDS("data/x45.RDS")

### read dataframe with screen annotations
phiphop <- readRDS("data/phiphop.RDS")

### read data matrix with all screens
xhiphop <- readRDS("data/xsgtc.RDS")
### read coinhibitory data matrix
xcoinh  <- readRDS("data/xcoinhib.RDS")
xpval   <- readRDS("data/xcoinhib_pval.RDS")
### read cofitness data matrix
xcofit  <- readRDS("data/xcofit.RDS")
xpfit   <- readRDS("data/xcofit_pval.RDS")


we     <-  which(rownames(xhiphop) %in% ess$sgd_gene)
wn     <-  which(rownames(xhiphop) %in% noness$sgd_gene)

xe     <-  xhiphop[we, phiphop$name]
xne    <-  xhiphop[wn, phiphop$name]

st=rev(seq(0,1,0.05))
 sq = rev(seq(0,1,0.001))
############
############

header <-
  dashboardHeader(
    title =  span("Chemogenomic Profiling: HIPHOP Science Lee et al. 2014",
                     style = "font-weight:bold; font-size:16px; text-align: right;"),
    titleWidth = 1200
  )

sidebar <- dashboardSidebar(
  width = 250,
  tags$style(
    HTML(
      ".main-sidebar { font-size: 16px !important;
                    background-color: #000066 !important; color: #fff !important;
                    font-weight:bold !important}
                   .treeview-menu>li>a { font-size: 16px !important;
                    font-weight:bold !important;
                    color: #fff !important;}",)),

sidebarMenu(
    id = "tabs",
    selected = "HIPHOP fitness profiles",

    menuItem(
      text = span("HIPHOP", style = "font-size: 16px"),
      tabName = "hiphop",
      icon = icon("bullseye"),
      selected = T
    ),

    menuItem(
      text = span("GO enrichments", style = "font-size: 16px"),
      tabName = "goenrich",
      icon = icon("dna")
    ),

    menuItem(
      text = span("Network of responses", style = "font-size: 16px"),
      tabName = "signature",
      icon = icon("file-signature")


    ),

    menuItem(
      text = span("Gene fitness by screen", style = "font-size: 16px"),
      tabName = "genebyscreen",
      icon = icon("chart-line")
    )
  )
)



body <- dashboardBody(


tags$script(func <- JS('function(event, ui){return $(event.target).offset();}')),


tags$script(
  HTML("
           // Get mouse coordinates
           var mouseX, mouseY;
           $(document).mousemove(function(e) {
           mouseX = e.pageX;
           mouseY = e.pageY;
           }).mouseover();

           // Function to position draggable, place on current mouse coordinates
           Shiny.addCustomMessageHandler ('placeDraggable',function (message) {
           var element = $('#click_info').parent();
           element.css({'top': mouseY + 'px', 'left' : mouseX + 'px'})
           });

           // Show or hide draggable
          Shiny.addCustomMessageHandler ('hideDraggable',function (message) {
            if(message.hide == true){
              $('#click_info').parent().hide();
            } else{
              $('#click_info').parent().show();
            }
          });

         // Get mouse coordinates
            var emouseX, emouseY;
           $(document).mousemove(function(e) {
           emouseX = e.pageX;
           emouseY = e.pageY;
           }).mouseover();

           // Function to position draggable, place on current mouse coordinates
           Shiny.addCustomMessageHandler ('eplaceDraggable',function (message) {
           var element = $('#eclick_info').parent();
           element.css({'top': emouseY + 'px', 'left' : emouseX + 'px'})
           });

           // Show or hide draggable
          Shiny.addCustomMessageHandler ('ehideDraggable',function (message) {
            if(message.hide == true){
              $('#eclick_info').parent().hide();
            } else{
              $('#eclick_info').parent().show();
            }
          });
           ")
),


  tags$style("


        #controls {
          background-color: #dcd0ff;
          opacity: 1;
        }
        #controls:hover{
          opacity: 1;
        }

        #econtrols {
          background-color: #dcd0ff;
          opacity: 1;
        }
        #econtrols:hover{
          opacity: 1;
          }"),

  #makes pngs same size as window after rescaling
  tags$script(
    "$(document).on('shiny:connected', function(event) {
  var myWidth = $(window).width();
  Shiny.onInputChange('shiny_width',myWidth)

  });"
  ),

  tags$script(
    "$(document).on('shiny:connected', function(event) {
  var myHeight = $(window).height();
  Shiny.onInputChange('shiny_height',myHeight)
    });"
  ),


includeCSS("www/chemogenomics.css"),


shinyjs::useShinyjs(),



tags$head(
    tags$style(
      HTML("table.dataTable tbody tr.selected td {
             color: black !important;
             font-size:2px;}"))),

tags$style(HTML(".selectize-input{white-space: nowrap;
    font-size: 12px !important; font-weight:bold!important;")),

  tabItems(
    tabItem(

         tabName = "genebyscreen",

         tags$head(tags$style(HTML("table {table-layout: fixed;}"))),

    fluidRow(column(
      width = 3,
      box(
        title = "Enter gene of interest:",
        textInput(
          inputId = "gene",
          label = "",
          value   = "TOR2"
        ),
        status = "primary",
        solidHeader = T,
        width = "100%",
        height = 250
      )
    ),
    column(
      width = 9,
      box(
        title = "Gene descriptor and function:",
        dataTableOutput('geneinfo'),
        status = "primary",
        solidHeader = T,
        width = "100%",
        height = 250
      )
    )),
    fluidRow(column(
      width = 12,
      box(
        title = "Fitness defect score across conditions:",
        h5(
          "click OR drag to select screens of interest; corresponding profiles can be found
          in compound menu om hiphop tab; cyan: p-value < 0.001;
          triangle: screens identifying drug target
          candidates"
        ),
        plotOutput(
          "genebydrug",
          click = clickOpts(id = "clickpts"),
          brush = brushOpts(id = "brushpts"),
          height = 800
        ),
        status = "primary",
        solidHeader = T,
        width = "80%"
      )
    )#coln),
    ),#row

    fluidRow(
      box(
        title = "Experimental detail; select row to view corresponding HOP chemogenomic profile",
        dataTableOutput("tabpts"),
        status = "primary",
        solidHeader = T,
        width = 12
      )
    ), #fluidRow
    fluidRow(
      box(
        title = "Cofit genes, select row to view corresponding gene fitness profile",
        dataTableOutput('coFitNess'),
        status = "primary",
        solidHeader = T,
        width = 12
      )
    ), #fluidRow

    fluidRow(
      column(width = 6,
             uiOutput("mxFIT")),
      column(width = 6,
             uiOutput("fiToutput"))

    ), #fluidRow

   fluidRow(
      box(
        title = "GO enrichment network: select a node to view details; right click to save image",

        width = 7,
        status = "primary",
        solidHeader = TRUE,
        height = 800,
        uiOutput("GOterms"),
        visNetworkOutput("network_Proxy", width = "85%", height = 652)
      ),

      box(
        title = "GO term set enrichment details:",
        width = 5,
        uiOutput("GoTable"),
        solidHeader = T,
        status = "primary",
        background = "navy",
        height = 300
      ),

      box(align = "center",
        title = "Top-contributing genes:",
        width = 5,
        br(),
        uiOutput("LeadingEdge"),
        solidHeader = T,
        status = "primary",
        height = 480
      )
    ), #fluidRow

    fluidRow(
      box(
        title = "GO enrichment table:",
        DT::dataTableOutput("EnrichTable"),
        status = "primary",
        solidHeader = TRUE,
        width = 12
      )
    ), #fluidRow
    fluidRow(column(
      width = 3,
      box(
        title = "Download CoFitness:",
        align = "center",
        br(),
        downloadButton("downloadcofitness", "HOP CoFitness"),
        solidHeader = T,
        status = "primary",
        width = "100%",
        height = 150
      )
    ), #fluidRow



    column(
      width = 3,
      box(
        title = "Download EnRichments:",
        align = "center",
        br(),
        downloadButton("enrichdownload", "GO EnRichments"),
        solidHeader = T,
        status = "primary",
        width = "100%",
        height = 150
      )
    ))
    ),

    tabItem("hiphop",

      fluidRow(column(
        width = 9,
        box(
          title = "Select HIPHOP screen:",
          status = "primary",
          solidHeader = T,
          width = "100%",
          height = 175,
          selectizeInput(
            "cmpSERV",
            "",
            width = "200px",
            choices = NULL,
            multiple = F))),

           column(
          width = 3,

          box(
            title = "Reset compound menu:",
            align = "center",
            br(),
            actionButton("resetCmp", "Reset cmpMenu"),

            status = "primary",
            solidHeader = T,
            width = "100%",
            height = 175
          )

        ),
      ),

 fluidRow(

        column(
          width = 4,

          box(
            title = "Fitness score threshold:",

            fluidRow(column(
              width = 12,
              align = "center",
              numericInput(
                "scorethreshSERV",
                label = "",
                min = 0,
                value = 1.0,
                step = 0.1,
                max = 5.0,
                width = "50%"
              )
            )),

            fluidRow(column(
              width = 12,
              align = "center",
              actionLink(
                inputId = "resetscore",
                label = "reset",
                style = "height: 100px;width: 150px;font-size:120%;text-align:center",
                size = "lg"
              )
            )),
            status = "primary",
            solidHeader = T,
            width = "100%",
            height = 200
          )
        ), #coln
        uiOutput("ylimits")
      ),#fluid row

      fluidRow(
        box(
          title = "HIP gene target & compound information:",
          status = "primary",
          tags$div(
            HTML(
              "<h5><b>Click datatable row to view response signature:</b></h5>"
            )
          ),

          solidHeader = T,
          width = 12,
          DT::dataTableOutput("targethip")
        )
      ),

      ######### HIP FITNESS PROFILES #########
      fluidRow(
        box(
          title = "HaploIsufficient chemogenomic Profile (HIP):
          mouse click on points to view gene description",
          plotOutput("efd", width = 300, height = 300,
          click = clickOpts(id="plot_eclick")),
           status = "primary",solidHeader = T),

          jqui_draggable(absolutePanel(id = "econtrols", width = 600,
           draggable = TRUE,uiOutput("eclick_info")),
           options = list(cancel = ".shiny-input-container")),

       box(title = "HOmozygous chemogenomic Profile (HOP):",

        plotOutput("nfd", width = 300, height = 300,
        click = clickOpts(id="plot_click")),
              status = "primary",solidHeader = T),

        jqui_draggable(absolutePanel(id = "controls", width = 600,
             draggable = TRUE,uiOutput("click_info")),
             options = list(cancel = ".shiny-input-container"))),

   fluidRow(
        box(
          title = "HIP genes",
          dataTableOutput("genesHIP"),
          status = "primary",
          solidHeader = T,
          width = 6
        ),

        box(
          title = "HOP genes",
          dataTableOutput("genesHOP"),
          status = "primary",
          solidHeader = T,
          width = 6
        )
      ),

      fluidRow(
        box(
          title = "download HIP results:",
          column(width = 6,

                 downloadButton("downloadhip", "HIP FD scores")),

          column(width = 6,

                 downloadButton("ExportessPlot", "HIP fitness plot")),

          status = "primary",
          solidHeader = T,
          width = 6
        ),

        box(
          title = "download HOP results:",
          column(width = 6,

                 downloadButton("downloadhop", "HOP FD scores")),

          column(width = 6,

                 downloadButton("ExportnonPlot", "HOP fitness plot")),

          status = "primary",
          solidHeader = T,
          width = 6
        )

      ),

      fluidRow(column(
        width = 12,
        box(
          title = "Coinhibitory screens;
          select rows to bring up coinhibitory fitness profiles:",

          dataTableOutput("coInhib"),
          status = "primary",
          solidHeader = T,
          width = "100%"
        )
      )),

      fluidRow(
        box(
          title = "coinhibition:",
          align = "center",
          downloadButton('downloadcoinhib', 'download coinhibition'),
          status = "primary",
          solidHeader = T,
          width = 3
        )
      )
    ),

   tabItem("goenrich",
            fluidRow(visNetworkModuleUI("visNetwork1"))),


    tabItem("signature",
            fluidRow(
              sigNetworkModuleUI("sigNetworkModule1")
            ),

            fluidRow(
              box(
                title = "HIPHOP profiles in response signature:",
                dataTableOutput("screensResp"),
                status = "primary",
                solidHeader = TRUE,
                width = 12
              )

            ))#fluidRow tabItem
            )
  )


ui <-
  dashboardPage(header,
                sidebar,
                body,
                setBackgroundColor(color = "#e6e6ff", shinydashboard = TRUE))


server <- function(input, output, session) {


##############################################################################
##########################    START CALL MODULES          ####################
##############################################################################
tabSEND = reactiveValues(

  tab = NULL)

xinput = reactive({
    xhiphop})

cmpSEND = reactiveValues(cmp = NULL)

threshSEND = reactiveValues(thresh = 1.0)

respSEND = reactiveValues(resp = NULL)

returnedMOD = callModule(visNetworkModule,'visNetwork1',
                    cmpInput = reactive(cmpSEND$cmp),
                    xinput = reactive(xhiphop),
                    threshInput = reactive(threshSEND$thresh),
                    tabsInput = reactive(tabSEND$tab))

cmpRETURN = reactive({
            returnedMOD[[1]]()})

threshRETURN = reactive({
               returnedMOD[[2]]()})

returnedSIG <- callModule(
      sigNetworkModule,
      'sigNetworkModule1',
      xRespDat = reactive(xsig),
      xRespInput = reactive(respSEND$resp),
      inputTab  =  reactive(tabSEND$tab),
      message = "No GO enrichment, try another signature"
    )

sigRETURN = reactive({
   returnedSIG[[1]]()})

tabRETURN = reactive({
    returnedSIG[[2]]()})


########################################################################
########################## END CALL MODULES ############################
########################################################################

########################################################################
########################## START OBSERVEEVENT ##########################
########################################################################

observeEvent(threshRETURN(),
               {
                 req(threshRETURN())
                 tabNOThop = input$tabs != "hiphop"

                 threshMOD = threshRETURN()
                 req(threshMOD)
                 req(threshMOD!="")
                 req(!is.null(input$cmpSERV))

                 threshDIFF = threshMOD!=input$scorethreshSERV

                 if(threshMOD!=input$scorethreshSERV){

                  updateNumericInput(session, "scorethreshSERV", "",
                    value = threshMOD, step = 0.1, min = 0, max = 5.0)
                 }

               },ignoreInit = F, ignoreNULL = T)

  observeEvent(phiphop$name, {
    updateSelectizeInput(
      session,
      "cmpSERV",
      label = "",
      choices = phiphop$name,
      selected = "rapamycin:4nM",
      server = T
    )
  })

observeEvent(input$tabs, {

  tabSEND$tab = input$tabs
  cmpSEND$cmp = input$cmpSERV

},  ignoreInit = F, ignoreNULL = T)

  ############################ IMPORTANTANT
  ### updates the SERVER compound if its different from the MODULE compound
  ### AND the user is currently on the GOENRICH fitness tab
  ############################

############## WHEN OBSERVING MORE THAN ONE EVENT IMPORTANT TO SET IGNORE NULL EQUAL TO FALSE
############## AS A NULL RETURN VALUE CAN ANNUL THE EFFECTS OF OTHER INPUTS, E.G. IF A BUTTON
############## IS ONE OF THE EVENTS, IT WILL NOT TRIGGER UNTIL THE BUTTON IS PRESSED AT LEAST
############## ONCE. IF ignoreNULL = TRUE, IT WILL IGNORE A NONNULL FIRST VALUE IF
############## THE LAST VALUE IS NULL

observeEvent(cmpRETURN(),{

	req(!is.null(input$tabs))

	if(is.null(input$tabs)) updateTabItems(session, "tabs", selected = "hiphop")

  req(input$tabs != "hiphop")

  tabNOThop = input$tabs != "hiphop"

  cmpMOD = cmpRETURN()

  if(tabNOThop){

    updateSelectizeInput(session,"cmpSERV",label = "", choices = phiphop$name, selected = cmpMOD)
  }

},ignoreInit = F, ignoreNULL = T)

  observeEvent(input$cmpSERV, {
      req(input$cmpSERV)
      req(input$scorethreshSERV)

      updateNumericInput(session, "scorethreshSERV", "", value = 1.0,
                         step = 0.1, min = 0, max = 5.0)

      cmpSEND$cmp = input$cmpSERV
      threshSEND$thresh = input$scorethreshSERV

  },ignoreInit = F, ignoreNULL = T)

  observeEvent(input$scorethreshSERV,{
      req(input$cmpSERV)
      req(input$scorethreshSERV)

      cmpSEND$cmp = input$cmpSERV
      threshSEND$thresh = input$scorethreshSERV


  },ignoreInit = F, ignoreNULL = T)



######################################################################
########################## END OBSERVEEVENT ##########################
######################################################################

######################################################################
############################# START TABLES ###########################
######################################################################

output$screensResp = renderDataTable({
  w = which(phiphop$signature %in% sigRETURN())

  validate(need(length(w) > 0 , message =
                  "please enter a valid signature"))

 style = "height: 100px;width: 150px;font-size:40px;text-align:center"
  df = data.frame(
    screen = phiphop$name[w],
    mechanism = phiphop$mechanism[w],
    target = phiphop$TARGET[w],
    PCID = phiphop$pcid_link[w],
    FDA = phiphop$FDA[w],
    gold_standard = phiphop$gold_standard[w],
    drug = phiphop$compound[w],
    stringsAsFactors = F
  )
  df = df %>% dplyr::arrange(desc(gold_standard), drug)
  wn = which(names(df) %in% c("gold_standard", "drug"))
  df = df[, -wn]


  opts = list(
    searching = T,
    paging = T,
    pageLength = 5,
    autoWidth = F,
    scrollX = F,
    columnDefs = list(
      list(className = 'dt-left', targets = c(0, 1, 2, 3, 4)),
      #list(className = 'dt-center',targets = c(4)),
      list(width = c('100px'), targets = c(2)),
      list(width = c('500px'), targets = c(1)),
      list(width = c('50px'), targets = c(4)),
      list(width = c('300px'), targets = 0)
    )
  )

  df =  DT::datatable(
    df,
    options = opts,
    rownames = F,
    escape = F,
    selection = "single")

  df  %>% formatStyle(c(1:6), fontWeight = 'bold', fontSize = '12px')

})

  observeEvent(input$screensResp_rows_selected, {
    w = which(phiphop$signature %in% sigRETURN())
    validate(need(length(w) > 0 , message =
                    "please enter a valid signature"))
    d = phiphop$name[w]

    df = data.frame(
      screen = phiphop$name[w],
      mechanism = phiphop$mechanism[w],
      target = phiphop$TARGET[w],
      PCID = phiphop$pcid_link[w],

      FDA = phiphop$FDA[w],
      gold_standard = phiphop$gold_standard[w],
      drug = phiphop$compound[w],
      stringsAsFactors = F
    )

    df = df %>% dplyr::arrange(desc(gold_standard), drug)
    wn = which(names(df) %in% c("gold_standard", "drug"))
    df = df[, -wn]

    row = input$screensResp_rows_selected
    w = which(df$screen %in% df$screen[row])

    validate(need(length(w) > 0 , message =
                    "please enter a valid signature"))

    selected = df$screen[row]
    cmpSEND$cmp = selected

    updateSelectizeInput(
                   session,
                   "cmpSERV",
                   label = "",
                   choices = phiphop$name,
                   selected = selected,server = T
                 )


    newtab <- switch(input$tabs,
                     "hiphop" = "signature",
                     "signature" = "hiphop")
    updateTabItems(session, "tabs", newtab)

  })

output$targethip <- DT::renderDataTable({
    w = which(phiphop$name %in% input$cmpSERV)
    d = phiphop[w, c(
      "screen",

      "name",
      #   "TARGET", # HIP target, for column hit
      #   HIPHIT is the target, TARGET is the link tothe gene
      #
      #   est yeast target is the gene, yeast is the link to the gene
      "TARGET",
      # "yeast", # known target, for column est_yeast_target

      "yeast",
      "signature",
      "mechanism",
      "FDA",
      "PCID",
      "png"
    )]


    d$FDA = toupper(d$FDA)
    d$FDA = factor(d$FDA)
    SGD = '<a href="https://www.yeastgenome.org" target="_blank">known target</a>'


    PCID = '<a href="https://pubchem.ncbi.nlm.nih.gov" target="_blank">PCID</a>'

    names(d)[1] = "screen"
    names(d)[3] = "HIP target"
    names(d)[8] =  PCID
    names(d)[5] = "signature"
    names(d)[2] = "compound"
    names(d)[9] = "structure"
    names(d)[4] =  SGD

    d = d[, c(
      "screen",
      "compound",
       "structure",
      "HIP target",
      '<a href="https://www.yeastgenome.org" target="_blank">known target</a>',
      "signature",
       '<a href="https://pubchem.ncbi.nlm.nih.gov" target="_blank">PCID</a>',
      "FDA",
      "mechanism"
    )]

     opts = list(
        dom = 'Bfrtip',
        paging = F,
        target = "cell",
        searching = F,
        info = F,
        autowidth = T,
        scrollX = TRUE,
        ordering = F,
        columnDefs = list(
          list(className = 'dt-left', targets = c(0, 4, 7)),
          list(className = 'dt-center', targets = c(1, 2, 3, 5, 6)),
          list(width = c('35px'), targets = c(4)),
          list(width = c('60px'), targets = c(3)),
          list(width = c('100px'), targets = c(1)),
          list(width = c('200px'), targets = c(2)),
          list(width = c('40px'), targets = c(0,5,6,7)),
          list(width = c('385px'), targets = c(8))))

    datatable(
      d,
      selection = "single",
      options = opts,
      escape = F,
      class = 'table-bordered stripe table-condensed',
      rownames = F
    )
  })

  observeEvent(input$targethip_rows_selected, {
    w = which(phiphop$name %in% input$cmpSERV)

    row = input$targethip_rows_selected

    req(phiphop$type[w][row]%in%"major")
    req(!is.na(phiphop$signature[w][row]))

    respSEND$resp = phiphop$signature[w][row]

    newtab <- switch(input$tabs,
                     "hiphop" = "signature",
                     "signature" = "hiphop")

    updateTabItems(session, "tabs", newtab)
  })


######################################################################
############################# END TABLES ###########################
######################################################################

##################################################
################ START MOUSE-OVERS ###############
##################################################

show_click = reactiveVal(NULL)
print_click = reactiveVal(NULL)
output$click_info <- renderUI   (show_click() )
output$point_info <- DT::renderDataTable({
            df = print_click()
    opts = list(
              autowidth = TRUE,
              scrollX = T,
              searching = F,
              paging = F,
              ordering = F,
              info = FALSE,
              columnDefs = list(list(
                width = c('10%'),
                targets = c(0, 1, 2)
              ),
              list(
                width = c('70%'), targets = c(3)
              ))
            )

            df =  DT::datatable(df, options = opts,rownames = F, escape = F, selection = "single") %>%
                  formatStyle(c(1:4),fontWeight = 'bold', fontSize = '12px',target="row")
                  })

 observeEvent(input$plot_click, {

  x = xne

  req(input$cmpSERV)
    w1 = which(colnames(x) == input$cmpSERV)

    req(length(w1) > 0)
    validate(
    need(length(w1) == 1 ,message =
          "please enter a valid compound"))

    hop = geneAnno(mat = x,colnames(x)[w1],fdat = fdat,sgdlink = T)

    pclick <- nearPoints(df = hop,  xvar = "gene", yvar = "FD",
                         coordinfo = input$plot_click,threshold = 10)

    hideTooltip <- function( hide ){
    session$sendCustomMessage(type = 'hideDraggable', message = list('hide'=hide))
  }

    g = grep("FD",names(pclick))

    if(length(g) > 0) pclick[,g] = format(round(pclick[,g],2),nsmall = 1,scientific = F)

    g = which(names(pclick) %in% c("xvar","gene"))

    if(length(g) > 0) pclick = pclick[,-g]
    if( nrow(pclick) == 0 ) {
    show_click(NULL)

    hideTooltip(TRUE) # Hide tooltip if there's no info to show
    return()

    } else {
    session$sendCustomMessage(type = 'placeDraggable', message = list())
    show_click(tagList(

      { DT::dataTableOutput("point_info", width = "100%") }

    )
    )
    print_click({pclick})
  }
})

show_eclick = reactiveVal(NULL)
print_eclick = reactiveVal(NULL)
output$eclick_info <- renderUI   (show_eclick() )
output$point_einfo <- DT::renderDataTable({
            df = print_eclick()


            opts = list(
              autowidth = TRUE,
              scrollX = T,
              searching = F,
              paging = F,
              ordering = F,
              info = FALSE,
              columnDefs = list(list(
                width = c('10%'),
                targets = c(0, 1, 2)
              ),
              list(
                width = c('70%'), targets = c(3)
              ))
            )

            df =  DT::datatable(df, options = opts,rownames = F,
                                escape = F, selection = "single") %>%
                  formatStyle(c(1:4),fontWeight = 'bold', fontSize = '12px',target="row")
                  })

 observeEvent(input$plot_eclick, {

  x = xe


  req(input$cmpSERV)
    w1 = which(colnames(x) == input$cmpSERV)

    req(length(w1) > 0)
    validate(
    need(length(w1) == 1 ,message =
          "please enter a valid compound"))

     hideTooltip <- function( hide ){
     session$sendCustomMessage(type = 'ehideDraggable', message = list('hide'=hide))
  }
     hop = geneAnno(mat = x,colnames(x)[w1],fdat = fdat,sgdlink = T)

     peclick <- nearPoints(df = hop,  xvar = "gene", yvar = "FD",
                           coordinfo = input$plot_eclick,threshold = 10)

    g = grep("FD",names(peclick))
    if(length(g) > 0) peclick[,g] = format(round(peclick[,g],2),nsmall = 1,scientific = F)

    g = which(names(peclick) %in% c("xvar","gene"))

    if(length(g) > 0) peclick = peclick[,-g]

    if( nrow(peclick) == 0 ) {
      show_eclick(NULL)

      hideTooltip(TRUE) # Hide tooltip if there's no info to show
      return()

    } else {

    session$sendCustomMessage(type = 'eplaceDraggable', message = list())
    show_eclick(tagList(

    { DT::dataTableOutput("point_einfo", width = "100%") }

    ))

    print_eclick({peclick})
  }
})


##################################################
################## END MOUSE-OVERS ###############
##################################################

##################################################
################### START PROXY ##################
##################################################

output$network_Proxy <- renderVisNetwork({

    req(visNet()$nodes)
    vis = visNet()

    n = visNet()$nodes
    req(n)

    w =  nrow(n)
    n <- n %>% arrange(term)

    names = n$id


    if(nrow(vis$edges)==0) {
      visNetwork(vis$nodes, width = "100%") %>%
        visNodes(shadow=list(enabled=T,size=25),borderWidth=1) %>%
        visOptions(

          highlightNearest = list(enabled = T, degree = 5, hover = T),


          nodesIdSelection = list(enabled = TRUE, values = names,
             style = 'width: 500px; height = 31px;
             font-size: 18px; color: #000066;border: 3px solid #4d88ff;'),

          selectedBy = list(variable="FDR",
             style = 'width: 200px; height = 31px; font-size: 18px;
             color: #000066;border: 3px solid #4d88ff;')) %>%

        visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}")
    }


    else {
      visNetwork(vis$nodes, vis$edges, width = "100%") %>%
        visNodes(shadow=list(enabled=T,size=25)) %>%

        visOptions(

          highlightNearest = list(enabled = T, degree = 5, hover = T),


          nodesIdSelection = list(enabled = TRUE, values = names,
            style = 'width: 500px; height = 31px;
            font-size: 18px; color: #000066;border: 3px solid #4d88ff;'),

          selectedBy = list(variable="FDR",
            style = 'width: 500px; height = 31px;
            font-size: 18px; color: #000066;border: 3px solid #4d88ff;')) %>%

        visIgraphLayout(type = "full") %>%

        visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}")
    }
  })



####################################      NETWORK     #########################################
visNet <- reactive({

    req(nrow(goEnrich()$enrichInfo)>0)

    enrich = goEnrich()$enrichInfo

    edge = goEnrich()$edgeMat

    vis = visSetup(enrichInfo = enrich,edgeMat = edge, fontsize = 20, fontface = "Courier")


    vis

    })

 output$network_Proxy <- renderVisNetwork({
    req(visNet()$nodes)
    vis = visNet()

    n = visNet()$nodes
    req(n)

    w =  nrow(n)
    n <- n %>% arrange(term)

    names = n$id


    if(nrow(vis$edges)==0) {
      visNetwork(vis$nodes, width = "100%") %>%
        visNodes(shadow=list(enabled=T,size=25),borderWidth=1) %>%
        visOptions(

          highlightNearest = list(enabled = T, degree = 5, hover = T),


          nodesIdSelection = list(enabled = TRUE, values = names,
             style = 'width: 500px; height = 31px;
             font-size: 18px; color: #000066;border: 3px solid #4d88ff;'),

          selectedBy = list(variable="FDR",
             style = 'width: 200px; height = 31px;
             font-size: 18px; color: #000066;border: 3px solid #4d88ff;')) %>%

        visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}")
    }


    else {visNetwork(vis$nodes, vis$edges, width = "100%") %>%
        visNodes(shadow=list(enabled=T,size=25)) %>%

        visOptions(

          highlightNearest = list(enabled = T, degree = 5, hover = T),


          nodesIdSelection = list(enabled = TRUE, values = names,
            style = 'width: 500px; height = 31px;
            font-size: 18px; color: #000066;border: 3px solid #4d88ff;'),

          selectedBy = list(variable="FDR",
            style = 'width: 200px; height = 31px;
            font-size: 18px; color: #000066;border: 3px solid #4d88ff;')) %>%

        visIgraphLayout(type = "full") %>%

        visEvents(select = "function(nodes) {
                Shiny.onInputChange('current_node_id', nodes.nodes);
                ;}")
    }
  })

####################################      NETWORK     #########################################
 observeEvent(input$network_Proxy_selectedBy,


               {
                 req(input$network_Proxy_selectedBy)

                 n = visNet()$nodes

                 w = which(n$FDR %in% as.numeric(input$network_Proxy_selectedBy))

                 id = n$id[w]


                 visNetworkProxy("network_Proxy") %>%
                   visSelectNodes(id = id)

               }, ignoreNULL = T, ignoreInit = F
  )


##################################################
################### END PROXY ##################
##################################################



##################################################
############## START GO ENRICHMENT ###############
##################################################

goEnrich <- reactive({
     xfit = outFit$dFit
     xfit$cofit = as.numeric(xfit$cofit)
     xfit$pvalue = as.numeric(xfit$pvalue)
     gseaMAX$goterms = NULL


     num = is.numeric(input$scoreFIT)
     req(num == TRUE)

     mfit = as.matrix(xfit[,2:3])
     mfit[,2] = 1:nrow(mfit)
     colnames(mfit)[2]="rank"
     wfit = which(mfit[,2] <= input$scoreFIT)

     validate(need(length(wfit)!=0, message = "No CoFitness scores above threshold"))

     req(length(wfit)!=0)

     qFIT = compRANK(mfit,coln = 2, rank  = input$scoreFIT)


     curr_exp = "cofit"

     FDR = input$fdrSERV
     network = runGOENRICH(fdrThresh = FDR, minSetSize = 5,curr_exp = curr_exp,
            score = qFIT,bp_path =  "2023_January30_GOBP.RDS",
            go_path = "2023_January30_GOID_GOBP_SGD.txt")


     validate(
      need(!is.null(network$enrichInfo),
           message = "No GO enrichment, try relaxing the FDR or scorethreshold")
    )

    enrichInfo = network$enrichInfo

    req(!(is.null(enrichInfo)))

    if(!is.null(enrichInfo)) gseaMAX$goterms = nrow(enrichInfo)
    edgeMat = network$edgeMat

    if(!is.null(enrichInfo)) return(network)
  })

  output$GoTable = renderUI({

    req(input$network_Proxy_selected)

    ns <- session$ns

    DT::dataTableOutput(ns("gotermTable"))

  })

##################################################
################ END GO ENRICHMENT ###############
##################################################


  output$gotermTable <- DT::renderDataTable({

    req(input$network_Proxy_selected)

    vis = visNet()

    n = visNet()$nodes

    w = which(vis$nodes$id %in% c(input$network_Proxy_selected))


    req(length(w) > 0)

    term = vis$nodes$label[w]

    nam = c("term","nGenes","geneSetFraction","FDR")

    m = match(nam,names(vis$nodes))

    n = vis$nodes[w,m]

    term = vis$nodes$label[w]

    nam = c("term","nGenes","geneSetFraction","FDR")

    m = match(nam,names(vis$nodes))

    names(vis$nodes)[m] = c("GO term","geneSet size","% of geneSet","FDR")

    n = vis$nodes[w,m]

    req(nrow(n)!=0)

    t = t(n[,2:4])

    datatable(t,width=220,caption = htmltools::tags$caption(term,
        style = "caption-side: top; text-align: center; color:black;
        background:white;font-weight:bold;"),

    options=list(paging = F,scrollY = F,dom = "t",scroller = F,searching = F,
                 ordering=F,rowCallback = JS(
                "function(row, data) {",
                "for (i = 1; i < data.length; i++) {",
                "if (data[i]>1000 | data[i]<1){",
                "$('td:eq('+i+')', row).html(data[i].toExponential(1));",
                "}",
                "}",
                "}")),
              height = 400,colnames = "") %>%

    formatStyle( target = "row", color = "black",backgroundColor = "white",
                   columns = c(1,2),fontWeight = "bold")
  })

##################################################
################### START BARPLOTS ###############
##################################################

output$LeadingEdge = renderUI({
    ns <- session$ns
    req(input$network_Proxy_selected)

    plotOutput(ns("goSigPlot"), width = 300,height = hgt())
  })

output$goSigPlot <- renderPlot({

  req(input$network_Proxy_selected)

  vis = visNet()

  n = visNet()$nodes

  w = which(as.numeric(vis$nodes$id) %in% as.numeric(input$network_Proxy_selected))

  req(length(w) > 0)

  n = vis$nodes[w,]

  req(nrow(n)!=0)

  s6 = geneBARPLOT(n$overlapGenes)
  d = s6


  tit = stringWINDOW(n$term, width = 50)

  if(nrow(d)> 10) d = d[1:10,]


     barplot(d$score, main = tit, names.arg = d$gene, las = 1, horiz = T,
             col="navy",xlab = "median fitness defect score")

   })

hgt = reactive({

  req(input$network_Proxy_selected)

  vis = visNet()

  n = visNet()$nodes

  w = which(vis$nodes$id %in% c(input$network_Proxy_selected))

  req(length(w) > 0)

  n = vis$nodes[w,]

  validate(need(nrow(n)!=0, message = "click node for detail"))

  o = geneBARPLOT(n$overlapGenes)

  height = genebarHEIGHT(o)

  height = height*1.75

  height
})


##################################################
################### END BARPLOTS #################
##################################################

###########################  GO ENRICHMENT TABLE  ###################################
  EnrichReact = reactive({

    req(!is.null(visNet()$nodes))

    enrich = visNet()$nodes

    req(length(nrow(enrich)) > 0)

    row <- input$EnrichTable_rows_selected

    out = outEnrich()

    id = out$id[row]

    id


  })

###########################  GO ENRICHMENT TABLE  ###################################

  observeEvent(EnrichReact(), {
    req(EnrichReact())
    id = EnrichReact()

    ns <- session$ns
    visNetworkProxy(ns("network_Proxy")) %>%
      visSelectNodes(id = id)
  })


###########################  GO ENRICHMENT TABLE  added ###################################
outEnrich = reactive({

    req(nrow(goEnrich()$enrichInfo)>0)

    enrich = goEnrich()$enrichInfo

    w = which(names(enrich) %in% c("querySetFraction", "geneSetFraction" ,
                                   "foldEnrichment" , "P" , "FDR" ))

    enrich[,c("querySetFraction","geneSetFraction", "foldEnrichment")] =
      format(round(enrich[,c("querySetFraction","geneSetFraction",
                             "foldEnrichment")],2),nsmall = 1,scientific = F)

    enrich[,c("P", "FDR")] =
      format(signif(enrich[,c("P", "FDR")],2),nsmall = 1,scientific = T)
    w = which(names(enrich) %in% c("formattedLabel","cluster","size",
                                   "filename","maxOverlapGeneScore"))
    enrich = enrich[,-w]
    enrich = enrich[,c("id",  "GOID","term","nGenes","nQuery",
                       "nOverlap", "querySetFraction","geneSetFraction",
                       "foldEnrichment","P","FDR","overlapGenes")]
    enrich
  }
  )

###########################  GO ENRICHMENT TABLE  ###################################

output$EnrichTable = DT::renderDataTable({

  out = outEnrich()
  w = which(names(out) %in% c("GOID","term","querySetFraction", "geneSetFraction" ,
                              "foldEnrichment" , "P" , "FDR","overlapGenes" ))

  out = out[,c("GOID","term","querySetFraction", "geneSetFraction" ,
               "foldEnrichment" , "FDR","overlapGenes" )]

  opts = list(pageLength = 5,
              autoWidth = F,scrollX = T)#,


  df =  DT::datatable(out, options = opts,rownames = F, escape = F, selection = "single") %>%
    formatStyle(c(1:7),fontWeight = 'bold')
})

output$enrichdownload <- downloadHandler(
    filename = function() {
      paste0("enrich:",input$cmpMOD,"_", Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(outEnrich(), file, row.names = F,sep="\t")
    }
  )


##################################################
########### START GO ENRICHMENT BY RANK ##########
##################################################

output$fiToutput = renderUI({

  xfit = outFit$dFit
  xfit$cofit = as.numeric(xfit$cofit)
  xfit$pvalue = as.numeric(xfit$pvalue)

  mfit = as.matrix(xfit[,2:3])

  mfit[,2] = 1:nrow(mfit)
  colnames(mfit)[2]="rank"

  tagList(
    fluidRow(
      box(title = "Set cofitness rank threshold:",
      HTML("<h5><b>Top: 1000 genes ranked by cofitness, from 1 to 1000, left to right;
           # on scale marks # of strains used in GO enrichment.
           Bottom: Corresponding cofitness values 1 to 1000, right to left.
           Click on arrow to animate GO enrichment
           dependency on cofitness rank.</b></h5>"),
    br(),

      br(),
      HTML("<h5><b>Cofitness values:</b></h5>"),

    st=rev(seq(0,1,0.05)),
    sq = rev(seq(0,1,0.001)),

    sliderTextInput(inputId = "corrFIT", label = NULL,

    choices = st,grid =T,animate = T),

      HTML("<h5><b>Corresponding cofitness rank:</b></h5>"),
        sliderInput("scoreFIT",label = "",
                value = 1, min = 1,max = 1000, step = 1,animate = TRUE),

      HTML("<h5><b>Set FDR threshold for GO enrichment by RANK:</b></h5>"),

          sliderInput( "fdrSERV", label = NULL,
            min = 0, max = 0.5,value = 0.1, step = 0.05),


    status = "primary", solidHeader = T, width="100%", height = 650)))
})

rankFIT = debounce(reactive(input$scoreFIT),500)
coFIT = debounce(reactive(input$corrFIT),500)

scoreVal = reactiveValues(
  previous = NULL,
  current  = NULL
)

corrVal = reactiveValues(
  previous = NULL,
  current  = NULL)

   observeEvent(input$corrFIT,{

     req(input$scoreFIT)

     req(input$corrFIT)

   print(c("input$scoreFIT",input$scoreFIT))
   print(c("input$corrFIT",input$corrFIT))

     corr = input$corrFIT

     mfit = outMit$mFit

     updateSliderTextInput(session,"corrFIT",label =  NULL,
      choices = sq, selected = score)

     w1 = slid(x =input$corrFIT,y= input$scoreFIT, mfit = mfit)
     score1 = w1$corr[2]

     w = which(mfit[,3] == corr)

     req(length(w) > 0)

     score = mfit[w,"rank"]

     print(c("obfit","score1",score1,"score",score, "wscore",w1$score,"current" ,corrVal$current,
             "corr",w1$corr,"scoreFIT",input$scoreFIT,"corrFITr",
             input$corrFIT,"previp" ,corrVal$previous,"currs" ,scoreVal$current,"current" ,scoreVal$previous))


     test = c(as.numeric(input$scoreFIT) != scoreVal$current)

     test2 = as.numeric(input$scoreFIT) != score


     print(c("test",test))


     print(c("test2",test2))


     print(c("scorefit",score,input$scoreFIT,scoreVal$current))

   cond =   is.null(scoreVal$current)

  if( is.null(scoreVal$current)){


     updateSliderInput(session = session,
          inputId = 'scoreFIT',
          value =score) # updateSliderInput

   } else if(!is.null(scoreVal$current)& as.numeric(input$scoreFIT) != score){

     updateSliderInput(session = session,
          inputId = 'scoreFIT',
          value =score) # updateSliderInput}

   }


     scoreVal$previous = scoreVal$current
     scoreVal$current = score

    }, ignoreNULL = F, ignoreInit = T)
#
#
#
    observeEvent(input$scoreFIT,{

      mfit = outMit$mFit

      req(input$scoreFIT)
      req(input$corrFIT)
#
# print(c("input$scoreFITRANK",input$scoreFIT))
#  print(c("input$corrFITRANK",input$corrFIT))
      w1 = slid(x =input$corrFIT,y= input$scoreFIT, mfit = mfit)
      score1 = w1$score[1]

      rank=input$scoreFIT
      w = which(mfit[,2] == rank)
      req(length(w)>0)
      score = mfit[w,3]

     print(c("obscore","score1",score1,"score",score, "wscore",w1$score,corrVal$current,
             "corr",w1$corr,"scoreFIT",input$scoreFIT,"corrFITr",
             input$corrFIT,"previcorrscore" ,corrVal$previous,"currscorecurr" ,
             scoreVal$current,"previcorrscore",scoreVal$previous))

      print(c("scorerank",score))
      cest = as.numeric(input$corrFIT) != score

      cest2 = as.numeric(input$corrFIT) != corrVal$current

      req(cest ==TRUE |cest ==FALSE )

      print(c("cest",cest))

      print(c("cest2",cest2))

      print(c("rankfit",score,input$corrFIT,corrVal$current))

   sq = rev(seq(0,1,0.001))

   if(as.numeric(input$corrFIT) != score){

      updateSliderTextInput(session,"corrFIT",label =  NULL,
      choices = sq, selected = score)

}
      corrVal$previous = corrVal$current
      corrVal$current  =score
  }, ignoreNULL = F, ignoreInit = F)

output$mxFIT <- renderUI({
  tagList(

    box(title = "Optimizing GO enrichment:",
        tags$head(tags$style(HTML("<h5><b>label(font-family:Arial;font-weight:bold;font-size;20)<h5><b>"))),
        HTML("<h5><b>Finds the cofitness/cofitness rank that maximizes the number of GO enrichment terms for the top 1000 cofit strains for a given FDR value. Stepsize is the increment between ranks tested.<h5><b>"),

        HTML("<h5><b>Choose a stepsize and FDR threshold, then hit GO to run.<h5><b>"),
        HTML("<h5><b>Warning: calculation time for stepsize 1: ~ 3min.<h5><b>"),
        br(),
        fluidRow(

          column(width = 12,align = "center",

                 prettyRadioButtons(inputId = "stepsize",label = "stepsize",
                                    choices = c(1,5,20,50), outline = T, fill = F,thick = T,
                                    status = "primary", shape = "round",bigger = T,width = "100%",
                                    selected = 50, inline = T)),

          HTML("<h5><b>&nbsp&nbsp&nbsp&nbsp&nbspSet FDR threshold (default = 0.1)</b></h5>"),
          br(),
          column(width = 12,align = "center",
                 sliderInput("fdrRANK",label = "", min = 0, max = 0.5,
                             value = 0.2, step = 0.1)),


         column(width = 12,align = "left",
               actionButton("rankGO", "GO!", class = "btn-success")),

         column(width = 12, align = "left",
               tags$head(tags$style(HTML("<h5><b>label(font-family:Arial;
                                         font-weight:bold;font-size;20)<h5><b>"))),
               htmlOutput("finishTEXT"),
               htmlOutput("stepTEXT"),

               withSpinner(htmlOutput("mxCOFITvalue"),type = 5,
                           color = "#4d88ff", hide.ui = F, size = 0.5))),

        status = "primary", solidHeader = T,width="100%", height = 650))

})

gseaMAX <- reactiveValues(
  rank     = NULL,
  cofit    = NULL,
  goterms     = NULL)

observeEvent(outFit$dFit,{
  useShinyjs()
  click("rankGO")
  reset("scoreFIT")
  reset("fdrRANK")
},ignoreNULL = T,ignoreInit = F, once = T)


observeEvent(input$gene,{
  useShinyjs()
  reset("scoreFIT")
  reset("fdrRANK")

  shinyjs::hide(id = "finishTEXT")
  shinyjs::hide(id = "stepTEXT")
  shinyjs::hide(id = "mxCOFITvalue")

},ignoreNULL = T,ignoreInit = T)


observeEvent(input$fdrRANK,{
  useShinyjs()

  reset("scoreFIT")
  shinyjs::hide(id = "finishTEXT")
  shinyjs::hide(id = "stepTEXT")
  shinyjs::hide(id = "mxCOFITvalue")
  reset("corrFIT")
},ignoreNULL = T, ignoreInit = T)

observeEvent(input$stepsize,{
  useShinyjs()
  #click("rankGO")
  reset("scoreFIT")
  reset("fdrRANK")
  shinyjs::hide(id = "finishTEXT")
  shinyjs::hide(id = "stepTEXT")
  shinyjs::hide(id = "mxCOFITvalue")
},ignoreNULL = T, ignoreInit = T, once = F)

observeEvent( input$rankGO,{
  gseaMAX$rank = NULL
  gseaMAX$cofit = NULL
  gseaMAX$goterms = NULL
  shinyjs::hide(id = "finishTEXT")
  shinyjs::hide(id = "stepTEXT")
  shinyjs::hide(id = "mxCOFITvalue")

  xfit = outFit$dFit
  xfit$cofit = as.numeric(xfit$cofit)
  xfit$pvalue = as.numeric(xfit$pvalue)

  mfit = as.matrix(xfit[,2:3])
  mfit[,2] = 1:nrow(mfit)

  colnames(mfit)[2]="rank"
  colnames(mfit)[1]="cofit"
  FDR = input$fdrRANK

  mn = 1
  mx = 1000

  steps = pretty(c(mn,mx),n = 100)
  range = max(steps)-min(steps)
  step = range/(length(steps) -1)

  if(as.numeric(input$stepsize) == 1) steps = 1:1000 else if(as.numeric(input$stepsize) == 5){
    steps = seq(0,1000,5)}  else if(as.numeric(input$stepsize) == 20){
      steps = seq(0,1000,20)} else if(as.numeric(input$stepsize) == 50){
        steps = seq(0,1000,50)}

  dFIT = NULL
  for(i in 1:length(steps)) dFIT[[i]] = compRANK(mfit,coln = 2, rank  = steps[i])

  names(dFIT)=steps

  net = lapply(dFIT,runGOENRICH,fdrThresh = FDR, minSetSize = 5,
               curr_exp = "tst",bp_path = "2023_January30_GOBP.RDS",
               go_path = "2023_January30_GOID_GOBP_SGD.txt")

  enrich = lapply(net,function(x) x = x[[1]])
  isnull = sapply(enrich,is.null)
  wisnull = which(isnull ==T)
  nrw = sapply(enrich,nrow)
  if(length(wisnull) > 0) isnull = lapply(nrw[wisnull],function(x) x = 0)
  nrw[wisnull] = isnull
  unrw = unlist(nrw)
  mx = which.max(unrw)
  nmx = as.numeric(names(mx))
  cofit = mfit[nmx,1]
  goterms = unrw[mx]

  if(length(nmx)>0) gseaMAX$rank = nmx
  if(length(nmx)>0) gseaMAX$cofit = cofit
  if(!is.null(unrw[mx])) gseaMAX$goterms = goterms

  shinyjs::show(id = "finishTEXT")
  shinyjs::show(id = "stepTEXT")
  shinyjs::show(id = "mxCOFITvalue")

  req(!is.null(gseaMAX$cofit))

  output$finishTEXT = renderText({

    paste("<h5><b>","Results:","</b></h5>")
  })
  output$stepTEXT = renderText({

    paste("<h5><b>","STEPSIZE = ",input$stepsize,";","FDR = ",input$fdrRANK,"</b></h5>")
  })
  output$mxCOFITvalue = renderText({
    req(!is.null(gseaMAX$cofit))

    paste("<h5><b>","COFITNESS = ",round(as.numeric(isolate(gseaMAX$cofit)),3),";","RANK = ", isolate(gseaMAX$rank),"</b></h5>")
  })

},ignoreNULL = T,ignoreInit = T)

#
output$mxCOFITvalue = renderText({
  NULL
})

output$GOterms <- renderUI({

  htmlOutput("countGO")

})

observeEvent(gseaMAX$goterms,{
  output$countGO = renderText({paste("<h5><b>","no. GO terms =", gseaMAX$goterms,"</b></h5>")})

})

observeEvent(gseaMAX$rank,{
  req(!is.null(gseaMAX$rank))

  xfit = outFit$dFit
  mx = nrow(xfit)
  value = gseaMAX$rank

  scoreVal$previous = input$scoreFIT
  scoreVal$current = gseaMAX$rank

  updateSliderInput(session, "scoreFIT",label = "",min = 1, max = 1000, value = gseaMAX$rank, step = 1)

},ignoreNULL = T, ignoreInit = T)

##################################################
############# END GO ENRICHMENT BY RANK ##########
##################################################

##################################################
#################### START COFITNESS #############
##################################################


outFit = reactiveValues(dFit = NULL)

outMit = reactiveValues(mFit = NULL)

 observeEvent(input$gene,{

       req(input$gene)
       req(xcofit)
       ww = which(colnames(xcofit) == toupper(input$gene))
       if(length(ww) != 0) {
        p = xpfit[,ww,drop=F]
        d = xcofit[,ww,drop = F]
       }

      validate(
        need(length(ww) == 1 ,message =
               "please enter a valid gene"))


      d = d[order(d[,1],decreasing = T),,drop=F]

      df = data.frame(gene = rownames(d), cofit = d[rownames(d),1],
                      pvalue = p[rownames(d),1],stringsAsFactors = F)

      w = which(df$cofit >= 0)

      if(length(w) >0) df = df[w,]

      xfit = df
      xfit$cofit = as.numeric(xfit$cofit)
      xfit$pvalue = as.numeric(xfit$pvalue)
      xfit$cofit =round(xfit$cofit,3)

      mfit = as.matrix(xfit[,2:3])
      mfit[,2] = 1:nrow(mfit)
      colnames(mfit)[2]="rank"
      m = mfit[1:1001,]
      ss=rev(seq(0,1,0.001))

      m=cbind(m,ss)
      m[,3]=round(m[,3],3)

      df$pvalue = formatC(df$pvalue,format = "e", digits = 2)
      df$cofit = format(round(df$cofit, 6), nsmall = 2)

      outFit$dFit = df
      outMit$mFit = m


})

####################################################
output$coFitNess = DT::renderDataTable({

      outFit$dFit
    },escape = F,rownames=F,options=list(pageLength=10),selection = "single",server = F)

####################################################
observeEvent(input$coFitNess_rows_selected, {
      row = input$coFitNess_rows_selected

      xfit = outFit$dFit

      w = xfit$gene[row]

      validate(
        need(length(w) == 1 ,message =
               "please enter a valid gene"))

      delay(200,updateTextInput(session, inputId = "gene", label = "", value = xfit$gene[row]))

    })
####################################################

    output$downloadcofitness <- downloadHandler(
      filename = function() {
        paste0("CoFit:",input$gene, "_",Sys.Date(), ".txt")
      },
      content = function(file) {

        write.table(as.data.frame(outFit$dFit), file, row.names = F,sep="\t",quote=F)
      }
    )


##################################################
###################### END COFITNESS #############
##################################################


########################################################################
########################## START GENEBYSCREEN ##########################
########################################################################

  data_for_genebydrug <- reactive({
    gene = toupper(input$gene)

    xdat = xhiphop

    w = which(rownames(xdat) == gene)

    validate(need(length(w) == 1 , message =
                    "please enter a valid gene"))

    if (length(w) > 0) {
      mx2 = meltDF(xdat, row = gene, df = phiphop)

      mx2
    }

    mx2
  })

  output$genebydrug <- renderPlot({
    x4 = data_for_genebydrug()
    mx2 = x4
    wna = which(is.na(mx2$fitness_defect))
    if (length(wna) > 0)
      mx2 = mx2[-wna, ]

    tit = toupper(input$gene)
    mx2$fitness_defect = round(mx2$fitness_defect, 2)
    mx2$sig = 0

    wx = which(mx2$fitness_defect > 5)
    if (length(wx) > 100)
      wx = wx[1:100]
    if (length(wx) > 0)
      mx2$sig[wx] = 1

    wsig = which(mx2$sig == T)


    g  = ggplot(mx2, aes(
      x =  screen,
      y = fitness_defect,
      col = factor(sig)
    )) + theme_bw() +
      geom_point(aes(col = factor(sig)),
                 shape = mx2$shape,
                 size = 4)


    g1 = g + theme(legend.position = "none") +
      theme(panel.grid.minor =   element_blank()) +
      theme(panel.grid.major = element_blank()) +
      theme(axis.ticks = element_blank(), axis.text.x =   element_blank())

    g1 = g1 + labs(y = "fitness defect score") + labs(x = "compound") +
      geom_hline(
        yintercept = median(mx2$fitness_defect),
        color = "black",
        linetype = "dashed",
        size = 1
      ) +
      ggtitle(tit)

    g2 = g1 + geom_text_repel(
      size = 5,
      data = subset(mx2, sig == TRUE),
      aes(x =  screen, y = fitness_defect, label = screen),
      point.padding = 0.25,
      segment.alpha = 0.2,
      col = "black"
    )

    g2 = g2 + theme(
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 24, face = "bold"),
      plot.title = element_text(size = 24, face = "bold")
    )
    g2
  })

  output$geneinfo  <- renderDataTable({

  w = which(fdat$sgd_gene %in% rownames(dint))

  fd = fdat[w,] %>% distinct(sgd_gene,.keep_all = T)

  w1 = which(fd$sgd_gene %in% toupper(input$gene))

  validate(
    need(length(w1) == 1 ,message =
           "please enter a valid gene"))

   f = fd[w1,c("sgd_gene","sgd_orf","descriptor")]

    m= myhyperlinkblank(x = f$sgd_gene,href = "https://www.yeastgenome.org/locus/")

    f$sgd_gene = m


  names(f)[1] = c("GENE")

  f

},escape = F,options=list(paging = F,searching = F,

  info = F,scrollX = T,scrollY=TRUE,scrollCollapse=TRUE,autowidth=TRUE),

  rownames = F)



  observeEvent(input$brushpts,
               {
                 mx2 = data_for_genebydrug()

                 mx2$fitness_defect = round(mx2$fitness_defect, 2)
                 pts = brushedPoints(mx2, input$brushpts,
                                     xvar = "screen", yvar = "fitness_defect")

                 g = grep("shape", names(pts))

                 if (length(g) > 0)
                   pts = pts[, -g]
                 output$tabpts = DT::renderDataTable({
                   pts

                 },  options = list(
                   dom = 'Bfrtip',
                   paging = F,
                   searching = F,
                   info = F,
                   autowidth = T,
                   scrollX = TRUE,
                   ordering = F,
                   columnDefs = list(
                     list(className = 'dt-left', targets = -c(1:8)),

                     list(width = c('100px'), targets = -c(2, 5, 8)),

                     list(width = c('30px'), targets = -c(1, 3, 6, 7)),
                     list(width = c('300px'), targets = -4)
                   )
                 ),

                 escape = F, class = 'table-bordered stripe table-condensed', rownames = F,

                 selection = "single", server = F)
               },
               ignoreInit = T,
               ignoreNULL = F)

  observeEvent(input$tabpts_rows_selected, {
    row = input$tabpts_rows_selected

    req(data_for_genebydrug())
    mx2 = data_for_genebydrug()

    nam = names(mx2)

    pts = brushedPoints(mx2, input$brushpts, xvar = "screen", yvar = "fitness_defect")

    choices = pts$screen[row]

    req(length(choices)>0)

    updateSelectizeInput(session,"cmpSERV",label = "",
                         choices = choices, selected = choices[1])

                 updateSelectizeInput(
                   session,
                   "cmpSERV",
                   label = "",
                   choices = phiphop$name,
                   selected = choices, server = TRUE
                 )



    newtab <- switch(input$tabs,
                     "genebyscreen" = "hiphop",
                     "hiphop" = "genebyscreen")
    updateTabItems(session, "tabs", newtab)



  })


########################################################################
############################ END GENEBYSCREEN ##########################
########################################################################


########################################################################
############################ START HIPHOP ##############################
########################################################################

  observeEvent(input$resetCmp,
               {

   updateSelectizeInput(
                   session,
                   "cmpSERV",
                   label = "",
                   choices = phiphop$name,
                   selected = "rapamycin:4nM",
                   server = T
   )
               },
               ignoreInit = F,
               ignoreNULL = T)

  output$genesHIP = renderDataTable({
    req(input$cmpSERV)
    hip = geneAnno(
      mat = xe,
      fdat = fdat, xvar = F,
      sgdlink = T, arrange = T,
      cmp = input$cmpSERV
    )

    hip$FD = format(round(hip$FD, 2), nsmall = 2)
    hip

  }, options = list(
    pageLength = 10,
    autoWidth = T,
    scrollX = TRUE,
    columnDefs = list(list(
      width = "60px", targets = c(0,1,2)
    ))
  ), rownames = F, escape = F)



  output$genesHOP = renderDataTable({
    req(input$cmpSERV)
    hop = geneAnno(
      mat = xne, xvar = F,
      fdat = fdat, arrange = T,
      sgdlink = T,
      cmp = input$cmpSERV
    )

    hop$FD = format(round(hop$FD, 2), nsmall = 2)
    hop

  }, options = list(
    pageLength = 10,
    autoWidth = T,
    scrollX = TRUE,
    columnDefs = list(list(
      width = "60px", targets = c(0,1,2)
    ))
  ), rownames = F, escape = F)


  ##########################    FITNESS PLOT OUTPUT

  output$efd = renderPlot({
    validate(need(input$cmpSERV, message =
                    "please select condition"))
    cond1 = input$cmpSERV

    w = which(phiphop$name %in% cond1)
    exp = phiphop$name[w]

    wx = which(colnames(xe) %in% cond1)

    colnames(xe)[wx] = paste0("HIP|", colnames(xe)[wx])


    p10(xe,wx,pch = 17, ylim = input$elim,sig = input$scorethreshSERV)

  })

  essPlot = function() {
    validate(need(input$cmpSERV, message =
                    "please select condition"))
    cond1 = input$cmpSERV

    w = which(phiphop$name %in% cond1)
    exp = phiphop$name[w]

    wx = which(colnames(xe) %in% cond1)

    colnames(xe)[wx] = paste0("HIP|", colnames(xe)[wx])


    p10(xe,wx,pch = 17, ylim = input$elim,sig = input$scorethreshSERV)

  }

  output$nfd = renderPlot({
    validate(need(input$cmpSERV, message =
                    "please select condition"))
    cond1 = input$cmpSERV

    wx = which(colnames(xne) %in% cond1)

    colnames(xne)[wx] = paste0("HOP|", colnames(xne)[wx])

    p10(xne,wx,pch = 17, ylim = input$ylim,sig = input$scorethreshSERV)

  })


  nonPlot = function() {
    validate(need(input$cmpSERV, message =
                    "please select condition"))
    cond1 = input$cmpSERV

    w = which(phiphop$name %in% cond1)
    exp = phiphop$name[w]

    wx = which(colnames(xne) %in% cond1)

    colnames(xne)[wx] = paste0("HOP|", colnames(xne)[wx])


    p10(xne,wx,pch = 17, ylim = input$ylim,sig = input$scorethreshSERV)


  }

  output$ExportessPlot <- downloadHandler(
    # file name
    filename = function() {
      paste0("HOPplot:", input$cmpSERV,  "_", Sys.Date(), ".png")
    },
    # content
    content = function(file) {
      # create plot
      png(file,
          width = (input$shiny_width / 12) * 6,
          height = 1000)
      essPlot()
      dev.off()
    }
  )

  output$ExportnonPlot <- downloadHandler(
    # file name
    filename = function() {
      paste0("HOPplot:", input$cmpSERV,  "_", Sys.Date(), ".png")
    },
    content = function(file) {
      # create plot
      png(file,
          width = (input$shiny_width / 12) * 6,
          height = 1000)

      nonPlot()
      dev.off()
    }
  )

  outhop = reactive({
    req(input$cmpSERV)
    hop = geneAnno(
      mat = xne,
      fdat = fdat,
      sgdlink = F,
      cmp = input$cmpSERV
    )
  })

  output$downloadhop <- downloadHandler(
    filename = function() {
      paste0("HOP:", input$cmpSERV, "_", Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(
        as.data.frame(outhop()),
        file,
        row.names = F,
        sep = "\t",
        quote = F
      )
    }
  )

  outhip = reactive({
    req(input$cmpSERV)
    hip = geneAnno(
      mat = xe,
      fdat = fdat,
      sgdlink = F,
      cmp = input$cmpSERV
    )
  })


  output$downloadhip <- downloadHandler(
    filename = function() {
      paste0("HIP:", input$cmpSERV, Sys.Date(), ".txt")
    },
    content = function(file) {
      write.table(
        as.data.frame(outhip()),
        file,
        row.names = F,
        sep = "\t",
        quote = F
      )
    }
  )


########################################################################
############################## END HIPHOP ##############################
########################################################################

#######################################################
#######################################################
################## START Y-AXIS LIMITS ################
#######################################################
#######################################################
output$ylimits = renderUI({

   x = xinput()

   req(input$cmpSERV)

   coln = colnames(x)


   wcoln = which(coln %in% input$cmpSERV)
   req(length(wcoln) > 0)

   req(xinput())
   req(input$cmpSERV)

   x = xinput()

   wne = which(rownames(x) %in% noness$sgd_gene)

   we = which(rownames(x) %in% ess$sgd_gene)

   if(length(wne) > 0) {

      ylims = c(floor(min(x[wne,wcoln,drop=F],na.rm = T)),
                ceiling(max(x[wne,wcoln,drop=F],na.rm = T)))
      }
     req(length(wne)>0)

     if(length(we) > 0) {

     elims = c(floor(min(x[we,wcoln,drop=F],na.rm = T)),
               ceiling(max(x[we,wcoln,drop=F],na.rm = T)))
     }

tagList(

  if(length(we) >0){
    column(width = 4,
  box(title= "HIP y-axis range:",

    fluidRow(
      column(width = 12,
      sliderInput("elim", label = NULL, min = elims[1],
        max = elims[2], value = c(-0.5,elims[2])))),

    fluidRow(
        column(width = 12,align = "center",
        actionLink(
          inputId = "eorig",
          label = "reset",
          style = "height: 100px;width: 150px;font-size:120%;text-align:center",
          size = "lg"))),
        status = "primary", solidHeader = T,width = "100%", height = 200))},

   if(length(ylims) == 2){
   column(width = 4,

    #column(1,
     box(title= "HOP y-axis range:",
       fluidRow(
         column(width=12,

       # Copy the line below to make a slider range
         sliderInput("ylim", label = NULL, min = ylims[1],
        max = ylims[2], value = c(-0.5,ylims[2])))),

        fluidRow(
          column(width = 12,align = "center",
          actionLink(
           inputId = "yorig",
           label = "reset",
           style = "height: 100px;width: 150px;font-size:120%;text-align:center",
           size = "lg"))),
          status = "primary", solidHeader = T,width = "100%", height = 200))})

})

 observeEvent(input$yorig,{

   x = xinput()
   coln = colnames(xinput())

   wcoln = which(coln %in% input$cmpSERV)

   wne = which(rownames(x) %in% noness$sgd_gene)

   if(length(wne) > 0) {
      xne = xinput()
      ylims = c(floor(min(xne[wne,wcoln,drop=F],na.rm = T)),
                ceiling(max(xne[wne,wcoln,drop=F],na.rm = T)))
      }

   updateNumericRangeInput(session, "ylim", "", value = ylims)

    #reset("ylim")

 },ignoreInit = F, ignoreNULL = T)


observeEvent(input$eorig,{

   cole = colnames(xinput())
   xe = xinput()
   wcole = which(cole %in% input$cmpSERV)

   we = which(rownames(xe)%in% ess$sgd_gene)

   if(length(we) > 0) {elims = c(floor(min(xe[we,wcole,drop=F],na.rm = T)),
                                   ceiling(max(xe[we,wcole,drop=F],na.rm = T)))
     }
   updateNumericRangeInput(session, "elim", "",value = elims)

})


################# Y AXES LIMITS ###################
###################################################
###################################################


observeEvent(input$resetscore, {
  updateNumericInput(session, "scorethreshSERV", "", value = 1.0, step =0.1, min = 0, max = 5.0)
},ignoreInit = F, ignoreNULL = T)

#######################################################
#######################################################
################## END Y-AXIS LIMITS ##################
#######################################################
#######################################################

#######################################################
#######################################################
################## START COINHIBITION #################
#######################################################
#######################################################


outcoInhib = reactive({

  req(input$cmpSERV)


      w = which(colnames(xcoinh) == input$cmpSERV)
        validate(
          need(length(w) == 1 ,message =
                 "please enter a valid condition"))
      p = xpval[,w,drop=F]
      d = xcoinh[,w,drop = F]

      d = d[order(d[,1],decreasing = T),,drop=F]



      df = data.frame(screen = rownames(d)[1:nrow(d)],
                      CoI = d[1:nrow(d),1],pvalue = p[rownames(d),1],stringsAsFactors = F)
      df$pvalue = formatC(df$pvalue,format = "e", digits = 2)
      w = which(df$CoI >= 0)
      if(length(w) > 0) df = df[w,]
      m = match(df$screen,phiphop$name)
      df$CoI = format(round(df$CoI,2),nsmall = 1,scientific = F)
      df$CID = phiphop$PCID[m]

      df$structure = phiphop$png[m]
      w = which(names(df) == "CID")
      names(df)[w] = '<a href="https://pubchem.ncbi.nlm.nih.gov" target="_blank">CID</a>'

      w=which(names(df)=="image")
      names(df)[w]="structure"

      df$mechanism = phiphop$mechanism[m]
      df
   })


####################################################
####################################################
##
####################################################
####################################################
####################################################
####################################################
output$coInhib = DT::renderDataTable({



      df = outcoInhib()

      opts = list(
        dom = 'Bfrtip',
        paging = T,
        target = "cell",
        searching = F,
        info = F,
        autowidth = T,
        scrollX = TRUE,
        ordering = F,
        pageLength = 5,
        columnDefs = list(
                  list(className = 'dt-left',targets = c(0,1,2,4,5)),
                  list(className = 'dt-center',targets = c(3)),
                  list(className = 'dt-nowrap',targets = c(0,2,4)),
                  list(width = c('25px'),targets = c(1)),
                  list(width = c('50px'),targets = c(2,3)),
                  list(width = c('415px'),targets = c(5)),
                  list(width = c('200px'),targets = c(4)),
                  list(width = c('200px'),targets = c(0))))




        DT::datatable( outcoInhib(), options = opts,
                    escape = F,  rownames = F, selection = "single")

    })
####################################################
#####################
observeEvent(input$coInhib_rows_selected, {

  row <- input$coInhib_rows_selected

  df = outcoInhib()
  delay(500,updateSelectizeInput(session,"cmpSERV",label = NULL,
                                 choices = df$screen, selected = df$screen[row][1],server = T))

})

##############

      output$downloadcoinhib <- downloadHandler(
      filename = function() { paste0("coinhibition:",
      input$cmpSERV, "_" ,Sys.Date(), ".txt")
      },
      content = function(file) {
        write.table(as.data.frame(outcoInhib()), file, row.names = T,sep="\t",quote=F)
      }
    )

#######################################################
#######################################################
#################### END COINHIBITION #################
#######################################################
#######################################################

 }
shinyApp(ui, server)
