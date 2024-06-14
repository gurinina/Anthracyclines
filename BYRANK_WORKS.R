# BYRANK.WORKS

tabItems(

  tabItem(tabName = "genebyscreen",

  fluidRow(

    column(width = 3,

 (title = "Enter gene of interest:",

        textInput(inputId = "gene", label = NULL, value   = "ARG4"),

      status = "primary", solidHeader = T, width = "100%", height = 250),

    column(width = 9,

 (title = "Gene descriptor and function:",

        dataTableOutput('geneinfo'),

      status = "primary", solidHeader = T, width = "100%", height = 250)))), # ok

  fluidRow(

    column(width = 12,

 (title = "Fitness defect score across conditions:",

        h5("click OR drag to select screens of interest;
          corresponding profiles can be foundin compound menu
          on hiphop tab; cyan: p-value < 0.001; triangle:
          screens identifying drug target candidates"),

        plotOutput("genebydrug",

          click = clickOpts(id = "clickpts"),

          brush = brushOpts(id = "brushpts"), height = 800),

        status = "primary", solidHeader = T, width = "100%"))), # ok

  fluidRow(

    box(title = "Experimental detail; select row to view corresponding

      HOP chemogenomic profile",

      dataTableOutput("tabpts"),

     status = "primary", solidHeader = T, width = 12, height = 250)), # ok

  fluidRow(

    box(title = "GO enrichment network: select a node to view details;
      right click to save image",

      width = 8, status = "primary", solidHeader = TRUE, height = 800,

      uiOutput("GOterms"),

      visNetworkOutput("network_Proxy", width = "85%", height = 652)),

    box(title = "GO term set enrichment details:",

        width = 4,

        uiOutput("GoTable"),

    solidHeader = T, status = "primary", height = 200),

    box(title = "Top-contributing genes:",

      width = 4, br(),

      uiOutput("leadingEdge"),

    solidHeader = T, status = "primary", height = 580)), # ok

  fluidRow(

    box(title = "GO enrichment network: select a node to view details;
      right click to save image",

      width = 8, status = "primary", solidHeader = TRUE, height = 800,

      uiOutput("GOterms"),

      visNetworkOutput("network_Proxy", width = "85%", height = 652)),

    box(title = "GO term set enrichment details:",

        width = 4,

        uiOutput("GoTable"),

    solidHeader = T, status = "primary", height = 200),

    box(title = "Top-contributing genes:",

      width = 4, br(),

      uiOutput("LeadingEdge"),

    solidHeader = T, status = "primary", height = 580)), # ok

  fluidRow(

    box(title = "GO enrichment table:",

      DT::dataTableOutput("EnrichTable"),

    status = "primary", background = "navy",solidHeader = T, width = 12)), # ok

  fluidRow(

    width = 3,

    box(align = "center", title = "Download CoFitness:",

      br(),

      downloadButton("downloadcofitness", "HOP CoFitness"),

    status = "primary", background = "navy",solidHeader = T, width = "100%", height = 150), # box ok

    column(width = 3,

 (align = "center", title = "Download EnRichments:",

        br(),

        downloadButton("enrichdownload", "GO EnRichments"),

     status = "primary", background = "navy",solidHeader = T,

     width = "100%", height = 150)))), # tab ok

tabItem("hiphop", class = "active",

  fluidRow(

    column(width = 3,

 (title = "Select input datasets:",

        br(),

        prettyRadioButtons("site", label = NULL,

        choices = c(

          "2008 Erikson PLOS Genetics" = "elke",
          "2021 HIPHOP Marjan" = "marjhip",
          "2020:2024 HOP Marjan" = "marjhop"),

           outline = T, fill = FALSE, thick = T, shape = "square",

           bigger = FALSE, selected = "marjhop", inline = F),

      status = "primary", background = "navy",solidHeader = T, width = "100%", height = 160)),

    column(width = 9,

 (title = "Select screen:",

        selectizeInput('cmpSERV', label = NULL,

          choices = phop$name, multiple = F, selected = "azithromycin_1.75mM",

          options = list('plugins' = list('autofill_disable'))),

      status = "primary", solidHeader = T, width = "100%", height = 160))), #

    column(width = 3,

 (title = "Fitness score threshold:")),

    (

          column( align = "center", width = 12,

          sliderInput("scorethreshSERV", label = NULL, min = 0, value = 1.0,

            step = 0.1, max = 5.0, width = "100%", size = "lg"))),

          column(width = 3,

    box(align = "center", title = "Reset compound menu:",

      br(),

        actionButton("resetCmp", "Reset cmpMenu"),

    status = "primary", solidHeader = T, width = "100%", height = 200)), # ok

  column(width = 3,

  column(width = 12, align = "center",

    box(title = "Fitness score threshold:",

      sliderInput("scorethreshSERV", label = NULL, min = 0, value = 1.0,

        step = 0.1, max = 5.0), # ok

      (column(width = 12, align = "center",

            actionLink(inputId = "resetScore", label = "reset",

            style = "height: 100px;width: 150px;font-size:120%;text-align:center", size = "lg"))),

   status = "primary", solidHeader = T, width = "100%", height = 200)),

     uiOutput("ylimits")), # fluidRow ok

     uiOutput("hiphoppanel")), # tab ok

tabItem("signature",

  fluidRow(sigNetworkModuleUI("sigNetworkModule1")),

  fluidRow(

    box(title = "HOP profiles in response signature:",

      dataTableOutput("screenResp"),

    status = "primary", solidHeader = T, width = "100%", height = 12))),

tabItem("goenrich",

  fluidRow(visNetworkModuleUI("visNetwork1")))))

ui<-dashboardPage(header, sidebar, body, skin = "blue",
  setBackgroundColor(color = "#e6e6ff",shinydashboard = TRUE))

server <- function(input, output, session) {

##############################################################################
########################### START HIPHOPPANEL ################################
##############################################################################


##################################################
########### START GO ENRICHMENT BY RANK ##########
##################################################

##################################################
############### START outFit outMit ##############
##################################################

outFit = reactiveValues(dFit = NULL)

outMit = reactiveValues(mFit = NULL)

observeEvent(input$gene,{

  req(input$gene)

  req(input$site)

  if(input$site == "marjhip") {xfit = xfithh}
  if(input$site == "elke") {xfit = xefit}
  if(input$site == "marjhop") {xfit = xcofit}

  if(input$site == "marjhip") {xpv = xpithh}
  if(input$site == "elke") {xpv = xefitpv}
  if(input$site == "marjhop") {xpv = xpfit}

  ww = which(colnames(xfit) == toupper(input$gene))

  print(c("ww",ww))

  validate(need(length(ww) == 1 ,message = "please enter a valid gene"))

  if(length(ww) != 0) {
        p = xpv[,ww,drop=F]
        d = xfit[,ww,drop = F]
       }

  d = d[order(d[,1],decreasing = T),,drop=F]

  df = data.frame(gene = rownames(d), cofit = d[rownames(d),1],
                      pvalue = p[rownames(d),1],stringsAsFactors = F)

  w = which(df$cofit >= 0)

  if(length(w) >0) df = df[w,]

  xfit = df

  xfit$cofit = as.numeric(xfit$cofit)

  xfit$pvalue = as.numeric(xfit$pvalue)

  xfit$pvalue = formatC(xfit$pvalue,format = "e", digits = 2)

  xfit$cofit = format(round(xfit$cofit, 6), nsmall = 2)

  mfit = as.matrix(xfit[,2:3])

  mfit[,2] = 1:nrow(mfit)

  print(c("mfitg",nrow(mfit)))

  colnames(mfit)[2]="rank"

  m = mfit

  sss = rev(seq(0, 1, length.out = nrow(mfit)))

  ss = round(sss, 4)

  m = cbind(m,ss)

  mm = apply(m,2,as.numeric)

  rownames(mm) = rownames(m)

  outFit$dFit = xfit

  outMit$mFit = mm

})

##################################################
############### END outFit outMit ################
##################################################

output$fiToutput = renderUI({

tagList(title = "Set cofitness rank threshold:",

  HTML("<h5><b>Top: 1000 genes ranked by cofitness, from 1 to 1000, left to right;
           # on scale marks # of strains used in GO enrichment.
           Bottom: Corresponding cofitness values 1 to 1000, right to left.
           Click on arrow to animate GO enrichment
           dependency on cofitness rank.</b></h5>"),

      br(), br(),

      HTML("<h5><b>Cofitness values:</b></h5>"),

      st =rev(seq(0,1,0.05)),

      sq = rev(seq(0,1,0.001)),

      sliderTextInput(inputId = "corrFIT", label = NULL,

      choices = st,grid = T,animate = T),

      HTML("<h5><b>Corresponding cofitness rank:</b></h5>"),

      sliderInput("scoreFIT",label = "",
   value = 1, min = 1,max = 1000, step = 1,animate = TRUE),

})

rankFIT = debounce(reactive(input$scoreFIT),500)
coFIT = debounce(reactive(input$corrFIT),500)

scoreVal = reactiveValues(
  previous = NULL,
  current  = NULL
)

corrVal = reactiveValues(
  previous = NULL,
  current  = NULL
)


# 0,49 621 0.28 721 rank 0.74 271 0.56 441,[ 0,908 93 ]0.86 93 102 0.899 102 0.867
   observeEvent(input$corrFIT,{

     req(input$scoreFIT)

     req(input$corrFIT)

     print(c("input$scoreFIT",input$scoreFIT))
     print(c("input$corrFIT",input$corrFIT))

     corr = input$corrFIT

     mfit = outMit$mFit

     sss = rev(seq(0, 1, length.out = nrow(mfit)))

     ss = round(sss, 4)

     w = which(mfit[,3] == corr)

     req(length(w) > 0)

     score = mfit[w,"rank"]

     w1 = slid(x =input$corrFIT,y = input$scoreFIT, mfit = mfit)

     score1 = w1$corr[2]

     print(c("obfit","score1",score1,"score",score, "wscore",
             w1$score,"current" ,corrVal$current,
             "corr",w1$corr,"scoreFIT",input$scoreFIT,"corrFITr",
             input$corrFIT,"previp" ,corrVal$previous,"currs" ,
             scoreVal$current,"current" ,scoreVal$previous))

     test = c(as.numeric(input$scoreFIT) != scoreVal$current)

     test2 = as.numeric(input$scoreFIT) != score

     print(c("test",test))

     print(c("test2",test2))

     print(c("scorefit",score,input$scoreFIT,scoreVal$current))

     cond =   is.null(scoreVal$current)

  if( is.null(scoreVal$current)){

     updateSliderInput(session = session,
          inputId = 'scoreFIT',
          value = score) # updateSliderInput

   } else if(!is.null(scoreVal$current) & as.numeric(input$scoreFIT) != score){

     updateSliderInput(session = session,
          inputId = 'scoreFIT',
          value = score)}

     scoreVal$previous = scoreVal$current

     scoreVal$current = score

    }, ignoreNULL = F, ignoreInit = T)


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


   sss = rev(seq(0, 1, length.out = nrow(mfit)))

   ss = round(sss, 4)


   if( is.null(corrVal$current)){

      updateSliderTextInput(session,"corrFIT",label =  NULL,
      choices = ss, selected = score)

    } else if(as.numeric(input$corrFIT) != score){

      updateSliderTextInput(session,"corrFIT",label =  NULL,
      choices = ss, selected = score)}

      corrVal$previous = corrVal$current

      corrVal$current  = score

  }, ignoreNULL = F, ignoreInit = F)

### this is for fdrRANK

output$mxFIT <- renderUI({
  tagList(

    box(title = "Optimizing GO enrichment:",

        tags$head(tags$style(HTML("<h5><b>label(font-family:Arial;
                                  font-weight:bold;font-size;20)<h5><b>"))),

        HTML("<h5><b>Finds the cofitness/cofitness rank that maximizes
             the number of GO enrichment terms for the top 1000 cofit
             strains for a given FDR value. Stepsize is the increment
             between ranks tested.<h5><b>"),

        HTML("<h5><b>Choose a stepsize and FDR threshold, then hit GO to run.<h5><b>"),

        HTML("<h5><b>Warning: calculation time for stepsize 1: ~ 3min.<h5><b>"),

        br(),

    (

          column(width = 12,align = "center",

                 prettyRadioButtons(inputId = "stepsize",label = "stepsize",
                                    choices = c(1,5,20,50), outline = T, fill = F,thick = T,
                                    status = "primary", shape = "round", bigger = T,
                                    width = "100%",
                                    selected = 50, inline = T)),

  HTML ("<h5><b>&nbsp&nbsp&nbsp&nbsp&nbspSet FDR threshold</b></h5>"),

          br(),
          column(width = 12,align = "center",

                 sliderInput("fdrRANK",label = "", min = 0, max = 0.5,
                             value = 0.2, step = 0.1)),

          column(width = 12,align = "left",
               actionButton("rankGO", "GO!", class = "btn-success")),

          column(width = 12, align = "left",
               tags$head(tags$style(HTML("<h5><b>label(font-family:Arial;
                                         font-weight:bold;font-size;20)<h5><b>"))),
       HTML Output("finishTEXT"),
       HTML Output("stepTEXT"),

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


observeEvent(input$rankGO,{

  req(outMit$mFit)

  req(input$fdrSERV)

  gseaMAX$rank = NULL
  gseaMAX$cofit = NULL
  gseaMAX$goterms = NULL
  shinyjs::hide(id = "finishTEXT")
  shinyjs::hide(id = "stepTEXT")
  shinyjs::hide(id = "mxCOFITvalue")

  mfit = outMit$mFit

  FDR = input$fdrRANK

  mn = 1

  mx = 1000

  steps = pretty(c(mn,mx),n = 100)

  range = max(steps)-min(steps)

  step = range/(length(steps) -1)


  if(as.numeric(input$stepsize) == 1) {

    steps = 1:1000} else if(as.numeric(input$stepsize) == 5){

    steps = seq(0,1000,5)} else if(as.numeric(input$stepsize) == 20){

    steps = seq(0,1000,20)} else if(as.numeric(input$stepsize) == 50){

    steps = seq(0,1000,50)}

  dFIT = NULL

  for(i in 1:length(steps)) dFIT[[i]] = compRANK(mfit,coln = 2, rank  = steps[i])

  names(dFIT)=steps

  net = lapply(dFIT,runGOENRICH,fdrThresh = FDR, minSetSize = 5,
    curr_exp = "tst",bp_path =  "2023_January30_GOBP.RDS",
    go_path = "2023_January30_GOID_GOBP_SGD.txt")

  enrich = lapply(net,function(x) x = x[[1]])

  isnull = sapply(enrich,is.null)

  wisnull = which(isnull ==T)

  nrw = sapply(enrich,nrow)

  if(length(wisnull) > 0) isnull = lapply(nrw[wisnull], function(x) x = 0)

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

    paste("<h5><b>","Results:","</b></h5>")})

  output$stepTEXT = renderText({

    paste("<h5><b>","STEPSIZE = ",input$stepsize,";","FDR = ",input$fdrSERV,"</b></h5>")})

  output$mxCOFITvalue = renderText({

   req(!is.null(gseaMAX$cofit))

   paste("<h5><b>","COFITNESS = ",round(as.numeric(isolate(gseaMAX$cofit)),3),
      ";","RANK = ", isolate(gseaMAX$rank),"</b></h5>")})

},ignoreNULL = T,ignoreInit = T)


output$mxCOFITvalue = renderText({

  NULL

})

output$GOterms <- renderUI({

  htmlOutput("countGO")

})

observeEvent(gseaMAX$goterms,{

  output$countGO = renderText({paste("<h5><b>","no. GO terms =",
    gseaMAX$goterms,"</b></h5>")})

})

observeEvent(gseaMAX$rank,{

  req(outMit$mFit)

  req(!is.null(gseaMAX$rank))

  mfit = outMit$mFit

  mx = nrow(mfit)

  value = gseaMAX$rank

  scoreVal$previous = input$scoreFIT

  scoreVal$current = gseaMAX$rank

  updateSliderInput(session, "scoreFIT",label = NULL,min = 1,
    max = 1000, value = gseaMAX$rank, step = 1)

},ignoreNULL = T, ignoreInit = T)

observeEvent(rankFIT(),{

  req(outMit$mFit)

  req(!is.null(input$corrFIT))

  if(is.null(scoreVal$current)) scoreVal$current = rankFIT()

  if(rankFIT()!=scoreVal$current) {

    scoreVal$previous = scoreVal$current

    scoreVal$current  = rankFIT()}

  mfit = outMit$mFit

  mx = nrow(mfit)

  w = which(mfit[,2]==rankFIT())

  req(length(w)!=0)

  corel = mfit[w,1]

  if(length(corel)>1) corel = corel[1]

    updateSliderInput(session,"corrFIT",label = NULL,
        min = 0, max = 1.0,  value = corel, step = 0.1)

  corrVal$previous = corrVal$current

  corrVal$current = corel#}

}, ignoreNULL = T,ignoreInit = F)

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

     mfit = outMit$mFit

     gseaMAX$goterms = NULL

     num = is.numeric(input$scoreFIT)

     req(num == TRUE)

     wfit = which(mfit[,2] <= input$scoreFIT)

     validate(need(length(wfit)!=0, message = "No CoFitness scores above threshold"))

     req(length(wfit)!=0)

     qFIT = compRANK(mfit,coln = 2, rank  = input$scoreFIT)

     curr_exp = "cofit"

     FDR = input$fdrSERV

     network = runGOENRICH(fdrThresh = FDR, minSetSize = 5,

        curr_exp = curr_exp,score = qFIT, bp_path =  "2023_January30_GOBP.RDS",

        go_path =    "2023_January30_GOID_GOBP_SGD.txt")


     validate(need(!is.null(network$enrichInfo), message = "No GO enrichment,
           try relaxing the FDR or scorethreshold"))

    enrichInfo = network$enrichInfo

    req(!(is.null(enrichInfo)))

    if(!is.null(enrichInfo)) gseaMAX$goterms = nrow(enrichInfo)

    edgeMat = network$edgeMat

    if(!is.null(enrichInfo)) return(network)

  })

##################################################
########### END GO ENRICHMENT BY RANK ############
##################################################
