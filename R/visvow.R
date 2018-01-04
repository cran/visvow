#' Visible Vowels
#'
#' Visible Vowels is an app that visualizes vowel variation in f0, F1, F2, F3 and duration.
#'
#' \code{visvow()} opens Visible Vowels in your default web browser.
#'
#' @seealso
#' The Help tab in the app provides more information about the format of the input file.
#'
#' @references
#' \insertRef{visvow:2017}{visvow}
#'
#' @examples
#' visvow
#'
#' @import shiny
#' @import shinyBS
#' @import ggplot2
#' @import plot3D
#' @import readxl
#' @import WriteXLS
#' @import pracma
#' @import plyr
#' @import grid
#' @import svglite
#' @import Cairo
#' @importFrom Rdpack reprompt
#'
#' @export

visvow <- function()
{
  options(shiny.sanitize.errors = TRUE)
  options(shiny.usecairo=FALSE)

  addResourcePath('www', system.file(package='visvow'))

  shinyApp(
    ui <- fluidPage
    (
      img(src = 'www/FA.jpg', height = 60, align = "right"),
      titlePanel("Visible Vowels"),

      navbarPage
      (
        title=NULL,

        tabPanel
        (
          title = "Load file",

          splitLayout
          (
            style = "border: 1px solid silver;",
            cellWidths = c("30%", "70%"),
            cellArgs = list(style = "padding: 6px"),

            verticalLayout
            (
              fileInput('vowelFile', 'Choose xlsx file',accept = c(".xlsx"), width="100%"),
              textOutput('checkFormat')
            ),

            column
            (
              width=12,
              align="center",
              DT::dataTableOutput('vowelRound')
            )
          )
        ),

        tabPanel
        (
          title = "f0",

          splitLayout
          (
            style = "border: 1px solid silver;",
            cellWidths = c("30%", "70%"),
            cellArgs = list(style = "padding: 6px"),

            verticalLayout
            (
              textInput("title0", "Plot title", "", width="100%"),
              uiOutput('selScale0'),

              splitLayout
              (
                uiOutput('selXaxis0'),
                uiOutput('selPlot0'),
                uiOutput('selLine0')
              ),

              splitLayout
              (
                uiOutput('catXaxis0'),
                uiOutput('catPlot0'),
                uiOutput('catLine0')
              )
            ),

            verticalLayout
            (
              column
              (
                width = 11,
                align = "center",
                plotOutput("graph0", height="627px")
              ),

              splitLayout
              (
                uiOutput('selFormat0a'),
                downloadButton('download0a', 'Table'),
                uiOutput('selFormat0b'),
                uiOutput('selSize0b'),
                downloadButton('download0b', 'Graph')
              )
            )
          )
        ),

        tabPanel
        (
          title = "Formants",

          splitLayout
          (
            style = "border: 1px solid silver;",
            cellWidths = c("30%", "70%"),
            cellArgs = list(style = "padding: 6px"),

            verticalLayout
            (
              textInput("title1", "Plot title", "", width="100%"),
              uiOutput('selTimes'),

              splitLayout
              (
                cellWidths = c("30%", "70%"),
                uiOutput('selScale1'),
                uiOutput('selNormal1')
              ),

              uiOutput('selTimesN'),

              splitLayout
              (
                uiOutput('selColor'),
                uiOutput('selShape'),
                uiOutput('selPlot1')
              ),

              splitLayout
              (
                uiOutput('catColor'),
                uiOutput('catShape'),
                uiOutput('catPlot1')
              ),

              uiOutput('selGeon'),
              uiOutput('selPars'),

              splitLayout
              (
                checkboxInput("grayscale", "grayscale", FALSE),
                checkboxInput("average"  , "average"  , FALSE)
              )
            ),

            verticalLayout
            (
              splitLayout
              (
                cellWidths = c("80%", "20%"),

                column
                (
                  width = 12,
                  align = "center",
                  plotOutput("graph1", height="627px")
                ),

                column
                (
                  width = 12,

                  HTML("<br style='margin-bottom:1px;'/>"),
                  HTML("<br style='margin-bottom:1px;'/>"),

                  selectInput('axisX', "x-axis", choices=c(     "F1","F2","F3"), selected="F2", selectize=FALSE),
                  selectInput('axisY', "y-axis", choices=c(     "F1","F2","F3"), selected="F1", selectize=FALSE),
                  selectInput('axisZ', "z-axis", choices=c("--","F1","F2","F3"), selected="--", selectize=FALSE),

                  uiOutput('manScale'),
                  uiOutput('selF1min'),
                  uiOutput('selF1max'),
                  uiOutput('selF2min'),
                  uiOutput('selF2max')
                )
              ),

              splitLayout
              (
                uiOutput('selFormat1a'),
                downloadButton('download1a', 'Table'),
                uiOutput('selFormat1b'),
                uiOutput('selSize1b'),
                downloadButton('download1b', 'Graph')
              )
            )
          )
        ),

        tabPanel
        (
          title = "Duration",

          splitLayout
          (
            style = "border: 1px solid silver;",
            cellWidths = c("30%", "70%"),
            cellArgs = list(style = "padding: 6px"),

            verticalLayout
            (
              textInput("title2", "Plot title", "", width="100%"),
              uiOutput('selNormal2'),
              uiOutput('selGraph'),

              splitLayout
              (
                cellWidths = c("70%", "30%"),

                radioButtons("selError", "Size of confidence intervals:", c("0%","90%","95%","99%"), selected = "0%", inline = TRUE),
                radioButtons("selMeasure", "Use:", c("SD","SE"), selected = "SD", inline = TRUE)
              ),

              splitLayout
              (
                uiOutput('selXaxis2'),
                uiOutput('selLine2'),
                uiOutput('selPlot2')
              ),

              splitLayout
              (
                uiOutput('catXaxis2'),
                uiOutput('catLine2'),
                uiOutput('catPlot2')
              )
            ),

            verticalLayout
            (
              column
              (
                width = 11,
                align = "center",
                plotOutput("graph2", height="627px")
              ),

              splitLayout
              (
                uiOutput('selFormat2a'),
                downloadButton('download2a', 'Table'),
                uiOutput('selFormat2b'),
                uiOutput('selSize2b'),
                downloadButton('download2b', 'Graph')
              )
            )
          )
        ),

        tabPanel
        (
          title = "Help",

          fluidPage
          (
            style = "border: 1px solid silver;",

            h5(strong("About")),

            p("Visible Vowels is a web app for the analysis of acoustic vowel measurements: f0, formants and duration. The progam has been developed at the Fryske Akademy (Leeuwarden, The Netherlands) by Wilbert Heeringa under supervision of Hans van de Velde. This program is implemented as a Shiny app. Shiny was developed by RStudio. This app uses the following R packages: shiny, shinyBS, ggplot2, plot3D, readxl, WriteXLS, DT, psych, pracma, plyr, grid, svglite, Cairo. A function for autocropping faceted plots made by using the ggplot2 package was developed by Z. Lin (Singapore). Visible Vowels is still under development. Comments are welcome and can be sent to", img(src = 'www/email.png', height = 19, align = "center"),"."),

            br(),

            h5(strong("Format of input file")),

            p("The input file should be a spreadsheet that is created in Excel or LibreOffice. It should be saved as an Excel 2007/2010/2013 XML file, i.e. with extension '.xlsx'. An example is schematically shown below. An example can also be downloaded ", a("here", href = "www/example_table.xlsx", target = "_blank"), "and be loaded by this program."),

            h6(strong("Speakers")),

            p("The first column should contain the speaker labels. Choose 'speaker' as column name. In our example there are three speakers labeled as 'A', 'B' and 'C'."),

            h6(strong("Vowels")),

            p("A column that contains the vowel labels should follow; for this column always choose 'vowel' as column name. In our example each of the speakers pronounced four different vowels: i\u02D0, \u025B, a\u02D0 and \u0254. Although in this table each vowel occurs just one time per speaker, multiple pronunciations are possible. In case you want to use IPA characters (as in the example), enter them as Unicode characters. In order to find Unicode IPA characters, use the online IPA chart of Weston Ruter. N.B.: the columns 'speaker' and 'vowel' are obligatory."),

            h6(strong("Categorical variables")),

            p("An arbitrary number of columns representing categorical variables such as location, language, gender, age group, etc. may follow, but is not obligatory. See to it that each categorical variable has an unique set of different values. Prevent the use of numbers, rather use meaningful codes. For example, rather then using codes '1' and '2' for a variable 'age group' use 'old' and 'young' or 'o' and 'y'."),

            h6(strong("Duration")),

            p("A column which contains the durations of the vowels should follow, with 'duration' as column name. The measurements may be either in seconds or milliseconds. This column is obligatory as well."),

            h6(strong("Spectral variables")),

            p("Finally, a set of five columns should follow: 'time', f0', 'F1', 'F2' and 'F3'. The variable 'time' gives the time point within the vowel interval in seconds or milliseconds, i.e. it is assumed that the vowel interval starts at 0 (milli)seconds. The f0, F1, F2 and F3 should be measured at the time given in the column 'time'. The program assumes that they are measured in Hertz and not normalized. The set of five columns may be repeated as ",em("many times"), " as the user wishes, but should occur at least one time. In our table f0, F1, F2 and F3 are given for two different time points, hence the set of five columns comprising 'time', 'f0', 'F1', 'F2' and 'F3' occurs twice, but may occur more often. For each repetition the same column names may be used, as was done in the example table below."),

            img(src = 'www/format.png', height=330),

            br(), br(),

            h5(strong("Normalization methods")),

            p("There are 12 vowel formant normalization methods available. Most of them are described and compared to each other in: Sander van der Harst (2011), The Vowel Space Paradox; A Sociophonetic Study on Dutch, Nijmegen: The Netherlands: Radboud Universiteit Nijmegen, section 4.3. The convex hull centroid procedure is not described in the thesis, but is similar to Watt & Fabricius' S-centroid procedure. However, the centroid is obtained on the basis of all the points the convex hull is made up of."),

            br(),

            h5(strong("References")),

            p("R Core Team (2017). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. https://www.R-project.org/ ."),
            p("Winston Chang, Joe Cheng, J.J. Allaire, Yihui Xie and Jonathan McPherson (2017). shiny: Web Application Framework for R. R package version 1.0.0. https://CRAN.R-project.org/package=shiny ."),
            p("Eric Bailey (2015). shinyBS: Twitter Bootstrap Components for Shiny. R package version 0.61. https://CRAN.R-project.org/package=shinyBS ."),
            p("H. Wickham (2009). ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York. http://ggplot2.org ."),
            p("Hadley Wickham (2016). readxl: Read Excel Files. R package version 0.1.1. https://CRAN.R-project.org/package=readxl ."),
            p("Hadley Wickham and Jennifer Bryan (2017). readxl: Read Excel Files. R package version 1.0.0. https://CRAN.R-project.org/package=readxl ."),
            p("Marc Schwartz and various authors. (2015). WriteXLS: Cross-Platform Perl Based R Function to Create Excel 2003 (XLS) and Excel 2007 (XLSX) Files. R package version 4.0.0. https://CRAN.R-project.org/package=WriteXLS ."),
            p("Yihui Xie (2016). DT: A Wrapper of the JavaScript Library 'DataTables'. R package version 0.2. https://CRAN.R-project.org/package=DT ."),
            p("Revelle, W. (2016) psych: Procedures for Personality and Psychological Research, Northwestern University, Evanston, Illinois, USA, Version = 1.6.12, https://CRAN.R-project.org/package=psych ."),
            p("Hans Werner Borchers (2017). pracma: Practical Numerical Math Functions. R package version 1.9.9. https://CRAN.R-project.org/package=pracma ."),
            p("Hadley Wickham (2011). The Split-Apply-Combine Strategy for Data Analysis. Journal of Statistical Software, 40(1), 1-29. URL http://www.jstatsoft.org/v40/i01/ ."),
            p("Hadley Wickham, Lionel Henry, T Jake Luciani, Matthieu Decorde and Vaudor Lise (2016). svglite: An 'SVG' Graphics Device. R package version 1.2.0. https://CRAN.R-project.org/package=svglite ."),
            p("Simon Urbanek and Jeffrey Horner (2015). Cairo: R graphics device using cairo graphics library for creating high-quality bitmap (PNG, JPEG, TIFF),  vector (PDF, SVG, PostScript) and display (X11 and Win32) output. R package version 1.5-9. https://CRAN.R-project.org/package=Cairo .")
          ),

          br()
        ),

        tabPanel
        (
          title = "!",

          fluidPage
          (
            style = "border: 1px solid silver;",

            h5(strong("Software disclaimer")),

            p("This app is provided 'as is' without warranty of any kind, either express or implied, including, but not limited to, the implied warranties of fitness for a purpose, or the warranty of non-infringement. Without limiting the foregoing, the Fryske Akademy makes no warranty that: 1) the app will meet your requirements, 2) the app will be uninterrupted, timely, secure or error-free, 3) the results that may be obtained from the use of the app will be effective, accurate or reliable, 4) the quality of the app will meet your expectations, 5) any errors in the app will be corrected."),
            p("The app and its documentation could include technical or other mistakes, inaccuracies or typographical errors. The Fryske Akademy may make changes to the app or documentation made available on its web site. The app and its documentation may be out of date, and the Fryske Akademy makes no commitment to update such materials."),
            p("The Fryske Akademy assumes no responsibility for errors or ommissions in the app or documentation available from its web site."),
            p("In no event shall the Fryske Akademy be liable to you or any third parties for any special, punitive, incidental, indirect or consequential damages of any kind, or any damages whatsoever, including, without limitation, those resulting from loss of use, data or profits, whether or not the Fryske Akademy has been advised of the possibility of such damages, and on any theory of liability, arising out of or in connection with the use of this software."),
            p("The use of the app is done at your own discretion and risk and with agreement that you will be solely responsible for any damage to your computer system or loss of data that results from such activities. No advice or information, whether oral or written, obtained by you from the Fryske Akademy shall create any warranty for the software.")
          )
        )
      )
    ),

    server <- function(input, output)
    {
      #############################################################################

      speaker=NULL
      indexColor=indexShape=indexPlot=NULL
      X=Y=NULL
      color=shape=vowel=plot=time=NULL
      ll=ul=NULL

      vowelTab <- reactive(
      {
        inFile <- input$vowelFile

        if (is.null(inFile))
          return(NULL)

        file.rename(inFile$datapath, paste0(inFile$datapath,".xlsx"))
        vT <- as.data.frame(read_excel(paste0(inFile$datapath,".xlsx"),1))

        ###

        indexDuration <- grep("^duration$", tolower(colnames(vT)))

        if (indexDuration > 3)
        {
          cnames <- colnames(vT)
          vT <- data.frame(vT[,1], vT[,3:(indexDuration-1)], vT[,2], vT[,indexDuration:ncol(vT)])
          cnames <- c(cnames[1],cnames[3:(indexDuration-1)],cnames[2],cnames[indexDuration:ncol(vT)])
          colnames(vT) <- cnames
        }
        else {}

        ###

        indexVowel <- grep("^vowel$", tolower(colnames(vT)))

        for (k in (1:(indexVowel+1)))
        {
          colnames(vT)[k] <- tolower(colnames(vT)[k])
        }

        k0 <- 0
        for (k in ((indexVowel+2):(ncol(vT))))
        {
          k0 <- k0 +1

          if (((k0 %% 5)==1) | ((k0 %% 5)==2))
          {
            colnames(vT)[k] <- tolower(colnames(vT)[k])
          }
          else
          {
            colnames(vT)[k] <- toupper(colnames(vT)[k])
          }
        }

        if (length(indexVowel)>0)
        {
          for (i in ((indexVowel+1):(ncol(vT))))
          {
            if (sum(is.na(vT[,i]))==nrow(vT))
            {
              vT[,i] <- 0
            }
          }

          vT <- vT[rowSums(is.na(vT)) == 0,]
        }

        return(vT)
      })

      Check <- reactive(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel   <- grep("^vowel$", colnames(vowelTab()))

        ok <- TRUE

        if (length(grep("^speaker$", colnames(vowelTab()), value = TRUE)) == 0)
        {
          ok <- FALSE
        }
        else

        if (sum(is.na(vowelTab()[,1]))==nrow(vowelTab()))
        {
          ok <- FALSE
        }
        else{}

        if (length(grep("^vowel$", colnames(vowelTab()), value = TRUE)) == 0)
        {
          ok <- FALSE
        }
        else

        if (sum(is.na(vowelTab()[,indexVowel]))==nrow(vowelTab()))
        {
          ok <- FALSE
        }
        else {}

        if (length(grep("^duration", colnames(vowelTab()), value = TRUE)) == 0)
        {
          ok <- FALSE
        }
        else {}

        if (length(grep("^time", colnames(vowelTab()), value = TRUE)) == 0)
        {
          ok <- FALSE
        }
        else {}

        if (length(grep("^f0", colnames(vowelTab()), value = TRUE)) == 0)
        {
          ok <- FALSE
        }
        else {}

        if (length(grep("^F1", colnames(vowelTab()), value = TRUE)) == 0)
        {
          ok <- FALSE
        }
        else

        if (sum(vowelTab()[,indexVowel+4]==0)==nrow(vowelTab()))
        {
          ok <- FALSE
        }
        else {}

        if (length(grep("^F2", colnames(vowelTab()), value = TRUE)) == 0)
        {
          ok <- FALSE
        }
        else

        if (sum(vowelTab()[,indexVowel+5]==0)==nrow(vowelTab()))
        {
          ok <- FALSE
        }
        else {}

        if (length(grep("^F3", colnames(vowelTab()), value = TRUE)) == 0)
        {
          ok <- FALSE
        }
        else {}

        if (ok)
        {
          return("")
        }
        else
        {
          return("Error: check table format! See Help.")
        }
      })

      output$checkFormat <- renderText(Check())

      Round <- function(x)
      {
        return(trunc(x+0.5))
      }

      vowelRound <- reactive(
      {
        if (is.null(vowelTab()))
          return(NULL)

        vT <- vowelTab()

        ###

        indexVowel <- grep("^vowel$", tolower(colnames(vT)))

        if (indexVowel > 2)
        {
          cnames <- colnames(vT)
          vT <- data.frame(vT[,1], vT[,indexVowel], vT[,2:(indexVowel-1)], vT[,(indexVowel+1):ncol(vT)])
          cnames <- c(cnames[1], cnames[indexVowel], cnames[2:(indexVowel-1)],cnames[(indexVowel+1):ncol(vT)])
          colnames(vT) <- cnames
        }
        else {}

        ###

        nColumns <- ncol(vT)

        for (i in (1:nColumns))
        {
          if (typeof(vT[,i])=="double")
          {
            if (max(vT[,i])<=1)
            {
              vT[,i] <- round(((Round(vT[,i]*1000))/1000),3)
            }
            else
            {
              vT[,i] <- Round(vT[,i])
            }
          }
        }

        return(vT)
      })

      output$vowelRound <- DT::renderDataTable(vowelRound(), options = list(scrollX = TRUE))

      #############################################################################

      Scale <- function(h,replyScale)
      {
        if (replyScale=="Hz")
        {
          s <- h
        }

        if (replyScale=="Bark")
        {
          s <- (26.81 * (h/(1960+h))) - 0.53
        }

        if (replyScale=="ERB")
        {
          s <- 21.4 * log10((0.00437*h)+1)
        }

        if (replyScale=="ln")
        {
          s <- log(h)
        }

        if (replyScale=="Mel")
        {
          s <-1127 * log(1 + (h/700))
        }

        if (replyScale=="ST")
        {
          s <- 12 * log2(h/50)
        }

        return(s)
      }

      vowelScale0 <- reactive(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))

        if (sum(vowelTab()[,indexVowel+3]==0)==nrow(vowelTab()))
          return(NULL)

        nColumns <- ncol(vowelTab())
        nPoints  <- (nColumns - (indexVowel + 1))/5

        vT <- vowelTab()

        for (i in (1:nPoints))
        {
          indexTime <- indexVowel + 2 + ((i-1)*5)

          for (j in ((indexTime+1):(indexTime+4)))
          {
            vT[,j] <- Scale(vT[,j],input$replyScale0)
          }
        }

        return(vT)
      })

      vowelSub0 <- reactive(
      {
        if (is.null(vowelScale0()) || (nrow(vowelScale0())==0) || (length(input$catXaxis0)==0))
          return(NULL)

        vT <- vowelScale0()

        vT$indexPlot <- fuseCols(vowelScale0(),input$replyPlot0)
        vT$indexLine <- fuseCols(vowelScale0(),input$replyLine0)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))

        nColumns   <- ncol(vowelTab())
        nPoints    <- (nColumns - (indexVowel + 1))/5

        xi <- as.numeric(input$catXaxis0)
        xn <- names(getTimeCode())[xi]

        if ((length(input$catPlot0)==0) && ((length(input$catLine0)==0) | (length(input$catLine0)>14)))
        {
          x <- c()
          y <- c()

          for (i in (1:nPoints))
          {
            if (is.element(i,xi))
            {
              ii <- which(xi==i)
              x <- as.numeric(as.character(c(x,rep(xn[ii],nrow(vT)))))

              indexTime <- indexVowel + 2 + ((i-1)*5)
              y <- c(y,vT[,indexTime+1])
            }
          }

          vT0 <- data.frame(x,y)
          ag <- stats::aggregate(y~x, data=vT0, FUN=mean)

          colnames(ag)[1] <- "time"
          colnames(ag)[2] <- "f0"

          return(ag)
        }
        else

        if ((length(input$catPlot0)>0) && ((length(input$catLine0)==0) | (length(input$catLine0)>14)))
        {
          vT <- subset(vT, is.element(vT$indexPlot,input$catPlot0))
          vT$indexPlot <- as.character(vT$indexPlot)

          if (nrow(vT)==0)
            return(NULL)

          x <- c()
          y <- c()
          p <- c()

          for (i in (1:nPoints))
          {
            if (is.element(i,xi))
            {
              ii <- which(xi==i)
              x <- as.numeric(as.character(c(x,rep(xn[ii],nrow(vT)))))

              indexTime <- indexVowel + 2 + ((i-1)*5)
              y <- c(y,vT[,indexTime+1])
              p <- c(p,vT$indexPlot)
            }
          }

          vT0 <- data.frame(x,p,y)
          ag <- stats::aggregate(y~x+p, data=vT0, FUN=mean)
          ag <- ag[order(ag[,2]),]

          colnames(ag)[1] <- "time"
          colnames(ag)[2] <- paste(input$replyPlot0, collapse = " ")
          colnames(ag)[3] <- "f0"

          return(ag)
        }
        else

        if ((length(input$catPlot0)==0) && ((length(input$catLine0)>0) & (length(input$catLine0)<=14)))
        {
          vT <- subset(vT, is.element(vT$indexLine,input$catLine0))
          vT$indexLine <- as.character(vT$indexLine)

          if (nrow(vT)==0)
            return(NULL)

          x <- c()
          y <- c()
          l <- c()

          for (i in (1:nPoints))
          {
            if (is.element(i,xi))
            {
              ii <- which(xi==i)
              x <- as.numeric(as.character(c(x,rep(xn[ii],nrow(vT)))))

              indexTime <- indexVowel + 2 + ((i-1)*5)
              y <- c(y,vT[,indexTime+1])
              l <- c(l,vT$indexLine)
            }
          }

          vT0 <- data.frame(x,l,y)
          ag <- stats::aggregate(y~x+l, data=vT0, FUN=mean)
          ag <- ag[order(ag[,2]),]

          colnames(ag)[1] <- "time"
          colnames(ag)[2] <- paste(input$replyLine0, collapse = " ")
          colnames(ag)[3] <- "f0"

          return(ag)
        }
        else

        if ((length(input$catPlot0)>0) && ((length(input$catLine0)>0) & (length(input$catLine0)<=14)))
        {
          vT <- subset(vT, is.element(vT$indexPlot,input$catPlot0) & is.element(vT$indexLine,input$catLine0))
          vT$indexPlot <- as.character(vT$indexPlot)
          vT$indexLine <- as.character(vT$indexLine)

          if (nrow(vT)==0)
            return(NULL)

          x <- c()
          y <- c()
          p <- c()
          l <- c()

          for (i in (1:nPoints))
          {
            if (is.element(i,xi))
            {
              ii <- which(xi==i)
              x <- as.numeric(as.character(c(x,rep(xn[ii],nrow(vT)))))

              indexTime <- indexVowel + 2 + ((i-1)*5)
              y <- c(y,vT[,indexTime+1])
              p <- c(p,vT$indexPlot)
              l <- c(l,vT$indexLine)
            }
          }

          vT0 <- data.frame(x,p,l,y)
          ag <- stats::aggregate(y~x+p+l, data=vT0, FUN=mean)
          ag <- ag[order(ag[,2]),]

          colnames(ag)[1] <- "time"
          colnames(ag)[2] <- paste(input$replyPlot0, collapse = " ")
          colnames(ag)[3] <- paste(input$replyLine0, collapse = " ")
          colnames(ag)[4] <- "f0"

          return(ag)
        }
        else

        {
          return(NULL)
        }
      })

      output$selScale0 <- renderUI(
      {
        options <- c("Hz","Bark","ERB","ln","Mel","ST")
        selectInput('replyScale0', 'Scale:', options, selected = options[1], selectize=FALSE, multiple=FALSE, width="100%")
      })

      getTimeCode <- reactive(
      {
        indexVowel   <- grep("^vowel$", colnames(vowelTab()))
        nColumns     <- ncol(vowelTab())
        nPoints      <- (nColumns - (indexVowel + 1))/5

        percentages <-FALSE

        if (sum(is.na(vowelTab()[,indexVowel + 1]))!=nrow(vowelTab()))
        {
          meanDuration <- mean(vowelTab()[,indexVowel+1])

          if (mean(vowelTab()[,indexVowel+2+((nPoints-1)*5)]) < meanDuration)
          {
            if (meanDuration==0)
            {
              meanDuration <- 0.000001
            }

            timeLabel <- c()
            timeCode  <- c()

            for (i in (1:nPoints))
            {
              indexTime <- indexVowel + 2 + ((i-1)*5)

              timeLabel[i] <- (mean(vowelTab()[,indexTime])/meanDuration) * 100
              timeCode [i] <- i

              names(timeCode) <- as.character(round(timeLabel))
            }

            percentages <-TRUE
          }
        }

        if (percentages==FALSE)
        {
          timeCode <- seq(from=1, to=nPoints, by=1)
          names(timeCode) <- as.character(timeCode)
        }

        return(timeCode)
      })

      fuseCols <- function(vT,replyValue)
      {
        columns <- ""

        if (length(replyValue)>0)
        {
          for (i in (1:length(replyValue)))
          {
            indexValue <- grep(paste0("^",as.character(replyValue)[i],"$"), colnames(vT))

            if (i==1)
              columns <- paste0(columns,vT[,indexValue])
            else
              columns <- paste (columns,vT[,indexValue])
          }
        }

        return(columns)
      }

      output$selXaxis0 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        options <- "time"
        selectInput('replyXaxis0', 'Variable x-axis:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$catXaxis0 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyXaxis0)>0)
        {
          timeCode   <- getTimeCode()
          indexVowel <- grep("^vowel$", colnames(vowelTab()))
          nColumns   <- ncol(vowelTab())
          nPoints    <- (nColumns - (indexVowel + 1))/5
        }
        else
          timeCode   <- NULL

        selectInput('catXaxis0', 'Select points:', timeCode, multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$selPlot0 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))

        selectInput('replyPlot0', 'Plot variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$catPlot0 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyPlot0)>0)
          options <- unique(fuseCols(vowelTab(),input$replyPlot0))
        else
          options <- NULL

        selectInput('catPlot0', 'Select plots:', options, multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$selLine0 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))

        selectInput('replyLine0', 'Color variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$catLine0 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyLine0)>0)
          options <- unique(fuseCols(vowelTab(),input$replyLine0))
        else
          options <- NULL

        selectInput('catLine0', 'Select colors:', options, multiple=TRUE, selectize=FALSE, width="100%")
      })

      plotGraph0 <- function()
      {
        if (is.null(vowelSub0()) || (nrow(vowelSub0())==0))
          return(NULL)

        if ((length(input$catPlot0)==0) && ((length(input$catLine0)==0) | (length(input$catLine0)>14)))
        {
          graphics::plot(ggplot(data=vowelSub0(), aes(x=vowelSub0()[,1], y=vowelSub0()[,2], group=1)) +
                 geom_line(colour="indianred2", size=1) + geom_point(colour="indianred2", size=3) +
                 ggtitle(input$title0) +
                 scale_x_continuous(breaks = unique(vowelSub0()[,1])) +
                 xlab("time percentage") + ylab(paste0("f0 (",input$replyScale0,")")) +
                 theme_bw() +
                 theme(plot.title     =element_text(size=22, face="bold", hjust = 0.5),
                       axis.text      =element_text(size=18),
                       axis.title     =element_text(size=18),
                       strip.text.x   =element_text(size=14),
                       aspect.ratio   =0.67))
        }
        else

        if ((length(input$catPlot0)>0) && ((length(input$catLine0)==0) | (length(input$catLine0)>14)))
        {
          graphics::plot(ggplot(data=vowelSub0(), aes(x=vowelSub0()[,1], y=vowelSub0()[,3], group=1)) +
                 geom_line(colour="indianred2", size=1) + geom_point(colour="indianred2", size=3) +
                 ggtitle(paste(input$title0,paste(input$replyPlot0, collapse = " "))) +
                 scale_x_continuous(breaks = unique(vowelSub0()[,1])) +
                 xlab("time percentage") + ylab(paste0("f0 (",input$replyScale0,")")) +
                 facet_wrap(~vowelSub0()[,2]) +
                 theme_bw() +
                 theme(plot.title     =element_text(size=22, face="bold", hjust = 0.5),
                       axis.text      =element_text(size=18),
                       axis.title     =element_text(size=18),
                       strip.text.x   =element_text(size=14),
                       aspect.ratio   =0.67))
        }
        else

        if ((length(input$catPlot0)==0) && ((length(input$catLine0)>0) & (length(input$catLine0)<=14)))
        {
          pd <- position_dodge(0.1)

          graphics::plot(ggplot(data=vowelSub0(), aes(x=vowelSub0()[,1], y=vowelSub0()[,3], group=vowelSub0()[,2], color=vowelSub0()[,2])) +
                 geom_line(size=1, position=pd) + geom_point(size=3, position=pd) +
                 ggtitle(input$title0) +
                 scale_x_continuous(breaks = unique(vowelSub0()[,1])) +
                 xlab("time percentage") + ylab(paste0("f0 (",input$replyScale0,")")) +
                 scale_colour_discrete(name=paste0(paste(input$replyLine0,collapse = " "),"\n")) +
                 theme_bw() +
                 theme(plot.title     =element_text(size=22, face="bold", hjust = 0.5),
                       axis.text      =element_text(size=18),
                       axis.title     =element_text(size=18),
                       legend.text    =element_text(size=18),
                       legend.title   =element_text(size=18),
                       legend.key.size=unit(1.5, 'lines'),
                       strip.text.x   =element_text(size=14),
                       aspect.ratio   =0.67))
        }
        else

        if ((length(input$catPlot0)>0) && ((length(input$catLine0)>0) & (length(input$catLine0)<=14)))
        {
          pd <- position_dodge(0.1)

          graphics::plot(ggplot(data=vowelSub0(), aes(x=vowelSub0()[,1], y=vowelSub0()[,4], group=vowelSub0()[,3], color=vowelSub0()[,3])) +
                 geom_line(size=1, position=pd) + geom_point(size=3, position=pd) +
                 ggtitle(paste(input$title0,paste(input$replyPlot0,collapse = " "))) +
                 scale_x_continuous(breaks = unique(vowelSub0()[,1])) +
                 xlab("time percentage") + ylab(paste0("f0 (",input$replyScale0,")")) +
                 scale_colour_discrete(name=paste0(paste(input$replyLine0, collapse = " "),"\n")) +
                 facet_wrap(~vowelSub0()[,2]) +
                 theme_bw() +
                 theme(plot.title     =element_text(size=22, face="bold", hjust = 0.5),
                       axis.text      =element_text(size=18),
                       axis.title     =element_text(size=18),
                       legend.text    =element_text(size=18),
                       legend.title   =element_text(size=18),
                       legend.key.size=unit(1.5, 'lines'),
                       strip.text.x   =element_text(size=14),
                       aspect.ratio   =0.67))
        }
        else {}
      }

      output$graph0 <- renderPlot(height = 550, width = 700,
      {
        if (length(input$catXaxis0)>0)
        {
          plotGraph0()
        }
      })

      output$selFormat0a <- renderUI(
      {
        options <- c("txt","xlsx")
        selectInput('replyFormat0a', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      fileName0a <- function()
      {
        return(paste0("f0Table.",input$replyFormat0a))
      }

      output$download0a <- downloadHandler(filename = fileName0a, content = function(file)
      {
        if (length(input$catXaxis0)>0)
          vT <- vowelSub0()
        else
          vT <- data.frame()

        if (input$replyFormat0a=="txt")
        {
          utils::write.table(vT, file, sep = "\t", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
        }
        else

          if (input$replyFormat0a=="xlsx")
          {
            WriteXLS(vT, file, SheetNames = "table", row.names=FALSE, col.names=TRUE, BoldHeaderRow = TRUE, na = "NA", FreezeRow = 1)
          }
        else {}
      })

      output$selFormat0b <- renderUI(
      {
        options <- c("JPG","PNG","SVG","EPS","PDF")
        selectInput('replyFormat0b', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      output$selSize0b <- renderUI(
      {
        options <- c("small","medium","large")
        selectInput('replySize0b', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      fileName0b <- function()
      {
        return(paste0("f0Plot.",input$replyFormat0b))
      }

      px2inch <- function(x)
      {
        return(x*0.010416666666819)
      }

      # Function ggSave developed by Z.Lin, zie: https://stackoverflow.com/questions/45731238/autocrop-faceted-plots-made-by-ggplot

      sumUnitNull <- function(x)
      {
        res <- 0
        for (i in 1:length(x))
        {
          if (!is.null(attr(x[i], "unit")))
            check.unit <- attr(x[i], "unit")
          else

            if (!is.null(attr(x[i][[1]], "unit")))
              check.unit <- attr(x[i][[1]], "unit")
            else
              check.unit <- NA

            if(!is.na(check.unit) && check.unit == "null")
              res <- res + as.numeric(x[i])
        }
        return(res)
      }

      ggSave <- function(filename, plot = last_plot(), device = NULL, path = NULL, scale = 1, max.dimension = 10, units = c("in", "cm", "mm"), dpi=300, limitsize = TRUE)
      {
        # get width/height information from the plot object (likely in a mixture of different units)
        w <- ggplotGrob(plot)$widths
        h <- ggplotGrob(plot)$heights

        # define maximum dimensions
        w.max <- as.numeric(convertUnit(unit(max.dimension, units),"in"))
        h.max <- as.numeric(convertUnit(unit(max.dimension, units),"in"))

        # sum the inflexible size components of the plot object's width/height, these components have unit = "in", "mm", "pt", "grobheight", etc
        w.in <- sum(as.numeric(convertUnit(w,"in")))
        h.in <- sum(as.numeric(convertUnit(h,"in")))

        # obtain the amount of space available for the flexible size components
        w.avail <- w.max - w.in
        h.avail <- h.max - h.in

        # sum the flexible sized components of the plot object's width/height, these components have unit = "null"
        w.f <- sumUnitNull(w)
        h.f <- sumUnitNull(h)

        # shrink the amount of avilable space based on what the flexible components would actually take up
        if (w.f/h.f > w.avail/h.avail)
          h.avail <- w.avail/w.f*h.f
        else
          w.avail <- h.avail/h.f*w.f

        w <- w.in + w.avail
        h <- h.in + h.avail

        ggsave(filename, plot = plot, device = device, path = path, scale = scale, width = w, height = h, units = units, dpi = dpi, limitsize = limitsize)
      }

      output$download0b <- downloadHandler(filename = fileName0b, content = function(file)
      {
        if (input$replySize0b=="small")
          scale <- 1.0
        if (input$replySize0b=="medium")
          scale <- 1.5
        if (input$replySize0b=="large")
          scale <- 2.0

        width0 <- px2inch(700)
        height0 <- px2inch(550)

        grDevices::pdf(NULL)

        if (length(input$catXaxis0)>0)
          print(plotGraph0())
        else
          print(ggplot()+theme_bw())

        if (input$replyFormat0b=="JPG")
          ggSave(file, scale=scale, device="jpeg", max.dimension = height0)
        else
          if (input$replyFormat0b=="PNG")
            ggSave(file, scale=scale, device="png", max.dimension = height0)
        else
          if (input$replyFormat0b=="SVG")
            ggSave(file, scale=scale, device="svg", max.dimension = height0)
        else
          if (input$replyFormat0b=="EPS")
            ggSave(file, scale=scale, device=grDevices::cairo_ps , max.dimension = height0)
        else
          if (input$replyFormat0b=="PDF")
            ggSave(file, scale=scale, device=grDevices::cairo_pdf, max.dimension = height0)
        else {}

        grDevices::dev.off()
      })

      #############################################################################

      vowelScale1 <- reactive(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel   <- grep("^vowel$", colnames(vowelTab()))
        nColumns     <- ncol(vowelTab())
        nPoints      <- (nColumns - (indexVowel + 1))/5

        vT <- vowelTab()

        for (i in (1:nPoints))
        {
          indexTime <- indexVowel + 2 + ((i-1)*5)

          for (j in ((indexTime+1):(indexTime+4)))
          {
            vT[,j] <- Scale(vT[,j],input$replyScale1)
          }
        }

        return(vT)
      })

      vowelLong1 <- reactive(
      {
        validate(
          need(length(input$replyTimesN)>0, "")
        )

        indexVowel   <- grep("^vowel$", colnames(vowelScale1()))
        nColumns     <- ncol(vowelScale1())
        nPoints      <- (nColumns - (indexVowel + 1))/5

        vT <- data.frame()

        for (i in (1:nPoints))
        {
          if (is.element(as.character(i),input$replyTimesN))
          {
            indexTime <- indexVowel + 2 + ((i-1)*5)

            vTsub <- data.frame(speaker = vowelScale1()[,1],
                                vowel   = vowelScale1()[,indexVowel],
                                point   = i,
                                f0      = vowelScale1()[,indexTime+1],
                                f1      = vowelScale1()[,indexTime+2],
                                f2      = vowelScale1()[,indexTime+3],
                                f3      = vowelScale1()[,indexTime+4])

            vT <- rbind(vT,vTsub)
          }
        }

        return(vT)
      })

      vowelLong2 <- reactive(
      {
        vT <- data.frame()

        for (j in (1:2))
        {
          vTsub <- data.frame(speaker = vowelLong1()$speaker,
                              vowel   = vowelLong1()$vowel,
                              point   = vowelLong1()$point,
                              formant = j,
                              f       = log(vowelLong1()[,j+4]))

          vT <- rbind(vT,vTsub)
        }

        return(vT)
      })

      vowelLong3 <- reactive(
      {
        vT <- data.frame()

        for (j in (1:3))
        {
          vTsub <- data.frame(speaker = vowelLong1()$speaker,
                              vowel   = vowelLong1()$vowel,
                              point   = vowelLong1()$point,
                              formant = j,
                              f       = log(vowelLong1()[,j+4]))

          vT <- rbind(vT,vTsub)
        }

        return(vT)
      })

      vowelLong4 <- reactive(
      {
        vT <- data.frame()

        for (j in (0:3))
        {
          vTsub <- data.frame(speaker = vowelLong1()$speaker,
                              vowel   = vowelLong1()$vowel,
                              point   = vowelLong1()$point,
                              formant = j,
                              f       = log(vowelLong1()[,j+4]))

          vT <- rbind(vT,vTsub)
        }

        return(vT)
      })

      vowelNorm1 <- reactive(
      {
        if (is.null(vowelScale1()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelScale1()))
        nColumns   <- ncol(vowelScale1())
        nPoints    <- (nColumns - (indexVowel + 1))/5

         SpeaKer   <- unique(vowelScale1()[,1])
        nSpeaKer   <- length(SpeaKer)

        if (input$replyNormal1=="None")
        {
          vT <- vowelScale1()
        }

        if (input$replyNormal1=="Gerstman's range normalization")
        {
          vT <- data.frame()

          for (q in (1:nSpeaKer))
          {
            vTLong1Sub <- subset(vowelLong1(), speaker==SpeaKer[q])

            minF <- rep(0,4)
            maxF <- rep(0,4)

            for (j in 0:3)
            {
              minF[j+1] <- min(vTLong1Sub[,j+4])
              maxF[j+1] <- max(vTLong1Sub[,j+4])
            }

            vTsub <- subset(vowelScale1(), vowelScale1()[,1]==SpeaKer[q])

            for (i in (1:nPoints))
            {
              indexTime <- indexVowel + 2 + ((i-1)*5)

              for (j in (0:3))
              {
                vTsub[,j+indexTime+1] <- 999 * ((vTsub[,j+indexTime+1]-minF[j+1]) / (maxF[j+1]-minF[j+1]))
              }
            }

            vT <- rbind(vT,vTsub)
          }
        }

        if (input$replyNormal1=="Lobanov's z-score transformation")
        {
          vT <- data.frame()
          vTLong1Ag <- stats::aggregate(cbind(f0,f1,f2,f3)~speaker+vowel+point, data=vowelLong1(), FUN=mean)

          for (q in (1:nSpeaKer))
          {
            vTLong1AgSub <- subset(vTLong1Ag, speaker==SpeaKer[q])

            meanF <- rep(0, 4)
            sdF <- rep(0, 4)

            for (j in 0:3)
            {
              meanF[j+1] <- mean(vTLong1AgSub[,j+4])
              sdF[j+1] <-   stats::sd(vTLong1AgSub[,j+4])
            }

            vTsub <- subset(vowelScale1(), vowelScale1()[,1]==SpeaKer[q])

            for (i in (1:nPoints))
            {
              indexTime <- indexVowel + 2 + ((i-1)*5)

              for (j in (0:3))
              {
                vTsub[,j+indexTime+1] <- (vTsub[,j+indexTime+1]-meanF[j+1])/sdF[j+1]
              }
            }

            vT <- rbind(vT,vTsub)
          }
        }

        if (input$replyNormal1=="Labov's ANAE method 1")
        {
          vT <- data.frame()

          vTLong2Ag <- stats::aggregate(f~speaker+vowel+point+formant, data=vowelLong2(), FUN=psych::geometric.mean)
          grandMean <- psych::geometric.mean(vTLong2Ag$f)

          for (q in (1:nSpeaKer))
          {
            vTLong2AgSub  <- subset(vTLong2Ag, speaker==SpeaKer[q])
            speakerMean   <- psych::geometric.mean(vTLong2AgSub$f)
            speakerFactor <- exp(grandMean - speakerMean)

            vTsub <- subset(vowelScale1(), vowelScale1()[,1]==SpeaKer[q])

            for (i in (1:nPoints))
            {
              indexTime <- indexVowel + 2 + ((i-1)*5)

              for (j in (1:2))
              {
                vTsub[,j+indexTime+1] <- speakerFactor * vTsub[,j+indexTime+1]
              }
            }

            vT <- rbind(vT,vTsub)
          }
        }

        if (input$replyNormal1=="Labov's ANAE method 2")
        {
          vT <- data.frame()

          vTLong3Ag <- stats::aggregate(f~speaker+vowel+point+formant, data=vowelLong3(), FUN=psych::geometric.mean)
          grandMean <- psych::geometric.mean(vTLong3Ag$f)

          for (q in (1:nSpeaKer))
          {
            vTLong3AgSub  <- subset(vTLong3Ag, speaker==SpeaKer[q])
            speakerMean   <- psych::geometric.mean(vTLong3AgSub$f)
            speakerFactor <- exp(grandMean - speakerMean)

            vTsub <- subset(vowelScale1(), vowelScale1()[,1]==SpeaKer[q])

            for (i in (1:nPoints))
            {
              indexTime <- indexVowel + 2 + ((i-1)*5)

              for (j in (1:3))
              {
                vTsub[,j+indexTime+1] <- speakerFactor * vTsub[,j+indexTime+1]
              }
            }

            vT <- rbind(vT,vTsub)
          }
        }

        if (input$replyNormal1=="Miller's formant ratio normalization")
        {
          vT <- data.frame()
          vTLong1Ag <- stats::aggregate(f0~speaker+vowel+point, data=vowelLong1(), FUN=psych::geometric.mean)

          for (q in (1:nSpeaKer))
          {
            vTLong1AgSub <- subset(vTLong1Ag, speaker==SpeaKer[q])

            GMf0 <- psych::geometric.mean(vTLong1AgSub$f0)
            SR   <- 168*((GMf0/168)^(1/3))

            vTsub <- subset(vowelScale1(), vowelScale1()[,1]==SpeaKer[q])

            for (i in (1:nPoints))
            {
              indexTime <- indexVowel + 2 + ((i-1)*5)

              vTsub[,indexTime+4] <- log(vTsub[,indexTime+4] / vTsub[,indexTime+3])
              vTsub[,indexTime+3] <- log(vTsub[,indexTime+3] / vTsub[,indexTime+2])
              vTsub[,indexTime+2] <- log(vTsub[,indexTime+2] / SR)
            }

            vT <- rbind(vT,vTsub)
          }
        }

        if (input$replyNormal1=="Nearey's log-mean model 1")
        {
          vT <- data.frame()
          vTLong4Ag <- stats::aggregate(f~speaker+vowel+point+formant, data=vowelLong4(), FUN=mean)

          for (q in (1:nSpeaKer))
          {
            vTLong4AgSub  <- subset(vTLong4Ag, speaker==SpeaKer[q])
            speakerMean   <- stats::aggregate(f~formant, data=vTLong4AgSub, FUN=mean)

            vTsub <- subset(vowelScale1(), vowelScale1()[,1]==SpeaKer[q])

            for (i in (1:nPoints))
            {
              indexTime <- indexVowel + 2 + ((i-1)*5)

              for (j in (0:3))
              {
                vTsub[,j+indexTime+1] <- log(vTsub[,j+indexTime+1]) - speakerMean$f[j+1]
              }
            }

            vT <- rbind(vT,vTsub)
          }
        }

        if (input$replyNormal1=="Nearey's log-mean model 2")
        {
          vT <- data.frame()
          vTLong4Ag <- stats::aggregate(f~speaker+vowel+point+formant, data=vowelLong4(), FUN=mean)

          for (q in (1:nSpeaKer))
          {
            vTLong4AgSub  <- subset(vTLong4Ag, speaker==SpeaKer[q])
            speakerMean   <- mean(vTLong4AgSub$f)

            vTsub <- subset(vowelScale1(), vowelScale1()[,1]==SpeaKer[q])

            for (i in (1:nPoints))
            {
              indexTime <- indexVowel + 2 + ((i-1)*5)

              for (j in (0:3))
              {
                vTsub[,j+indexTime+1] <- log(vTsub[,j+indexTime+1]) - speakerMean
              }
            }

            vT <- rbind(vT,vTsub)
          }
        }

        if (input$replyNormal1=="Syrdal & Gopal's distance transformation")
        {
          vT <- vowelScale1()

          for (i in (1:nPoints))
          {
            indexTime <- indexVowel + 2 + ((i-1)*5)

            vT[,indexTime+2] <-  1 * (vT[,indexTime+2] - vT[,indexTime+1])
            vT[,indexTime+3] <- -1 * (vT[,indexTime+4] - vT[,indexTime+3])
          }
        }

        if (input$replyNormal1=="Tyler & Kendall's difference metric")
        {
          vT <- vowelScale1()

          for (i in (1:nPoints))
          {
            indexTime <- indexVowel + 2 + ((i-1)*5)

            vT[,indexTime+2] <- -1 * (vT[,indexTime+4] - vT[,indexTime+2])
            vT[,indexTime+3] <- -1 * (vT[,indexTime+4] - vT[,indexTime+3])
          }
        }

        if (input$replyNormal1=="Watt & Fabricius' S-centroid procedure 1")
        {
          vT <- data.frame()
          vTLong1Ag <- stats::aggregate(cbind(f0,f1,f2,f3)~speaker+vowel+point, data=vowelLong1(), FUN=mean)

          for (q in (1:nSpeaKer))
          {
            vTLong1AgSub <- subset(vTLong1Ag, speaker==SpeaKer[q])

            vowelF1 <- stats::aggregate(f1~vowel, data=vTLong1AgSub, FUN=mean)
            vowelF2 <- stats::aggregate(f2~vowel, data=vTLong1AgSub, FUN=mean)

            iF1 <- min(vowelF1$f1)
            iF2 <- max(vowelF2$f2)

            uF1 <- min(vowelF1$f1)
            uF2 <- min(vowelF2$f2)

            aF1 <- max(vowelF1$f1)
            aF2 <- vowelF2$f2[which(vowelF1$f1 == aF1)]

            centroidF1 <- (iF1 + uF1 + aF1)/3
            centroidF2 <- (iF2 + uF2 + aF2)/3

            vTsub <- subset(vowelScale1(), vowelScale1()[,1]==SpeaKer[q])

            for (i in (1:nPoints))
            {
              indexTime <- indexVowel + 2 + ((i-1)*5)

              vTsub[,indexTime+2] <- vTsub[,indexTime+2] / centroidF1
              vTsub[,indexTime+3] <- vTsub[,indexTime+3] / centroidF2
            }

            vT <- rbind(vT,vTsub)
          }
        }

        if (input$replyNormal1=="Watt & Fabricius' S-centroid procedure 2")
        {
          vT <- data.frame()
          vTLong1Ag <- stats::aggregate(cbind(f0,f1,f2,f3)~speaker+vowel+point, data=vowelLong1(), FUN=mean)

          for (q in (1:nSpeaKer))
          {
            vTLong1AgSub <- subset(vTLong1Ag, speaker==SpeaKer[q])

            vowelF1 <- stats::aggregate(f1~vowel, data=vTLong1AgSub, FUN=mean)
            vowelF2 <- stats::aggregate(f2~vowel, data=vTLong1AgSub, FUN=mean)

            iF1 <- min(vowelF1$f1)
            iF2 <- max(vowelF2$f2)

            uF1 <- min(vowelF1$f1)
            uF2 <- min(vowelF2$f2)

            aF1 <- max(vowelF1$f1)

            centroidF1 <- (iF1 + uF1 + aF1)/3
            centroidF2 <- (iF2 + uF2      )/2

            vTsub <- subset(vowelScale1(), vowelScale1()[,1]==SpeaKer[q])

            for (i in (1:nPoints))
            {
              indexTime <- indexVowel + 2 + ((i-1)*5)

              vTsub[,indexTime+2] <- vTsub[,indexTime+2] / centroidF1
              vTsub[,indexTime+3] <- vTsub[,indexTime+3] / centroidF2
            }

            vT <- rbind(vT,vTsub)
          }
        }

        if (input$replyNormal1=="Convex hull centroid procedure")
        {
          vT <- data.frame()
          vTLong1Ag <- stats::aggregate(cbind(f0,f1,f2,f3)~speaker+vowel+point, data=vowelLong1(), FUN=mean)

          for (q in (1:nSpeaKer))
          {
            vTLong1AgSub <- subset(vTLong1Ag, speaker==SpeaKer[q])

            vowelF1 <- stats::aggregate(f1~vowel, data=vTLong1AgSub, FUN=mean)
            vowelF2 <- stats::aggregate(f2~vowel, data=vTLong1AgSub, FUN=mean)

            k <- grDevices::chull(vowelF1$f1,vowelF2$f2)

            xx <- vowelF1$f1[k]
            xx[length(xx)+1] <- xx[1]
            yy <- vowelF2$f2[k]
            yy[length(yy)+1] <- yy[1]

            pc <- poly_center(xx,yy)

            vTsub <- subset(vowelScale1(), vowelScale1()[,1]==SpeaKer[q])

            for (i in (1:nPoints))
            {
              indexTime <- indexVowel + 2 + ((i-1)*5)

              vTsub[,indexTime+2] <- vTsub[,indexTime+2] / pc[1]
              vTsub[,indexTime+3] <- vTsub[,indexTime+3] / pc[2]
            }

            vT <- rbind(vT,vTsub)
          }
        }

        return(vT)
      })

      vowelSub1 <- reactive(
      {
        if ((is.null(vowelNorm1())) | (nrow(vowelNorm1())==0))
          return(NULL)

        vT <- vowelNorm1()

        vT$indexColor <- fuseCols(vowelNorm1(),input$replyColor)
        vT$indexShape <- fuseCols(vowelNorm1(),input$replyShape)
        vT$indexPlot  <- fuseCols(vowelNorm1(),input$replyPlot1)

        indexVowel <- grep("^vowel$", colnames(vowelNorm1()))

        ### check begin

        nPoints <- (ncol(vT) - (indexVowel + 1))/5

        if (length(input$replyTimes)>nPoints)
        {
          return(NULL)
        }
        else

        if (length(vT$indexColor)==0)
        {
          return(NULL)
        }
        else

        if (length(input$replyTimes)>1)
        {
        }
        else

        if (input$axisZ!="--")
        {
        }
        else

        if (length(vT$indexShape)==0)
        {
          return(NULL)
        }
        else

        if (length(vT$indexPlot)==0)
        {
          return(NULL)
        }
        else {}

        ### check end

        if (length(input$catColor)>0)
        {
          vT1 <- data.frame()

          for (q in (1:length(input$catColor)))
          {
            vT1 <- rbind(vT1, subset(vT, indexColor==input$catColor[q]))
          }
        }
        else
        {
          vT1 <- vT
        }

        if (length(input$catShape)>0)
        {
          vT2 <- data.frame()

          for (q in (1:length(input$catShape)))
          {
            vT2 <- rbind(vT2, subset(vT1, indexShape==input$catShape[q]))
          }
        }
        else
        {
          vT2 <- vT1
        }

        if (length(input$catPlot1)>0)
        {
          vT3 <- data.frame()

          for (q in (1:length(input$catPlot1)))
          {
            vT3 <- rbind(vT3, subset(vT2, indexPlot==input$catPlot1[q]))
          }
        }
        else
        {
          vT3 <- vT2
        }

        vT <- vT3

        ###

        if (nrow(vT)>0)
        {
          vT0 <- data.frame()

          for (i in (1:length(input$replyTimes)))
          {
            Code <- strtoi(input$replyTimes[i])

            indexF1 <- indexVowel + 4 + ((Code-1) * 5)
            indexF2 <- indexVowel + 5 + ((Code-1) * 5)
            indexF3 <- indexVowel + 6 + ((Code-1) * 5)

            if (length(input$catColor)>0)
              Color <- vT$indexColor
            else
              Color <- rep("none",nrow(vT))

            if (length(input$catShape)>0)
              Shape <- vT$indexShape
            else
              Shape <- rep("none",nrow(vT))

            if (length(input$catPlot1)>0)
              Plot  <- vT$indexPlot
            else
              Plot  <- rep("none",nrow(vT))

            if (input$axisX=="F1")
              Xaxis <- vT[,indexF1]
            if (input$axisX=="F2")
              Xaxis <- vT[,indexF2]
            if (input$axisX=="F3")
              Xaxis <- vT[,indexF3]

            if (input$axisY=="F1")
              Yaxis <- vT[,indexF1]
            if (input$axisY=="F2")
              Yaxis <- vT[,indexF2]
            if (input$axisY=="F3")
              Yaxis <- vT[,indexF3]

            if (input$axisZ=="--")
              Zaxis <- 0
            if (input$axisZ=="F1")
              Zaxis <- vT[,indexF1]
            if (input$axisZ=="F2")
              Zaxis <- vT[,indexF2]
            if (input$axisZ=="F3")
              Zaxis <- vT[,indexF3]

            vT0 <- rbind(vT0, data.frame(vowel = vT$vowel    ,
                                         color = Color       ,
                                         shape = Shape       ,
                                         plot  = Plot        ,
                                         index = rownames(vT),
                                         time  = i           ,
                                         X     = Xaxis,
                                         Y     = Yaxis,
                                         Z     = Zaxis))
          }

          vT <- vT0
        }
        else {}

        ###

        if ((nrow(vT)>0) & (input$average==TRUE))
        {
          vT <- stats::aggregate(cbind(X,Y,Z)~vowel+color+shape+plot+time, data=vT, FUN=mean)

          no <- nrow(stats::aggregate(cbind(X,Y,Z)~vowel+color+shape+plot, data=vT, FUN=mean))
          index <- seq(1:no)
          vT$index <- rep(index,length(input$replyTimes))
        }

        ###

        if (nrow(vT)>0)
        {
          vT$vowel <- factor(vT$vowel)
          vT$color <- factor(vT$color)
          vT$shape <- factor(vT$shape)
          vT$plot  <- factor(vT$plot)
          vT$time  <- factor(vT$time)
          vT$index <- factor(vT$index)

          #     utils::write.table(vT, "vT.csv", sep = "\t", row.names = FALSE)
          return(vT)
        }
        else
        {
          return(NULL)
        }
      })

      output$selTimes <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        timeCode     <- getTimeCode()
        indexVowel   <- grep("^vowel$", colnames(vowelTab()))
        nColumns     <- ncol(vowelTab())
        nPoints      <- (nColumns - (indexVowel + 1))/5

        checkboxGroupInput('replyTimes', 'Time points to be shown:', timeCode, selected = Round(nPoints/2), TRUE)
      })

      output$selScale1 <- renderUI(
      {
        if ((length(input$replyNormal1)>0) && ((input$replyNormal1=="Labov's ANAE method 1") |
                                               (input$replyNormal1=="Labov's ANAE method 2") |
                                               (input$replyNormal1=="Miller's formant ratio normalization") |
                                               (input$replyNormal1=="Nearey's log-mean model 1") |
                                               (input$replyNormal1=="Nearey's log-mean model 2")))
        {
          options <- c("Hz")
        }
        else
        {
          options <- c("Hz","Bark","ERB","ln","Mel")
        }

        selectInput('replyScale1', 'Scale:', options, selected = options[1], selectize=FALSE, multiple=FALSE)
      })

      output$selNormal1 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel   <- grep("^vowel$", colnames(vowelTab()))

        options <- c("None",
                     "Gerstman's range normalization",
                     "Lobanov's z-score transformation",
                     "Labov's ANAE method 1")

        if  (sum(vowelTab()[,indexVowel+6]==0)!=nrow(vowelTab()))
        {
          options <- c(options,
                       "Labov's ANAE method 2")
        }

        if ((sum(vowelTab()[,indexVowel+3]==0)!=nrow(vowelTab())) &
            (sum(vowelTab()[,indexVowel+6]==0)!=nrow(vowelTab())))
        {
          options <- c(options,
                       "Miller's formant ratio normalization",
                       "Nearey's log-mean model 1",
                       "Nearey's log-mean model 2",
                       "Syrdal & Gopal's distance transformation")
        }

        if  (sum(vowelTab()[,indexVowel+6]==0)!=nrow(vowelTab()))
        {
          options <- c(options,
                       "Tyler & Kendall's difference metric")
        }

        options <- c(options,
                     "Watt & Fabricius' S-centroid procedure 1",
                     "Watt & Fabricius' S-centroid procedure 2",
                     "Convex hull centroid procedure")

        selectInput('replyNormal1', 'Normalization method:', options, selected = options[1], selectize=FALSE, multiple=FALSE)
      })

      output$selTimesN <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if ((length(input$replyNormal1)>0) && ((input$replyNormal1=="None") |
                                               (input$replyNormal1=="Syrdal & Gopal's distance transformation") |
                                               (input$replyNormal1=="Tyler & Kendall's difference metric")))
          return(NULL)

        timeCode     <- getTimeCode()
        indexVowel   <- grep("^vowel$", colnames(vowelTab()))
        nColumns     <- ncol(vowelTab())
        nPoints      <- (nColumns - (indexVowel + 1))/5

        checkboxGroupInput('replyTimesN', 'Normalization based on:', timeCode, selected = Round(nPoints/2), TRUE)
      })

      output$manScale <- renderUI(
      {
        if (input$axisZ=="--")
        {
          checkboxInput("selManual", "Manual scaling", FALSE)
        }
      })

      output$selF1min <- renderUI(
      {
        if ((length(input$selManual)>0) && (input$selManual==TRUE) && (input$axisZ=="--"))
        {
          numericInput('replyXmin', 'min. x', value=NULL, step=10, width = "92%")
        }
      })

      output$selF1max <- renderUI(
      {
        if ((length(input$selManual)>0) && (input$selManual==TRUE) && (input$axisZ=="--"))
        {
          numericInput('replyXmax', 'max. x', value=NULL, step=10, width = "92%")
        }
      })

      output$selF2min <- renderUI(
      {
        if ((length(input$selManual)>0) && (input$selManual==TRUE) && (input$axisZ=="--"))
        {
          numericInput('replyYmin', 'min. y', value=NULL, step=10, width = "92%")
        }
      })

      output$selF2max <- renderUI(
      {
        if ((length(input$selManual)>0) && (input$selManual==TRUE) && (input$axisZ=="--"))
        {
          numericInput('replyYmax', 'max. y', value=NULL, step=10, width = "92%")
        }
      })

      output$selColor <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))

        selectInput('replyColor', 'Color variable:', options, selected=options[1], multiple=TRUE, selectize=FALSE, size=3, width="100%")
      })

      output$catColor <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyColor)>0)
          options <- unique(fuseCols(vowelTab(),input$replyColor))
        else
          options <- NULL

        selectInput('catColor', 'Select colors:', options, multiple=TRUE, selectize = FALSE, size=3, width="100%")
      })

      output$selShape <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if ((length(input$replyTimes)==1) && (input$axisZ=="--"))
        {
          indexVowel <- grep("^vowel$", colnames(vowelTab()))

          if (input$geon1 | input$geon2 | input$geon3 | input$geon4 | input$geon5)
            options <- "vowel"
          else
            options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))
        }
        else
        {
          options <- "none"
        }

        selectInput('replyShape', 'Shape variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, size=3, width="100%")
      })

      output$catShape <- renderUI(
      {
        if  (is.null(vowelTab()))
          return(NULL)

        if ((length(input$replyShape)>0) && (length(input$replyTimes)==1) && (input$axisZ=="--"))
        {
          if (input$geon1 | input$geon2 | input$geon3 | input$geon4 | input$geon5)
            options <- NULL
          else
            options <- unique(fuseCols(vowelTab(),input$replyShape))
        }
        else
          options <- NULL

        selectInput('catShape', 'Select shapes:', options, multiple=TRUE, selectize = FALSE, size=3, width="100%")
      })

      output$selPlot1 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (input$axisZ=="--")
        {
          indexVowel <- grep("^vowel$", colnames(vowelTab()))
          options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))
        }
        else
        {
          options <- "none"
        }

        selectInput('replyPlot1', 'Plot variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, size=3, width="100%")
      })

      output$catPlot1 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if ((length(input$replyPlot1)>0) && (input$axisZ=="--"))
          options <- unique(fuseCols(vowelTab(),input$replyPlot1))
        else
          options <- NULL

        selectInput('catPlot1', 'Select plots:', options, multiple=TRUE, selectize = FALSE, size=3, width="100%")
      })

      output$selGeon <- renderUI(
      {
        if ((is.null(vowelTab())) || (length(input$replyTimes)>1))
          return(NULL)

        if (input$axisZ=="--")
          tagList(splitLayout
                  (
                    cellWidths = c("19%", "17%", "15%", "21%", "19%"),

                    checkboxInput("geon1", "labels" , value = FALSE),
                    checkboxInput("geon2", "cent."  , value = FALSE),
                    checkboxInput("geon3", "hull"   , value = FALSE),
                    checkboxInput("geon4", "spokes" , value = FALSE),
                    checkboxInput("geon5", "ellipse", value = FALSE)
                  ))
        else
          tagList(splitLayout
                  (
                    cellWidths = c("19%", "19%", "19%"),

                    checkboxInput("geon1", "labels", value = FALSE),
                    checkboxInput("geon2", "lines" , value = TRUE )
                  ))
      })

      output$selPars <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if ((input$axisZ=="--") && (length(input$replyTimes)==1) && (length(input$geon5)>0) && input$geon5)
          numericInput('replyLevel', 'Confidence level:', value=0.95, step=0.01, width = "100%")
        else

          if  (input$axisZ!="--")
          {
            tagList(splitLayout
                    (
                      cellWidths = c("50%", "50%"),
                      numericInput('replyPhi'  , 'Angle x-axis:', value=40, step=1, width = "100%"),
                      numericInput('replyTheta', 'Angle z-axis:', value=40, step=1, width = "100%")
                    ))
          }
        else
          return(NULL)
      })

      numColor <- function()
      {
        if ((length(input$replyColor)>0) && (length(input$catColor)>0))
          return(length(input$catColor))
        else
          return(0)
      }

      numShape <- function()
      {
        if ((length(input$replyShape)>0) && (length(input$catShape)>0))
          return(length(input$catShape))
        else
          return(0)
      }

      numAll <- function()
      {
        return(numColor()+numShape())
      }

      colPalette <- function(n)
      {
        if (input$grayscale==FALSE)
        {
          labColors  <- c("#c87e66","#b58437","#988a00","#709000","#27942e","#00965c","#009482","#008ea3","#0081bd","#386acc","#8d46c8","#b315b1","#bd0088","#b61a51")
          labPalette <- grDevices::colorRampPalette(labColors, space = "Lab")

          if (n==1)
            return(labColors[c(10)])

          if (n==2)
            return(labColors[c(8,14)])

          if (n==3)
            return(labColors[c(5,10,13)])

          if (n==4)
            return(labColors[c(3,7,11,14)])

          if (n==5)
            return(labColors[c(3,5,9,11,14)])

          if (n==6)
            return(labColors[c(2,5,7,10,12,14)])

          if (n==7)
            return(labColors[c(2,4,5,8,10,12,14)])

          if (n==8)
            return(labColors[c(1,3,5,7,9,11,12,14)])

          if (n==9)
            return(labColors[c(1,3,4,6,8,10,11,12,14)])

          if (n==10)
            return(labColors[c(1,3,4,5,7,9,10,11,12,14)])

          if (n==11)
            return(labColors[c(1,2,4,5,6,7,9,10,11,12,14)])

          if (n==12)
            return(labColors[c(1,2,3,4,5,7,8,9,11,12,13,14)])

          if (n==13)
            return(labColors[c(1,2,3,4,5,6,7,8,10,11,12,13,14)])

          if (n==14)
            return(labColors[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)])

          if (n>=15)
            return(labPalette(n))
        }
        else
        {
          return(grDevices::gray(0:(n-1)/n))
        }
      }

      shpPalette <- function()
      {
        return(c(19,1,17,2,15,0,18,5,3,4,8))
      }

      normLab <- function()
      {
        if (input$replyNormal1=="none")
          return("")

        if (input$replyNormal1=="Gerstman's range normalization")
          return("Gerstman")

        if (input$replyNormal1=="Lobanov's z-score transformation")
          return("Lobanov")

        if (input$replyNormal1=="Labov's ANAE method 1")
          return("Labov 1")

        if (input$replyNormal1=="Labov's ANAE method 2")
          return("Labov 2")

        if (input$replyNormal1=="Miller's formant ratio normalization")
          return("Miller")

        if (input$replyNormal1=="Nearey's log-mean model 1")
          return("Nearey 1")

        if (input$replyNormal1=="Nearey's log-mean model 2")
          return("Nearey 2")

        if (input$replyNormal1=="Syrdal & Gopal's distance transformation")
          return("Syrdal & Gopal")

        if (input$replyNormal1=="Tyler & Kendall's difference metric")
          return("Tyler & Kendall")

        if (input$replyNormal1=="Watt & Fabricius' S-centroid procedure 1")
          return("Watt & Fabricius 1")

        if (input$replyNormal1=="Watt & Fabricius' S-centroid procedure 2")
          return("Watt & Fabricius 2")

        if (input$replyNormal1=="Convex hull centroid procedure")
          return("CHC")
      }

      plotGraph1 <- function()
      {
        if (is.null(vowelSub1()) || (nrow(vowelSub1())==0) | (length(input$replyTimes)==0))
          return(NULL)

        if ((length(input$replyTimes)==1) && (!(input$geon1 | input$geon2 | input$geon3 | input$geon4 | input$geon5)) && (input$axisZ=="--"))
        {
          vT <- vowelSub1()

          if ((numColor()>0) & (numShape()>0) & (numShape()<=11))
            Basis <- ggplot(data=vT, aes(x=X, y=Y, color=color, shape=shape)) +
              geom_point(size=2.5) +
              scale_shape_manual(values=shpPalette())
          else
          if  (numColor()>0)
            Basis <- ggplot(data=vT, aes(x=X, y=Y, color=color)) +
              geom_point(size=2.5)
          else
          if ((numShape()>0) & (numShape()<=11))
            Basis <- ggplot(data=vT, aes(x=X, y=Y, shape=shape)) +
              geom_point(size=2.5, colour=colPalette(1)) +
              scale_shape_manual(values=shpPalette())
          else
            Basis <- ggplot(data=vT, aes(x=X, y=Y, color=color)) +
              geom_point(size=2.5)

          if ((length(input$selManual)>0) && (input$selManual==TRUE))
          {
            scaleX <- scale_x_reverse(name=paste0(input$axisX," (",input$replyScale1," ",normLab(),")"), position="top"  , limits = c(input$replyXmax, input$replyXmin))
            scaleY <- scale_y_reverse(name=paste0(input$axisY," (",input$replyScale1," ",normLab(),")"), position="right", limits = c(input$replyYmax, input$replyYmin))
          }
          else
          {
            scaleX <- scale_x_reverse(name=paste0(input$axisX," (",input$replyScale1," ",normLab(),")"), position="top"  )
            scaleY <- scale_y_reverse(name=paste0(input$axisY," (",input$replyScale1," ",normLab(),")"), position="right")
          }

          if (length(input$catPlot1)>0)
          {
            Title <- ggtitle(paste(input$title1,paste(input$replyPlot1, collapse = " ")))
            Facet <- facet_wrap(~plot)
          }
          else
          {
            Title <- ggtitle(input$title1)
            Facet <- facet_null()
          }

          if ((numAll()>0) & (numAll()<=18))
            Legend <- theme(legend.position="right")
          else
          if ((numColor()>0) & (numColor()<=18))
            Legend <- guides(shape=FALSE)
          else
          if ((numShape()>0) & (numShape()<=11))
            Legend <- guides(color=FALSE)
          else
            Legend <- theme(legend.position="none")

          graphics::plot(Basis + scaleX + scaleY + Title + Facet +
                 scale_color_manual(values=colPalette(length(unique(vT$color)))) +
                 labs(colour=paste(input$replyColor, collapse = " "),shape=paste(input$replyShape, collapse = " ")) +
                 theme_bw() +
                 theme(plot.title     =element_text(size=22, face="bold", hjust = 0.5),
                       axis.text      =element_text(size=18),
                       axis.title     =element_text(size=18),
                       legend.text    =element_text(size=18),
                       legend.title   =element_text(size=18),
                       legend.key.size=unit(1.5,'lines'),
                       strip.text.x   =element_text(size=18),
                       aspect.ratio   =1) +
                 Legend)
        }
        else

        if ((length(input$replyTimes)==1) && (input$geon1 | input$geon2 | input$geon3 | input$geon4 | input$geon5) && (input$axisZ=="--"))
        {
          vT <- vowelSub1()

          centers <- stats::aggregate(cbind(X,Y)~color+plot, data=vT, FUN=mean)

          vT <- vT[order(vT$plot, vT$index, vT$time),]
          vT$index <- paste0(vT$time,vT$index)

          Basis <- ggplot(data = vT, aes(x=X, y=Y, fill=color, color=color))

          if (input$geon1)
          {
            if (!(input$geon2 | input$geon3 | input$geon4 | input$geon5))
              Points <- geom_text(position="identity", aes(label=vowel), hjust=0.5, vjust=0.5, size=5, alpha=1.0)
            else
              Points <- geom_text(position="identity", aes(label=vowel), hjust=0.5, vjust=0.5, size=5, alpha=0.3)
          }
          else
          {
            Points <- geom_blank()
          }

          if (input$geon2)
          {
            if (input$geon4)
              Centers <- geom_text(data=centers, position="identity", aes(label=color), hjust=0.5, vjust=0.5, size= 7, alpha=1.0, color="black")
            else
              Centers <- geom_text(data=centers, position="identity", aes(label=color), hjust=0.5, vjust=0.5, size=10, alpha=1.0)

            Legend <- theme(legend.position="none")
          }
          else
          {
            Centers <- geom_blank()
            Legend <- theme(legend.position="right")
          }

          if (input$geon3)
          {
            chulls <- ddply(vT, .(color,plot), function(df) df[grDevices::chull(df$X, df$Y), ])
            Hull <- geom_polygon(data=chulls, aes(x=X, y=Y, group=color, fill=color), alpha=0.1)
          }
          else
          {
            Hull <- element_blank()
          }

          if (input$geon4)
          {
            vT0 <- vT
            for (i in (1:nrow(vT0)))
            {
              centersSub <- subset(centers, (centers$color==vT0$color[i]) & (centers$plot==vT0$plot[i]))

              vT0$X[i] <- centersSub$X
              vT0$Y[i] <- centersSub$Y
            }

            vT0 <- rbind(vT,vT0)
            vT0 <- vT0[order(vT0$plot, vT0$index, vT0$time),]
            vT0$index <- paste0(vT0$time,vT0$index)

            Spokes <- geom_path(data=vT0, aes(group = index), arrow = arrow(ends = "last", length = unit(0, "inches")), size=1.0, alpha=0.3)
          }
          else
          {
            Spokes <- element_blank()
          }

          if (input$geon5)
          {
            if ((input$geon1) | (input$geon3))
              Ellipse <- stat_ellipse(position="identity", type="norm", level=input$replyLevel)
            else
              Ellipse <- stat_ellipse(position="identity", type="norm", level=input$replyLevel, geom="polygon", alpha=0.3)
          }
          else
          {
            Ellipse <- element_blank()
          }

          if ((length(input$selManual)>0) && (input$selManual==TRUE))
          {
            scaleX <- scale_x_reverse(name=paste0(input$axisX," (",input$replyScale1," ",normLab(),")"), position="top"  , limits = c(input$replyXmax, input$replyXmin))
            scaleY <- scale_y_reverse(name=paste0(input$axisY," (",input$replyScale1," ",normLab(),")"), position="right", limits = c(input$replyYmax, input$replyYmin))
          }
          else
          {
            scaleX <- scale_x_reverse(name=paste0(input$axisX," (",input$replyScale1," ",normLab(),")"), position="top"  )
            scaleY <- scale_y_reverse(name=paste0(input$axisY," (",input$replyScale1," ",normLab(),")"), position="right")
          }

          if (length(input$catPlot1)>0)
          {
            Title <- ggtitle(paste(input$title1,paste(input$replyPlot1, collapse = " ")))
            Facet <- facet_wrap(~plot)
          }
          else
          {
            Title <- ggtitle(input$title1)
            Facet <- facet_null()
          }

          if ((numColor()==0) | (numColor()>18))
          {
            Legend <- theme(legend.position="none")
          }

          graphics::plot(Basis + Points + Hull +Spokes + Ellipse + Centers + scaleX + scaleY + Title +Facet +
                 scale_color_manual(values=colPalette(length(unique(vT$color)))) +
                 scale_fill_manual (values=colPalette(length(unique(vT$color)))) +
                 labs(colour=paste(input$replyColor, collapse = " "), fill=paste(input$replyColor, collapse = " ")) +
                 theme_bw() +
                 theme(plot.title     =element_text(size=22, face="bold", hjust = 0.5),
                       axis.text      =element_text(size=18),
                       axis.title     =element_text(size=18),
                       legend.text    =element_text(size=18),
                       legend.title   =element_text(size=18),
                       legend.key.size=unit(1.5,'lines'),
                       strip.text.x   =element_text(size=18),
                       aspect.ratio   =1) +
                 Legend)
        }
        else

        if ((length(input$replyTimes)>1) && (input$axisZ=="--"))
        {
          vT <- vowelSub1()[order(vowelSub1()$index, vowelSub1()$time),]

          Basis <- ggplot(data=vT, aes(x=X, y=Y, colour=color, label=""))

          if ((length(input$selManual)>0) && (input$selManual==TRUE))
          {
            scaleX <- scale_x_reverse(name=paste0(input$axisX," (",input$replyScale1," ",normLab(),")"), position="top"  , limits = c(input$replyXmax, input$replyXmin))
            scaleY <- scale_y_reverse(name=paste0(input$axisY," (",input$replyScale1," ",normLab(),")"), position="right", limits = c(input$replyYmax, input$replyYmin))
          }
          else
          {
            scaleX <- scale_x_reverse(name=paste0(input$axisX," (",input$replyScale1," ",normLab(),")"), position="top"  )
            scaleY <- scale_y_reverse(name=paste0(input$axisY," (",input$replyScale1," ",normLab(),")"), position="right")
          }

          if (length(input$catPlot1)>0)
          {
            Title <- ggtitle(paste(input$title1,paste(input$replyPlot1, collapse = " ")))
            Facet <- facet_wrap(~plot)
          }
          else
          {
            Title <- ggtitle(input$title1)
            Facet <- facet_null()
          }

          if ((numColor()>0) & (numColor()<=18))
            Legend <- theme(legend.position="right")
          else
            Legend <- theme(legend.position="none")

          graphics::plot(Basis + scaleX + scaleY + Title + Facet +
                 geom_path(aes(group = index), arrow = arrow(ends = "last", length = unit(0.1, "inches")), size=0.7) +
                 scale_color_manual(values=colPalette(length(unique(vT$color)))) +
                 labs(colour=paste(input$replyColor, collapse = " ")) +
                 theme_bw() +
                 theme(plot.title     =element_text(size=22, face="bold", hjust = 0.5),
                       axis.text      =element_text(size=18),
                       axis.title     =element_text(size=18),
                       legend.text    =element_text(size=18),
                       legend.title   =element_text(size=18),
                       legend.key.size=unit(1.5,'lines'),
                       strip.text.x   =element_text(size=18),
                       aspect.ratio   =1) +
                 Legend
          )
        }
        else

        if ((length(input$replyTimes)==1) && (input$axisZ!="--"))
        {
          vT <- vowelSub1()

          if (nrow(vT)==1)
            return(NULL)

          if ((input$axisX=="F1") | (input$axisX=="F2"))
            vT$X <- -1 * vT$X

          if ((input$axisY=="F1") | (input$axisY=="F2"))
            vT$Y <- -1 * vT$Y

          if ((input$axisZ=="F1") | (input$axisZ=="F2"))
            vT$Z <- -1 * vT$Z

          if (input$geon1)
            cex <- 0.0
          else
            cex <- 1.0

          if (input$geon1)
            Cex <- 1.0
          else
            Cex <- 0.00001

          if (input$geon2)
            alpha <- 0.2
          else
            alpha <- 0.0

          if ((length(input$replyPhi)==0) || (is.na(input$replyPhi)))
            Phi <- 40
          else
            Phi <- input$replyPhi

          if ((length(input$replyPhi)==0) || (is.na(input$replyTheta)))
            Theta <- 40
          else
            Theta <- input$replyTheta

          mar   <- ((length(unique(vT$color))-1)/length(unique(vT$color)))/2
          first <- 1+mar
          last  <- length(unique(vT$color))-mar
          step  <- (last-first)/(length(unique(vT$color))-1)

          at <- c(first)

          if (length(unique(vT$color))>2)
          {
            for (i in 1:(length(unique(vT$color))-2))
            {
              at <- c(at,first + (i*step))
            }
          }

          at <- c(at,last)

          if ((numColor()>1) & (numColor()<=18))
          {
            colvar <- as.integer(as.factor(vT$color))
            colkey <- list(at       = at,
                           side     = 4,
                           addlines = TRUE,
                           length   = 0.04*length(unique(vT$color)),
                           width    = 0.5,
                           labels   = unique(vT$color))
          }
          else
          {
            colvar <- F
            colkey <- FALSE
          }

          graphics::par(mar=c(1,2,2.0,4))

          scatter3D(x        = vT$X,
                    y        = vT$Y,
                    z        = vT$Z,
                    phi      = Phi,
                    theta    = Theta,
                    bty      = "g",
                    type     = "h",
                    cex      = 0,
                    alpha    = alpha,
                    ticktype = "detailed",
                    colvar   = colvar,
                    col      = colPalette(length(unique(vT$color))),
                    colkey   = FALSE,
                    main     = input$title1,
                    cex.main = 1.75,
                    xlab     = paste0(input$axisX," (",input$replyScale1," ",normLab(),")"),
                    ylab     = paste0(input$axisY," (",input$replyScale1," ",normLab(),")"),
                    zlab     = paste0(input$axisZ," (",input$replyScale1," ",normLab(),")"),
                    add      = FALSE)

          scatter3D(x        = vT$X,
                    y        = vT$Y,
                    z        = vT$Z,
                    type     = "p",
                    pch      = 19,
                    cex      = cex,
                    alpha    = 1,
                    colvar   = colvar,
                    col      = colPalette(length(unique(vT$color))),
                    colkey   = colkey,
                    add      = TRUE)

          text3D   (x        = vT$X,
                    y        = vT$Y,
                    z        = vT$Z,
                    cex      = Cex,
                    alpha    = 1,
                    labels   = as.character(vT$vowel),
                    colvar   = colvar,
                    col      = colPalette(length(unique(vT$color))),
                    colkey   = FALSE,
                    add      = TRUE)

          graphics::par(mar=c(5.1,4.1,4.1,2.1))
        }
        else

        if ((length(input$replyTimes)>1) && (input$axisZ!="--"))
        {
          vT <- vowelSub1()

          if ((input$axisX=="F1") | (input$axisX=="F2"))
            vT$X <- -1 * vT$X

          if ((input$axisY=="F1") | (input$axisY=="F2"))
            vT$Y <- -1 * vT$Y

          if ((input$axisZ=="F1") | (input$axisZ=="F2"))
            vT$Z <- -1 * vT$Z

          if ((length(input$replyPhi)==0) || (is.na(input$replyPhi)))
            Phi <- 40
          else
            Phi <- input$replyPhi

          if ((length(input$replyPhi)==0) || (is.na(input$replyTheta)))
            Theta <- 40
          else
            Theta <- input$replyTheta

          mar   <- ((length(unique(vT$color))-1)/length(unique(vT$color)))/2
          first <- 1+mar
          last  <- length(unique(vT$color))-mar
          step  <- (last-first)/(length(unique(vT$color))-1)

          at <- c(first)

          if (length(unique(vT$color))>2)
          {
            for (i in 1:(length(unique(vT$color))-2))
            {
              at <- c(at,first + (i*step))
            }
          }

          at <- c(at,last)

          if ((numColor()>1) & (numColor()<=18))
          {
            VT     <- subset(vT, time==1)

            colvar <- as.integer(as.factor(vT$color))
            ColVar <- as.integer(as.factor(VT$color))
            ColKey <- list(at       = at,
                           side     = 4,
                           addlines = TRUE,
                           length   = 0.04*length(unique(VT$color)),
                           width    = 0.5,
                           labels   = unique(VT$color))
          }
          else
          {
            colvar <- F
            ColVar <- F
            ColKey <- FALSE
          }

          graphics::par(mar=c(1,2,2.0,4))

          scatter3D(x        = vT$X,
                    y        = vT$Y,
                    z        = vT$Z,
                    phi      = Phi,
                    theta    = Theta,
                    bty      = "g",
                    type     = "h",
                    pch      = 19,
                    cex      = 0,
                    ticktype = "detailed",
                    colvar   = colvar,
                    col      = colPalette(length(unique(vT$color))),
                    colkey   = FALSE,
                    main     = input$title1,
                    cex.main = 1.75,
                    alpha    = 0.2,
                    xlab     = paste0(input$axisX," (",input$replyScale1," ",normLab(),")"),
                    ylab     = paste0(input$axisY," (",input$replyScale1," ",normLab(),")"),
                    zlab     = paste0(input$axisZ," (",input$replyScale1," ",normLab(),")"),
                    add      = FALSE)

          nTimes <- length(unique(vT$time))

          if (nTimes > 2)
          {
            for (i in (2:(nTimes-1)))
            {
              vT0 <- subset(vT, time==i-1)
              vT1 <- subset(vT, time==i)

              arrows3D(x0     = vT0$X,
                       y0     = vT0$Y,
                       z0     = vT0$Z,
                       x1     = vT1$X,
                       y1     = vT1$Y,
                       z1     = vT1$Z,
                       colvar = ColVar,
                       col    = colPalette(length(unique(vT0$color))),
                       colkey = FALSE,
                       code   = 0,
                       length = 0.2,
                       type   = "triangle",
                       lwd    = 2,
                       alpha  = 1,
                       add    = TRUE)
            }
          }

          vT0 <- subset(vT, time==nTimes-1)
          vT1 <- subset(vT, time==nTimes  )

          arrows3D(x0     = vT0$X,
                   y0     = vT0$Y,
                   z0     = vT0$Z,
                   x1     = vT1$X,
                   y1     = vT1$Y,
                   z1     = vT1$Z,
                   colvar = ColVar,
                   col    = colPalette(length(unique(vT0$color))),
                   colkey = ColKey,
                   code   = 2,
                   length = 0.3,
                   type   = "simple",
                   lwd    = 2,
                   alpha  = 1,
                   add    = TRUE)

          graphics::par(mar=c(5.1,4.1,4.1,2.1))
        }
        else {}
      }

      output$graph1 <- renderPlot(height = 550, width = 650,
      {
        if (length(input$replyTimes)>0)
        {
          plotGraph1()
        }
      })

      output$selFormat1a <- renderUI(
      {
        options <- c("txt","xlsx")
        selectInput('replyFormat1a', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      fileName1a <- function()
      {
        return(paste0("formantsTable.",input$replyFormat1a))
      }

      output$download1a <- downloadHandler(filename = fileName1a, content = function(file)
      {
        if (length(input$replyTimes)>0)
        {
          vT <- vowelSub1()

          colnames(vT)[which(colnames(vT)=="X")] <- input$axisX
          colnames(vT)[which(colnames(vT)=="Y")] <- input$axisY
          colnames(vT)[which(colnames(vT)=="Z")] <- input$axisZ
        }
        else
          vT <- data.frame()

        if (input$replyFormat1a=="txt")
        {
          utils::write.table(vT, file, sep = "\t", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
        }
        else

          if (input$replyFormat1a=="xlsx")
          {
            WriteXLS(vT, file, SheetNames = "table", row.names=FALSE, col.names=TRUE, BoldHeaderRow = TRUE, na = "NA", FreezeRow = 1)
          }
        else {}
      })

      output$selFormat1b <- renderUI(
      {
        options <- c("JPG","PNG","SVG","EPS","PDF")
        selectInput('replyFormat1b', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      output$selSize1b <- renderUI(
      {
        options <- c("small","medium","large")
        selectInput('replySize1b', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      fileName1b <- function()
      {
        return(paste0("formantPlot.",input$replyFormat1b))
      }

      save2D <- function(file)
      {
        if (input$replySize1b=="small")
          scale <- 1.0
        if (input$replySize1b=="medium")
          scale <- 1.5
        if (input$replySize1b=="large")
          scale <- 2.0

        width0 <- px2inch(700)
        height0 <- px2inch(550)

        grDevices::pdf(NULL)

        if (length(input$replyTimes)>0)
          print(plotGraph1())
        else
          print(ggplot()+theme_bw())

        if (input$replyFormat1b=="JPG")
          ggSave(file, scale=scale, device="jpeg", max.dimension = height0)
        else
          if (input$replyFormat1b=="PNG")
            ggSave(file, scale=scale, device="png", max.dimension = height0)
        else
          if (input$replyFormat1b=="SVG")
            ggSave(file, scale=scale, device="svg", max.dimension = height0)
        else
          if (input$replyFormat1b=="EPS")
            ggSave(file, scale=scale, device=grDevices::cairo_ps , max.dimension = height0)
        else
          if (input$replyFormat1b=="PDF")
            ggSave(file, scale=scale, device=grDevices::cairo_pdf, max.dimension = height0)
        else {}

        grDevices::dev.off()
      }

      save3D <- function(file)
      {
        if (input$replySize1b=="small")
          scale <- 1.00
        if (input$replySize1b=="medium")
          scale <- 1.25
        if (input$replySize1b=="large")
          scale <- 1.50

        width0 <- scale * 700
        height0 <- scale * 550

        width  <- px2inch( width0)
        height  <- px2inch(height0)

        if (input$replyFormat1b=="PNG")
          grDevices::png(file, width = width0, height = height0)
        else
          if (input$replyFormat1b=="JPG")
            grDevices::jpeg(file, width = width0, height = height0)
        else
          if (input$replyFormat1b=="SVG")
            grDevices::svg(file, width = width , height = height )
        else
          if (input$replyFormat1b=="EPS")
            grDevices::cairo_ps (file, width = width , height = height , onefile = FALSE)
        else
          if (input$replyFormat1b=="PDF")
            grDevices::cairo_pdf(file, width = width , height = height )
        else {}

        if (length(input$replyTimes)>0)
          print(plotGraph1())
        else
          graphics::plot.new()

        grDevices::dev.off()
      }

      output$download1b <- downloadHandler(filename = fileName1b, content = function(file)
      {
        if (input$axisZ=="--")
          save2D(file)
        else
          save3D(file)
      })

      #############################################################################

      vowelNorm2 <- reactive(
      {
        if ((is.null(vowelTab())) || (length(input$replyNormal2)==0))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))

        if (sum(vowelTab()[,indexVowel+1]==0)==nrow(vowelTab()))
          return(NULL)

        SpeaKer <- unique(vowelTab()[,1])
        nSpeaKer <- length(SpeaKer)

        VoWel   <- unique(vowelTab()[,indexVowel])
        nVoWel   <- length(VoWel)

        if (input$replyNormal2=="None")
        {
          vT <- vowelTab()
        }

        if ((input$replyNormal2=="Lobanov's z-score transformation") & (nSpeaKer==1))
        {
          return(NULL)
        }

        if ((input$replyNormal2=="Lobanov's z-score transformation") & (nVoWel  ==1))
        {
          vT <- data.frame()
          vTAg <- data.frame(vowelTab()[,1],vowelTab()[,indexVowel+1])

          for (q in (1:nSpeaKer))
          {
            vTAgSub <- subset(vTAg, vTAg[,1]==SpeaKer[q])

            meanD <- mean(vTAgSub[,2])
            sdD <-   stats::sd(vTAgSub[,2])

            vTsub <- subset(vowelTab(), vowelTab()[,1]==SpeaKer[q])
            vTsub[,indexVowel+1] <- (vTsub[,indexVowel+1]-meanD)/sdD

            vT <- rbind(vT,vTsub)
          }
        }

        if ((input$replyNormal2=="Lobanov's z-score transformation") & (nVoWel  > 1))
        {
          vT <- data.frame()
          vTAg <- stats::aggregate(vowelTab()[,indexVowel+1]~vowelTab()[,1]+vowelTab()[,indexVowel], FUN=mean)

          for (q in (1:nSpeaKer))
          {
            vTAgSub <- subset(vTAg, vTAg[,1]==SpeaKer[q])

            meanD <- mean(vTAgSub[,3])
            sdD <-   stats::sd(vTAgSub[,3])

            vTsub <- subset(vowelTab(), vowelTab()[,1]==SpeaKer[q])
            vTsub[,indexVowel+1] <- (vTsub[,indexVowel+1]-meanD)/sdD

            vT <- rbind(vT,vTsub)
          }
        }

        return(vT)
      })

      vowelSub2 <- reactive(
      {
        if (is.null(vowelNorm2()) || (nrow(vowelNorm2())==0) || (length(input$catXaxis2)==0))
          return(NULL)

        vT <- vowelNorm2()

        indexVowel <- grep("^vowel$", colnames(vT))

        if (sum(vT[,indexVowel+1]==0)==nrow(vT))
          return(NULL)

        vT$indexXaxis <- fuseCols(vowelNorm2(),input$replyXaxis2)
        vT$indexLine  <- fuseCols(vowelNorm2(),input$replyLine2)
        vT$indexPlot  <- fuseCols(vowelNorm2(),input$replyPlot2)

        if (input$selError=="0%")
          z <- 0
        if (input$selError=="90%")
          z <- 1.645
        if (input$selError=="95%")
          z <- 1.96
        if (input$selError=="99%")
          z <- 2.575

        if (((length(input$catLine2)==0) | (length(input$catLine2)>14)) && (length(input$catPlot2)==0))
        {
          vT <- subset(vT, is.element(vT$indexXaxis,input$catXaxis2))

          ag    <- stats::aggregate(vT[,indexVowel+1] ~ vT$indexXaxis, FUN=mean)
          ag$sd <- stats::aggregate(vT[,indexVowel+1] ~ vT$indexXaxis, FUN=stats::sd)[,2]
          ag$n  <- stats::aggregate(vT[,indexVowel+1] ~ vT$indexXaxis, FUN=length)[,2]
          ag$se <- ag$sd / sqrt(ag$n)

          if (input$selMeasure=="SD")
          {
            ag$ll <- ag[,2] - z * ag$sd
            ag$ul <- ag[,2] + z * ag$sd
          }
          if (input$selMeasure=="SE")
          {
            ag$ll <- ag[,2] - z * ag$se
            ag$ul <- ag[,2] + z * ag$se
          }

          ag <- ag[order(ag[,2]),]
          ag[,1] <- factor(ag[,1], levels=ag[,1])

          colnames(ag)[1] <- paste(input$replyXaxis2, collapse = " ")
          colnames(ag)[2] <- "duration"

          return(ag)
        }
        else

          if (((length(input$catLine2)==0) | (length(input$catLine2)>14)) && (length(input$catPlot2)>0))
          {
            vT <- subset(vT, is.element(vT$indexXaxis,input$catXaxis2) & is.element(vT$indexPlot,input$catPlot2))

            if (nrow(vT)==0)
            {
              return(NULL)
            }

            ag    <- stats::aggregate(vT[,indexVowel+1] ~ vT$indexXaxis + vT$indexPlot, FUN=mean)
            ag$sd <- stats::aggregate(vT[,indexVowel+1] ~ vT$indexXaxis + vT$indexPlot, FUN=stats::sd)[,3]
            ag$n  <- stats::aggregate(vT[,indexVowel+1] ~ vT$indexXaxis + vT$indexPlot, FUN=length)[,3]
            ag$se <- ag$sd / sqrt(ag$n)

            if (input$selMeasure=="SD")
            {
              ag$ll <- ag[,3] - z * ag$sd
              ag$ul <- ag[,3] + z * ag$sd
            }
            if (input$selMeasure=="SE")
            {
              ag$ll <- ag[,3] - z * ag$se
              ag$ul <- ag[,3] + z * ag$se
            }

            ag <- ag[order(ag[,3]),]
            xx <- unique(ag[,1])

            ag0 <- data.frame()

            for (q in (1:length(xx)))
            {
              ag0 <- rbind(ag0,ag[ag[,1]==xx[q],])
            }

            ag <- ag0
            ag[,1] <- factor(ag[,1], levels=xx)
            ag[,2] <- as.character(ag[,2])

            ag <- ag[order(ag[,2]),]

            colnames(ag)[1] <- paste(input$replyXaxis2, collapse = " ")
            colnames(ag)[2] <- paste(input$replyPlot2 , collapse = " ")
            colnames(ag)[3] <- "duration"

            return(ag)
          }
        else

          if (((length(input$catLine2)>0) & (length(input$catLine2)<=14)) && (length(input$catPlot2)==0))
          {
            vT <- subset(vT, is.element(vT$indexXaxis,input$catXaxis2) & is.element(vT$indexLine,input$catLine2))

            if (nrow(vT)==0)
            {
              return(NULL)
            }

            ag    <- stats::aggregate(vT[,indexVowel+1] ~ vT$indexXaxis + vT$indexLine, FUN=mean)
            ag$sd <- stats::aggregate(vT[,indexVowel+1] ~ vT$indexXaxis + vT$indexLine, FUN=stats::sd)[,3]
            ag$n  <- stats::aggregate(vT[,indexVowel+1] ~ vT$indexXaxis + vT$indexLine, FUN=length)[,3]
            ag$se <- ag$sd / sqrt(ag$n)

            if (input$selMeasure=="SD")
            {
              ag$ll <- ag[,3] - z * ag$sd
              ag$ul <- ag[,3] + z * ag$sd
            }
            if (input$selMeasure=="SE")
            {
              ag$ll <- ag[,3] - z * ag$se
              ag$ul <- ag[,3] + z * ag$se
            }

            ag <- ag[order(ag[,3]),]
            xx <- unique(ag[,1])

            ag0 <- data.frame()

            for (q in (1:length(xx)))
            {
              ag0 <- rbind(ag0,ag[ag[,1]==xx[q],])
            }

            ag <- ag0
            ag[,1] <- factor(ag[,1], levels=xx)
            ag[,2] <- as.character(ag[,2])

            colnames(ag)[1] <- paste(input$replyXaxis2, collapse = " ")
            colnames(ag)[2] <- paste(input$replyLine2 , collapse = " ")
            colnames(ag)[3] <- "duration"

            return(ag)
          }
        else

          if (((length(input$catLine2)>0) & (length(input$catLine2)<=14))  && (length(input$catPlot2)>0))
          {
            vT <- subset(vT, is.element(vT$indexXaxis,input$catXaxis2) & is.element(vT$indexLine,input$catLine2) & is.element(vT$indexPlot,input$catPlot2))

            if (nrow(vT)==0)
            {
              return(NULL)
            }

            ag    <- stats::aggregate(vT[,indexVowel+1] ~ vT$indexXaxis + vT$indexLine + vT$indexPlot, FUN=mean)
            ag$sd <- stats::aggregate(vT[,indexVowel+1] ~ vT$indexXaxis + vT$indexLine + vT$indexPlot, FUN=stats::sd)[,4]
            ag$n  <- stats::aggregate(vT[,indexVowel+1] ~ vT$indexXaxis + vT$indexLine + vT$indexPlot, FUN=length)[,4]
            ag$se <- ag$sd / sqrt(ag$n)

            if (input$selMeasure=="SD")
            {
              ag$ll <- ag[,4] - z * ag$sd
              ag$ul <- ag[,4] + z * ag$sd
            }
            if (input$selMeasure=="SE")
            {
              ag$ll <- ag[,4] - z * ag$se
              ag$ul <- ag[,4] + z * ag$se
            }

            ag <- ag[order(ag[,4]),]
            xx <- unique(ag[,1])

            ag0 <- data.frame()

            for (q in (1:length(xx)))
            {
              ag0 <- rbind(ag0,ag[ag[,1]==xx[q],])
            }

            ag <- ag0
            ag[,1] <- factor(ag[,1], levels=xx)
            ag[,2] <- as.character(ag[,2])
            ag[,3] <- as.character(ag[,3])

            ag <- ag[order(ag[,3]),]

            colnames(ag)[1] <- paste(input$replyXaxis2, collapse = " ")
            colnames(ag)[2] <- paste(input$replyLine2 , collapse = " ")
            colnames(ag)[3] <- paste(input$replyPlot2 , collapse = " ")
            colnames(ag)[4] <- "duration"

            return(ag)
          }
        else

        {
          return(NULL)
        }
      })

      output$selNormal2 <- renderUI(
      {
        options <- c("None",
                     "Lobanov's z-score transformation")

        selectInput('replyNormal2', 'Normalization method:', options, selected = options[1], selectize=FALSE, multiple=FALSE, width="100%")
      })

      output$selGraph <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        options <- c("Dot plot","Bar chart")
        selectInput('replyGraph', 'Select graph type:', options, selected = options[1], selectize=FALSE, multiple=FALSE, width="100%")
      })

      output$selXaxis2 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))

        selectInput('replyXaxis2', 'Variable x-axis:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$catXaxis2 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyXaxis2)>0)
          options <- unique(fuseCols(vowelTab(),input$replyXaxis2))
        else
          options <- NULL

        selectInput('catXaxis2', 'Sel. categories:', options, multiple=TRUE, selectize = FALSE, width="100%")
      })

      output$selLine2 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))

        indexVowel <- grep("^vowel$",options)
        selectInput('replyLine2', 'Color variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$catLine2 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyLine2)>0)
          options <- unique(fuseCols(vowelTab(),input$replyLine2))
        else
          options <- NULL

        selectInput('catLine2', 'Select colors:', options, multiple=TRUE, selectize = FALSE, width="100%")
      })

      output$selPlot2 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        indexVowel <- grep("^vowel$", colnames(vowelTab()))
        options <- c(colnames(vowelTab()[indexVowel]),colnames(vowelTab()[1:(indexVowel-1)]))

        selectInput('replyPlot2', 'Plot variable:', options, selected = options[1], multiple=TRUE, selectize=FALSE, width="100%")
      })

      output$catPlot2 <- renderUI(
      {
        if (is.null(vowelTab()))
          return(NULL)

        if (length(input$replyPlot2)>0)
          options <- unique(fuseCols(vowelTab(),input$replyPlot2))
        else
          options <- NULL

        selectInput('catPlot2', 'Select plots:', options, multiple=TRUE, selectize = FALSE, width="100%")
      })

      plotGraph2 <- function()
      {
        if (is.null(vowelSub2()) || (nrow(vowelSub2())==0))
          return(NULL)

        if (input$selError=="0%")
          w <- 0
        if (input$selError=="90%")
          w <- 0.4
        if (input$selError=="95%")
          w <- 0.4
        if (input$selError=="99%")
          w <- 0.4

        if (((length(input$catLine2)==0) | (length(input$catLine2)>14)) && (length(input$catPlot2)==0))
        {
          if (input$replyGraph=="Dot plot")
          {
            graphics::plot(ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,2], group=1)) +
                   geom_line(colour="indianred2", size=1) + geom_point(colour="indianred2", size=3) +
                   geom_errorbar(colour="indianred2", aes(ymin=ll, ymax=ul), width=w) +
                   ggtitle(input$title2) +
                   xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                   theme_bw() +
                   theme(plot.title     =element_text(size=22, face="bold", hjust = 0.5),
                         axis.text      =element_text(size=18),
                         axis.title     =element_text(size=18),
                         strip.text.x   =element_text(size=14),
                         aspect.ratio   =0.67))
          }
          else

          if (input$replyGraph=="Bar chart")
          {
            graphics::plot(ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,2])) +
                   geom_bar(stat="identity", colour="black", fill="indianred2", size=.3) +
                   geom_errorbar(aes(ymin=ll, ymax=ul), width=w) +
                   ggtitle(input$title2) +
                   xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                   theme_bw() +
                   theme(plot.title     =element_text(size=22, face="bold", hjust = 0.5),
                         axis.text      =element_text(size=18),
                         axis.title     =element_text(size=18),
                         strip.text.x   =element_text(size=14),
                         aspect.ratio   =0.67))
          }
        }
        else

          if (((length(input$catLine2)==0) | (length(input$catLine2)>14)) && (length(input$catPlot2)>0))
          {
            if (input$replyGraph=="Dot plot")
            {
              graphics::plot(ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,3], group=1)) +
                     geom_line(colour="indianred2", size=1) + geom_point(colour="indianred2", size=3) +
                     geom_errorbar(colour="indianred2", aes(ymin=ll, ymax=ul), width=w) +
                     ggtitle(paste(input$title2,paste(input$replyPlot2, collapse = " "))) +
                     xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                     facet_wrap(~vowelSub2()[,2]) +
                     theme_bw() +
                     theme(plot.title     =element_text(size=22, face="bold", hjust = 0.5),
                           axis.text      =element_text(size=18),
                           axis.title     =element_text(size=18),
                           strip.text.x   =element_text(size=14),
                           aspect.ratio   =0.67))
            }

            else

            if (input$replyGraph=="Bar chart")
            {
              graphics::plot(ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,3])) +
                     geom_bar(stat="identity", colour="black", fill="indianred2", size=.3) +
                     geom_errorbar(aes(ymin=ll, ymax=ul), width=w) +
                     ggtitle(paste(input$title2,paste(input$replyPlot2, collapse = " "))) +
                     xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                     facet_wrap(~vowelSub2()[,2]) +
                     theme_bw() +
                     theme(plot.title     =element_text(size=22, face="bold", hjust = 0.5),
                           axis.text      =element_text(size=18),
                           axis.title     =element_text(size=18),
                           strip.text.x   =element_text(size=14),
                           aspect.ratio   =0.67))
            }
            else {}
          }
        else

        if (((length(input$catLine2)>0) & (length(input$catLine2)<=14)) && (length(input$catPlot2)==0))
        {
          if (input$replyGraph=="Dot plot")
          {
            pd <- position_dodge(0.1)

            graphics::plot(ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,3], group=vowelSub2()[,2], color=vowelSub2()[,2])) +
                   geom_line(size=1, position=pd) + geom_point(size=3, position=pd) +
                   geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
                   ggtitle(input$title2) +
                   xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                   scale_colour_discrete(name=paste0(paste(input$replyLine2, collapse = " "),"\n")) +
                   theme_bw() +
                   theme(plot.title     =element_text(size=22, face="bold", hjust = 0.5),
                         axis.text      =element_text(size=18),
                         axis.title     =element_text(size=18),
                         legend.text    =element_text(size=18),
                         legend.title   =element_text(size=18),
                         legend.key.size=unit(1.5, 'lines'),
                         strip.text.x   =element_text(size=14),
                         aspect.ratio   =0.67))
          }
          else

          if (input$replyGraph=="Bar chart")
          {
            pd <- position_dodge(0.9)

            graphics::plot(ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,3], fill=vowelSub2()[,2])) +
                   geom_bar(position=position_dodge(), stat="identity", colour="black", size=.3) +
                   geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
                   ggtitle(input$title2) +
                   xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                   scale_fill_hue(name=paste0(paste(input$replyLine2, collapse = " "),"\n")) +
                   theme_bw() +
                   theme(plot.title     =element_text(size=22, face="bold", hjust = 0.5),
                         axis.text      =element_text(size=18),
                         axis.title     =element_text(size=18),
                         legend.text    =element_text(size=18),
                         legend.title   =element_text(size=18),
                         legend.key.size=unit(1.5, 'lines'),
                         strip.text.x   =element_text(size=14),
                         aspect.ratio   =0.67))
          }
          else {}
        }
        else

        if (((length(input$catLine2)>0) & (length(input$catLine2)<=14))  && (length(input$catPlot2)>0))
        {
          if (input$replyGraph=="Dot plot")
          {
            pd <- position_dodge(0.1)

            graphics::plot(ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,4], group=vowelSub2()[,2], color=vowelSub2()[,2])) +
                   geom_line(size=1, position=pd) + geom_point(size=3, position=pd) +
                   geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
                   ggtitle(paste(input$title2,paste(input$replyPlot2, collapse = " "))) +
                   xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                   scale_colour_discrete(name=paste0(paste(input$replyLine2, collapse = " "),"\n")) +
                   facet_wrap(~vowelSub2()[,3]) +
                   theme_bw() +
                   theme(plot.title     =element_text(size=22, face="bold", hjust = 0.5),
                         axis.text      =element_text(size=18),
                         axis.title     =element_text(size=18),
                         legend.text    =element_text(size=18),
                         legend.title   =element_text(size=18),
                         legend.key.size=unit(1.5, 'lines'),
                         strip.text.x   =element_text(size=14),
                         aspect.ratio   =0.67))
          }
          else

          if (input$replyGraph=="Bar chart")
          {
            pd <- position_dodge(0.9)

            graphics::plot(ggplot(data=vowelSub2(), aes(x=vowelSub2()[,1], y=vowelSub2()[,4], fill=vowelSub2()[,2])) +
                   geom_bar(position=position_dodge(), stat="identity", colour="black", size=.3) +
                   geom_errorbar(aes(ymin=ll, ymax=ul), width=w, position=pd) +
                   ggtitle(paste(input$title2,paste(input$replyPlot2, collapse = " "))) +
                   xlab(paste(input$replyXaxis2, collapse = " ")) + ylab("duration") +
                   scale_fill_hue(name=paste0(paste(input$replyLine2, collapse = " "),"\n")) +
                   facet_wrap(~vowelSub2()[,3]) +
                   theme_bw() +
                   theme(plot.title     =element_text(size=22, face="bold", hjust = 0.5),
                         axis.text      =element_text(size=18),
                         axis.title     =element_text(size=18),
                         legend.text    =element_text(size=18),
                         legend.title   =element_text(size=18),
                         legend.key.size=unit(1.5, 'lines'),
                         strip.text.x   =element_text(size=14),
                         aspect.ratio   =0.67))
          }
          else {}
        }
        else {}
      }

      output$graph2 <- renderPlot(height = 550, width = 700,
      {
        if (length(input$catXaxis2)>0)
        {
          plotGraph2()
        }
      })

      output$selFormat2a <- renderUI(
      {
        options <- c("txt","xlsx")
        selectInput('replyFormat2a', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      fileName2a <- function()
      {
        return(paste0("durationTable.",input$replyFormat2a))
      }

      output$download2a <- downloadHandler(filename = fileName2a, content = function(file)
      {
        if (length(input$catXaxis2)>0)
        {
          vT <- vowelSub2()

          colnames(vT)[which(colnames(vT)=="sd")] <- "standard deviation"
          colnames(vT)[which(colnames(vT)=="se")] <- "standard error"
          colnames(vT)[which(colnames(vT)=="n" )] <- "number of observations"
          colnames(vT)[which(colnames(vT)=="ll")] <- "lower limit"
          colnames(vT)[which(colnames(vT)=="ul")] <- "upper limit"
        }
        else
          vT <- data.frame()

        if (input$replyFormat2a=="txt")
        {
          utils::write.table(vT, file, sep = "\t", na = "NA", dec = ".", row.names = FALSE, col.names = TRUE)
        }
        else

          if (input$replyFormat2a=="xlsx")
          {
            WriteXLS(vT, file, SheetNames = "table", row.names=FALSE, col.names=TRUE, BoldHeaderRow = TRUE, na = "NA", FreezeRow = 1, AdjWidth = TRUE)
          }
        else {}
      })

      output$selFormat2b <- renderUI(
      {
        options <- c("JPG","PNG","SVG","EPS","PDF")
        selectInput('replyFormat2b', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      output$selSize2b <- renderUI(
      {
        options <- c("small","medium","large")
        selectInput('replySize2b', label=NULL, options, selected = options[2], selectize=FALSE, multiple=FALSE)
      })

      fileName2b <- function()
      {
        return(paste0("durationPlot.",input$replyFormat2b))
      }

      output$download2b <- downloadHandler(filename = fileName2b, content = function(file)
      {
        if (input$replySize2b=="small")
          scale <- 1.0
        if (input$replySize2b=="medium")
          scale <- 1.5
        if (input$replySize2b=="large")
          scale <- 2.0

        width0 <- px2inch(700)
        height0 <- px2inch(550)

        grDevices::pdf(NULL)

        if (length(input$catXaxis2)>0)
          print(plotGraph2())
        else
          print(ggplot()+theme_bw())

        if (input$replyFormat2b=="JPG")
          ggSave(file, scale=scale, device="jpeg", max.dimension = height0)
        else
          if (input$replyFormat2b=="PNG")
            ggSave(file, scale=scale, device="png", max.dimension = height0)
        else
          if (input$replyFormat2b=="SVG")
            ggSave(file, scale=scale, device="svg", max.dimension = height0)
        else
          if (input$replyFormat2b=="EPS")
            ggSave(file, scale=scale, device=grDevices::cairo_ps , max.dimension = height0)
        else
          if (input$replyFormat2b=="PDF")
            ggSave(file, scale=scale, device=grDevices::cairo_pdf, max.dimension = height0)
        else {}

        grDevices::dev.off()
      })
    }
  )
}
