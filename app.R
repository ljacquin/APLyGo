#---------------------------------------------------------------------------------------------#
# Copyright (C) 2025, aply_go, Author:  Laval Yannis Julien Jacquin                           #
#---------------------------------------------------------------------------------------------#
# This file is part of the APLyGo software                                                    #
#                                                                                             #
# APLyGo software suite can be redistributed and/or modified under the terms of the           #
# GNU General Public License as published by the Free Software Foundation; either version 2   #
# of the License, or (at your option) any later version.                                      #
#                                                                                             #
# This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;   #
# without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.   #
# See the GNU General Public License for more details.                                        #
#                                                                                             #
# You should have received a copy of the GNU General Public License along with this program;  #
# if not, write to the Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,           #
# Boston, MA  02110-1301  USA                                                                 #
#---------------------------------------------------------------------------------------------#
options(rlib_downstream_check = FALSE)
library(devtools)
library(magick)
library(pdftools)
library(tesseract)
library(stringr)
library(stringdist)
library(tools)
library(shiny)
library(shinyjs)
library(V8)
library(imager)
library(viridis)
library(data.table)
library(png)
library(stringr)
library(shinymanager)
library(shinycustomloader)
library(shiny.i18n)
library(shinyWidgets)
library(RSQLite)
# library(rstudioapi)
# setwd(dirname(getActiveDocumentContext()$path))
options(encoding = "UTF-8")

# source custom function for concurrent writing in SQLite database
source("dbWriteTable_.R")
source("ocr_functions.R")

# set dbname
dbname_ <- "../aply_suite_db"

# user interface parameters

# set countries languages
countries <- c(
  "Français", "English"
)
flags <- c(
  "french.png",
  "us.png"
)

# file with translations
i18n <- Translator$new(translation_json_path = "aply_go_translation.json")
i18n$set_translation_language("Français")

# create a connection to aply suite db and disconnect on exit
db_connect <- dbConnect(SQLite(), dbname = dbname_)
on.exit(DBI::dbDisconnect(db_connect))

# credentials
# current month usage
aply_go_cred_curr_mon_use_df <- as.data.frame(dbReadTable(
  db_connect,
  "aply_go_credential_current_month_usage"
))

# data.frame with credentials info
cred_curr_mon_use_df <- unique(data.frame(
  user = aply_go_cred_curr_mon_use_df$Login,
  password = aply_go_cred_curr_mon_use_df$Password,
  stringsAsFactors = FALSE
))

# loading css content
appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"

# user interface components
ui <- secure_app(
  choose_language = FALSE,
  tags_top = tags$img(src = "aply_go_logo.png", width = 200, height = 180),

  # ui <- fluidPage(
  fluidPage(
    HTML('<meta name="viewport" content="width=1024">'),
    useShinyjs(),
    inlineCSS(appCSS),

    # loading message
    div(
      id = "loading-content",
      h2(i18n$t("Chargement de l'application APLyGo..."))
    ),

    # language selection
    shiny.i18n::usei18n(i18n),
    div(
      style = "float: right;", class = "chooselang",
      pickerInput(
        inputId = "selected_language",
        label = i18n$t("Définir la langue de APLyGo"),
        choices = i18n$get_languages(),
        selected = i18n$get_key_translation(),
        choicesOpt = list(
          content =
            mapply(countries, flags, FUN = function(country, flagUrl) {
              HTML(paste(
                tags$img(src = flagUrl, width = 20, height = 15),
                country
              ))
            }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )
      )
    ),

    # main app code goes here
    div(
      id = "app-content",
      titlePanel("", windowTitle = "APLyGo"),
      sidebarLayout(
        sidebarPanel(
          HTML('<center><img src="logo_moju_ai.png" width="200" height="180"></header>'),
          column(width = 1.5, offset = 10, style = "padding:6px;"),
          titlePanel(h5(p(" "), align = "left")),
          fileInput(
            input = "file1",
            label = h4(p(strong(i18n$t("Charger le document")))),
            accept = c(".png", ".jpeg", ".jpg")
          ),
          uiOutput("UIselectInput"),
          verbatimTextOutput("res_auth"),
          column(width = 1.5, offset = 10, style = "padding:6px;"),
          titlePanel(h5(p(" "), align = "left")),
          titlePanel(h4(p(strong(i18n$t("Remarques et recommandations :"))), style = "color:#A30000", align = "left")),
          titlePanel(h5(strong(i18n$t("- Les résultats d'APLyGo doivent être vérifiés")),
            style = "color:#A30000", align = "left"
          )),
          titlePanel(h5(strong(i18n$t("- Aucun résultat ni données ne sont stockés dans APLyGo")),
            style = "color:#A30000", align = "left"
          )),
          width = 3
        ),
        mainPanel(
          tags$style(
            type = "text/css", ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
          ),
          tags$head(
            tags$style("#text1{color: blue;
                             font-size: 50px;
                             font-style: italic;
                             }")
          ),
          h4(verbatimTextOutput("text_"), align = "left"),
          withLoader(imageOutput("img_"),
            type = "image", loader = "computation_loader_new.gif"
          )
        )
      )
    )
  )
)

# server component
server <- shinyServer(
  function(input, output, session) {
    options(shiny.maxRequestSize = 4 * 1024^2)

    # check credentials
    result_auth <- secure_server(check_credentials = check_credentials(cred_curr_mon_use_df))
    output$res_auth <- renderPrint({
      reactiveValuesToList(result_auth)
    })

    # hide the loading message when the reset of the server function has executed
    hide(id = "loading-content", anim = TRUE, animType = "fade", time = 4)

    # create reactive values for input file and patient id
    rv <- reactiveValues(
      file1 = NULL
    )

    # set input file during non analysis state only
    observeEvent(input$file1, {
      rv$file1 <- input$file1
    })

    # set language during non analysis state only
    observeEvent(input$selected_language, {
      res <- list_out_aply_go()
      if (is.null(res$text_)) {
        shiny.i18n::update_lang(session, input$selected_language)
      }
    })

    # print results
    observeEvent(input$print, {
      js$winprint()
    })

    # make aply go computations and return results as a list
    list_out_aply_go <- reactive({
      # initialize an empty list for aply go results
      list_out_aply_go <- list(
        text_ = NULL
      )

      if (!is.null(unlist(rv$file1)) && as.numeric(rv$file1$size) > 1 &&
        tolower(file_ext(rv$file1$datapath)) %in% c("jpg", "jpeg", "png", "tiff", "bmp", "pdf")) {
        # extract text from file
        text_ <- extract_text_from_file(rv$file1$datapath)

        if (nchar(text_) > 5) {
          # create a connection to aply suite db and disconnect on exit
          db_connect <- dbConnect(SQLite(), dbname = dbname_)
          on.exit(DBI::dbDisconnect(db_connect))

          # update login usage
          auth_ind <- as.character(reactiveValuesToList(result_auth))

          # get last updated data frame to track usage
          aply_go_cred_curr_mon_use_df <- as.data.frame(dbReadTable(
            db_connect,
            "aply_go_credential_current_month_usage"
          ))
          id_subscriber_curr_mon <- na.omit(match(auth_ind, aply_go_cred_curr_mon_use_df$Login))

          if (length(id_subscriber_curr_mon) > 0) {
            current_count <- aply_go_cred_curr_mon_use_df[id_subscriber_curr_mon, ]$Count
            aply_go_cred_curr_mon_use_df[id_subscriber_curr_mon, ]$Count <- current_count + 1
            aply_go_cred_curr_mon_use_df[id_subscriber_curr_mon, ]$Last_analysis_timestamp <-
              paste0(as.character(Sys.time()), "sec")
            dbWriteTable_(db_connect, "aply_go_credential_current_month_usage",
              aply_go_cred_curr_mon_use_df,
              overwrite_ = T
            )
          }

          list_out_aply_go$text_ <- text_
        }
      }
      list_out_aply_go
    })

    # output image
    output$img_ <- renderImage(
      {
        res <- list_out_aply_go()

        test_render_img <- (
          !is.null(unlist(rv$file1)) &&
            as.numeric(rv$file1$size) > 1 &&
            !is.null(res) &&
            !is.null(res$text_) &&
            nchar(res$text_) > 5
        )

        if (!test_render_img) {
          list(
            src = "www/aply_go_background.png",
            contentType = "image/png",
            width = 800, height = 800, align = "center"
          )
        } else {
          tmpF_img <- tempfile(fileext = ".png")

          file_ext <- tolower(file_ext(rv$file1$datapath))

          if (file_ext %in% c("jpg", "jpeg", "png", "tiff", "bmp")) {
            img_ <- image_read(rv$file1$datapath)
            width_ <- 1200
            height_ <- 550
          } else if (file_ext == "pdf") {
            img_ <- image_read_pdf(rv$file1$datapath, density = 300)
            width_ <- 1000
            height_ <- 1200
          }
          magick::image_write(img_, path = tmpF_img, format = "png")
          list(
            src = tmpF_img,
            contentType = "image/png",
            width = width_,
            height = height_
          )
        }
      },
      deleteFile = F
    )

    # render text for ui
    output$text_ <- renderText({
      res <- list_out_aply_go()
      if (!is.null(res) && !is.null(res$text_)) {
        res$text_
      } else {
        ""
      }
    })
  }
)

shinyApp(ui, server)
