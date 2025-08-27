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
library(shinymanager)
library(shinycustomloader)
library(shiny.i18n)
library(shinyWidgets)
library(RSQLite)
library(dipsaus)
library(base64enc)
options(encoding = "UTF-8")

# source custom function for concurrent writing in SQLite database
source("dbWriteTable_.R")
source("ocr_functions.R")

# set dbname
dbname_ <- "../aply_suite_db"

# set countries languages
countries <- c("Français", "English")
flags <- c("french.png", "us.png")

# file with translations
i18n <- Translator$new(translation_json_path = "aply_go_translation.json")
i18n$set_translation_language("Français")

# create a connection to aply suite db and disconnect on exit
db_connect <- dbConnect(SQLite(), dbname = dbname_)
on.exit(DBI::dbDisconnect(db_connect))

# credentials
aply_go_cred_curr_mon_use_df <- as.data.frame(dbReadTable(
  db_connect,
  "aply_go_credential_current_month_usage"
))

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

# user interface
ui <- secure_app(
  choose_language = FALSE,
  tags_top = tags$img(src = "aply_go_logo.png", width = 200, height = 180),
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
              HTML(paste(tags$img(src = flagUrl, width = 20, height = 15), country))
            }, SIMPLIFY = FALSE, USE.NAMES = FALSE)
        )
      )
    ),

    # main app
    div(
      id = "app-content",
      titlePanel("", windowTitle = "APLyGo"),
      sidebarLayout(
        sidebarPanel(
          HTML('<center><img src="logo_moju_ai.png" width="180" height="180"></header>'),
          column(width = 1.5, offset = 10, style = "padding:6px;"),
          titlePanel(h5(p(" "), align = "left")),
          fancyFileInput(
            input = "file1",
            label = h4(HTML(paste0(
              p(strong(i18n$t("Coller (Ctrl + V) une capture d'écran dans APLyGo"))), 
              p(strong(i18n$t("ou"))),
              p(strong(i18n$t("Glisser-déposer un fichier ci-dessous")))
            ))),
            accept = c(".png", ".jpeg", ".jpg", ".tiff", ".bmp", ".pdf")
          ),
          radioButtons(
            inputId = "rot_img_auto",
            label = h5(strong(i18n$t("Rotation automatique d'image"))),
            choices = c("Non-auto" = "FALSE", "Auto" = "TRUE"),
            selected = "FALSE",
            inline = TRUE
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
          titlePanel(h5(strong(i18n$t("- ⚠️ La rotation automatique doit être appliquée uniquement aux images mal orientées, car elle ralentit le traitement des images déjà correctement orientées")),
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
    ),

    # global JS for capturing pasted image
    tags$script(HTML("
      document.addEventListener('paste', function(e) {
        var items = (e.clipboardData || e.originalEvent.clipboardData).items;
        for (var i = 0; i < items.length; i++) {
          if (items[i].type.indexOf('image') !== -1) {
            var blob = items[i].getAsFile();
            var reader = new FileReader();
            reader.onload = function(event){
              Shiny.setInputValue('pasted_image', event.target.result, {priority: 'event'});
            };
            reader.readAsDataURL(blob);
          }
        }
      });
    "))
  )
)

# server
server <- shinyServer(function(input, output, session) {
  options(shiny.maxRequestSize = 4 * 1024^2)

  result_auth <- secure_server(check_credentials = check_credentials(cred_curr_mon_use_df))
  output$res_auth <- renderPrint({
    reactiveValuesToList(result_auth)
  })

  hide(id = "loading-content", anim = TRUE, animType = "fade", time = 4)

  rv <- reactiveValues(file1 = NULL)

  # file input via drag and drop
  observeEvent(input$file1, {
    rv$file1 <- input$file1
  })

  # file input via CTRL+V (clipboard image)
  observeEvent(input$pasted_image, {
    tmpF <- tempfile(fileext = ".png")
    base64_data <- sub("^data:image/.+;base64,", "", input$pasted_image)
    writeBin(base64enc::base64decode(base64_data), tmpF)
    rv$file1 <- list(datapath = tmpF, size = file.info(tmpF)$size)
  })

  observeEvent(input$selected_language, {
    if (is.null(list_out_aply_go()$text_)) shiny.i18n::update_lang(session, input$selected_language)
  })

  observeEvent(input$print, {
    js$winprint()
  })

  list_out_aply_go <- reactive({
    list_out_aply_go <- list(text_ = NULL, img_ = NULL)

    if (!is.null(unlist(rv$file1)) && as.numeric(rv$file1$size) > 1 &&
      tolower(file_ext(rv$file1$datapath)) %in% c("jpg", "jpeg", "png", "tiff", "bmp", "pdf")) {
      rot_img_auto_choice <- as.logical(input$rot_img_auto)

      list_out_aply_go <- extract_text_img_from_file(
        rv$file1$datapath,
        rot_img_auto_ = rot_img_auto_choice
      )

      db_connect <- dbConnect(SQLite(), dbname = dbname_)
      on.exit(DBI::dbDisconnect(db_connect))

      auth_ind <- as.character(reactiveValuesToList(result_auth))

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
          overwrite_ = TRUE
        )
      }
    }
    list_out_aply_go
  })

  output$img_ <- renderImage(
    {
      test_render_img <- (
        !is.null(unlist(rv$file1)) &&
          as.numeric(rv$file1$size) > 1 &&
          !is.null(list_out_aply_go()) &&
          !is.null(list_out_aply_go()$text_) &&
          nchar(list_out_aply_go()$text_) > 5
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
          img_ <- list_out_aply_go()$img_
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
    deleteFile = FALSE
  )

  output$text_ <- renderText({
    if (!is.null(list_out_aply_go()) && !is.null(list_out_aply_go()$text_)) {
      list_out_aply_go()$text_
    } else {
      ""
    }
  })
})

shinyApp(ui, server)
