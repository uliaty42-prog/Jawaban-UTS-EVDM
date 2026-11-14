# ============================================================
# DASHBOARD EKSPLORASI DATA WHOLESALE CUSTOMERS
# Nama : Marcella Marenti Uliaty Graselia
# NIM  : 231067021
#
# Paket yang diperlukan:
# install.packages(c("shiny", "shinydashboard", "ggplot2",
#                    "dplyr", "DT", "GGally", "corrplot"))
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(DT)
library(GGally)
library(corrplot)

# -------------------- WARNA DASHBOARD ------------------------
pink <- "#20AFA3"       # teal terang
pink_tua <- "#3478D4"   # biru cerah
ungu <- "#F28C65"       # coral hangat
panel <- "#FFFFFF"      # latar grafik putih
teks <- "#243746"       # teks navy gelap

# -------------------- FUNGSI BANTU ---------------------------
tema_gelap <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "#FFFFFF", color = NA),
      panel.background = element_rect(fill = "#FFFFFF", color = NA),
      text = element_text(color = "#243746"),
      axis.text = element_text(color = "#455B6B", size = 10),
      axis.title = element_text(color = "#25465D", face = "bold"),
      plot.title = element_text(color = "#173B57", face = "bold", size = 14,
                                margin = margin(b = 10)),
      legend.text = element_text(color = "#354D5E"),
      legend.title = element_text(color = "#25465D", face = "bold"),
      panel.grid.major = element_line(color = "#E5EDF3", linewidth = .55),
      panel.grid.minor = element_line(color = "#F1F5F8", linewidth = .35),
      axis.line = element_line(color = "#B8C9D5"),
      plot.margin = margin(12, 14, 12, 12)
    )
}

fmt <- function(x, digits = 2) {
  if (length(x) == 0) return("NA")

  x <- suppressWarnings(as.numeric(x))

  hasil <- vapply(seq_along(x), function(i) {
    nilai <- x[i]

    if (is.na(nilai) || !is.finite(nilai)) {
      return("NA")
    }

    format(
      round(nilai, digits),
      big.mark = ".",
      decimal.mark = ",",
      nsmall = digits,
      scientific = FALSE,
      trim = TRUE
    )
  }, character(1))

  paste(hasil, collapse = ", ")
}

awali_interpretasi <- function(teks) {
  if (is.null(teks) || length(teks) == 0) return(teks)

  teks <- as.character(teks)
  awalan <- "Berdasarkan hasil output di atas diketahui bahwa "

  vapply(teks, function(kalimat) {
    if (is.na(kalimat) || !nzchar(trimws(kalimat))) return(kalimat)
    kalimat <- trimws(kalimat)

    if (startsWith(kalimat, awalan)) return(kalimat)

    # Jika isi sudah diawali "Berdasarkan ...", rapikan agar tidak berulang.
    if (startsWith(kalimat, "Berdasarkan ")) {
      kalimat <- paste0(tolower(substr(kalimat, 1, 1)),
                        substr(kalimat, 2, nchar(kalimat)))
    } else if (!grepl("^(R-squared|RMSE|MAE|IQR|Q[123]|[A-Z][A-Z0-9]+)", kalimat)) {
      kalimat <- paste0(tolower(substr(kalimat, 1, 1)),
                        substr(kalimat, 2, nchar(kalimat)))
    }

    paste0(awalan, kalimat)
  }, character(1), USE.NAMES = FALSE)
}

# ============================================================
# USER INTERFACE
# ============================================================

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "WHOLESALE ANALYTICS", titleWidth = 280),

  dashboardSidebar(
    width = 280,
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Data", tabName = "data", icon = icon("table")),
      menuItem("Univariat", tabName = "univariat", icon = icon("bar-chart")),
      menuItem("Bivariat", tabName = "bivariat", icon = icon("line-chart")),
      menuItem("Multivariat", tabName = "multivariat", icon = icon("th")),
      menuItem("Regresi", tabName = "regresi", icon = icon("calculator"))
    ),
    br(),
    div(
      style = "padding:15px;",
      h4("UPLOAD DATA", style = "color:#7DD3FC;"),
      fileInput("file", "Pilih file CSV", accept = c(".csv", "text/csv")),
      p("Unggah dataset Wholesale Customers.",
        style = "color:#FFFFFF;font-size:12px;"),
      p("Pastikan nama kolom sesuai dataset.",
        style = "color:#FFFFFF;font-size:11px;")
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* ===== TEMA TERANG: BLUE, TEAL, CORAL, DAN MINT ===== */
        body, .content-wrapper, .right-side {
          background:#EEF4F8 !important;
          color:#243746 !important;
        }

        /* Header dan navigasi */
        .main-header .logo, .main-header .navbar {
          background:#173B57 !important;
          border-bottom:1px solid #2E6078;
        }
        .main-header .logo {
          color:#FFFFFF !important;
          font-weight:800;
          letter-spacing:1.1px;
        }
        .main-header .navbar .sidebar-toggle {
          color:#FFFFFF !important;
        }
        .main-sidebar {
          background:#1D405B !important;
          border-right:1px solid #D4E1E9;
        }
        .sidebar-menu > li > a {
          color:#EAF4F8 !important;
          padding:15px 18px;
          font-size:14px;
          font-weight:600;
          transition:all .2s ease;
        }
        .sidebar-menu > li.active > a,
        .sidebar-menu > li:hover > a {
          background:#287F83 !important;
          color:#FFFFFF !important;
          border-left:4px solid #FFD166;
        }
        .sidebar-menu .treeview-menu > li > a {
          color:#EAF4F8 !important;
        }
        .content {padding:22px 25px;}

        /* Kartu konten */
        .box {
          background:#FFFFFF !important;
          border:1px solid #DCE7EE !important;
          border-top:3px solid #25A99A !important;
          border-radius:15px !important;
          box-shadow:0 7px 22px rgba(28,65,88,.09) !important;
          overflow:hidden;
        }
        .box-header {
          color:#1D4964 !important;
          background:#FFFFFF !important;
          border-bottom:1px solid #E8EFF4;
          padding:14px 18px;
          font-weight:800;
        }
        .box-title {color:#1D4964 !important;font-weight:800;}
        .box-body {color:#30495A !important;padding:18px;}
        h2 {color:#174766 !important;font-weight:850;letter-spacing:.2px;}
        h3, h4 {color:#247F7A !important;font-weight:750;}
        p, li, label {color:#30495A;}
        .text-muted, small, .help-block {color:#647C8C !important;}
        hr {border-color:#DCE7EE;}

        /* Kotak angka utama dengan aksen warna cerah */
        .small-box {
          border-radius:15px !important;
          box-shadow:0 7px 18px rgba(28,65,88,.12);
          border:1px solid rgba(255,255,255,.7);
          overflow:hidden;
        }
        .small-box h3 {font-size:32px;font-weight:850;color:#FFFFFF !important;}
        .small-box p {font-size:14px;letter-spacing:.25px;color:#FFFFFF !important;}
        .small-box .icon-large {color:rgba(255,255,255,.23) !important;}
        .small-box.bg-aqua {background:linear-gradient(135deg,#159EAA,#39C6BC) !important;}
        .small-box.bg-teal {background:linear-gradient(135deg,#167D79,#39B69B) !important;}
        .small-box.bg-blue {background:linear-gradient(135deg,#3478D4,#61A8F4) !important;}
        .small-box.bg-green {background:linear-gradient(135deg,#299B73,#69C58B) !important;}

        /* Input dan tombol */
        .form-control, .selectize-input {
          background:#FFFFFF !important;
          color:#243746 !important;
          border:1px solid #C9D9E3 !important;
          border-radius:8px !important;
        }
        .control-label {color:#244A63 !important;font-weight:700;}
        .btn {
          background:linear-gradient(135deg,#218F91,#2CB5A5) !important;
          color:#FFFFFF !important;
          border:1px solid #218F91 !important;
          border-radius:9px !important;
          padding:9px 16px;
          font-weight:700;
          box-shadow:0 3px 9px rgba(33,143,145,.18);
        }
        .btn:hover {background:#176F76 !important;}

        /* Tabel: kepala tabel berwarna, isi putih dan mudah dibaca */
        html body .content-wrapper .dataTables_wrapper,
        html body .content-wrapper .dataTables_wrapper label,
        html body .content-wrapper .dataTables_wrapper .dataTables_info,
        html body .content-wrapper .dataTables_wrapper .dataTables_paginate,
        html body .content-wrapper .dataTables_wrapper .dataTables_length,
        html body .content-wrapper .dataTables_wrapper .dataTables_filter {
          color:#354F60 !important;
          opacity:1 !important;
        }
        html body .content-wrapper table.dataTable,
        html body .content-wrapper table.dataTable tbody,
        html body .content-wrapper table.dataTable tbody tr,
        html body .content-wrapper table.dataTable tbody tr td,
        html body .content-wrapper table.dataTable tbody tr th {
          color:#30495A !important;
          background:#FFFFFF !important;
          opacity:1 !important;
          text-shadow:none !important;
        }
        html body .content-wrapper table.dataTable tbody tr:nth-child(even) td {
          background:#F1F7FA !important;
          color:#30495A !important;
        }
        html body .content-wrapper table.dataTable tbody tr:hover td {
          background:#DDF4EF !important;
          color:#173B57 !important;
        }
        html body .content-wrapper table.dataTable thead th,
        html body .content-wrapper table.dataTable thead td {
          background:#247F83 !important;
          color:#FFFFFF !important;
          font-weight:800 !important;
          border-bottom:2px solid #69C9B8 !important;
        }
        html body .content-wrapper .dataTables_wrapper input,
        html body .content-wrapper .dataTables_wrapper select {
          background:#FFFFFF !important;
          color:#243746 !important;
          border:1px solid #C9D9E3 !important;
          border-radius:5px;
        }
        html body .content-wrapper .dataTables_wrapper .paginate_button {
          color:#35566A !important;
        }
        html body .content-wrapper .dataTables_wrapper .paginate_button.current {
          background:#247F83 !important;
          color:#FFFFFF !important;
          border:1px solid #247F83 !important;
        }
        table.dataTable {
          background:#FFFFFF !important;
          color:#30495A !important;
          border-radius:8px;
          overflow:hidden;
        }

        /* Interpretasi dalam paragraf */
        .interpretasi {
          background:linear-gradient(110deg,#EAF8F5,#F0F7FF) !important;
          border:1px solid #C8E7E0 !important;
          border-left:5px solid #24A895 !important;
          padding:16px 18px;
          border-radius:11px;
          color:#294657 !important;
          line-height:1.8;
          margin-top:14px;
          font-size:14px;
          box-shadow:0 4px 12px rgba(30,103,111,.06);
        }

        /* Header halaman depan */
        .hero-panel {
          position:relative;
          overflow:hidden;
          padding:34px;
          border-radius:20px;
          background:linear-gradient(118deg,#1D5B83 0%,#228D91 58%,#42B99B 100%);
          border:1px solid #70CDBA;
          box-shadow:0 12px 28px rgba(28,82,112,.17);
          margin-bottom:18px;
          min-height:250px;
        }
        .hero-panel:before {
          content:'';
          position:absolute;
          width:270px;height:270px;
          border:34px solid rgba(255,255,255,.15);
          border-radius:50%;
          right:-72px;top:-112px;
        }
        .hero-panel:after {
          content:'';
          position:absolute;
          width:145px;height:145px;
          background:rgba(255,209,102,.20);
          border-radius:50%;
          right:150px;bottom:-88px;
        }
        .hero-content {position:relative;z-index:2;max-width:780px;}
        .hero-tag {
          display:inline-block;
          padding:7px 14px;
          border-radius:30px;
          background:rgba(255,255,255,.18);
          color:#FFFFFF;
          font-size:11px;
          letter-spacing:1.2px;
          font-weight:800;
          margin-bottom:13px;
          border:1px solid rgba(255,255,255,.3);
        }
        .hero-title {
          font-size:36px;
          font-weight:850;
          color:#FFFFFF;
          line-height:1.2;
          margin:5px 0 10px 0;
          text-shadow:0 2px 5px rgba(0,0,0,.18);
        }
        .hero-subtitle {font-size:16px;color:#F1FFFC;margin-bottom:17px;}
        .hero-meta {display:inline-block;margin:5px 14px 0 0;color:#FFFFFF;font-size:13px;}
        .front-card {
          background:#FFFFFF;
          border:1px solid #DCE8EF;
          border-top:4px solid #2BB6A3;
          border-radius:15px;
          padding:19px;
          min-height:150px;
          margin-bottom:15px;
          box-shadow:0 7px 18px rgba(30,75,98,.08);
        }
        .front-card h4 {margin-top:3px;color:#1D6C78 !important;}
        .front-card p, .front-section p {color:#405A6A !important;line-height:1.65;}
        .front-icon {
          width:42px;height:42px;line-height:42px;text-align:center;
          border-radius:12px;
          background:linear-gradient(135deg,#3478D4,#29B4A4);
          color:#FFFFFF;font-size:18px;margin-bottom:11px;
          box-shadow:0 5px 12px rgba(52,120,212,.18);
        }
        .front-section {
          background:#FFFFFF;
          border:1px solid #DCE8EF;
          border-radius:15px;
          padding:20px 22px;
          margin:13px 0;
          box-shadow:0 5px 15px rgba(30,75,98,.05);
        }
        .front-section h4 {margin-top:0;color:#1D6C78 !important;}
        .variable-chip {
          display:inline-block;
          padding:8px 12px;
          margin:4px;
          border-radius:20px;
          background:#E7F5F3;
          border:1px solid #B8E0D8;
          color:#205C62 !important;
          font-size:12px;
          font-weight:700;
        }
        .front-link {display:inline-block;margin-top:12px;color:#167CB0 !important;font-weight:800;}
        a {color:#167CB0 !important;}
        .main-footer {background:#FFFFFF !important;color:#526B7B !important;border-top:1px solid #DCE7EE;}
        @media (max-width:767px) {
          .content {padding:12px 8px;}
          .box-body {padding:13px;}
          .hero-panel {padding:23px 18px;}
          .hero-title {font-size:26px;}
        }
      "))
    ),

    tabItems(
      # ---------------- DASHBOARD ----------------
      tabItem(
        tabName = "dashboard",

        # Header utama halaman depan dengan elemen dekoratif
        fluidRow(
          box(width = 12,
            div(class = "hero-panel",
              div(class = "hero-content",
                div(class = "hero-tag", "DATA EXPLORATION  •  MULTIVARIATE ANALYSIS"),
                div(class = "hero-title", "Wholesale Customers"),
                div(class = "hero-subtitle",
                    "Eksplorasi pola pengeluaran dan karakteristik pelanggan grosir"),
                div(class = "hero-meta", tags$i(class = "fa fa-user"),
                    " Marcella Marenti Uliaty Graselia"),
                div(class = "hero-meta", tags$i(class = "fa fa-id-card"),
                    " NIM 231067021")
              )
            ),
            fluidRow(
              column(4,
                div(class = "front-card",
                  div(class = "front-icon", tags$i(class = "fa fa-bullseye")),
                  h4("Tujuan Analisis"),
                  p("Mengenali karakteristik data, distribusi, pencilan, hubungan variabel, dan model regresi.")
                )
              ),
              column(4,
                div(class = "front-card",
                  div(class = "front-icon", tags$i(class = "fa fa-database")),
                  h4("Sumber Data"),
                  p("UCI Machine Learning Repository – Wholesale Customers Dataset (Data Set 292)."),
                  tags$a(class = "front-link",
                    href = "https://archive.ics.uci.edu/dataset/292/wholesale+customers",
                    "Kunjungi sumber data ", tags$i(class = "fa fa-external-link"),
                    target = "_blank")
                )
              ),
              column(4,
                div(class = "front-card",
                  div(class = "front-icon", tags$i(class = "fa fa-chart-pie")),
                  h4("Ruang Lingkup"),
                  p("Analisis univariat, bivariat, multivariat, pemeriksaan data, dan regresi linear berganda.")
                )
              )
            ),
            div(class = "front-section",
              h4("Latar Belakang"),
              p(paste(
                "Aktivitas distribusi grosir menghasilkan data pengeluaran pelanggan",
                "pada berbagai kategori produk. Dataset Wholesale Customers dapat",
                "digunakan untuk memahami pola belanja pelanggan berdasarkan saluran",
                "penjualan, wilayah, serta kategori produk. Eksplorasi data dilakukan",
                "untuk melihat karakteristik, pola distribusi, hubungan antarvariabel,",
                "dan faktor yang berkaitan dengan pengeluaran pelanggan."
              ))
            ),
            div(class = "front-section",
              h4("Definisi Variabel"),
              p("Dataset terdiri atas dua variabel kategorik dan enam variabel numerik."),
              span(class = "variable-chip", "Channel – Saluran pelanggan"),
              span(class = "variable-chip", "Region – Wilayah pelanggan"),
              span(class = "variable-chip", "Fresh – Produk segar"),
              span(class = "variable-chip", "Milk – Produk susu"),
              span(class = "variable-chip", "Grocery – Produk grosir"),
              span(class = "variable-chip", "Frozen – Produk beku"),
              span(class = "variable-chip", "Detergents_Paper – Deterjen & kertas"),
              span(class = "variable-chip", "Delicassen – Produk delicatessen"),
              p("Channel: 1 = Horeca dan 2 = Retail. Region: 1 = Lisbon, 2 = Oporto, dan 3 = Other Region.",
                style = "margin-top:12px;color:#D7EEF5;")
            )
          )
        ),
        fluidRow(
          valueBoxOutput("total_obs", width = 4),
          valueBoxOutput("total_var", width = 4),
          valueBoxOutput("total_missing", width = 4)
        ),
        fluidRow(
          box(title = "Distribusi Channel", width = 6,
              plotOutput("grafik_channel", height = 300),
              div(class = "interpretasi", textOutput("interpretasi_channel"))),
          box(title = "Distribusi Region", width = 6,
              plotOutput("grafik_region", height = 300),
              div(class = "interpretasi", textOutput("interpretasi_region")))
        ),
        fluidRow(
          box(title = "Statistik Deskriptif", width = 12,
              DTOutput("tabel_deskriptif"),
              div(class = "interpretasi", textOutput("interpretasi_deskriptif")))
        ),
        fluidRow(
          box(title = "Informasi Dataset", width = 12,
              div(class = "interpretasi", textOutput("interpretasi_dashboard")))
        )
      ),

      # ---------------- DATA ----------------
      tabItem(
        tabName = "data",
        fluidRow(box(title = "Tampilan Dataset", width = 12, DTOutput("tabel_data"))),
        fluidRow(
          box(title = "Struktur Dataset", width = 12,
              verbatimTextOutput("struktur_data"),
              div(class = "interpretasi", textOutput("interpretasi_struktur")))
        ),
        fluidRow(
          box(title = "Pemeriksaan Missing Value", width = 6,
              DTOutput("tabel_missing"),
              div(class = "interpretasi", textOutput("interpretasi_missing"))),
          box(title = "Deteksi Pencilan (Metode IQR)", width = 6,
              DTOutput("tabel_outlier"),
              div(class = "interpretasi", textOutput("interpretasi_outlier")))
        ),
        fluidRow(
          box(title = "Catatan Pra-pemrosesan", width = 12,
              div(class = "interpretasi", textOutput("interpretasi_prep")))
        )
      ),

      # ---------------- UNIVARIAT ----------------
      tabItem(
        tabName = "univariat",
        fluidRow(
          box(title = "Pemilihan Variabel", width = 12,
              selectInput("variabel_uni", "Pilih variabel numerik:", choices = NULL))
        ),
        fluidRow(
          box(title = "Histogram", width = 6,
              plotOutput("histogram", height = 300),
              div(class = "interpretasi", textOutput("interpretasi_histogram"))),
          box(title = "Boxplot", width = 6,
              plotOutput("boxplot_uni", height = 300),
              div(class = "interpretasi", textOutput("interpretasi_boxplot_uni")))
        ),
        fluidRow(
          box(title = "Visualisasi Variabel Kategorik: Channel", width = 12,
              plotOutput("bar_uni", height = 300),
              div(class = "interpretasi", textOutput("interpretasi_bar_uni")))
        ),
        fluidRow(box(title = "Ukuran Statistik Deskriptif", width = 12, DTOutput("statistik_uni"))),
        fluidRow(
          box(title = "Interpretasi Analisis Univariat", width = 12,
              div(class = "interpretasi", textOutput("interpretasi_uni")))
        )
      ),

      # ---------------- BIVARIAT ----------------
      tabItem(
        tabName = "bivariat",
        fluidRow(
          box(
            title = "Pemilihan Variabel", width = 12,
            column(6, selectInput("variabel_x", "Variabel X:", choices = NULL)),
            column(6, selectInput("variabel_y", "Variabel Y:", choices = NULL))
          )
        ),
        fluidRow(
          box(title = "Scatter Plot dan Garis Regresi", width = 12,
              plotOutput("scatter_biv", height = 350),
              div(class = "interpretasi", textOutput("interpretasi_scatter")))
        ),
        fluidRow(
          box(title = "Boxplot Grocery Berdasarkan Channel", width = 6,
              plotOutput("boxplot_channel", height = 350),
              div(class = "interpretasi", textOutput("interpretasi_box_grocery"))),
          box(title = "Boxplot Milk Berdasarkan Channel", width = 6,
              plotOutput("boxplot_milk_channel", height = 350),
              div(class = "interpretasi", textOutput("interpretasi_box_milk")))
        ),
        fluidRow(
          box(title = "Interpretasi Analisis Bivariat", width = 12,
              div(class = "interpretasi", textOutput("interpretasi_biv")))
        )
      ),

      # ---------------- MULTIVARIAT ----------------
      tabItem(
        tabName = "multivariat",
        fluidRow(
          box(title = "Pair Plot Antarvariabel", width = 12,
              plotOutput("pairplot", height = 650),
              div(class = "interpretasi", textOutput("interpretasi_pairplot")))
        ),
        fluidRow(
          box(title = "Matriks Korelasi", width = 12,
              plotOutput("matriks_korelasi", height = 500),
              div(class = "interpretasi", textOutput("interpretasi_korelasi")))
        ),
        fluidRow(
          box(title = "Interpretasi Analisis Multivariat", width = 12,
              div(class = "interpretasi", textOutput("interpretasi_multi")))
        )
      ),

      # ---------------- REGRESI ----------------
      tabItem(
        tabName = "regresi",
        fluidRow(
          box(
            title = "Pengaturan Model Regresi", width = 12,
            selectInput("respon", "Variabel Dependen (Y):", choices = NULL),
            checkboxGroupInput("prediktor", "Variabel Independen (X):", choices = NULL),
            actionButton("hitung", "Jalankan Analisis Regresi", icon = icon("calculator"))
          )
        ),
        fluidRow(box(title = "Hasil Regresi Linear Berganda", width = 12,
                     verbatimTextOutput("hasil_regresi"))),
        fluidRow(
          box(title = "Aktual vs Prediksi", width = 6,
              plotOutput("plot_prediksi", height = 300),
              div(class = "interpretasi", textOutput("interpretasi_plot_prediksi"))),
          box(title = "Residual vs Fitted", width = 6,
              plotOutput("plot_residual", height = 300),
              div(class = "interpretasi", textOutput("interpretasi_plot_residual")))
        ),
        fluidRow(
          box(title = "Interpretasi Regresi", width = 12,
              div(class = "interpretasi", textOutput("interpretasi_regresi"))),
          box(title = "Interpretasi Koefisien dan Uji Parsial", width = 12,
              DTOutput("tabel_koefisien"),
              div(class = "interpretasi", textOutput("interpretasi_koefisien")))
        ),
        fluidRow(
          box(title = "Persamaan Model dan Keterbatasan", width = 12,
              div(class = "interpretasi",
                  textOutput("persamaan_regresi"),
                  br(),
                  textOutput("keterbatasan_regresi")))
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {

  # Membaca CSV dan memeriksa kolom
  data_reaktif <- reactive({
    req(input$file)
    df <- tryCatch(
      read.csv(input$file$datapath, check.names = FALSE,
               stringsAsFactors = FALSE),
      error = function(e) {
        validate(need(FALSE, paste("File CSV tidak dapat dibaca:", e$message)))
      }
    )

    validate(need(nrow(df) > 0, "File CSV tidak memiliki baris data."))

    if ("Channel" %in% names(df)) {
      df$Channel <- factor(
        as.character(df$Channel),
        levels = c("1", "2"),
        labels = c("Horeca", "Retail")
      )
    }
    if ("Region" %in% names(df)) {
      df$Region <- factor(
        as.character(df$Region),
        levels = c("1", "2", "3"),
        labels = c("Lisbon", "Oporto", "Other Region")
      )
    }

    df
  })

  data_num <- reactive({
    df <- data_reaktif()
    kolom_num <- names(df)[vapply(df, is.numeric, logical(1))]
    validate(need(length(kolom_num) >= 2,
                  "Dataset harus memiliki minimal dua variabel numerik."))
    df[, kolom_num, drop = FALSE]
  })

  observeEvent(data_reaktif(), {
    df <- data_reaktif()
    variabel <- names(data_num())
    req(length(variabel) >= 2)

    updateSelectInput(session, "variabel_uni",
                      choices = variabel, selected = variabel[1])
    updateSelectInput(session, "variabel_x",
                      choices = variabel, selected = variabel[1])
    updateSelectInput(session, "variabel_y",
                      choices = variabel, selected = variabel[2])
    updateSelectInput(session, "respon",
                      choices = variabel,
                      selected = if ("Grocery" %in% variabel) "Grocery" else variabel[2])
  })

  observeEvent(input$respon, {
    req(data_reaktif(), input$respon)
    semua_prediktor <- setdiff(names(data_reaktif()), input$respon)
    updateCheckboxGroupInput(
      session, "prediktor",
      choices = semua_prediktor,
      selected = head(semua_prediktor, min(3, length(semua_prediktor)))
    )
  })

  # ---------------- DASHBOARD RINGKASAN ----------------
  output$total_obs <- renderValueBox({
    req(data_reaktif())
    valueBox(nrow(data_reaktif()), "Total Observasi",
             icon = icon("users"), color = "aqua")
  })

  output$total_var <- renderValueBox({
    req(data_reaktif())
    valueBox(ncol(data_reaktif()), "Total Variabel",
             icon = icon("database"), color = "teal")
  })

  output$total_missing <- renderValueBox({
    req(data_reaktif())
    valueBox(sum(is.na(data_reaktif())), "Missing Values",
             icon = icon("exclamation-circle"), color = "blue")
  })

  output$grafik_channel <- renderPlot({
    req(data_reaktif())
    validate(need("Channel" %in% names(data_reaktif()), "Kolom Channel tidak ditemukan."))
    ggplot(data_reaktif(), aes(x = Channel, fill = Channel)) +
      geom_bar() +
      scale_fill_manual(values = c("#20AFA3", "#F4A261"), na.value = "#B8C4CE") +
      labs(x = "Channel", y = "Frekuensi") + tema_gelap() +
      theme(legend.position = "none")
  })

  output$grafik_region <- renderPlot({
    req(data_reaktif())
    validate(need("Region" %in% names(data_reaktif()), "Kolom Region tidak ditemukan."))
    ggplot(data_reaktif(), aes(x = Region, fill = Region)) +
      geom_bar() +
      scale_fill_manual(values = c("#3478D4", "#20AFA3", "#F4A261"),
                        na.value = "#B8C4CE") +
      labs(x = "Region", y = "Frekuensi") + tema_gelap() +
      theme(legend.position = "none")
  })

  output$tabel_deskriptif <- renderDT({
    req(data_num())
    d <- data_num()
    hasil <- data.frame(
      Variabel = names(d),
      Minimum = vapply(d, function(x) min(x, na.rm = TRUE), numeric(1)),
      Maksimum = vapply(d, function(x) max(x, na.rm = TRUE), numeric(1)),
      Mean = vapply(d, function(x) mean(x, na.rm = TRUE), numeric(1)),
      Median = vapply(d, function(x) median(x, na.rm = TRUE), numeric(1)),
      Standar_Deviasi = vapply(d, function(x) sd(x, na.rm = TRUE), numeric(1))
    )
    datatable(hasil, rownames = FALSE,
              options = list(pageLength = 8, scrollX = TRUE)) %>%
      formatRound(2:6, digits = 2)
  })

  output$interpretasi_channel <- renderText({
    awali_interpretasi((function() {
    req(data_reaktif())
    validate(need("Channel" %in% names(data_reaktif()), "Kolom Channel tidak ditemukan."))
    tab <- table(data_reaktif()$Channel, useNA = "ifany")
    persen <- prop.table(tab) * 100
    paste0("Distribusi Channel: ",
           paste(names(tab), as.integer(tab), "pelanggan (",
                 round(as.numeric(persen), 2), "%)", collapse = "; "),
           ". Kategori dengan frekuensi terbesar merupakan saluran pelanggan yang paling banyak terwakili.")
  
    })())
  })

  output$interpretasi_region <- renderText({
    awali_interpretasi((function() {
    req(data_reaktif())
    validate(need("Region" %in% names(data_reaktif()), "Kolom Region tidak ditemukan."))
    tab <- table(data_reaktif()$Region, useNA = "ifany")
    persen <- prop.table(tab) * 100
    paste0("Distribusi wilayah pelanggan: ",
           paste(names(tab), as.integer(tab), "pelanggan (",
                 round(as.numeric(persen), 2), "%)", collapse = "; "),
           ". Wilayah dengan frekuensi tertinggi memiliki observasi terbanyak.")
  
    })())
  })

  output$interpretasi_deskriptif <- renderText({
    awali_interpretasi((function() {
    req(data_num())
    d <- data_num()
    narasi <- vapply(names(d), function(nm) {
      x <- d[[nm]]
      paste0(nm, ": minimum ", fmt(min(x, na.rm = TRUE)),
             ", maksimum ", fmt(max(x, na.rm = TRUE)),
             ", rata-rata ", fmt(mean(x, na.rm = TRUE)),
             ", median ", fmt(median(x, na.rm = TRUE)),
             ", dan standar deviasi ", fmt(sd(x, na.rm = TRUE)), ".")
    }, character(1))
    mean_all <- vapply(d, mean, numeric(1), na.rm = TRUE)
    sd_all <- vapply(d, sd, numeric(1), na.rm = TRUE)
    paste(
      paste(narasi, collapse = " "),
      paste0("Rata-rata terbesar terdapat pada ", names(which.max(mean_all)),
             " (", fmt(max(mean_all)), "), sedangkan standar deviasi terbesar terdapat pada ",
             names(which.max(sd_all)), " (", fmt(max(sd_all)),
             "). Standar deviasi menunjukkan variasi absolut sehingga perbandingan antarvariabel perlu mempertimbangkan perbedaan skala.")
    )
  
    })())
  })

  output$interpretasi_dashboard <- renderText({
    awali_interpretasi((function() {
    req(data_reaktif())
    paste0("Dataset memiliki ", nrow(data_reaktif()), " observasi dan ",
           ncol(data_reaktif()), " variabel. Terdapat ",
           sum(is.na(data_reaktif())), " missing value. Data dapat digunakan ",
           "untuk analisis univariat, bivariat, multivariat, dan regresi.")
  
    })())
  })

  # ---------------- MENU DATA ----------------
  output$tabel_data <- renderDT({
    req(data_reaktif())
    datatable(data_reaktif(), filter = "top",
              options = list(pageLength = 10, scrollX = TRUE))
  })

  output$struktur_data <- renderPrint({
    req(data_reaktif())
    str(data_reaktif())
  })

  output$interpretasi_struktur <- renderText({
    awali_interpretasi((function() {
    req(data_reaktif())
    df <- data_reaktif()
    tipe <- vapply(df, function(x) {
      if (is.factor(x)) "kategorik (factor)"
      else if (is.numeric(x)) "numerik"
      else class(x)[1]
    }, character(1))
    paste0(
      "Struktur data menunjukkan ", nrow(df), " baris (observasi) dan ",
      ncol(df), " kolom (variabel). Variabel numerik: ",
      if (any(tipe == "numerik")) paste(names(tipe)[tipe == "numerik"], collapse = ", ") else "tidak ada",
      ". Variabel kategorik: ",
      if (any(grepl("kategorik", tipe))) paste(names(tipe)[grepl("kategorik", tipe)], collapse = ", ") else "tidak ada",
      ". Pemisahan tipe data ini memastikan Channel dan Region diperlakukan sebagai kategori, bukan besaran numerik."
    )
  
    })())
  })

  output$tabel_missing <- renderDT({
    req(data_reaktif())
    df <- data_reaktif()
    hasil <- data.frame(
      Variabel = names(df),
      Missing_Value = vapply(df, function(x) sum(is.na(x)), integer(1))
    )
    datatable(hasil, rownames = FALSE, options = list(dom = "t"))
  })

  output$tabel_outlier <- renderDT({
    req(data_num())
    d <- data_num()
    hasil <- lapply(names(d), function(nm) {
      x <- d[[nm]]
      x <- x[is.finite(x)]
      if (length(x) == 0) {
        return(data.frame(Variabel = nm, Batas_Bawah = NA_real_,
                          Batas_Atas = NA_real_, Banyak_Pencilan = 0))
      }
      q1 <- as.numeric(quantile(x, 0.25))
      q3 <- as.numeric(quantile(x, 0.75))
      iqr <- IQR(x)
      bawah <- q1 - 1.5 * iqr
      atas <- q3 + 1.5 * iqr
      data.frame(Variabel = nm, Batas_Bawah = bawah,
                 Batas_Atas = atas,
                 Banyak_Pencilan = sum(x < bawah | x > atas))
    })
    hasil <- do.call(rbind, hasil)
    datatable(hasil, rownames = FALSE,
              options = list(pageLength = 8, scrollX = TRUE)) %>%
      formatRound(c("Batas_Bawah", "Batas_Atas"), digits = 2)
  })

  output$interpretasi_prep <- renderText({
    awali_interpretasi((function() {
    req(data_reaktif())
    df <- data_reaktif()
    paste0(
      "Dataset terdiri atas ", nrow(df), " observasi dan ", ncol(df),
      " variabel. Jumlah missing value adalah ", sum(is.na(df)), ". ",
      "Pencilan pada variabel numerik diperiksa menggunakan aturan IQR ",
      "(di bawah Q1 - 1,5 x IQR atau di atas Q3 + 1,5 x IQR). ",
      "Pencilan tidak otomatis dihapus karena perlu ditinjau berdasarkan ",
      "konteks data. Variabel Channel dan Region diperlakukan sebagai ",
      "variabel kategorik; variabel pengeluaran dipertahankan dalam skala asli."
    )
  
    })())
  })

  output$interpretasi_missing <- renderText({
    awali_interpretasi((function() {
    req(data_reaktif())
    df <- data_reaktif()
    jumlah <- vapply(df, function(x) sum(is.na(x)), integer(1))
    persen <- jumlah / nrow(df) * 100
    if (sum(jumlah) == 0) {
      return(paste0("Seluruh ", ncol(df),
                    " variabel memiliki 0 missing value (0%). Tidak ditemukan data hilang pada ",
                    nrow(df), " observasi yang diunggah."))
    }
    detail <- paste0(names(jumlah)[jumlah > 0], " = ", jumlah[jumlah > 0],
                     " (", sprintf("%.2f", persen[jumlah > 0]), "%)")
    paste0("Ditemukan ", sum(jumlah), " missing value dari ",
           nrow(df) * ncol(df), " sel data (",
           sprintf("%.2f", sum(jumlah) / (nrow(df) * ncol(df)) * 100),
           "%). Rinciannya: ", paste(detail, collapse = "; "),
           ". Variabel dengan missing value terbanyak adalah ",
           names(which.max(jumlah)), " sebanyak ", max(jumlah),
           " nilai. Data hilang perlu ditangani sebelum analisis terkait.")
  
    })())
  })

  output$interpretasi_outlier <- renderText({
    awali_interpretasi((function() {
    req(data_num())
    d <- data_num()
    hasil <- lapply(names(d), function(nm) {
      x <- d[[nm]]
      x <- x[is.finite(x)]
      if (length(x) == 0) {
        return(data.frame(Variabel = nm, Bawah = NA_real_,
                          Atas = NA_real_, Pencilan = 0L))
      }
      q1 <- as.numeric(quantile(x, .25))
      q3 <- as.numeric(quantile(x, .75))
      i <- IQR(x)
      bawah <- q1 - 1.5 * i
      atas <- q3 + 1.5 * i
      data.frame(Variabel = nm, Bawah = bawah, Atas = atas,
                 Pencilan = sum(x < bawah | x > atas))
    })
    hasil <- do.call(rbind, hasil)
    total <- sum(hasil$Pencilan)
    if (total == 0) {
      return("Berdasarkan aturan IQR (Q1 - 1,5 x IQR dan Q3 + 1,5 x IQR), tidak ditemukan pencilan pada seluruh variabel numerik.")
    }
    detail <- paste0(hasil$Variabel, " = ", hasil$Pencilan,
                     " pencilan (batas bawah ", vapply(hasil$Bawah, fmt, character(1)),
                     "; batas atas ", vapply(hasil$Atas, fmt, character(1)), ")")
    paste0("Aturan IQR mendeteksi ", total, " pencilan. Rinciannya: ",
           paste(detail, collapse = "; "),
           ". Variabel dengan pencilan terbanyak adalah ",
           hasil$Variabel[which.max(hasil$Pencilan)], " sebanyak ",
           max(hasil$Pencilan), " observasi. Pencilan tidak otomatis dihapus; periksa apakah nilai ekstrem valid atau kesalahan pencatatan.")
  
    })())
  })

  # ---------------- ANALISIS UNIVARIAT ----------------
  output$histogram <- renderPlot({
    req(input$variabel_uni, data_reaktif())
    x <- data_reaktif()[[input$variabel_uni]]
    validate(need(is.numeric(x), "Pilih variabel numerik."))
    ggplot(data_reaktif(), aes(x = .data[[input$variabel_uni]])) +
      geom_histogram(bins = 30, fill = "#20AFA3", color = "#FFFFFF", alpha = .95, linewidth = .45) +
      labs(title = paste("Distribusi", input$variabel_uni),
           x = input$variabel_uni, y = "Frekuensi") + tema_gelap()
  })

  output$boxplot_uni <- renderPlot({
    req(input$variabel_uni, data_reaktif())
    ggplot(data_reaktif(), aes(y = .data[[input$variabel_uni]])) +
      geom_boxplot(fill = "#3478D4", color = "#1E5CA8", width = .48, alpha = .88) +
      labs(title = paste("Boxplot", input$variabel_uni),
           y = input$variabel_uni, x = NULL) + tema_gelap()
  })

  output$bar_uni <- renderPlot({
    req(data_reaktif())
    validate(need("Channel" %in% names(data_reaktif()), "Kolom Channel tidak ditemukan."))
    ggplot(data_reaktif(), aes(x = Channel, fill = Channel)) +
      geom_bar() +
      scale_fill_manual(values = c("#20AFA3", "#F4A261"),
                        na.value = "#B8C4CE") +
      labs(title = "Frekuensi Pelanggan Berdasarkan Channel",
           x = "Channel", y = "Frekuensi") + tema_gelap() +
      theme(legend.position = "none")
  })

  output$statistik_uni <- renderDT({
    req(input$variabel_uni, data_reaktif())
    x <- data_reaktif()[[input$variabel_uni]]
    hasil <- data.frame(
      Statistik = c("Minimum", "Maksimum", "Mean", "Median", "Standar Deviasi"),
      Nilai = c(min(x, na.rm = TRUE), max(x, na.rm = TRUE),
                mean(x, na.rm = TRUE), median(x, na.rm = TRUE),
                sd(x, na.rm = TRUE))
    )
    datatable(hasil, rownames = FALSE, options = list(dom = "t")) %>%
      formatRound("Nilai", digits = 2)
  })

  output$interpretasi_histogram <- renderText({
    awali_interpretasi((function() {
    req(input$variabel_uni, data_reaktif())
    x <- data_reaktif()[[input$variabel_uni]]
    x <- x[is.finite(x)]
    validate(need(length(x) > 1, "Data tidak cukup untuk interpretasi histogram."))
    sk <- if (sd(x) == 0) 0 else mean((x - mean(x))^3) / sd(x)^3
    bentuk <- if (sk > .5) "menceng ke kanan" else if (sk < -.5) "menceng ke kiri" else "relatif simetris"
    paste0("Histogram memperlihatkan distribusi ", input$variabel_uni,
           ". Berdasarkan koefisien kemencengan sederhana, distribusi ",
           "variabel ini ", bentuk,
           ". Batang yang lebih tinggi menunjukkan rentang nilai yang lebih sering muncul.")
  
    })())
  })

  output$interpretasi_boxplot_uni <- renderText({
    awali_interpretasi((function() {
    req(input$variabel_uni, data_reaktif())
    x <- data_reaktif()[[input$variabel_uni]]
    x <- x[is.finite(x)]
    validate(need(length(x) > 0, "Tidak ada data valid."))
    q <- quantile(x, c(.25, .5, .75))
    i <- IQR(x)
    nout <- sum(x < q[1] - 1.5 * i | x > q[3] + 1.5 * i)
    paste0("Median sebesar ", fmt(q[2]), ", kuartil bawah ", fmt(q[1]),
           ", kuartil atas ", fmt(q[3]), ", dan rentang interkuartil ",
           fmt(i), ". Terdapat ", nout,
           " kandidat pencilan menurut aturan 1,5 x IQR.")
  
    })())
  })

  output$interpretasi_bar_uni <- renderText({
    awali_interpretasi((function() {
    req(data_reaktif())
    validate(need("Channel" %in% names(data_reaktif()), "Kolom Channel tidak ditemukan."))
    tb <- sort(table(data_reaktif()$Channel), decreasing = TRUE)
    paste0("Kategori Channel terbanyak adalah ", names(tb)[1],
           " dengan ", as.integer(tb[1]),
           " pelanggan. Diagram membandingkan frekuensi kategori, bukan besarnya pengeluaran.")
  
    })())
  })

  output$interpretasi_uni <- renderText({
    awali_interpretasi((function() {
    req(input$variabel_uni, data_reaktif())
    x <- data_reaktif()[[input$variabel_uni]]
    x <- x[is.finite(x)]
    validate(need(length(x) > 1, "Data valid tidak cukup untuk diinterpretasikan."))
    sk <- if (sd(x) == 0) 0 else mean((x - mean(x))^3) / sd(x)^3
    bentuk <- if (sk > .5) "menceng ke kanan" else if (sk < -.5) "menceng ke kiri" else "relatif simetris"
    q <- quantile(x, c(.25, .5, .75))
    i <- IQR(x)
    nout <- sum(x < q[1] - 1.5 * i | x > q[3] + 1.5 * i)
    paste0("Berdasarkan ", length(x), " data valid, variabel ", input$variabel_uni,
           " memiliki minimum ", fmt(min(x)), ", maksimum ", fmt(max(x)),
           ", rata-rata ", fmt(mean(x)), ", median ", fmt(median(x)),
           ", dan standar deviasi ", fmt(sd(x)), ". Q1 = ", fmt(q[1]),
           ", Q3 = ", fmt(q[3]), ", dan IQR = ", fmt(i),
           ". Distribusi ", bentuk, " (skewness = ", fmt(sk, 3),
           "). Aturan IQR mengidentifikasi ", nout, " kandidat pencilan. ",
           if (mean(x) > median(x)) "Rata-rata lebih besar daripada median, sejalan dengan kemungkinan tarikan nilai tinggi."
           else if (mean(x) < median(x)) "Rata-rata lebih kecil daripada median, sejalan dengan kemungkinan tarikan nilai rendah."
           else "Rata-rata dan median memiliki nilai yang sama.")
  
    })())
  })

  # ---------------- ANALISIS BIVARIAT ----------------
  output$scatter_biv <- renderPlot({
    req(input$variabel_x, input$variabel_y, data_reaktif())
    validate(need(input$variabel_x != input$variabel_y,
                  "Pilih dua variabel yang berbeda."))
    ggplot(data_reaktif(),
           aes(x = .data[[input$variabel_x]],
               y = .data[[input$variabel_y]])) +
      geom_point(color = "#3478D4", alpha = .72, size = 2.2) +
      geom_smooth(method = "lm", se = TRUE, color = "#F28C65", fill = "#FCE2D8", linewidth = 1.1) +
      labs(title = paste(input$variabel_x, "dan", input$variabel_y),
           x = input$variabel_x, y = input$variabel_y) + tema_gelap()
  })

  output$boxplot_channel <- renderPlot({
    req(data_reaktif())
    validate(need(all(c("Channel", "Grocery") %in% names(data_reaktif())),
                  "Kolom Channel atau Grocery tidak ditemukan."))
    ggplot(data_reaktif(), aes(x = Channel, y = Grocery, fill = Channel)) +
      geom_boxplot(alpha = .85) +
      scale_fill_manual(values = c("#20AFA3", "#F4A261")) +
      labs(title = "Distribusi Grocery Berdasarkan Channel",
           x = "Channel", y = "Grocery") + tema_gelap() +
      theme(legend.position = "none")
  })

  output$boxplot_milk_channel <- renderPlot({
    req(data_reaktif())
    validate(need(all(c("Channel", "Milk") %in% names(data_reaktif())),
                  "Kolom Channel atau Milk tidak ditemukan."))
    ggplot(data_reaktif(), aes(x = Channel, y = Milk, fill = Channel)) +
      geom_boxplot(alpha = .85) +
      scale_fill_manual(values = c("#20AFA3", "#F4A261")) +
      labs(title = "Distribusi Milk Berdasarkan Channel",
           x = "Channel", y = "Milk") + tema_gelap() +
      theme(legend.position = "none")
  })

  output$interpretasi_scatter <- renderText({
    awali_interpretasi((function() {
    req(input$variabel_x, input$variabel_y, data_reaktif())
    validate(need(input$variabel_x != input$variabel_y,
                  "Pilih dua variabel yang berbeda."))
    x <- data_reaktif()[[input$variabel_x]]
    y <- data_reaktif()[[input$variabel_y]]
    valid <- complete.cases(x, y)
    x <- x[valid]; y <- y[valid]
    validate(need(length(x) >= 3, "Diperlukan minimal tiga observasi lengkap."))
    validate(need(sd(x) > 0 && sd(y) > 0,
                  "Korelasi tidak dapat dihitung karena salah satu variabel tidak memiliki variasi."))
    r <- cor(x, y)
    uji <- cor.test(x, y)
    kekuatan <- if (abs(r) < .20) "sangat lemah" else if (abs(r) < .40) "lemah" else if (abs(r) < .60) "sedang" else if (abs(r) < .80) "kuat" else "sangat kuat"
    arah <- if (r > 0) "positif (searah)" else if (r < 0) "negatif (berlawanan arah)" else "tidak menunjukkan hubungan linear"
    paste0("Scatter plot memuat ", length(x), " observasi lengkap untuk ",
           input$variabel_x, " dan ", input$variabel_y,
           ". Korelasi Pearson r = ", fmt(r, 3),
           " menunjukkan hubungan ", arah, " dengan kekuatan ", kekuatan,
           ". P-value = ", signif(uji$p.value, 4), ". ",
           if (uji$p.value < .05) "Hubungan linear signifikan pada taraf 5%."
           else "Belum terdapat bukti hubungan linear signifikan pada taraf 5%.",
           " Garis regresi memperlihatkan kecenderungan linear; titik yang jauh dari pola umum dapat menunjukkan observasi ekstrem.")
  
    })())
  })

  output$interpretasi_box_grocery <- renderText({
    awali_interpretasi((function() {
    req(data_reaktif())
    validate(need(all(c("Channel", "Grocery") %in% names(data_reaktif())),
                  "Kolom Channel atau Grocery tidak ditemukan."))
    z <- data_reaktif() %>%
      group_by(Channel) %>%
      summarise(n = sum(!is.na(Grocery)),
                median = median(Grocery, na.rm = TRUE),
                mean = mean(Grocery, na.rm = TRUE), .groups = "drop")
    rincian <- vapply(seq_len(nrow(z)), function(i) {
      paste0(as.character(z$Channel[i]),
             " memiliki median ", fmt(z$median[i]),
             " dan rata-rata ", fmt(z$mean[i]),
             " (n = ", z$n[i], ")")
    }, character(1))
    paste0("Perbandingan pengeluaran Grocery: ",
           paste(rincian, collapse = "; "),
           ". Perbedaan median menunjukkan perbedaan nilai tengah pengeluaran antarkelompok.")
  
    })())
  })

  output$interpretasi_box_milk <- renderText({
    awali_interpretasi((function() {
    req(data_reaktif())
    validate(need(all(c("Channel", "Milk") %in% names(data_reaktif())),
                  "Kolom Channel atau Milk tidak ditemukan."))
    z <- data_reaktif() %>%
      group_by(Channel) %>%
      summarise(n = sum(!is.na(Milk)),
                median = median(Milk, na.rm = TRUE),
                mean = mean(Milk, na.rm = TRUE), .groups = "drop")
    rincian <- vapply(seq_len(nrow(z)), function(i) {
      paste0(as.character(z$Channel[i]),
             " memiliki median ", fmt(z$median[i]),
             " dan rata-rata ", fmt(z$mean[i]),
             " (n = ", z$n[i], ")")
    }, character(1))
    paste0("Perbandingan pengeluaran Milk: ",
           paste(rincian, collapse = "; "),
           ". Kelompok dengan median lebih tinggi memiliki nilai tengah pengeluaran Milk yang lebih besar.")
  
    })())
  })

  output$interpretasi_biv <- renderText({
    awali_interpretasi((function() {
    req(input$variabel_x, input$variabel_y, data_reaktif())
    validate(need(input$variabel_x != input$variabel_y,
                  "Pilih dua variabel yang berbeda."))
    x <- data_reaktif()[[input$variabel_x]]
    y <- data_reaktif()[[input$variabel_y]]
    valid <- complete.cases(x, y)
    x <- x[valid]; y <- y[valid]
    if (length(x) < 3) return("Data lengkap kurang dari tiga observasi sehingga korelasi belum dapat dihitung.")
    validate(need(sd(x) > 0 && sd(y) > 0,
                  "Korelasi tidak dapat dihitung karena salah satu variabel tidak memiliki variasi."))
    r <- cor(x, y)
    uji <- cor.test(x, y)
    kekuatan <- if (abs(r) < .20) "sangat lemah" else if (abs(r) < .40) "lemah" else if (abs(r) < .60) "sedang" else if (abs(r) < .80) "kuat" else "sangat kuat"
    arah <- if (r > 0) "positif atau searah" else if (r < 0) "negatif atau berlawanan arah" else "tidak memiliki hubungan linear"
    paste0("Berdasarkan ", length(x), " observasi lengkap, korelasi Pearson antara ",
           input$variabel_x, " dan ", input$variabel_y,
           " adalah r = ", fmt(r, 3), ". Hubungan bersifat ", arah,
           " dengan kekuatan ", kekuatan, ". P-value = ",
           signif(uji$p.value, 4), ". ",
           if (uji$p.value < .05) "Hubungan signifikan secara statistik pada taraf 5%."
           else "Hubungan tidak signifikan secara statistik pada taraf 5%.",
           " Korelasi menunjukkan keterkaitan linear dan tidak membuktikan sebab-akibat.")
  
    })())
  })

  # ---------------- ANALISIS MULTIVARIAT ----------------
  output$pairplot <- renderPlot({
    req(data_num())
    d <- data_num()
    validate(need(ncol(d) >= 2, "Diperlukan minimal dua variabel numerik."))
    validate(need(ncol(d) <= 8, "Pair plot dibatasi maksimal 8 variabel agar mudah dibaca."))
    GGally::ggpairs(
      d,
      upper = list(continuous = GGally::wrap("cor", size = 3, color = "#1D6C78")),
      lower = list(continuous = GGally::wrap("points", alpha = .58, color = "#3478D4")),
      diag = list(continuous = GGally::wrap("densityDiag", fill = "#20AFA3", alpha = .48))
    )
  })

  output$matriks_korelasi <- renderPlot({
    req(data_num())
    d <- data_num()
    validate(need(ncol(d) >= 2, "Diperlukan minimal dua variabel numerik."))
    matriks <- cor(d, use = "pairwise.complete.obs")
    validate(need(any(is.finite(matriks)), "Matriks korelasi tidak dapat dihitung."))
    corrplot(
      matriks, method = "color", type = "upper",
      addCoef.col = "#243746", number.cex = .65,
      tl.col = "#243746", tl.srt = 45,
      col = colorRampPalette(c("#3478D4", "#FFFFFF", "#F28C65"))(200)
    )
  })

  output$interpretasi_pairplot <- renderText({
    awali_interpretasi((function() {
    req(data_num())
    cm <- cor(data_num(), use = "pairwise.complete.obs")
    diag(cm) <- NA_real_
    if (!any(is.finite(cm))) return("Korelasi pasangan variabel tidak dapat dihitung.")
    cm[!is.finite(cm)] <- NA_real_
    pos <- which(abs(cm) == max(abs(cm), na.rm = TRUE), arr.ind = TRUE)[1, ]
    paste0("Pair plot menampilkan distribusi masing-masing variabel pada diagonal dan pola hubungan pasangan variabel pada panel lainnya. Pasangan dengan korelasi absolut terbesar adalah ",
           rownames(cm)[pos[1]], " dan ", colnames(cm)[pos[2]],
           " (r = ", fmt(cm[pos[1], pos[2]], 3),
           "). Pola titik yang mengikuti garis menunjukkan kecenderungan hubungan linear.")
  
    })())
  })

  output$interpretasi_korelasi <- renderText({
    awali_interpretasi((function() {
    req(data_num())
    cm <- cor(data_num(), use = "pairwise.complete.obs")
    diag(cm) <- NA_real_
    upper <- cm
    upper[lower.tri(upper, diag = TRUE)] <- NA_real_
    vals <- upper[is.finite(upper)]
    if (length(vals) == 0) {
      return("Tidak terdapat pasangan variabel numerik dengan korelasi yang dapat dihitung.")
    }
    pos <- which(upper == max(vals, na.rm = TRUE), arr.ind = TRUE)[1, ]
    neg <- which(upper == min(vals, na.rm = TRUE), arr.ind = TRUE)[1, ]
    paste0("Matriks korelasi menggambarkan hubungan linear seluruh pasangan variabel numerik. Korelasi positif terbesar ditemukan antara ",
           rownames(cm)[pos[1]], " dan ", colnames(cm)[pos[2]],
           " sebesar r = ", fmt(cm[pos[1], pos[2]], 3),
           ". Korelasi negatif terkuat ditemukan antara ",
           rownames(cm)[neg[1]], " dan ", colnames(cm)[neg[2]],
           " sebesar r = ", fmt(cm[neg[1], neg[2]], 3),
           ". Nilai mendekati +1 menunjukkan hubungan positif kuat, mendekati -1 menunjukkan hubungan negatif kuat, dan mendekati 0 menunjukkan hubungan linear lemah. Korelasi tidak membuktikan sebab-akibat.")
  
    })())
  })

  output$interpretasi_multi <- renderText({
    awali_interpretasi((function() {
    req(data_num())
    cm <- cor(data_num(), use = "pairwise.complete.obs")
    diag(cm) <- NA_real_
    if (!any(is.finite(cm))) return("Hubungan simultan belum dapat diringkas karena korelasi tidak dapat dihitung.")
    cm[!is.finite(cm)] <- NA_real_
    pos <- which(abs(cm) == max(abs(cm), na.rm = TRUE), arr.ind = TRUE)[1, ]
    paste0("Pasangan variabel dengan korelasi terbesar secara absolut adalah ",
           rownames(cm)[pos[1]], " dan ", colnames(cm)[pos[2]],
           " dengan nilai korelasi ", fmt(cm[pos[1], pos[2]], 3),
           ". Korelasi positif menunjukkan hubungan searah, sedangkan korelasi negatif menunjukkan hubungan berlawanan arah.")
  
    })())
  })

  # ---------------- REGRESI LINEAR BERGANDA ----------------
  model_regresi <- eventReactive(input$hitung, {
    req(input$respon, input$prediktor)
    validate(need(length(input$prediktor) >= 1,
                  "Pilih minimal satu variabel prediktor."))
    df <- data_reaktif()
    pred <- setdiff(input$prediktor, input$respon)
    validate(need(length(pred) >= 1,
                  "Pilih prediktor yang berbeda dari variabel dependen."))
    validate(need(input$respon %in% names(df), "Variabel dependen tidak ditemukan."))
    validate(need(all(pred %in% names(df)), "Terdapat prediktor yang tidak ditemukan."))
    rumus <- reformulate(pred, response = input$respon)
    model <- tryCatch(
      lm(rumus, data = df, na.action = na.omit),
      error = function(e) {
        validate(need(FALSE, paste("Model regresi gagal dibentuk:", e$message)))
      }
    )
    validate(need(nobs(model) > length(coef(model)),
                  "Jumlah observasi tidak cukup dibandingkan jumlah parameter model."))
    model
  })

  output$hasil_regresi <- renderPrint({
    req(model_regresi())
    summary(model_regresi())
  })

  output$plot_prediksi <- renderPlot({
    req(model_regresi())
    model <- model_regresi()
    hasil <- data.frame(Aktual = model$model[[1]], Prediksi = fitted(model))
    ggplot(hasil, aes(x = Aktual, y = Prediksi)) +
      geom_point(color = "#3478D4", alpha = .72) +
      geom_abline(intercept = 0, slope = 1,
                  color = "#F28C65", linetype = "dashed") +
      labs(title = "Aktual vs Prediksi",
           x = "Nilai Aktual", y = "Nilai Prediksi") + tema_gelap()
  })

  output$plot_residual <- renderPlot({
    req(model_regresi())
    hasil <- data.frame(Fitted = fitted(model_regresi()),
                        Residual = residuals(model_regresi()))
    ggplot(hasil, aes(x = Fitted, y = Residual)) +
      geom_point(color = "#3478D4", alpha = .72) +
      geom_hline(yintercept = 0, color = "#F28C65", linetype = "dashed") +
      labs(title = "Residual vs Fitted",
           x = "Fitted Value", y = "Residual") + tema_gelap()
  })

  output$interpretasi_plot_prediksi <- renderText({
    awali_interpretasi((function() {
    req(model_regresi())
    model <- model_regresi()
    y <- model$model[[1]]
    pred <- fitted(model)
    rmse <- sqrt(mean((y - pred)^2, na.rm = TRUE))
    mae <- mean(abs(y - pred), na.rm = TRUE)
    paste0("Plot membandingkan nilai aktual dan prediksi. Titik yang semakin dekat dengan garis diagonal menunjukkan prediksi semakin dekat dengan nilai aktual. RMSE = ",
           fmt(rmse), " dan MAE = ", fmt(mae),
           ". Semakin kecil nilai keduanya, semakin kecil kesalahan prediksi.")
  
    })())
  })

  output$interpretasi_plot_residual <- renderText({
    awali_interpretasi((function() {
    req(model_regresi())
    r <- residuals(model_regresi())
    f <- fitted(model_regresi())
    cr <- suppressWarnings(cor(f, r))
    paste0("Residual adalah selisih nilai aktual dan prediksi. Plot yang diharapkan memperlihatkan titik menyebar acak di sekitar garis nol dengan lebar sebaran relatif konstan. Korelasi fitted-residual = ",
           fmt(cr, 3),
           ". Plot ini merupakan pemeriksaan awal dan tidak menggantikan seluruh uji asumsi regresi.")
  
    })())
  })

  output$interpretasi_regresi <- renderText({
    awali_interpretasi((function() {
    req(model_regresi())
    hasil <- summary(model_regresi())
    fs <- hasil$fstatistic
    p_model <- if (is.null(fs)) NA_real_ else pf(fs[1], fs[2], fs[3], lower.tail = FALSE)
    kesimpulan <- if (is.na(p_model)) {
      "Uji F tidak dapat dihitung."
    } else if (p_model < .05) {
      "Model regresi signifikan secara simultan pada taraf 5%."
    } else {
      "Model regresi tidak signifikan secara simultan pada taraf 5%."
    }
    ko <- hasil$coefficients
    ko <- ko[rownames(ko) != "(Intercept)", , drop = FALSE]
    signif_ko <- if (nrow(ko) > 0) rownames(ko)[ko[, 4] < .05] else character(0)
    narasi <- if (length(signif_ko) == 0) {
      "Tidak ada koefisien prediktor yang signifikan secara parsial pada taraf 5%."
    } else {
      paste0("Koefisien yang signifikan secara parsial: ",
             paste(signif_ko, collapse = ", "), ".")
    }
    paste0("R-squared = ", fmt(hasil$r.squared, 4),
           " berarti ", fmt(hasil$r.squared * 100),
           "% variasi respons dapat dijelaskan oleh prediktor dalam model. Adjusted R-squared = ",
           fmt(hasil$adj.r.squared, 4),
           ". Statistik F = ", fmt(if (is.null(fs)) NA else fs[1], 4),
           " dengan p-value = ", signif(p_model, 4),
           ". Residual standard error = ", fmt(hasil$sigma, 4),
           ". ", kesimpulan, " ", narasi,
           " Koefisien numerik ditafsirkan dengan prediktor lain konstan; koefisien kategorik menunjukkan perbedaan terhadap kategori referensi.")
  
    })())
  })

  output$tabel_koefisien <- renderDT({
    req(model_regresi())
    sm <- summary(model_regresi())
    koef <- as.data.frame(sm$coefficients)
    koef$Variabel <- rownames(koef)
    rownames(koef) <- NULL
    names(koef) <- c("Estimasi", "Std_Error", "t_hitung", "p_value", "Variabel")
    koef <- koef[, c("Variabel", "Estimasi", "Std_Error", "t_hitung", "p_value")]
    koef$Keputusan <- ifelse(koef$p_value < .05,
                             "Signifikan (p < 0,05)",
                             "Tidak signifikan (p >= 0,05)")
    datatable(koef, rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE)) %>%
      formatRound(c("Estimasi", "Std_Error", "t_hitung", "p_value"), digits = 4)
  })

  output$interpretasi_koefisien <- renderText({
    awali_interpretasi((function() {
    req(model_regresi())
    ko <- as.data.frame(summary(model_regresi())$coefficients)
    ko$Variabel <- rownames(ko)
    ko <- ko[ko$Variabel != "(Intercept)", , drop = FALSE]
    if (nrow(ko) == 0) return("Tidak terdapat prediktor yang dapat diinterpretasikan.")
    narasi <- vapply(seq_len(nrow(ko)), function(i) {
      nm <- ko$Variabel[i]
      b <- ko[i, 1]
      pv <- ko[i, 4]
      status <- if (pv < .05) "signifikan" else "tidak signifikan"
      if (grepl("Channel|Region", nm)) {
        arah <- if (b > 0) "lebih tinggi" else if (b < 0) "lebih rendah" else "sama"
        paste0(nm, " memiliki koefisien ", fmt(b, 4), " dan ", status,
               " (p-value = ", signif(pv, 4),
               "). Kategori tersebut memiliki nilai respons yang diperkirakan ",
               arah, " dibandingkan kategori referensi, dengan prediktor lain konstan.")
      } else {
        arah <- if (b > 0) "meningkat" else if (b < 0) "menurun" else "tidak berubah"
        paste0(nm, " memiliki koefisien ", fmt(b, 4), " dan ", status,
               " (p-value = ", signif(pv, 4),
               "). Kenaikan satu satuan prediktor berkaitan dengan respons yang ",
               arah, " sebesar ", fmt(abs(b), 4), " satuan, dengan prediktor lain konstan.")
      }
    }, character(1))
    paste(narasi, collapse = " ")
  
    })())
  })

  output$persamaan_regresi <- renderText({
    req(model_regresi())
    b <- coef(model_regresi())
    bagian <- paste0(
      ifelse(b[-1] >= 0, " + ", " - "),
      format(abs(b[-1]), digits = 4, scientific = FALSE, trim = TRUE),
      "(", names(b)[-1], ")",
      collapse = ""
    )
    paste0("Persamaan model: ", input$respon, " = ",
           format(b[1], digits = 4, scientific = FALSE, trim = TRUE),
           bagian, ".")
  })

  output$keterbatasan_regresi <- renderText({
    paste0(
      "Keterbatasan: model regresi menunjukkan hubungan statistik, bukan ",
      "hubungan sebab-akibat. Hasil dapat dipengaruhi pencilan, distribusi ",
      "pengeluaran yang menceng, multikolinearitas antarjenis pengeluaran, ",
      "serta asumsi linearitas, normalitas residual, homoskedastisitas, ",
      "dan independensi residual. Pemeriksaan diagnostik lanjutan tetap ",
      "diperlukan sebelum menarik kesimpulan akhir."
    )
  })
}

# ============================================================
# MENJALANKAN APLIKASI
# ============================================================
shinyApp(ui = ui, server = server)
