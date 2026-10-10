# ==============================================================================
# Dashboard: Kuantifikasi Ketidakpastian pada Regresi Nonlinier Sederhana
# Data Concrete Compressive Strength (Yeh, 1998)
# M. Faishal Amirulloh (140720260024) dan Irma Setiyani Rahayu (140720260025)
#
# Cara pakai: letakkan app.R di folder yang memuat "hasil_final", lalu Run App.
# Dashboard ini TIDAK menghitung ulang model. Semua angka dibaca dari keluaran
# skrip analisis_final_tugas_anareg.R, sehingga sama dengan angka di laporan.
# Paket: shiny, ggplot2. Paket plotly bersifat opsional (grafik 3D yang bisa diputar).
# Persamaan model ditampilkan dengan MathJax, jadi perlu sambungan internet.
# Kurva Model 1 dan Model 2 perlu berkas hasil_final/02_parameter_model_lama.csv,
# yang dibuat satu kali oleh simpan_parameter_model_lama.R. Tanpa berkas itu dashboard
# tetap jalan dan hanya menggambar kurva Model A, C, dan D.
# ==============================================================================
library(shiny)
library(ggplot2)

dir_hasil  <- "hasil_final"
path_data  <- "Concrete_Data.xls"
ada_plotly <- requireNamespace("plotly", quietly = TRUE)

# ---- Pembaca berkas dan pemformat angka ----------------------------------------
baca <- function(nama) {
  f <- file.path(dir_hasil, paste0(nama, ".csv"))
  if (!file.exists(f)) stop("Berkas hasil tidak ditemukan: ", f,
                            ". Letakkan app.R di folder yang memuat '", dir_hasil, "'.")
  read.csv(f, stringsAsFactors = FALSE)
}
ambil <- function(df, kolom, ...) {
  syarat <- list(...)
  ok <- rep(TRUE, nrow(df))
  for (nm in names(syarat)) {
    kiri <- df[[nm]]
    kanan <- syarat[[nm]]
    cocok <- if (is.numeric(kiri) && is.numeric(kanan)) abs(kiri - kanan) < 1e-8 else
      as.character(kiri) == as.character(kanan)
    ok <- ok & cocok
  }
  ok[is.na(ok)] <- FALSE
  if (sum(ok) != 1 || is.null(df[[kolom]])) return(NA_real_)
  df[[kolom]][ok]
}
a <- function(x, d = 2, kosong = "-") {
  out <- formatC(as.numeric(x), format = "f", digits = d, decimal.mark = ",")
  out <- sub("^-", "\u2212", out)
  out[!is.finite(as.numeric(x))] <- kosong
  out
}
pr   <- function(x, d = 1) ifelse(is.finite(x), paste0(a(100 * x, d), "%"), "-")
ivl  <- function(lo, hi, d = 2) paste0(a(lo, d), " \u2013 ", a(hi, d))
koma <- function(x) format(x, decimal.mark = ",", trim = TRUE, drop0trailing = TRUE)
lab_f <- function(x, peta) factor(unname(peta[as.character(x)]), levels = unname(peta))

# ---- Hasil analisis --------------------------------------------------------------
GRID  <- baca("N_grid_prediksi")
FIT   <- baca("00_fit_data_penuh")
PAR   <- baca("00_parameter_asli")
SIL   <- baca("02_silsilah_model")
VF    <- baca("05_1_variance_function")
KAP   <- baca("05_2_kappa")
PP    <- baca("05_3_profil_p")
COV   <- baca("P_coverage")
QA    <- baca("Q_coverage_age")
RW    <- baca("R_coverage_wb")
TS    <- baca("T_calibration_sharpness")
CEK   <- baca("00_cek_data")
n_komposisi <- ambil(CEK, "nilai", butir = "kelompok komposisi identik")

PI5   <- baca("N_pi_lima_titik")

# Di lima titik yang dilaporkan, PI residual bootstrap dihitung dengan simulasi yang lebih
# besar. Nilai itu dipakai di sini supaya angka dashboard sama dengan angka di laporan.
pts <- data.frame(label = c("(0.45, 28)", "(0.30, 7)", "(0.30, 90)", "(0.70, 7)", "(0.70, 90)"),
                  wb = c(0.45, 0.30, 0.30, 0.70, 0.70), Age = c(28, 7, 90, 7, 90),
                  stringsAsFactors = FALSE)
for (j in seq_len(nrow(pts))) for (L in c(80, 90, 95)) {
  i  <- which(abs(GRID$wb - pts$wb[j]) < 1e-6 & GRID$Age == pts$Age[j])
  lo <- ambil(PI5, "C_lo", level = L / 100, titik = pts$label[j])
  hi <- ambil(PI5, "C_hi", level = L / 100, titik = pts$label[j])
  if (length(i) == 1 && is.finite(lo) && is.finite(hi)) {
    GRID[i, paste0("pi_resid_lo", L)] <- lo
    GRID[i, paste0("pi_resid_hi", L)] <- hi
  }
}

berkas_rds <- file.path(dir_hasil, "objek_final.rds")
if (file.exists(berkas_rds)) {
  dat <- as.data.frame(readRDS(berkas_rds)$data)
} else if (file.exists(path_data) && requireNamespace("readxl", quietly = TRUE)) {
  dat <- as.data.frame(readxl::read_excel(path_data))
  names(dat) <- c("Cement", "Slag", "FlyAsh", "Water", "SP", "CoarseAgg", "FineAgg", "Age", "Strength")
  dat$wb <- dat$Water / (dat$Cement + dat$Slag + dat$FlyAsh)
} else {
  stop("Data tidak ditemukan: perlu '", berkas_rds, "' atau '", path_data, "' (dengan paket readxl).")
}
dat <- dat[c("Age", "Strength", "wb")]

# Mean function Model D, dari parameter tersimpan
f_D <- function(th, wb, age) th[1] * wb^(-th[2]) * (1 - exp(-th[3] * wb^(-th[5]) * age^th[4]))
th  <- sapply(c("alpha", "beta", "gamma", "p", "kappa"),
              function(nm) ambil(PAR, "estimasi", model = "D", parameter = nm))

# ---- Lima model yang dibandingkan --------------------------------------------------
# Model 1, 2, dan A (model lama) serta Model C hanya menjadi pembanding. Inferensi dan
# prediksi di dashboard seluruhnya untuk Model D.
MA    <- baca("02_model_A_lama")
XCD   <- baca("X_model_C_vs_D")
n_obs <- nrow(dat)

kode_model <- c("M1", "M2", "A", "C", "D")
nama_model <- c(M1 = "Model 1", M2 = "Model 2", A = "Model A", C = "Model C", D = "Model D")
kunci_sil  <- c(M1 = "Model 1: asimtotik, Age^1", M2 = "Model 2: logaritmik",
                A  = "Model A: Box-Cox 0.8, Age^1.7 (transformasi balik)",
                C  = "Model C + varPower", D = "Model D + varPower (model utama)")
rmse_model <- sapply(kunci_sil, function(q) ambil(SIL, "RMSE", model = q))
rss_model  <- sapply(kunci_sil, function(q) ambil(SIL, "RSS", model = q))

# Parameter Model A (skala Box-Cox 0,8) dan Model C
kA    <- setNames(MA$Estimate, MA$parameter)
thC   <- sapply(c("alpha", "beta", "gamma", "p"),
                function(nm) ambil(PAR, "estimasi", model = "C", parameter = nm))
eta_A <- function(wb, age) kA[["alpha"]] / wb^kA[["beta"]] * (1 - exp(-kA[["gamma"]] * age^1.7))

# Parameter Model 1 dan Model 2 bersifat opsional. Berkasnya dibuat oleh skrip
# simpan_parameter_model_lama.R. Tanpa berkas itu, kurva kedua model tidak digambar.
berkas_lama <- file.path(dir_hasil, "02_parameter_model_lama.csv")
LAMA <- if (file.exists(berkas_lama)) read.csv(berkas_lama, stringsAsFactors = FALSE) else NULL
par_lama <- function(m, nm) if (is.null(LAMA)) NA_real_ else ambil(LAMA, "estimasi", model = m, parameter = nm)
k1 <- sapply(c("alpha", "beta", "gamma"), function(nm) par_lama("M1", nm))
k2 <- sapply(c("alpha", "beta"), function(nm) par_lama("M2", nm))

# Nilai kurva tiap model dalam MPa
f_model <- function(kode, wb, age) switch(kode,
                                          M1 = k1[["alpha"]] / wb^k1[["beta"]] * (1 - exp(-k1[["gamma"]] * age)),
                                          M2 = k2[["alpha"]] * exp(-k2[["beta"]] * wb) * log(age),
                                          A  = pmax(0.8 * eta_A(wb, age) + 1, 0)^(1 / 0.8),        # back-transformation Box-Cox 0,8
                                          C  = f_D(c(thC, 0), wb, age),                             # Model C = Model D dengan kappa = 0
                                          D  = f_D(th, wb, age))

# Kurva hanya digambar bila parameternya mereproduksi RMSE yang tersimpan (selisih < 0,01 MPa)
cek_rmse <- function(kode) {
  r <- sqrt(mean((dat$Strength - f_model(kode, dat$wb, dat$Age))^2))
  is.finite(r) && abs(r - rmse_model[[kode]]) < 0.01
}
ada_kurva <- sapply(kode_model, cek_rmse)

# AIC pada skala MPa, supaya kelima model sebanding.
# - Model 1 dan 2: least squares dengan constant variance; AIC dari RSS tersimpan.
# - Model A: likelihood normal pada skala Box-Cox ditambah suku Jacobian (lambda - 1) * sum(log y).
#   Lambda 0,8 dan pangkat 1,7 dihitung sebagai nilai tetap, bukan parameter.
# - Model C dan D: dari fit dengan variance function (berkas 05_1).
# Semua memakai working likelihood yang mengandaikan baris independen.
aic_ls <- function(rss, k) n_obs * (log(2 * pi * rss / n_obs) + 1) + 2 * (k + 1)
rss_A  <- sum(((dat$Strength^0.8 - 1) / 0.8 - eta_A(dat$wb, dat$Age))^2)
aic_model <- c(
  M1 = aic_ls(rss_model[["M1"]], 3),
  M2 = aic_ls(rss_model[["M2"]], 2),
  A  = aic_ls(rss_A, 3) - 2 * (0.8 - 1) * sum(log(dat$Strength)),
  C  = ambil(VF, "AIC", model = "C", variance_function = "power"),
  D  = ambil(VF, "AIC", model = "D", variance_function = "power"))
d_aic       <- aic_model - aic_model[["D"]]
d_aic_lama  <- range(d_aic[c("M1", "M2", "A")])
d_aic_const <- min(aic_model[c("M1", "M2", "A")]) - ambil(VF, "AIC", model = "D", variance_function = "const")

# Persamaan (MathJax) dan keterangan tiap model
sd_sama  <- "SD <em>error</em> dianggap sama untuk semua observasi."
sd_naik  <- "SD <em>error</em> naik mengikuti rata-rata: sd(&epsilon;) = &sigma;&middot;<em>f</em><sup>&nbsp;&delta;</sup>, dengan <em>f</em> = <em>mean function</em>."
info_model <- list(
  M1 = list(
    sub = "Kurva asimtotik sederhana", kelompok = "lama",
    kurva = "pangkat dipatok 1", peran = "menentukan <em>plateau</em>",
    lemah_singkat = "terlalu cepat jenuh",
    rumus = "$$\\text{Strength} = \\frac{\\alpha}{wb^{\\beta}}\\left[1 - e^{-\\gamma\\,\\text{Age}}\\right] + \\varepsilon$$",
    baca = "&alpha; / <em>wb</em><sup>&beta;</sup> adalah <em>plateau</em>, yaitu batas atas Strength, yang mengecil bila w/b membesar. Suku dalam kurung naik dari 0 menuju 1 seiring Age, dan &gamma; mengatur seberapa cepat.",
    error = sd_sama,
    lemah = "Satu parameter &gamma; harus mengatur dua hal sekaligus: cepatnya kenaikan di awal dan kapan kurva berhenti naik. Kurva yang cukup cepat untuk beton muda menjadi terlalu cepat jenuh."),
  M2 = list(
    sub = "Kurva logaritmik", kelompok = "lama",
    kurva = "logaritma, tanpa <em>plateau</em>", peran = "menurunkan seluruh kurva",
    lemah_singkat = "nol pada Age 1 hari; naik tanpa batas",
    rumus = "$$\\text{Strength} = \\alpha\\, e^{-\\beta\\, wb}\\,\\ln(\\text{Age}) + \\varepsilon$$",
    baca = "Strength naik mengikuti logaritma Age: cepat di awal, lalu makin lambat. Faktor <em>e</em><sup>&minus;&beta;&middot;wb</sup> menurunkan seluruh kurva bila w/b membesar.",
    error = sd_sama,
    lemah = "Pada Age 1 hari nilainya nol, karena ln(1) = 0. Kurvanya juga tidak punya <em>plateau</em>, sehingga Strength naik tanpa batas."),
  A = list(
    sub = "Model 1 dengan transformasi Box-Cox (model akhir lama)", kelompok = "lama",
    kurva = "pangkat dipatok 1,7", peran = "menentukan <em>plateau</em>",
    lemah_singkat = "berhenti naik pada 28 hari; bukan rata-rata dalam MPa",
    rumus = "$$\\frac{\\text{Strength}^{0{,}8} - 1}{0{,}8} = \\frac{\\alpha}{wb^{\\beta}}\\left[1 - e^{-\\gamma\\,\\text{Age}^{1{,}7}}\\right] + \\varepsilon$$",
    baca = "Bentuknya sama dengan Model 1, dengan dua perubahan. Ruas kiri bukan Strength, melainkan Strength yang ditransformasi Box-Cox (&lambda; = 0,8) supaya keragaman <em>error</em> tampak merata. Selain itu, Age dipangkatkan 1,7.",
    error = "SD <em>error</em> dianggap sama setelah transformasi.",
    lemah = "Pangkat 1,7 dipatok, bukan diestimasi, dan kurvanya sudah berhenti naik pada sekitar 28 hari. Hasil model harus dikembalikan ke MPa lewat <em>back-transformation</em>, dan nilai itu bukan lagi rata-rata Strength. Model ini dulu dipilih karena gagal menolak uji asumsi, padahal uji itu hanya memeriksa <em>error</em>, bukan apakah kurvanya mengikuti data."),
  C = list(
    sub = "Pangkat Age diestimasi", kelompok = "baru",
    kurva = "pangkat <em>p</em> diestimasi", peran = "menentukan <em>plateau</em>",
    lemah_singkat = "pengaruh w/b sama di semua Age",
    rumus = "$$\\text{Strength} = \\frac{\\alpha}{wb^{\\beta}}\\left[1 - e^{-\\gamma\\,\\text{Age}^{\\,p}}\\right] + \\varepsilon$$",
    baca = "Sama dengan Model 1, kecuali Age dipangkatkan <em>p</em> yang diestimasi dari data. Bila <em>p</em> lebih kecil dari 1, kurva naik cepat di awal lalu melandai lama tanpa segera jenuh. Model ditulis langsung dalam MPa, tanpa transformasi.",
    error = sd_naik,
    lemah = "w/b hanya menggeser <em>plateau</em>. Semua campuran dianggap mencapai bagian yang sama dari <em>plateau</em>-nya pada Age yang sama, padahal pada data pengaruh w/b berubah menurut Age."),
  D = list(
    sub = "Model C ditambah satu parameter", kelompok = "baru",
    kurva = "pangkat <em>p</em> diestimasi", peran = "menentukan <em>plateau</em> dan <em>rate</em>",
    lemah_singkat = "lebih banyak parameter; lebih sulit di-<em>fit</em>",
    rumus = "$$\\text{Strength} = \\frac{\\alpha}{wb^{\\beta}}\\left[1 - e^{-\\gamma\\, wb^{-\\kappa}\\,\\text{Age}^{\\,p}}\\right] + \\varepsilon$$",
    baca = "Satu-satunya tambahan dari Model C adalah <em>wb</em><sup>&minus;&kappa;</sup> di dalam kurung. Tambahan ini membuat kecepatan mendekati <em>plateau</em> (<em>rate</em>) ikut bergantung pada w/b: beton dengan w/b rendah mendekati <em>plateau</em>-nya lebih cepat. Bila &kappa; = 0, Model D kembali menjadi Model C.",
    error = sd_naik,
    lemah = "Parameternya paling banyak (lima) dan lebih sulit di-<em>fit</em> pada <em>resampling</em>. Parameter <em>p</em> dan &kappa; mengatur bentuk kurva; keduanya bukan konstanta fisik pengerasan beton."))

# Menjauhkan label yang berimpit di ujung kurva
sebar <- function(y, jarak) {
  o <- order(y); z <- y[o]
  for (i in seq_along(z)[-1]) if (z[i] - z[i - 1] < jarak) z[i] <- z[i - 1] + jarak
  z <- z - (mean(z) - mean(y))
  out <- numeric(length(y)); out[o] <- z
  out
}

wb_grid  <- sort(unique(GRID$wb))
age_grid <- sort(unique(GRID$Age))
z975     <- qnorm(0.975)
prm      <- c("S_ref", "beta", "gamma28", "p", "kappa")
nama_se  <- c(se_model = "model-based", se_sandwich = "sandwich",
              se_identical = "identical composition", se_linked05 = "linked 0.5")
nama_pi  <- c(resid = "residual bootstrap", normal = "normal-theory")
kode_pi  <- c(resid = "C_residual", normal = "A_normal")
lab_age  <- c("1-7" = "1-7 hari", "14-28" = "14-28 hari", "56" = "56 hari",
              "90-100" = "90-100 hari", "120-365" = "120-365 hari")
lab_wb   <- c("<=0.40" = "w/b sampai 0,40", "0.40-0.55" = "w/b 0,40-0,55", ">0.55" = "w/b di atas 0,55")
lab_skema <- c("random-row" = "Random-row", "identical" = "Identical",
               "linked 0.5" = "Linked 0.5", "linked 1.0" = "Linked 1.0")

# ---- Tampilan grafik ---------------------------------------------------------------
w_utama <- "#0F4C81"; w_aksen <- "#C8553D"; w_hijau <- "#2A9D8F"; w_abu <- "#7B8794"; w_tinta <- "#1F2933"
theme_set(theme_minimal(base_size = 13) + theme(
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(colour = "#E4E7EB", linewidth = 0.3),
  axis.title = element_text(size = 11.5, colour = "#3E4C59"),
  strip.text = element_text(face = "bold", colour = w_tinta),
  legend.position = "bottom", legend.title = element_blank(),
  plot.margin = margin(8, 12, 6, 8)))

# ---- Gaya halaman ------------------------------------------------------------------
css <- "
body { background: #F4F7FB; color: #1F2933; font-family: 'Segoe UI', system-ui, -apple-system, 'Helvetica Neue', Arial, sans-serif; font-size: 15px; line-height: 1.6; }
.container-fluid { padding: 0; }
.sisi { position: fixed; top: 0; left: 0; bottom: 0; width: 300px; overflow-y: auto; background: #0B3558; color: #FFFFFF; padding: 24px 22px 30px; }
.isi { margin-left: 300px; padding: 26px 34px 70px; max-width: 1320px; }
.merek { font-size: 18px; font-weight: 700; line-height: 1.3; margin-bottom: 4px; }
.merek-kecil { font-size: 12.5px; color: #B9CDE2; margin-bottom: 22px; }
.sisi h4 { font-size: 11.5px; font-weight: 700; letter-spacing: 0.12em; text-transform: uppercase; color: #9FB9D4; margin: 22px 0 10px; }
.sisi .control-label { color: #FFFFFF; font-weight: 600; font-size: 13.5px; }
.sisi .radio label, .sisi .radio-inline, .sisi .checkbox label { color: #E3ECF5; font-size: 13.5px; }
.sisi .form-group { margin-bottom: 16px; }
.sisi .irs--shiny .irs-min, .sisi .irs--shiny .irs-max { background: rgba(255,255,255,0.14); color: #DCE8F4; }
.sisi .irs--shiny .irs-single { background: #C8553D; }
.sisi .irs--shiny .irs-bar { background: #C8553D; border-color: #C8553D; }
.sisi .irs--shiny .irs-handle { border-color: #C8553D; }
.sisi .irs--shiny .irs-line { background: rgba(255,255,255,0.22); border: 0; }
.sisi .irs--shiny .irs-grid-text { color: #B9CDE2; }
.sisi .bantu { font-size: 12.5px; line-height: 1.55; color: #B9CDE2; }
.sisi .bantu strong { color: #FFFFFF; font-weight: 600; }
.kepala { background: linear-gradient(135deg, #0B3558 0%, #0F4C81 60%, #1B6CA8 100%); color: #FFFFFF; border-radius: 14px; padding: 30px 34px 26px; margin-bottom: 22px; box-shadow: 0 6px 20px rgba(15,76,129,0.18); }
.kepala .atas { font-size: 12px; font-weight: 700; letter-spacing: 0.14em; text-transform: uppercase; color: #F2B8A8; margin-bottom: 10px; }
.kepala h1 { font-size: 36px; font-weight: 800; line-height: 1.18; color: #FFFFFF; margin: 0 0 12px; max-width: 900px; }
.kepala .tanya { font-size: 19px; font-weight: 600; color: #FFFFFF; margin-bottom: 8px; }
.kepala p { color: #D6E6F5; max-width: 900px; margin: 0; }
.nav-pills { margin-bottom: 20px; border-bottom: 1px solid #DDE4EC; padding-bottom: 12px; }
.nav-pills > li > a { border-radius: 8px; color: #52606D; font-weight: 600; padding: 8px 16px; }
.nav-pills > li > a:hover { background: #E8EEF5; color: #0F4C81; }
.nav-pills > li.active > a, .nav-pills > li.active > a:hover, .nav-pills > li.active > a:focus { background: #0F4C81; color: #FFFFFF; }
.kartu { background: #FFFFFF; border: 1px solid #DDE4EC; border-radius: 12px; padding: 18px 20px 16px; margin-bottom: 18px; box-shadow: 0 2px 8px rgba(31,41,51,0.04); }
.kartu h3 { font-size: 16.5px; font-weight: 700; color: #0B3558; margin: 0 0 4px; }
.kartu .ket { font-size: 13.5px; color: #52606D; margin-bottom: 10px; }
.kartu ul, .kartu ol { padding-left: 20px; margin-bottom: 0; }
.kartu li { margin-bottom: 6px; }
.petak-baris { display: grid; grid-template-columns: repeat(4, minmax(0, 1fr)); gap: 14px; margin-bottom: 18px; }
.petak { background: #FFFFFF; border: 1px solid #DDE4EC; border-radius: 12px; padding: 16px 18px 14px; box-shadow: 0 2px 8px rgba(31,41,51,0.04); }
.petak .angka { font-size: 24px; font-weight: 700; line-height: 1.2; color: #0F4C81; white-space: nowrap; }
.petak .satuan { font-size: 13px; font-weight: 600; color: #52606D; margin-left: 4px; }
.petak .ket-angka { font-size: 13px; line-height: 1.45; color: #52606D; margin-top: 6px; }
.petak.pi .angka { color: #C8553D; }
.kalimat { background: #FDF4F1; border-left: 4px solid #C8553D; border-radius: 0 10px 10px 0; padding: 14px 20px; margin-bottom: 18px; }
.awas { background: #FFF8EB; border-left: 4px solid #D98E04; border-radius: 0 10px 10px 0; padding: 12px 20px; margin-bottom: 18px; }
.awas strong { color: #A56A00; }
table.table { font-size: 14px; margin-bottom: 4px; }
table.table > thead > tr > th { background: #F4F7FB; color: #0B3558; border-bottom: 2px solid #DDE4EC; }
table.table > tbody > tr > td { border-top: 1px solid #EDF1F5; }
.catatan-kecil { font-size: 12.5px; color: #52606D; margin-top: 6px; }
table.model { width: 100%; min-width: 800px; font-size: 14px; border-collapse: collapse; }
table.model th { background: #F4F7FB; color: #0B3558; text-align: left; padding: 9px 12px; border-bottom: 2px solid #DDE4EC; }
table.model td { padding: 9px 12px; border-top: 1px solid #EDF1F5; vertical-align: top; }
table.model td.angka, table.model th.angka { text-align: right; white-space: nowrap; }
table.model tr.sorot td { background: #FDF4F1; }
table.model tr.sorot td:first-child { box-shadow: inset 4px 0 0 #C8553D; font-weight: 700; color: #C8553D; }
.gulir { overflow-x: auto; }
.lencana { display: inline-block; background: #0F4C81; color: #FFFFFF; font-size: 11px; font-weight: 700; border-radius: 10px; padding: 1px 9px; margin-left: 8px; vertical-align: middle; white-space: nowrap; }
.lencana.lama { background: #E8EEF5; color: #52606D; margin-left: 0; }
.lencana.baru { background: #E3F2EF; color: #1F7A6F; margin-left: 0; }
.langkah-baris { display: grid; grid-template-columns: repeat(3, minmax(0, 1fr)); gap: 14px; margin: 6px 0 14px; }
.langkah { display: flex; flex-direction: column; background: #F9FBFD; border: 1px solid #E4EAF1; border-radius: 10px; padding: 14px 16px 12px; font-size: 14px; }
.langkah .kepala-langkah { display: flex; align-items: flex-start; gap: 10px; }
.langkah .no { flex: none; width: 24px; height: 24px; line-height: 24px; text-align: center; border-radius: 50%; background: #0F4C81; color: #FFFFFF; font-size: 13px; font-weight: 700; }
.langkah .judul { font-weight: 700; color: #0B3558; line-height: 1.4; padding-top: 1px; }
.langkah p { margin: 8px 0 10px; }
.langkah .hasil { margin-top: auto; padding-top: 8px; border-top: 1px dashed #D3DCE6; font-weight: 700; color: #C8553D; font-size: 13.5px; }
.awas.dalam { margin-bottom: 2px; font-size: 14px; }
.pilih-model { margin: 6px 0 14px; }
.pilih-model .form-group { margin-bottom: 0; width: auto; }
.pilih-model .control-label { display: block; font-size: 16.5px; font-weight: 700; color: #0B3558; margin-bottom: 10px; }
.pilih-model .radio-inline, .pilih-model .radio-inline + .radio-inline { padding: 0; margin: 0 8px 8px 0; }
.pilih-model input[type='radio'] { position: absolute; opacity: 0; width: 0; height: 0; margin: 0; }
.pilih-model .radio-inline span { display: inline-block; border: 1px solid #DDE4EC; background: #FFFFFF; color: #52606D; font-weight: 600; border-radius: 8px; padding: 8px 16px; font-size: 14px; }
.pilih-model .radio-inline:hover span { background: #E8EEF5; color: #0F4C81; }
.pilih-model input[type='radio']:checked + span, .pilih-model .radio-inline:hover input[type='radio']:checked + span { background: #C8553D; border-color: #C8553D; color: #FFFFFF; }
.pilih-model input[type='radio']:focus-visible + span { outline: 2px solid #0F4C81; outline-offset: 2px; }
.pilih-model .radio-inline:nth-child(1)::before, .pilih-model .radio-inline:nth-child(4)::before { font-size: 11.5px; font-weight: 700; letter-spacing: 0.1em; text-transform: uppercase; color: #7B8794; margin-right: 10px; }
.pilih-model .radio-inline:nth-child(1)::before { content: 'Model lama'; }
.pilih-model .radio-inline:nth-child(4)::before { content: 'Model baru'; margin-left: 10px; }
.kartu.sorot { border-color: #C8553D; box-shadow: inset 4px 0 0 #C8553D, 0 2px 8px rgba(31,41,51,0.04); }
.kartu h5 { font-size: 11.5px; font-weight: 700; letter-spacing: 0.1em; text-transform: uppercase; color: #7B8794; margin: 14px 0 4px; }
.rumus { background: #F9FBFD; border: 1px solid #EDF1F5; border-radius: 8px; padding: 4px 12px; margin: 8px 0 2px; min-height: 70px; overflow-x: auto; overflow-y: hidden; font-size: 95%; }
.tak-patah td:nth-child(n+3), .tak-patah th:nth-child(n+3) { white-space: nowrap; }
@media (max-width: 1250px) { .dua-kolom > [class*='col-sm-'] { width: 100%; float: none; } }
@media (max-width: 1100px) { .petak-baris { grid-template-columns: repeat(2, minmax(0, 1fr)); } .langkah-baris { grid-template-columns: minmax(0, 1fr); } }
@media (max-width: 900px) { .sisi { position: static; width: auto; } .isi { margin-left: 0; padding: 20px 16px 50px; } }
"
kartu <- function(judul, ket = NULL, ...) div(class = "kartu", h3(judul), if (!is.null(ket)) div(class = "ket", ket), ...)
miring <- function(x) tags$em(x)

# ---- Kartu "Mengapa Model D" (angka dibaca dari hasil analisis) --------------------
langkah <- function(no, judul, isi, hasil) div(class = "langkah",
                                               div(class = "kepala-langkah", span(class = "no", no), span(class = "judul", judul)),
                                               p(HTML(isi)), div(class = "hasil", hasil))
x_cd <- function(kolom, m) ambil(XCD, kolom, model = m)

kartu_alasan <- kartu("Mengapa Model D menjadi model akhir?",
                      "Tiga langkah keputusan, dibaca berurutan.",
                      div(class = "langkah-baris",
                          langkah(1, "Kurva Age harus mengikuti data",
                                  paste0("Pada data, Strength masih naik setelah 28 hari. Bila pangkat Age pada Model D dipatok 1 (seperti Model 1) atau 1,7 (seperti Model A), <em>deviance</em> memburuk ",
                                         a(ambil(PP, "deviance", p = 1), 0), " dan ", a(ambil(PP, "deviance", p = 1.7), 0),
                                         ", jauh di atas patokan 3,84. Model 2 tidak punya <em>plateau</em>. AIC ketiga model lama ",
                                         a(d_aic_lama[1], 0), " sampai ", a(d_aic_lama[2], 0), " poin lebih buruk."),
                                  "Model 1, Model 2, dan Model A gugur."),
                          langkah(2, "Pengaruh w/b boleh berubah menurut Age",
                                  paste0("Model C dan Model D sama-sama mengestimasi pangkat Age. Bedanya, Model D membiarkan <em>rate</em> bergantung pada w/b lewat &kappa;. Menghapus &kappa; memperburuk <em>deviance</em> sebesar ",
                                         a(ambil(KAP, "deviance_kappa0", struktur_galat = "varPower"), 1), " dan AIC ", a(d_aic[["C"]], 0), " poin."),
                                  "Model D lebih cocok daripada Model C."),
                          langkah(3, "Diuji pada data yang tidak dipakai membangun model",
                                  paste0("Pada <em>cross-validation</em> (skema <em>linked 0.5</em>), RMSE Model D ", a(x_cd("RMSE", "D"), 2),
                                         " MPa dan Model C ", a(x_cd("RMSE", "C"), 2), " MPa. PI 95% <em>normal-theory</em> Model D lebih sempit (",
                                         a(x_cd("lebar_PI95", "D"), 1), " lawan ", a(x_cd("lebar_PI95", "C"), 1), " MPa) pada <em>coverage</em> yang setara (",
                                         pr(x_cd("cov95", "D")), " dan ", pr(x_cd("cov95", "C")), "). Di tepi data (w/b 0,235 dan Age 365 hari), Model C memberi ",
                                         a(x_cd("prediksi_wb0235_Age365", "C"), 0), " MPa, jauh di atas Strength terbesar yang teramati (",
                                         a(max(dat$Strength), 1), " MPa); Model D memberi ", a(x_cd("prediksi_wb0235_Age365", "D"), 0), " MPa."),
                                  "Model D dipakai sebagai model akhir.")),
                      div(class = "awas dalam", HTML(paste0(
                        "<strong>Dua catatan.</strong> Keputusan ini tidak bertumpu pada <em>p-value</em> uji asumsi: gagal menolak uji normalitas atau homoskedastisitas bukan bukti bahwa kurvanya benar. ",
                        "Model D juga dipilih setelah melihat data, sehingga ukuran <em>in-sample</em> pada tabel cenderung optimistis. Karena itu langkah 3 dan tab Validasi memakai data <em>out-of-sample</em>."))))

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  title = "Kuantifikasi Ketidakpastian - Concrete Compressive Strength",
  tags$head(tags$style(HTML(css))),
  withMathJax(),
  
  div(class = "sisi",
      div(class = "merek", "Pengaturan"),
      div(class = "merek-kecil", "Berlaku untuk semua tab"),
      
      h4("Skenario beton"),
      sliderInput("wb", "w/b (water-to-binder ratio)", min = min(wb_grid), max = max(wb_grid),
                  value = 0.45, step = 0.025),
      selectInput("age", "Age (hari)", choices = age_grid, selected = 28),
      div(class = "bantu",
          tags$strong(paste0("Mengapa hanya ", length(age_grid), " pilihan Age?")), tags$br(),
          "Rata-rata dan CI bisa dihitung di Age berapa pun dari rumus. Yang membatasi adalah PI ",
          tags$em("residual bootstrap"), ": ia hasil simulasi, dan baru dihitung pada ",
          length(age_grid), " nilai Age (", paste(age_grid, collapse = ", "), " hari)."),
      
      h4("Interval"),
      radioButtons("level", "Tingkat PI", choices = c("80%" = "80", "90%" = "90", "95%" = "95"),
                   selected = "95", inline = TRUE),
      radioButtons("pi_metode", "Cara menghitung PI",
                   choices = c("Residual bootstrap (utama)" = "resid", "Normal-theory" = "normal")),
      selectInput("se", "Asumsi dependensi untuk SE dan CI",
                  choices = c("Model-based (baris independen)" = "se_model",
                              "Sandwich (baris independen)" = "se_sandwich",
                              "Identical composition" = "se_identical",
                              "Linked 0.5 (paling hati-hati)" = "se_linked05"),
                  selected = "se_linked05"),
      div(class = "bantu",
          tags$strong("Mengapa perlu asumsi dependensi?"), tags$br(),
          "SE dan CI mengukur seberapa teliti rata-rata diketahui, dan itu bergantung pada berapa banyak informasi yang benar-benar independen. Data ini memuat ",
          nrow(dat), " baris, tetapi hanya ", n_komposisi, " komposisi campuran. Baris dari komposisi yang sama atau mirip cenderung meleset ke arah yang sama, jadi bukan ulangan yang independen. ",
          "Kalau semua baris dianggap independen, SE terlalu kecil dan CI terlalu sempit. ",
          "Data tidak memuat penanda ", tags$em("batch"), ", jadi tidak ada satu asumsi yang pasti benar. Pilihan ini menunjukkan seberapa jauh CI berubah; PI hampir tidak terpengaruh."),
      
      h4("Cara pakai"),
      div(class = "bantu",
          "Pilih w/b dan Age. Semua tab mengikuti pilihan di panel ini; tab Model hanya memakai w/b. ",
          "Angka dibaca dari hasil analisis yang sama dengan laporan, jadi tidak ada yang dihitung ulang.")
  ),
  
  div(class = "isi",
      div(class = "kepala",
          div(class = "atas", "Dashboard \u00b7 Concrete Compressive Strength (Yeh, 1998)"),
          h1("Kuantifikasi Ketidakpastian pada Regresi Nonlinier Sederhana"),
          div(class = "tanya", "Seberapa yakin model terhadap prediksinya?"),
          p("Dashboard ini menampilkan prediksi Strength beton bersama ketidakpastiannya: ",
            miring("confidence interval"), " (CI) untuk rata-rata dan ", miring("prediction interval"),
            " (PI) untuk satu observasi baru. Model: regresi nonlinier dengan dua prediktor, w/b dan Age, pada ",
            nrow(dat), " observasi (Yeh, 1998).")),
      
      tabsetPanel(id = "tab", type = "pills",
                  
                  # ---------------------------------------------------------------- Tab 1
                  tabPanel("Model",
                           kartu("Lima model yang dibandingkan",
                                 HTML("Tiga model lama dari tahap awal project dan dua model baru. Baris merah bata: model yang sedang dilihat (pilih di bawah). Lencana biru: model akhir."),
                                 div(class = "gulir", uiOutput("tab_lima")),
                                 div(class = "catatan-kecil", HTML(paste0(
                                   "RMSE dihitung <em>in-sample</em> pada skala MPa; makin kecil makin dekat ke data. Selisih AIC dihitung terhadap Model D; makin besar, makin lemah dukungan data untuk model itu. ",
                                   "AIC dihitung pada skala MPa supaya sebanding: untuk Model A ditambahkan suku Jacobian transformasi Box-Cox, dengan &lambda; = 0,8 dan pangkat 1,7 sebagai nilai tetap. ",
                                   "AIC memakai <em>working likelihood</em> yang mengandaikan baris independen, jadi dibaca sebagai ukuran besarnya bukti, bukan uji formal. ",
                                   "Model lama memakai <em>constant variance</em> dan model baru memakai <em>variance function</em>; bila Model D juga memakai <em>constant variance</em>, AIC-nya masih ",
                                   a(d_aic_const, 0), " poin lebih baik daripada model lama terbaik.")))),
                           kartu_alasan,
                           div(class = "pilih-model",
                               radioButtons("model_pilih", "Lihat persamaan dan kurva tiap model",
                                            choices = setNames(kode_model, nama_model[kode_model]), selected = "D", inline = TRUE)),
                           fluidRow(class = "dua-kolom",
                                    column(5, uiOutput("kartu_model")),
                                    column(7, kartu(textOutput("judul_kurva", inline = TRUE), uiOutput("ket_kurva"),
                                                    plotOutput("plot_model", height = "400px")))
                           )
                  ),
                  
                  # ---------------------------------------------------------------- Tab 2
                  tabPanel("Inferensi",
                           div(class = "kalimat", HTML("Semua inferensi di tab ini untuk <strong>Model D</strong>, model akhir. Empat model lain hanya menjadi pembanding di tab Model.")),
                           fluidRow(
                             column(7,
                                    kartu("Parameter Model D", uiOutput("ket_param"), div(class = "tak-patah", tableOutput("tab_param"))),
                                    kartu("Uji parsial",
                                          "Hipotesis yang menyederhanakan model. Rasio Wald = (estimate - nilai hipotesis) / SE; patokannya 1,96.",
                                          div(class = "tak-patah", tableOutput("tab_parsial")))),
                             column(5,
                                    kartu("Mengapa SE berbeda-beda?",
                                          "SE tiap parameter relatif terhadap SE model-based. Makin longgar asumsi independensinya, makin besar SE.",
                                          plotOutput("plot_se", height = "300px")),
                                    kartu("Uji simultan",
                                          "Model D dibandingkan dengan model tanpa prediktor (rata-rata konstan).",
                                          div(class = "tak-patah", tableOutput("tab_simultan")),
                                          div(class = "catatan-kecil", "Distribusi acuannya hanya pendekatan dan kedua statistik mengandaikan baris independen. Besarnya statistik membuat kesimpulan tidak bergantung pada ketelitian itu.")))
                           )
                  ),
                  
                  # ---------------------------------------------------------------- Tab 3
                  tabPanel("Prediksi",
                           uiOutput("peringatan"),
                           uiOutput("petak"),
                           uiOutput("kalimat"),
                           fluidRow(
                             column(6, kartu("Sepanjang w/b, pada Age terpilih",
                                             "Pita biru: PI. Pita merah bata: CI untuk rata-rata. Titik kelabu: observasi pada Age itu.",
                                             plotOutput("plot_wb", height = "360px"))),
                             column(6, kartu("Sepanjang Age, pada w/b terpilih",
                                             "Sumbu Age berskala akar. Daerah kelabu: Age di atas 100 hari, tempat data sangat sedikit.",
                                             plotOutput("plot_age", height = "360px")))
                           ),
                           kartu("Cara membaca",
                                 tags$ul(
                                   tags$li(HTML("<strong>CI</strong> menjawab: seberapa teliti <em>rata-rata</em> Strength diketahui? Lebarnya bergantung pada asumsi dependensi yang dipilih di panel kiri.")),
                                   tags$li(HTML("<strong>PI</strong> menjawab: di mana <em>satu</em> hasil uji baru akan jatuh? PI jauh lebih lebar karena memuat variasi antarobservasi.")),
                                   tags$li("Menambah data sejenis mempersempit CI, tetapi hampir tidak mengubah PI.")))
                  ),
                  
                  # ---------------------------------------------------------------- Tab 4
                  tabPanel("Permukaan 3D",
                           fluidRow(
                             column(8, kartu("Permukaan menurut w/b dan Age",
                                             if (ada_plotly) "Seret untuk memutar, gulir untuk memperbesar. Titik merah bata: skenario terpilih."
                                             else "Atur sudut pandang dengan dua penggeser. Titik merah bata: skenario terpilih.",
                                             if (ada_plotly) plotly::plotlyOutput("permukaan", height = "520px")
                                             else tagList(
                                               fluidRow(column(6, sliderInput("theta", "Putar mendatar", min = -180, max = 180, value = 35, step = 5)),
                                                        column(6, sliderInput("phi", "Sudut pandang dari atas", min = 0, max = 90, value = 24, step = 2))),
                                               plotOutput("permukaan_statis", height = "500px")))),
                             column(4,
                                    kartu("Yang ditampilkan", NULL,
                                          radioButtons("jenis", NULL,
                                                       choices = c("Rata-rata Strength (mean function)" = "mean",
                                                                   "Batas bawah PI" = "bawah", "Batas atas PI" = "atas",
                                                                   "Lebar PI" = "lebar")),
                                          checkboxInput("tampil_obs", "Tampilkan observasi", value = FALSE),
                                          uiOutput("ket_permukaan")),
                                    kartu("Interpretasi", NULL,
                                          tags$ol(
                                            tags$li("Permukaan rata-rata hanya naik ke satu arah: Strength naik bila Age bertambah dan turun bila w/b bertambah. Tidak ada puncak di tengah."),
                                            tags$li("Maksimumnya karena itu selalu di sudut: w/b terkecil dan Age terbesar. Ini maksimum matematis di dalam batas grid, bukan kombinasi terbaik dari data."),
                                            tags$li("Sudut itu tidak punya data: tidak ada observasi dengan w/b sampai 0,30 dan Age di atas 100 hari."),
                                            tags$li("Pilih \"Lebar PI\" untuk melihat bahwa ketidakpastian paling besar justru di sudut yang sama."))))
                           )
                  ),
                  
                  # ---------------------------------------------------------------- Tab 5
                  tabPanel("Validasi",
                           kartu("Apakah PI bisa dipercaya?",
                                 HTML("PI diuji pada data yang tidak dipakai untuk membangunnya (10-<em>fold</em> <em>cross-validation</em>). PI yang baik memuat observasi sesering tingkat nominalnya. Cara menghitung PI mengikuti pilihan di panel kiri."),
                                 plotOutput("plot_cov", height = "300px")),
                           fluidRow(
                             column(6, kartu("Coverage menurut Age", uiOutput("ket_cov_age"), plotOutput("plot_cov_age", height = "300px"))),
                             column(6, kartu("Coverage menurut w/b", uiOutput("ket_cov_wb"), plotOutput("plot_cov_wb", height = "300px")))
                           ),
                           kartu("Tepat saja belum cukup: interval juga harus sempit",
                                 "Lebar dalam MPa. Interval score menggabungkan ketepatan dan lebar; makin kecil makin baik.",
                                 tableOutput("tab_tajam"))
                  ),
                  
                  # ---------------------------------------------------------------- Tab 6
                  tabPanel("Catatan",
                           fluidRow(
                             column(6, kartu("Yang tidak boleh disimpulkan", NULL,
                                             tags$ul(
                                               tags$li(HTML("<strong>Sebab-akibat.</strong> Model menggambarkan asosiasi pada data observasional dari banyak sumber.")),
                                               tags$li(HTML("<strong>Makna fisik parameter.</strong> p dan &kappa; mengatur bentuk kurva, bukan konstanta pengerasan beton.")),
                                               tags$li(HTML("<strong>Rancangan campuran.</strong> Titik maksimum permukaan adalah optimum matematis, bukan rekomendasi rekayasa.")),
                                               tags$li(HTML("<strong>Ketepatan di setiap kondisi.</strong> Coverage terbukti secara rata-rata, belum untuk tiap Age.")),
                                               tags$li(HTML("<strong>Ekstrapolasi.</strong> Age di atas 100 hari berada di wilayah yang datanya sangat sedikit."))))),
                             column(6,
                                    kartu("Tentang dashboard ini", NULL,
                                          p("Dashboard ini pelengkap laporan \"Kuantifikasi Ketidakpastian pada Regresi Nonlinier Sederhana\". Semua angka dibaca dari keluaran skrip analisis yang sama dengan laporan."),
                                          p(HTML(paste0("Mengapa hanya ", length(age_grid), " pilihan Age? Rata-rata dan CI bisa dihitung di Age berapa pun dari rumus. Yang membatasi adalah PI <em>residual bootstrap</em>: ia hasil simulasi, dan baru dihitung pada ", length(age_grid), " nilai Age (", paste(age_grid, collapse = ", "), " hari). Pilihan w/b dibatasi dengan alasan yang sama."))),
                                          p(HTML("CI dihitung sebagai interval Wald dari SE yang dipilih, sehingga bisa berbeda tipis dari CI <em>bootstrap</em> di laporan.")),
                                          p("Analisis ini frekuentis. Tidak ada bagian yang Bayesian.")),
                                    kartu("Penyusun", NULL,
                                          p(HTML("M. Faishal Amirulloh (NPM 140720260024)<br>Irma Setiyani Rahayu (NPM 140720260025)")),
                                          p("Tugas Analisis Regresi Tingkat Lanjut, Magister Statistika Terapan, Universitas Padjadjaran."),
                                          p("Data: Yeh, I.-C. (1998). Cement and Concrete Research, 28(12), 1797-1808.")))
                           )
                  )
      )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  kol_pi <- reactive(list(lo = paste0("pi_", input$pi_metode, "_lo", input$level),
                          hi = paste0("pi_", input$pi_metode, "_hi", input$level)))
  
  # Satu baris grid prediksi untuk skenario terpilih
  titik <- reactive({
    i <- which(abs(GRID$wb - input$wb) < 1e-6 & GRID$Age == as.numeric(input$age))
    validate(need(length(i) == 1, "Kombinasi w/b dan Age ini tidak ada di grid prediksi."))
    g  <- GRID[i, ]
    se <- g[[input$se]]
    list(wb = g$wb, age = g$Age, f = g$f, sd = g$sd_new, se = se,
         ci_lo = g$f - z975 * se, ci_hi = g$f + z975 * se,
         pi_lo = g[[kol_pi()$lo]], pi_hi = g[[kol_pi()$hi]])
  })
  
  # ------------------------------------------------------------------ Tab 1: Model
  output$tab_lima <- renderUI({
    m <- input$model_pilih
    baris <- vapply(kode_model, function(k) {
      i <- info_model[[k]]
      paste0("<tr", if (identical(k, m)) " class='sorot'" else "", ">",
             "<td>", nama_model[[k]], if (k == "D") "<span class='lencana'>model akhir</span>" else "", "</td>",
             "<td><span class='lencana ", i$kelompok, "'>", i$kelompok, "</span></td>",
             "<td>", i$kurva, "</td><td>", i$peran, "</td>",
             "<td class='angka'>", a(rmse_model[[k]], 2), "</td>",
             "<td class='angka'>", if (k == "D") "0 (acuan)" else paste0("+", a(d_aic[[k]], 0)), "</td>",
             "<td>", i$lemah_singkat, "</td></tr>")
    }, character(1))
    HTML(paste0(
      "<table class='model'><thead><tr><th>Model</th><th>Kelompok</th><th>Kurva Age</th><th>Peran w/b</th>",
      "<th class='angka'>RMSE (MPa)</th><th class='angka'>Selisih AIC</th><th>Kelemahan utama</th></tr></thead><tbody>",
      paste(baris, collapse = ""), "</tbody></table>"))
  })
  
  output$kartu_model <- renderUI({
    m <- input$model_pilih
    i <- info_model[[m]]
    div(class = "kartu sorot",
        h3(nama_model[[m]], if (m == "D") span(class = "lencana", "model akhir")),
        div(class = "ket", i$sub),
        withMathJax(div(class = "rumus", i$rumus)),
        h5("Cara membaca"), div(HTML(i$baca)),
        h5(HTML("Keragaman <em>error</em>")), div(HTML(i$error)),
        h5("Kelemahan"), div(HTML(i$lemah)),
        div(class = "catatan-kecil", HTML("Lambang &alpha;, &beta;, dan &gamma; dipakai di semua model, tetapi nilainya berbeda di tiap model.")))
  })
  
  output$judul_kurva <- renderText(paste0(
    "Kurva ", if (all(ada_kurva)) "kelima model" else "model", " pada w/b ", koma(input$wb)))
  
  output$ket_kurva <- renderUI({
    m <- input$model_pilih
    rasio <- if (ada_kurva[[m]]) f_model(m, input$wb, 365) / f_model(m, input$wb, 28) else NA_real_
    HTML(paste0(
      "Merah bata: ", nama_model[[m]], ". Kelabu: model lain. Titik: observasi dengan w/b ",
      koma(input$wb - 0.03), " sampai ", koma(input$wb + 0.03), " (w/b mengikuti panel kiri). ",
      if (is.finite(rasio)) paste0("Menurut ", nama_model[[m]], ", nilai kurva pada Age 365 hari adalah <strong>",
                                   a(rasio, 2), " kali</strong> nilai pada 28 hari.") else "",
      if (!all(ada_kurva)) " Kurva Model 1 dan Model 2 belum tersedia: jalankan <code>simpan_parameter_model_lama.R</code> satu kali, lalu muat ulang dashboard." else ""))
  })
  
  output$plot_model <- renderPlot({
    m    <- input$model_pilih
    kode <- kode_model[ada_kurva[kode_model]]
    ag   <- 1:365
    kur  <- do.call(rbind, lapply(kode, function(k)
      data.frame(kode = k, Age = ag, f = f_model(k, input$wb, ag), stringsAsFactors = FALSE)))
    kur$terpilih <- kur$kode == m
    obs   <- dat[abs(dat$wb - input$wb) <= 0.03, ]
    ujung <- kur[kur$Age == 365, ]
    ujung$label   <- unname(nama_model[ujung$kode])
    ujung$tebal   <- ifelse(ujung$terpilih, "bold", "plain")
    ujung$y_label <- sebar(ujung$f, 0.055 * diff(range(c(kur$f, obs$Strength))))
    ggplot() +
      annotate("rect", xmin = 100, xmax = 365, ymin = -Inf, ymax = Inf, fill = "#EEF2F6") +
      geom_point(data = obs, aes(Age, Strength), colour = "#3E4C59", alpha = 0.3, size = 1.2) +
      geom_line(data = kur[!kur$terpilih, ], aes(Age, f, group = kode), colour = "#9AA5B1", linewidth = 0.6) +
      geom_line(data = kur[kur$terpilih, ], aes(Age, f), colour = w_aksen, linewidth = 1.5) +
      geom_text(data = ujung, aes(365, y_label, label = label, colour = terpilih, fontface = tebal),
                hjust = -0.12, size = 3.5, show.legend = FALSE) +
      scale_colour_manual(values = c("TRUE" = w_aksen, "FALSE" = "#7B8794")) +
      scale_x_sqrt(breaks = c(1, 7, 28, 56, 100, 180, 365)) +
      coord_cartesian(clip = "off") +
      labs(x = "Age (hari, skala akar)", y = "Strength (MPa)") +
      theme(plot.margin = margin(8, 58, 6, 8))
  }, res = 96)
  
  # ------------------------------------------------------------------ Tab 3: Prediksi
  output$peringatan <- renderUI({
    if (as.numeric(input$age) > 100)
      div(class = "awas", HTML("<strong>Peringatan.</strong> Age di atas 100 hari: datanya sangat sedikit dan hanya dari jenis campuran tertentu. Prediksi di sini lebih banyak ditentukan oleh bentuk kurva daripada oleh data."))
  })
  
  output$petak <- renderUI({
    t <- titik()
    div(class = "petak-baris",
        div(class = "petak", div(class = "angka", a(t$f, 1), span(class = "satuan", "MPa")),
            div(class = "ket-angka", HTML("<em>Estimate</em> rata-rata Strength"))),
        div(class = "petak", div(class = "angka", ivl(t$ci_lo, t$ci_hi, 1)),
            div(class = "ket-angka", HTML(paste0("CI 95% untuk rata-rata (Wald, SE <em>", nama_se[[input$se]], "</em>)")))),
        div(class = "petak pi", div(class = "angka", ivl(t$pi_lo, t$pi_hi, 1)),
            div(class = "ket-angka", HTML(paste0("PI ", input$level, "% untuk satu observasi baru (<em>", nama_pi[[input$pi_metode]], "</em>)")))),
        div(class = "petak", div(class = "angka", a(t$sd, 1), span(class = "satuan", "MPa")),
            div(class = "ket-angka", "SD satu observasi di titik ini")))
  })
  
  output$kalimat <- renderUI({
    t <- titik()
    div(class = "kalimat", HTML(paste0(
      "Untuk beton dengan w/b ", koma(t$wb), " pada Age ", t$age, " hari, rata-rata Strength diestimasi <strong>",
      a(t$f, 1), " MPa</strong> (CI 95% ", ivl(t$ci_lo, t$ci_hi, 1), "). Satu observasi baru diperkirakan jatuh di antara <strong>",
      a(t$pi_lo, 1), " dan ", a(t$pi_hi, 1), " MPa</strong> (PI ", input$level, "%). PI ini ",
      a((t$pi_hi - t$pi_lo) / (t$ci_hi - t$ci_lo), 0), " kali lebih lebar daripada CI.")))
  })
  
  output$plot_wb <- renderPlot({
    t <- titik()
    g <- GRID[GRID$Age == t$age, ]
    g <- g[order(g$wb), ]
    g$pi_lo <- g[[kol_pi()$lo]]; g$pi_hi <- g[[kol_pi()$hi]]
    g$ci_lo <- g$f - z975 * g[[input$se]]; g$ci_hi <- g$f + z975 * g[[input$se]]
    obs <- dat[dat$Age == t$age & dat$wb >= min(wb_grid) & dat$wb <= max(wb_grid), ]
    nm  <- c(paste0("PI ", input$level, "%"), "CI 95% rata-rata")
    ggplot(g, aes(wb)) +
      geom_ribbon(aes(ymin = pi_lo, ymax = pi_hi, fill = nm[1])) +
      geom_point(data = obs, aes(wb, Strength), colour = "#3E4C59", alpha = 0.35, size = 1.2) +
      geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi, fill = nm[2])) +
      geom_line(aes(y = f), colour = "#FFFFFF", linewidth = 0.5) +
      annotate("segment", x = t$wb, xend = t$wb, y = t$pi_lo, yend = t$pi_hi, colour = w_tinta, linewidth = 0.9) +
      annotate("point", x = t$wb, y = t$f, colour = w_tinta, size = 3.5) +
      scale_fill_manual(values = setNames(c("#C6D9EC", w_aksen), nm), breaks = nm) +
      scale_x_continuous(labels = koma) +
      labs(x = "w/b", y = "Strength (MPa)")
  }, res = 96)
  
  output$plot_age <- renderPlot({
    t <- titik()
    g <- GRID[abs(GRID$wb - t$wb) < 1e-6, ]
    g <- g[order(g$Age), ]
    g$pi_lo <- g[[kol_pi()$lo]]; g$pi_hi <- g[[kol_pi()$hi]]
    kur <- data.frame(Age = 3:365)
    kur$f <- f_D(th, t$wb, kur$Age)
    obs <- dat[abs(dat$wb - t$wb) <= 0.03 & dat$Age >= 3, ]
    nm  <- paste0("PI ", input$level, "%")
    ggplot() +
      annotate("rect", xmin = 100, xmax = 365, ymin = -Inf, ymax = Inf, fill = "#EEF2F6") +
      geom_ribbon(data = g, aes(Age, ymin = pi_lo, ymax = pi_hi, fill = nm)) +
      geom_point(data = obs, aes(Age, Strength), colour = "#3E4C59", alpha = 0.35, size = 1.2) +
      geom_line(data = kur, aes(Age, f), colour = w_utama, linewidth = 0.9) +
      annotate("segment", x = t$age, xend = t$age, y = t$pi_lo, yend = t$pi_hi, colour = w_tinta, linewidth = 0.9) +
      annotate("point", x = t$age, y = t$f, colour = w_tinta, size = 3.5) +
      scale_fill_manual(values = setNames("#C6D9EC", nm)) +
      scale_x_sqrt(breaks = c(3, 7, 28, 56, 100, 180, 365)) +
      labs(x = "Age (hari, skala akar)", y = "Strength (MPa)")
  }, res = 96)
  
  # ------------------------------------------------------------------ Tab 4: Permukaan 3D
  permukaan <- reactive({
    if (input$jenis == "mean") {
      x <- wb_grid
      y <- c(1, 3, 7, 14, 21, 28, 42, 56, 70, 85, 100, seq(125, 365, by = 20))
      z <- outer(y, x, function(u, w) f_D(th, w, u))       # baris = Age, kolom = w/b
      satuan <- "Rata-rata Strength (MPa)"
    } else {
      lo <- GRID[[kol_pi()$lo]]; hi <- GRID[[kol_pi()$hi]]
      nilai <- switch(input$jenis, bawah = lo, atas = hi, lebar = hi - lo)
      z <- unname(tapply(nilai, list(GRID$Age, GRID$wb), mean))
      x <- wb_grid; y <- age_grid
      satuan <- switch(input$jenis, bawah = "Batas bawah PI (MPa)", atas = "Batas atas PI (MPa)", lebar = "Lebar PI (MPa)")
    }
    k <- which(z == max(z), arr.ind = TRUE)[1, ]
    list(x = x, y = y, z = z, satuan = satuan, maks = max(z), maks_wb = x[k[2]], maks_age = y[k[1]])
  })
  nilai_titik <- reactive({
    t <- titik()
    switch(input$jenis, mean = t$f, bawah = t$pi_lo, atas = t$pi_hi, lebar = t$pi_hi - t$pi_lo)
  })
  
  output$ket_permukaan <- renderUI({
    s <- permukaan(); t <- titik()
    div(class = "catatan-kecil", HTML(paste0(
      "Nilai tertinggi permukaan ini: <strong>", a(s$maks, 1), " MPa</strong> pada w/b ", koma(s$maks_wb),
      " dan Age ", s$maks_age, " hari.<br>Skenario terpilih: <strong>", a(nilai_titik(), 1), " MPa</strong>.",
      if (input$jenis != "mean") paste0("<br>PI ", input$level, "%, ", nama_pi[[input$pi_metode]], ".") else "")))
  })
  
  if (ada_plotly) {
    output$permukaan <- plotly::renderPlotly({
      s <- permukaan(); t <- titik()
      p <- plotly::plot_ly()
      p <- plotly::add_surface(p, x = s$x, y = s$y, z = s$z, opacity = 0.95, showscale = TRUE,
                               colorscale = list(list(0, "#DCE8F4"), list(1, "#0F4C81")),
                               colorbar = list(title = "MPa", len = 0.6), name = s$satuan)
      if (isTRUE(input$tampil_obs) && input$jenis == "mean") {
        obs <- dat[dat$wb >= min(wb_grid) & dat$wb <= max(wb_grid), ]
        p <- plotly::add_trace(p, x = obs$wb, y = obs$Age, z = obs$Strength, type = "scatter3d", mode = "markers",
                               marker = list(size = 2, color = "#3E4C59", opacity = 0.45), name = "Observasi")
      }
      p <- plotly::add_trace(p, x = t$wb, y = t$age, z = nilai_titik(), type = "scatter3d", mode = "markers",
                             marker = list(size = 6, color = w_aksen), name = "Skenario terpilih")
      plotly::layout(p, showlegend = FALSE, margin = list(l = 0, r = 0, t = 0, b = 0),
                     scene = list(xaxis = list(title = "w/b"), yaxis = list(title = "Age (hari)"),
                                  zaxis = list(title = s$satuan),
                                  camera = list(eye = list(x = 1.7, y = -1.7, z = 0.9))))
    })
  } else {
    output$permukaan_statis <- renderPlot({
      s <- permukaan(); t <- titik()
      zz <- t(s$z)                                           # persp: baris = w/b, kolom = Age
      nx <- nrow(zz); ny <- ncol(zz)
      zf <- (zz[-1, -1] + zz[-1, -ny] + zz[-nx, -1] + zz[-nx, -ny]) / 4
      pal <- colorRampPalette(c("#DCE8F4", "#0F4C81"))(50)
      op <- par(mar = c(1.5, 1, 0.5, 0.5))
      pm <- persp(s$x, s$y, zz, theta = input$theta, phi = input$phi, expand = 0.62,
                  col = pal[as.integer(cut(zf, 50, include.lowest = TRUE))], border = "#FFFFFF", lwd = 0.4,
                  ticktype = "detailed", nticks = 5, cex.axis = 0.75, cex.lab = 0.9,
                  xlab = "w/b", ylab = "Age (hari)", zlab = s$satuan)
      points(trans3d(t$wb, t$age, nilai_titik(), pm), pch = 19, cex = 1.6, col = w_aksen)
      par(op)
    }, res = 96)
  }
  
  # ------------------------------------------------------------------ Tab 2: Inferensi (Model D)
  fitD <- FIT[FIT$model == "D", ]
  fitD <- fitD[match(c(prm, "delta", "sigma"), fitD$parameter), ]
  nama_param <- c("S<sub>ref</sub>", "&beta;", "&gamma;<sub>28</sub>", "p", "&kappa;", "&delta;", "&sigma;")
  arti_param <- c("rata-rata Strength pada w/b 0,45 dan Age 28 hari (MPa)",
                  "seberapa kuat plateau bergantung pada w/b", "rate pada reference point",
                  "bentuk kurva Age", "seberapa kuat rate bergantung pada w/b",
                  "pangkat variance function", "skala error")
  
  output$ket_param <- renderUI(HTML(paste0(
    "SE dan CI 95% (Wald) memakai asumsi <em>", nama_se[[input$se]], "</em> yang dipilih di panel kiri.")))
  
  output$tab_param <- renderTable({
    se <- fitD[[input$se]]
    data.frame(Parameter = nama_param, Arti = arti_param, Estimate = a(fitD$estimasi, 3), SE = a(se, 3),
               CI = ifelse(is.finite(se), ivl(fitD$estimasi - z975 * se, fitD$estimasi + z975 * se, 3), "-"),
               check.names = FALSE, stringsAsFactors = FALSE)
  }, striped = TRUE, spacing = "s", width = "100%", align = "llrrr", sanitize.text.function = function(x) x)
  
  output$tab_parsial <- renderTable({
    est <- setNames(fitD$estimasi, fitD$parameter); se <- setNames(fitD[[input$se]], fitD$parameter)
    data.frame(
      `Hipotesis nol` = c("&kappa; = 0", "&beta; = 0", "p = 1", "p = 1,7"),
      Makna = c("rate tidak bergantung pada w/b (Model C)", "plateau tidak bergantung pada w/b",
                "kurva Age eksponensial biasa", "pangkat Age pada model lama"),
      `Rasio Wald` = a(c(est[["kappa"]] / se[["kappa"]], est[["beta"]] / se[["beta"]],
                         (est[["p"]] - 1) / se[["p"]], (est[["p"]] - 1.7) / se[["p"]]), 1),
      Deviance = c(a(ambil(KAP, "deviance_kappa0", struktur_galat = "varPower"), 1), "-",
                   a(ambil(PP, "deviance", p = 1), 1), a(ambil(PP, "deviance", p = 1.7), 1)),
      check.names = FALSE, stringsAsFactors = FALSE)
  }, striped = TRUE, spacing = "s", width = "100%", align = "llrr", sanitize.text.function = function(x) x)
  
  output$tab_simultan <- renderTable({
    kunci <- "Model D: C + kappa, NLS biasa (skala MPa)"
    n <- nrow(dat); k <- 5
    R2 <- ambil(SIL, "R2", model = kunci); RSS <- ambil(SIL, "RSS", model = kunci)
    Fs <- (R2 / (k - 1)) / ((1 - R2) / (n - k))
    ll_D <- ambil(VF, "loglik", model = "D", variance_function = "power")
    ll_0 <- -n / 2 * (log(2 * pi * (RSS / (1 - R2)) / n) + 1)
    LR <- 2 * (ll_D - ll_0)
    pk <- function(p) if (!is.finite(p)) "-" else if (p < 0.001) "< 0,001" else a(p, 3)
    data.frame(
      Statistik = c("F pendekatan (nonlinear least squares, constant variance)",
                    "Likelihood ratio (model utama lawan tanpa prediktor)"),
      Nilai = c(a(Fs, 1), a(LR, 1)),
      df = c(paste0(k - 1, " dan ", n - k), "5"),
      `p-value` = c(pk(pf(Fs, k - 1, n - k, lower.tail = FALSE)), pk(pchisq(LR, 5, lower.tail = FALSE))),
      check.names = FALSE, stringsAsFactors = FALSE)
  }, striped = TRUE, spacing = "s", width = "100%", align = "lrrr")
  
  output$plot_se <- renderPlot({
    f5 <- fitD[match(prm, fitD$parameter), ]
    lv <- c("Sandwich", "Identical composition", "Linked 0.5")
    d <- rbind(data.frame(parameter = prm, rasio = f5$se_sandwich / f5$se_model, Asumsi = lv[1]),
               data.frame(parameter = prm, rasio = f5$se_identical / f5$se_model, Asumsi = lv[2]),
               data.frame(parameter = prm, rasio = f5$se_linked05 / f5$se_model, Asumsi = lv[3]))
    d$parameter <- factor(d$parameter, levels = prm)
    d$Asumsi <- factor(d$Asumsi, levels = lv)
    ggplot(d, aes(parameter, rasio, colour = Asumsi)) +
      geom_hline(yintercept = 1, linetype = 2, colour = w_abu) +
      geom_point(position = position_dodge(width = 0.5), size = 3.2) +
      scale_colour_manual(values = setNames(c(w_utama, w_hijau, w_aksen), lv)) +
      scale_y_continuous(labels = koma) +
      labs(x = NULL, y = "SE relatif terhadap SE model-based")
  }, res = 96)
  
  # ------------------------------------------------------------------ Tab 5: Validasi
  output$plot_cov <- renderPlot({
    d <- COV[COV$model == "D" & COV$metode == kode_pi[[input$pi_metode]], ]
    d$Skema <- lab_f(d$skema, lab_skema)
    d$Nominal <- factor(paste0("Nominal ", 100 * d$level, "%"))
    ggplot(d, aes(Skema, 100 * coverage)) +
      geom_hline(aes(yintercept = 100 * level), linetype = 2, colour = w_abu) +
      geom_point(size = 3.4, colour = w_aksen) +
      facet_wrap(~ Nominal, scales = "free_y") +
      scale_y_continuous(labels = koma) +
      labs(x = "Unit fold (makin ke kanan, makin berat ujiannya)", y = "Coverage (%)") +
      theme(axis.text.x = element_text(angle = 20, hjust = 1))
  }, res = 96)
  
  cov_strata <- function(tab, kol, peta) {
    nominal <- as.numeric(input$level)
    d <- tab[tab$metode == kode_pi[[input$pi_metode]] & abs(tab$level - nominal / 100) < 1e-8, ]
    d <- d[match(names(peta), d[[kol]]), ]
    nama <- paste0(unname(peta), "\n(n = ", d$n, ")")
    d$Kelompok <- factor(nama, levels = nama)
    d$Arah <- ifelse(d$coverage < d$level, "Di bawah nominal (PI terlalu sempit)", "Sesuai atau di atas nominal")
    ggplot(d, aes(Kelompok, 100 * coverage, colour = Arah)) +
      geom_hline(yintercept = nominal, linetype = 2, colour = w_tinta) +
      geom_segment(aes(xend = Kelompok, y = nominal, yend = 100 * coverage), linewidth = 1.6) +
      geom_point(size = 4.2) +
      geom_text(aes(label = a(100 * coverage, 1)), nudge_x = 0.3, size = 3.6, colour = "#3E4C59") +
      scale_colour_manual(values = c("Di bawah nominal (PI terlalu sempit)" = w_aksen, "Sesuai atau di atas nominal" = w_utama)) +
      scale_y_continuous(labels = koma) +
      labs(x = NULL, y = "Coverage (%)") +
      theme(axis.text.x = element_text(size = 9, lineheight = 0.9))
  }
  output$plot_cov_age <- renderPlot(cov_strata(QA, "age_grp", lab_age), res = 96)
  output$plot_cov_wb  <- renderPlot(cov_strata(RW, "wb_grp", lab_wb), res = 96)
  output$ket_cov_age <- renderUI(HTML(paste0("Nominal ", input$level, "% (garis putus-putus). Coverage belum merata menurut Age: ini keterbatasan yang dilaporkan.")))
  output$ket_cov_wb  <- renderUI(HTML(paste0("Nominal ", input$level, "% (garis putus-putus). Menurut w/b, coverage merata berkat <em>variance function</em>.")))
  
  output$tab_tajam <- renderTable({
    L <- as.numeric(input$level) / 100
    km <- c("tanpa_prediktor", "D", "C", "D", "D")
    kt <- c("tanpa_prediktor", "D_varians_konstan", "A_normal", "A_normal", "C_residual")
    dari <- function(kolom) unname(mapply(function(m, q) ambil(TS, kolom, model = m, metode = q, level = L), km, kt))
    data.frame(
      Model = c("Tanpa prediktor", "Model D, constant variance", "Model C dengan variance function",
                "Model D dengan variance function, PI normal-theory",
                "<strong>Model D dengan variance function, PI residual bootstrap</strong>"),
      Coverage = pr(dari("coverage")), `Lebar PI` = a(dari("mean_width"), 1),
      `Interval score` = a(dari("interval_score"), 1),
      check.names = FALSE, stringsAsFactors = FALSE)
  }, striped = TRUE, spacing = "s", width = "100%", align = "lrrr", sanitize.text.function = function(x) x)
}

shinyApp(ui, server)
