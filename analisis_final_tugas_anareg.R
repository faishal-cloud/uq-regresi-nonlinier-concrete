###############################################################################
# ANALISIS FINAL - TUGAS ANALISIS REGRESI TINGKAT LANJUT
# Kuantifikasi ketidakpastian pada regresi nonlinier sederhana
# Data: UCI Concrete Compressive Strength (Yeh, 1998), n = 1030
#
# Satu skrip untuk seluruh analisis, dari membaca data sampai semua keluaran yang
# dipakai laporan (R Markdown), dashboard, dan paparan. Dapat dijalankan dari
# lingkungan R yang bersih.
# MODE_UJI <- TRUE  : uji alur dengan replikasi kecil (cepat).
# MODE_UJI <- FALSE : analisis penuh; angka di laporan berasal dari mode ini.
#
# SPESIFIKASI MODEL:
#   mean  : f(wb, Age) = alpha / wb^beta * [1 - exp(-gamma * wb^(-kappa) * Age^p)]   (Model D)
#           Model C = Model D dengan kappa = 0, hanya pembanding
#   galat : E(eps | x) = 0,  sd(eps | x) = sigma * f^delta   (varPower, tanpa Box-Cox)
#   wb    : Water / (Cement + Slag + FlyAsh); prediktor mean HANYA Age dan wb
#   data  : n = 1030 utama; n = 1005 (tanpa 25 baris identik) hanya sensitivity
#   lapor : S_ref = f(0.45, 28), beta, gamma28, p, kappa, delta
#   CI    : Wald (berbasis model, sandwich) + percentile bootstrap kelompok komposisi
#   PI    : residual bootstrap (utama), normal-theory (cek)
#
# CATATAN INTERPRETASI:
#   - UQ bersifat frequentist. Simulasi parameter MVN bukan bootstrap dan tidak dipakai.
#   - Kelompok komposisi BUKAN batch terverifikasi; tidak ada "cluster bootstrap" sejati.
#   - p dan kappa adalah parameter fungsi respons-umur EMPIRIS, bukan parameter pematangan.
#   - PI terkalibrasi secara marginal, tidak per komposisi dan tidak merata menurut Age.
#   - Prediksi pada Age > 100 hari ditopang 62 baris berkomposisi khusus: beri warning.
#
# URUTAN: 0 konfigurasi | 1 data | 1b EDA | 2 model dan fit | 4 silsilah model |
#   5 bukti spesifikasi | 6 alat UQ | M seleksi model | N prediction interval |
#   O-P validasi dan coverage | Q-S coverage per strata | T calibration-sharpness |
#   U dependence sensitivity | V sensitivity 25 baris | X Model C vs D | penutup
# Keluaran: CSV, PNG, dan objek_final.rds di folder dir_out; konsol tersimpan otomatis.
###############################################################################

# ---- 0. KONFIGURASI ---------------------------------------------------------
path_data  <- "Concrete_Data.xls"   # letakkan di working directory, atau ganti path
dir_out    <- "hasil_final"
MODE_UJI   <- TRUE                  # TRUE = uji alur dengan replikasi kecil
seed_utama <- 20261006

B_boot   <- if (MODE_UJI) 100 else 2000   # replikasi bootstrap utama
B_sel    <- if (MODE_UJI) 50  else 1000   # replikasi untuk seleksi model (M)
B_nested <- if (MODE_UJI) 30  else 200    # bootstrap bersarang di tiap data latih
N_NEW    <- if (MODE_UJI) 20  else 100    # observasi baru per replikasi (PI titik)
N_NEW_GRID <- if (MODE_UJI) 5 else 20      # observasi baru per replikasi (PI grid)
R_REP    <- if (MODE_UJI) 1   else 5      # ulangan validasi silang
K_FOLD   <- 10
LEVELS   <- c(0.80, 0.90, 0.95)
W0 <- 0.45; A0 <- 28                       # titik acuan parameterisasi S_ref
PTS <- data.frame(wb  = c(0.45, 0.30, 0.30, 0.70, 0.70),
                  Age = c(28,   7,    90,   7,    90))
PTS$label <- sprintf("(%.2f, %d)", PTS$wb, PTS$Age)

paket <- c("readxl", "minpack.lm", "dplyr", "tidyr", "ggplot2")
belum <- paket[!vapply(paket, requireNamespace, logical(1), quietly = TRUE)]
if (length(belum)) stop("Pasang dulu: install.packages(c('", paste(belum, collapse = "', '"), "'))")
library(readxl)
library(minpack.lm)
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(ggplot2) })
dir.create(dir_out, showWarnings = FALSE)
eda_dir <- file.path(dir_out, "EDA")
dir.create(eda_dir, showWarnings = FALSE)
simpan <- function(df, nama, dir = dir_out) {
  write.csv(df, file.path(dir, nama), row.names = FALSE); invisible(df)
}
akhiran <- if (MODE_UJI) "_uji" else ""
lab <- function(L) sprintf("%02.0f", 100 * L)       # 0.95 -> "95"
# Simpan keluaran konsol. Bila skrip berhenti karena galat, ketik sink() di konsol.
while (sink.number() > 0) sink()
sink(file.path(dir_out, paste0("konsol", akhiran, ".txt")), split = TRUE)

# Simpan/muat hasil agar bagian yang lama tidak perlu diulang
cache <- function(nama, expr) {
  f <- file.path(dir_out, paste0(nama, akhiran, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  t0 <- Sys.time()
  x <- expr
  cat("[", nama, "] selesai dalam ", format(round(difftime(Sys.time(), t0, units = "mins"), 1)), "\n", sep = "")
  saveRDS(x, f)
  x
}
tulis <- function(df, nama) {
  write.csv(df, file.path(dir_out, paste0(nama, akhiran, ".csv")), row.names = FALSE)
  invisible(df)
}

# ---- 1. DATA DAN KELOMPOK ---------------------------------------------------
raw <- as.data.frame(read_excel(path_data))
stopifnot(ncol(raw) == 9, nrow(raw) == 1030)
names(raw) <- c("Cement", "Slag", "FlyAsh", "Water", "SP", "CoarseAgg", "FineAgg",
                "Age", "Strength")
KOMP <- c("Cement", "Slag", "FlyAsh", "Water", "SP", "CoarseAgg", "FineAgg")
dat <- raw
dat$wb    <- dat$Water / (dat$Cement + dat$Slag + dat$FlyAsh)
dat$baris <- seq_len(nrow(dat))

# Kelompok komposisi identik: tujuh komponen sama persis. BUKAN batch terverifikasi.
kunci <- do.call(paste, c(lapply(dat[KOMP], function(v) sprintf("%.8f", v)), sep = "|"))
dat$g_ident <- as.integer(factor(kunci, levels = unique(kunci)))

# Kelompok komposisi bertaut: komposisi berjarak <= ambang digabung (single linkage).
sd_pop <- function(v) sqrt(mean((v - mean(v))^2))
Z  <- scale(as.matrix(dat[KOMP]), center = TRUE, scale = apply(dat[KOMP], 2, sd_pop))
Zg <- Z[!duplicated(dat$g_ident), , drop = FALSE]      # satu baris per kelompok identik
taut <- function(Zg, ambang) {
  D <- as.matrix(dist(Zg)); G <- nrow(D); lab_g <- rep(NA_integer_, G); cur <- 0L
  for (i in seq_len(G)) {
    if (!is.na(lab_g[i])) next
    cur <- cur + 1L; lab_g[i] <- cur; antre <- i
    while (length(antre)) {
      j <- antre[1]; antre <- antre[-1]
      tetangga <- which(D[j, ] <= ambang & is.na(lab_g))
      lab_g[tetangga] <- cur
      antre <- c(antre, tetangga)
    }
  }
  lab_g
}
dat$g_l05  <- taut(Zg, 0.5)[dat$g_ident]
dat$g_l10  <- taut(Zg, 1.0)[dat$g_ident]
dat$g_blok <- (dat$baris - 1) %/% 50 + 1
dat$identik_tambahan <- duplicated(dat[c(KOMP, "Age", "Strength")])
dat$age_grp <- cut(dat$Age, c(0, 7, 28, 56, 100, 365),
                   labels = c("1-7", "14-28", "56", "90-100", "120-365"))
dat$wb_grp  <- cut(dat$wb, c(0, 0.40, 0.55, Inf),
                   labels = c("<=0.40", "0.40-0.55", ">0.55"))

cat("\n== CEK DATA ==\n")
cat("n baris                         :", nrow(dat), "\n")
cat("kelompok komposisi identik      :", max(dat$g_ident), "\n")
cat("kelompok bertaut jarak 0.5      :", max(dat$g_l05), "\n")
cat("kelompok bertaut jarak 1.0      :", max(dat$g_l10), "\n")
cat("baris identik kedua dst         :", sum(dat$identik_tambahan), "\n")
cat("baris dengan Age > 100          :", sum(dat$Age > 100), "\n")
print(table(dat$age_grp, dat$wb_grp))
tulis(data.frame(
  butir = c("baris", "kelompok komposisi identik", "kelompok bertaut 0.5", "kelompok bertaut 1.0",
            "baris identik kedua dst", "baris Age > 100", "Strength minimum", "Strength maksimum"),
  nilai = c(nrow(dat), max(dat$g_ident), max(dat$g_l05), max(dat$g_l10), sum(dat$identik_tambahan),
            sum(dat$Age > 100), min(dat$Strength), max(dat$Strength))), "00_cek_data")
tulis(as.data.frame(table(age_grp = dat$age_grp, wb_grp = dat$wb_grp)), "00_sel_age_wb")
# Bila jumlah kelompok berbeda dari Master Record 2B, BERHENTI dan laporkan:
# definisi kelompok harus sama sebelum hasil bisa dibandingkan.

# ---- 1b. EXPLORATORY DATA ANALYSIS --------------------------------------------
# Landasan: Tukey (1977) untuk EDA sebelum pemodelan; Cleveland (1979) untuk LOESS.
# EDA di sini deskriptif. Bentuk model yang dipilih setelah melihat data yang sama
# membawa optimisme seleksi (Chatfield, 1995); itu dicatat sebagai limitation.

# 2.1 Ringkasan numerik
eda_summary <- dat %>%
  select(Strength, Age, wb) %>%
  pivot_longer(everything(), names_to = "variabel", values_to = "nilai") %>%
  group_by(variabel) %>%
  summarise(n = n(), mean = mean(nilai), sd = sd(nilai), min = min(nilai),
            q1 = quantile(nilai, 0.25), median = median(nilai),
            q3 = quantile(nilai, 0.75), max = max(nilai), .groups = "drop")
print(eda_summary); simpan(eda_summary, "EDA_01_summary.csv", eda_dir)

# 2.2 Age bersifat diskret: frekuensi per nilai, bukan histogram
age_freq <- dat %>% count(Age, name = "n")
print(age_freq); simpan(age_freq, "EDA_02_age_frequency.csv", eda_dir)
ggsave(file.path(eda_dir, "EDA_03_age_frequency.png"),
       ggplot(age_freq, aes(factor(Age), n)) + geom_col() +
         labs(title = "Jumlah observasi per nilai Age", x = "Age (hari)", y = "n") + theme_minimal(),
       width = 7, height = 4.5, dpi = 160)

# 2.3 Strength per nilai Age (bentuk kurva umur: naik lalu melandai?)
ggsave(file.path(eda_dir, "EDA_04_strength_per_age.png"),
       ggplot(dat, aes(factor(Age), Strength)) + geom_boxplot(outlier.alpha = 0.4) +
         labs(title = "Strength per nilai Age",
              subtitle = "Titik di luar kotak bukan outlier yang harus dibuang",
              x = "Age (hari)", y = "Strength (MPa)") + theme_minimal(),
       width = 8, height = 5, dpi = 160)

# 2.4 Konfounding: komposisi yang diuji berbeda antar-umur
komp_per_age <- dat %>%
  group_by(Age) %>%
  summarise(n = n(), ada_fly_ash = mean(FlyAsh > 0), ada_superplasticizer = mean(SP > 0),
            ada_slag = mean(Slag > 0), rata_wb = mean(wb),
            rata_binder = mean(Cement + Slag + FlyAsh), .groups = "drop")
print(komp_per_age); simpan(komp_per_age, "EDA_05_komposisi_per_age.csv", eda_dir)
ggsave(file.path(eda_dir, "EDA_05_komposisi_per_age.png"),
       komp_per_age %>%
         pivot_longer(c(ada_fly_ash, ada_superplasticizer, ada_slag), names_to = "bahan", values_to = "proporsi") %>%
         ggplot(aes(factor(Age), proporsi, fill = bahan)) + geom_col(position = "dodge") +
         labs(title = "Proporsi campuran yang memakai bahan tertentu, per Age",
              x = "Age (hari)", y = "Proporsi baris") + theme_minimal(),
       width = 9, height = 5, dpi = 160)

# 2.5 Lintasan per komposisi identik (benda uji berbeda; bukan satu spesimen yang diikuti)
traj <- dat %>%
  group_by(g_ident) %>% filter(n_distinct(Age) >= 3) %>%
  group_by(g_ident, Age) %>% summarise(Strength = mean(Strength), wb = first(wb), .groups = "drop")
ggsave(file.path(eda_dir, "EDA_06_lintasan_per_komposisi.png"),
       ggplot(traj, aes(Age, Strength, group = g_ident, colour = wb)) +
         geom_line(alpha = 0.45) + scale_x_log10() + scale_colour_viridis_c() +
         labs(title = "Strength menurut Age pada komposisi yang sama",
              subtitle = "Komposisi dengan sedikitnya tiga umur; sumbu Age logaritmik",
              x = "Age (hari)", y = "Strength (MPa)", colour = "w/b") + theme_minimal(),
       width = 8, height = 5, dpi = 160)

# 2.6 Strength vs w/b pada skala log-log, per kelompok Age (motivasi bentuk pangkat)
slope_wb <- dat %>%
  group_by(age_grp) %>%
  summarise(n = n(), kemiringan_log_log = coef(lm(log(Strength) ~ log(wb)))[2], .groups = "drop")
print(slope_wb); simpan(slope_wb, "EDA_07_kemiringan_log_wb.csv", eda_dir)
ggsave(file.path(eda_dir, "EDA_07_strength_vs_wb_loglog.png"),
       ggplot(dat, aes(wb, Strength)) + geom_point(alpha = 0.3) +
         geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.7) +
         scale_x_log10() + scale_y_log10() + facet_wrap(~age_grp) +
         labs(title = "Strength vs w/b, skala log-log, per kelompok Age",
              subtitle = "Garis lurus deskriptif; kemiringan yang berubah antar-panel mengisyaratkan interaksi",
              x = "w/b", y = "Strength (MPa)") + theme_minimal(),
       width = 9, height = 6, dpi = 160)

# 2.7 Hubungan rata-rata dan sebaran, tanpa model (motivasi variance function)
sel <- dat %>%
  group_by(Age, wb_grp) %>%
  summarise(n = n(), rata = mean(Strength), sd = sd(Strength), .groups = "drop") %>%
  filter(n >= 8)
slope_mv <- coef(lm(log(sd) ~ log(rata), data = sel))[2]
cat("\nKemiringan log(sd) terhadap log(rata-rata) antar-sel Age x w/b:", round(slope_mv, 3),
    "\n(deskriptif; sebaran dalam sel juga memuat variasi w/b dan komposisi)\n")
simpan(sel, "EDA_08_rata_vs_sebaran.csv", eda_dir)
ggsave(file.path(eda_dir, "EDA_08_rata_vs_sebaran.png"),
       ggplot(sel, aes(rata, sd)) + geom_point(aes(size = n), alpha = 0.6) +
         geom_smooth(method = "lm", formula = y ~ x, se = FALSE, linewidth = 0.7) +
         scale_x_log10() + scale_y_log10() +
         labs(title = "Simpangan baku vs rata-rata Strength per sel Age x w/b",
              x = "Rata-rata Strength (MPa)", y = "Simpangan baku (MPa)") + theme_minimal(),
       width = 7, height = 5, dpi = 160)

# 2.8 Korelasi: HANYA diagnostik. Tujuh komponen tidak masuk ke model mean.
corr_matrix <- cor(dat[c(KOMP, "Age", "wb", "Strength")])
print(round(corr_matrix, 3))
write.csv(round(corr_matrix, 6), file.path(eda_dir, "EDA_09_correlation_matrix.csv"))
cat("\n================ EDA SELESAI ================\n")

landasan <- data.frame(
  langkah = c("Sumber data", "EDA sebelum model", "LOESS deskriptif", "Hubungan Strength dengan w/b",
              "Kurva umur 1 - exp(-g t^p)", "Parameterisasi S_ref", "Estimasi NLS dan sifat asimtotik",
              "Algoritme Levenberg-Marquardt", "Variance function dan GLS", "gnls dan varPower",
              "Box-Cox", "Transformasi lawan pembobotan", "Bias transformasi balik",
              "Profil likelihood", "AIC", "Deskripsi heteroskedastisitas", "Residual nonlinier",
              "Tidak memilih model lewat p-value uji asumsi"),
  pustaka = c("Yeh (1998)", "Tukey (1977)", "Cleveland (1979)", "Abrams (1918); Neville (2011)",
              "Seber & Wild (1989); Ratkowsky (1990); Yang, Kozak & Smith (1978)",
              "Ratkowsky (1990); Bates & Watts (1988)", "Jennrich (1969); Seber & Wild (1989)",
              "Marquardt (1963)", "Carroll & Ruppert (1988); Davidian & Carroll (1987)",
              "Pinheiro & Bates (2000)", "Box & Cox (1964)", "Carroll & Ruppert (1984, 1988)",
              "Duan (1983)", "Bates & Watts (1988); Venzon & Moolgavkar (1988)",
              "Akaike (1974); Burnham & Anderson (2002)", "Breusch & Pagan (1979); Koenker (1981)",
              "Cook & Tsai (1985)", "Chatfield (1995)"))
simpan(landasan, "00_landasan_ilmiah.csv")

# ---- 2. MODEL, ESTIMASI, COVARIANCE -----------------------------------------
f_D <- function(th, wb, age) th[1] * wb^(-th[2]) * (1 - exp(-th[3] * wb^(-th[5]) * age^th[4]))
J_D <- function(th, wb, age) {
  u <- th[3] * wb^(-th[5]) * age^th[4]; e <- exp(-u); w <- wb^(-th[2])
  f <- th[1] * w * (1 - e); c0 <- th[1] * w * e * u
  cbind(f / th[1], -log(wb) * f, c0 / th[3], c0 * log(age), -c0 * log(wb))
}
f_C <- function(th, wb, age) th[1] * wb^(-th[2]) * (1 - exp(-th[3] * age^th[4]))
J_C <- function(th, wb, age) {
  ap <- age^th[4]; e <- exp(-th[3] * ap); w <- wb^(-th[2]); g <- 1 - e
  cbind(w * g, -th[1] * log(wb) * w * g, th[1] * w * ap * e, th[1] * w * th[3] * ap * log(age) * e)
}
# Titik awal: beberapa kandidat; yang dipakai adalah yang log-likelihood-nya terbesar.
MODEL <- list(
  D = list(f = f_D, J = J_D, k = 5,
           starts = list(c(36, 0.55, 0.08, 0.49, 1.2), c(30, 0.8, 0.1, 0.45, 0.8),
                         c(24, 1.2, 0.2, 0.40, 0.3))),
  C = list(f = f_C, J = J_C, k = 4,
           starts = list(c(25, 1.2, 0.2, 0.42), c(30, 1.0, 0.3, 0.35), c(20, 1.2, 0.1, 0.60))))

# NLS berbobot pada skala log parameter (menjamin semua parameter positif)
wnls <- function(model, y, wb, age, start, w = rep(1, length(y))) {
  res_fn <- function(phi) (y - model$f(exp(phi), wb, age)) * w
  jac_fn <- function(phi) { th <- exp(phi); -sweep(model$J(th, wb, age) * w, 2, th, "*") }
  fit <- nls.lm(par = log(start), fn = res_fn, jac = jac_fn,
                control = nls.lm.control(maxiter = 200, ftol = 1e-10, ptol = 1e-10))
  list(th = exp(fit$par), info = fit$info)
}
# Variance function: sd(eps) = sigma * s(f)
s_fun <- function(vform, psi, ff) switch(vform,
  const = rep(1, length(ff)), power = ff^psi, exp = exp(psi * ff),
  stop("vform tidak dikenal"))

# Generalized NLS: bergantian (a) NLS berbobot untuk theta, (b) ML untuk parameter varians
fit_gnls <- function(model, y, wb, age, start, vform = "power", maxit = 200) {
  n <- length(y); k <- length(start)
  th <- wnls(model, y, wb, age, start)$th
  psi <- 0; konv <- (vform == "const"); it <- 0
  if (vform != "const") {
    rentang <- if (vform == "power") c(-0.5, 2) else c(-0.2, 0.2)
    for (it in seq_len(maxit)) {
      ff <- model$f(th, wb, age); r <- y - ff
      obj <- function(q) { s <- s_fun(vform, q, ff); sum(log(s)) + n / 2 * log(mean((r / s)^2)) }
      psi_baru <- optimize(obj, interval = rentang, tol = 1e-9)$minimum
      th_baru  <- wnls(model, y, wb, age, th, w = 1 / s_fun(vform, psi_baru, ff))$th
      selesai  <- abs(psi_baru - psi) < 1e-6 && max(abs(th_baru - th) / abs(th)) < 1e-7
      th <- th_baru; psi <- psi_baru
      if (selesai && it > 1) { konv <- TRUE; break }
    }
  }
  ff <- model$f(th, wb, age); s <- s_fun(vform, psi, ff); es <- (y - ff) / s
  list(th = th, psi = psi, vform = vform,
       sigma = sqrt(sum(es^2) / (n - k)),   # dengan koreksi derajat bebas
       sigma_ml = sqrt(mean(es^2)),
       fitted = ff, s = s, es = es,
       loglik = -n / 2 * log(2 * pi * mean(es^2)) - sum(log(s)) - n / 2,
       npar = k + 1 + (vform != "const"), converged = konv, iter = it)
}
# Pembungkus aman: status 0 = konvergen; 1 = batas iterasi; 2 = titik awal cadangan; NULL = gagal
fit_aman <- function(model, y, wb, age, start, vform = "power") {
  faktor <- list(1, c(0.8, 1.25, 0.8, 1, 1), c(1.25, 0.8, 1.25, 1, 1))
  for (i in seq_along(faktor)) {
    st  <- start * (if (i == 1) 1 else faktor[[i]][seq_along(start)])
    out <- tryCatch(fit_gnls(model, y, wb, age, st, vform), error = function(e) NULL)
    if (!is.null(out) && all(is.finite(out$th)) && is.finite(out$loglik)) {
      out$status <- if (!out$converged) 1L else if (i > 1) 2L else 0L
      return(out)
    }
  }
  NULL
}
aic <- function(fit) -2 * fit$loglik + 2 * fit$npar

# Parameterisasi pelaporan: S_ref = f(W0, A0), beta, gamma28, p, (kappa)
repar <- function(th) {
  k <- length(th); kap <- if (k == 5) th[5] else 0
  g28 <- th[3] * W0^(-kap) * A0^th[4]
  out <- c(S_ref = th[1] * W0^(-th[2]) * (1 - exp(-g28)), beta = th[2], gamma28 = g28, p = th[4])
  if (k == 5) out <- c(out, kappa = th[5])
  out
}
jac_repar <- function(th) {
  k <- length(th); Jm <- matrix(0, k, k)
  for (j in seq_len(k)) {
    h <- 1e-6 * th[j]; tp <- th; tm <- th; tp[j] <- tp[j] + h; tm[j] <- tm[j] - h
    Jm[, j] <- (repar(tp) - repar(tm)) / (2 * h)
  }
  Jm
}
# Covariance theta (parameterisasi asli): berbasis model, sandwich, dan sandwich berkelompok
cov_gnls <- function(model, fit, wb, age, groups = NULL) {
  w <- 1 / fit$s; Jw <- model$J(fit$th, wb, age) * w
  A <- solve(crossprod(Jw)); n <- length(w); k <- length(fit$th)
  e <- fit$es                                     # residual terstandar (belum dibagi sigma)
  out <- list(model = fit$sigma^2 * A,
              sandwich = A %*% crossprod(Jw * e) %*% A * n / (n - k))
  for (nm in names(groups)) {
    S <- rowsum(Jw * e, groups[[nm]]); G <- nrow(S)
    out[[nm]] <- A %*% crossprod(S) %*% A * G / (G - 1) * (n - 1) / (n - k)
  }
  out
}

# Fit data penuh, dengan cek banyak titik awal
MULTI <- list()
fit_penuh <- function(nama) {
  model <- MODEL[[nama]]
  kand <- lapply(model$starts, function(s) fit_aman(model, dat$Strength, dat$wb, dat$Age, s))
  ll <- vapply(kand, function(z) if (is.null(z)) NA_real_ else z$loglik, numeric(1))
  MULTI[[nama]] <<- data.frame(model = nama, titik_awal = seq_along(ll), loglik = ll)
  cat("Model", nama, ": log-likelihood dari tiap titik awal =", format(ll, digits = 10), "\n")
  kand[[which.max(ll)]]
}
fitD <- fit_penuh("D")
fitC <- fit_penuh("C")
FIT  <- list(D = fitD, C = fitC)
GRP  <- list(identical = dat$g_ident, linked05 = dat$g_l05, linked10 = dat$g_l10, block = dat$g_blok)
VD   <- cov_gnls(MODEL$D, fitD, dat$wb, dat$Age, GRP)
VC   <- cov_gnls(MODEL$C, fitC, dat$wb, dat$Age, GRP)

ringkas_fit <- function(nama, fit, V) {
  r <- repar(fit$th); Tm <- jac_repar(fit$th)
  data.frame(model = nama, parameter = c(names(r), "delta", "sigma"),
             estimasi = c(r, fit$psi, fit$sigma),
             se_model = c(sqrt(diag(Tm %*% V$model %*% t(Tm))), NA, NA),
             se_sandwich = c(sqrt(diag(Tm %*% V$sandwich %*% t(Tm))), NA, NA),
             se_identical = c(sqrt(diag(Tm %*% V$identical %*% t(Tm))), NA, NA),
             se_linked05 = c(sqrt(diag(Tm %*% V$linked05 %*% t(Tm))), NA, NA))
}
tab_fit <- rbind(ringkas_fit("D", fitD, VD), ringkas_fit("C", fitC, VC))
z975 <- qnorm(0.975)
tab_fit$wald_model_lo    <- tab_fit$estimasi - z975 * tab_fit$se_model
tab_fit$wald_model_hi    <- tab_fit$estimasi + z975 * tab_fit$se_model
tab_fit$wald_sandwich_lo <- tab_fit$estimasi - z975 * tab_fit$se_sandwich
tab_fit$wald_sandwich_hi <- tab_fit$estimasi + z975 * tab_fit$se_sandwich
tulis(do.call(rbind, MULTI), "01_multi_start")
tulis(data.frame(model = c(rep("D", 5), rep("C", 4)),
                 parameter = c("alpha", "beta", "gamma", "p", "kappa", "alpha", "beta", "gamma", "p"),
                 estimasi = c(fitD$th, fitC$th)), "00_parameter_asli")
kor_D <- cov2cor(jac_repar(fitD$th) %*% VD$model %*% t(jac_repar(fitD$th)))
dimnames(kor_D) <- list(names(repar(fitD$th)), names(repar(fitD$th)))
write.csv(round(kor_D, 4), file.path(dir_out, paste0("00_korelasi_parameter_D", akhiran, ".csv")))
cat("\n== FIT DATA PENUH (cek terhadap Master Record 2B) ==\n"); print(tab_fit, digits = 5)
cat("Status konvergensi D, C:", fitD$status, fitC$status, " | iterasi:", fitD$iter, fitC$iter, "\n")
tulis(tab_fit, "00_fit_data_penuh")
# SE delta tidak diberikan oleh rumus di atas; untuk delta pakai interval bootstrap (bagian U).

# ---- 4. SILSILAH MODEL: MODEL LAMA SEBAGAI PEMBANDING -------------------------
# Landasan: hubungan Strength-w/c (Abrams, 1918; Neville, 2011); Box-Cox (Box & Cox, 1964).
# Model 1, Model 2, dan Model A disimpan sebagai pembanding, bukan dipakai.
ctrl_lm <- nls.lm.control(maxiter = 500, ftol = 1e-10, ptol = 1e-10)
bc     <- function(y, lam) if (abs(lam) < 1e-8) log(y) else (y^lam - 1) / lam
bc_inv <- function(z, lam) if (abs(lam) < 1e-8) exp(z) else pmax(lam * z + 1, 0)^(1 / lam)
ukur <- function(pred, nama, k) {
  res <- dat$Strength - pred
  data.frame(model = nama, parameter_mean = k, RSS = sum(res^2), RMSE = sqrt(mean(res^2)),
             MAE = mean(abs(res)), R2 = 1 - sum(res^2) / sum((dat$Strength - mean(dat$Strength))^2))
}

fit_M1 <- nlsLM(Strength ~ alpha / wb^beta * (1 - exp(-gamma * Age)), data = dat,
                start = list(alpha = 30, beta = 1, gamma = 0.05), control = ctrl_lm)
fit_M2 <- nlsLM(Strength ~ alpha * exp(-beta * wb) * log(Age), data = dat,
                start = list(alpha = 30, beta = 1), control = ctrl_lm)
# Model A: Box-Cox lambda = 0.8 pada respons, Age^1.7 (model final lama)
dat$y_bc08 <- bc(dat$Strength, 0.8)
fit_MA <- nlsLM(y_bc08 ~ alpha / wb^beta * (1 - exp(-gamma * Age^1.7)), data = dat,
                start = list(alpha = 10, beta = 1, gamma = 0.10), control = ctrl_lm)
cat("\n--- Model A (model final lama); bandingkan dengan hasil lama Anda ---\n")
print(coef(fit_MA))
# Catatan: transformasi balik fitted Box-Cox bukan rata-rata bersyarat dalam MPa (Duan, 1983).
silsilah <- bind_rows(
  ukur(predict(fit_M1), "Model 1: asimtotik, Age^1", 3),
  ukur(predict(fit_M2), "Model 2: logaritmik", 2),
  ukur(bc_inv(predict(fit_MA), 0.8), "Model A: Box-Cox 0.8, Age^1.7 (transformasi balik)", 3))
# Model 2 memberi prediksi nol pada Age = 1 karena log(1) = 0: cacat struktural.
cat("Prediksi Model 2 pada Age = 1:", unique(round(predict(fit_M2)[dat$Age == 1], 6)), "\n")

ko_A <- summary(fit_MA)$coefficients          # reproduksi model final lama (lambda 0.8, Age^1.7)
tulis(data.frame(parameter = rownames(ko_A), ko_A, check.names = FALSE), "02_model_A_lama")
# Parameter Model 1 dan Model 2: dipakai dashboard untuk menggambar kurva kedua model
tulis(rbind(data.frame(model = "M1", parameter = names(coef(fit_M1)), estimasi = unname(coef(fit_M1))),
            data.frame(model = "M2", parameter = names(coef(fit_M2)), estimasi = unname(coef(fit_M2)))),
      "02_parameter_model_lama")
fitD_const <- fit_aman(MODEL$D, dat$Strength, dat$wb, dat$Age, fitD$th, "const")
fitC_const <- fit_aman(MODEL$C, dat$Strength, dat$wb, dat$Age, fitC$th, "const")
silsilah <- bind_rows(silsilah,
  ukur(fitC_const$fitted, "Model C: p diestimasi, NLS biasa (skala MPa)", 4),
  ukur(fitD_const$fitted, "Model D: C + kappa, NLS biasa (skala MPa)", 5),
  ukur(fitC$fitted, "Model C + varPower", 4),
  ukur(fitD$fitted, "Model D + varPower (model utama)", 5))
cat("\n== 4. SILSILAH MODEL (dalam sampel, skala MPa) ==\n"); print(as.data.frame(silsilah), digits = 5)
tulis(as.data.frame(silsilah), "02_silsilah_model")

# ---- 5. BUKTI UNTUK KEPUTUSAN SPESIFIKASI -----------------------------------
# Landasan: variance function sebagai fungsi rata-rata (Carroll & Ruppert, 1988; Davidian &
# Carroll, 1987); profil likelihood (Bates & Watts, 1988); AIC (Akaike, 1974); Box-Cox
# (Box & Cox, 1964); residual nonlinier (Cook & Tsai, 1985).
# Log-likelihood di sini adalah working likelihood normal yang mengandaikan baris
# independen. Deviance, AIC, dan BIC dibaca sebagai ukuran bukti, bukan uji formal,
# dan p-value uji asumsi tidak dipakai untuk memilih model (Chatfield, 1995).
y <- dat$Strength
bic <- function(fit) -2 * fit$loglik + log(length(fit$es)) * fit$npar

# 5.1 Variance function: konstan, varPower (sigma f^delta), varExp (sigma exp(delta f))
B1 <- do.call(rbind, lapply(c("C", "D"), function(nm) do.call(rbind, lapply(c("const", "power", "exp"), function(vf) {
  ft <- fit_aman(MODEL[[nm]], y, dat$wb, dat$Age, FIT[[nm]]$th, vf)
  if (is.null(ft)) return(data.frame(model = nm, variance_function = vf, par_varians = NA, sigma = NA,
                                     loglik = NA, npar = NA, AIC = NA, BIC = NA, RMSE = NA, status = 9L))
  data.frame(model = nm, variance_function = vf, par_varians = ft$psi, sigma = ft$sigma, loglik = ft$loglik,
             npar = ft$npar, AIC = aic(ft), BIC = bic(ft), RMSE = sqrt(mean((y - ft$fitted)^2)), status = ft$status)
}))))
cat("\n== 5.1 VARIANCE FUNCTION ==\n"); print(B1, digits = 7); tulis(B1, "05_1_variance_function")

# 5.2 kappa = 0 (Model C lawan Model D)
B2 <- data.frame(struktur_galat = c("konstan", "varPower"),
                 deviance_kappa0 = c(2 * (fitD_const$loglik - fitC_const$loglik), 2 * (fitD$loglik - fitC$loglik)),
                 selisih_AIC_D_minus_C = c(aic(fitD_const) - aic(fitC_const), aic(fitD) - aic(fitC)),
                 kappa = c(fitD_const$th[5], fitD$th[5]))
cat("\n== 5.2 kappa = 0 ==\n"); print(B2, digits = 5); tulis(B2, "05_2_kappa")

# 5.3 Profil p: Model D + varPower di-fit ulang dengan p dipatok; delta ikut diestimasi ulang
model_D_p <- function(p0) list(
  f = function(th, wb, age) f_D(c(th[1:3], p0, th[4]), wb, age),
  J = function(th, wb, age) J_D(c(th[1:3], p0, th[4]), wb, age)[, c(1, 2, 3, 5), drop = FALSE],
  k = 4)
p_grid <- c(0.30, 0.35, 0.40, 0.45, 0.50, 0.55, 0.60, 0.80, 1.00, 1.30, 1.70)
B3 <- do.call(rbind, lapply(p_grid, function(p0) {
  md <- model_D_p(p0)
  st <- fitD$th[c(1, 2, 3, 5)]; st[3] <- st[3] * A0^(fitD$th[4] - p0)   # laju pada 28 hari dijaga
  ft <- fit_aman(md, y, dat$wb, dat$Age, st, "power")
  if (is.null(ft)) return(data.frame(p = p0, loglik = NA, deviance = NA, RMSE = NA, rasio_f365_f28 = NA, rasio_f7_f28 = NA))
  data.frame(p = p0, loglik = ft$loglik, deviance = 2 * (fitD$loglik - ft$loglik),
             RMSE = sqrt(mean((y - ft$fitted)^2)),
             rasio_f365_f28 = md$f(ft$th, W0, 365) / md$f(ft$th, W0, 28),
             rasio_f7_f28 = md$f(ft$th, W0, 7) / md$f(ft$th, W0, 28))
}))
cat("\n== 5.3 PROFIL p (p terestimasi =", round(fitD$th[4], 4), ") ==\n"); print(B3, digits = 5)
tulis(B3, "05_3_profil_p")
# Cara membaca: deviance 0 di p terestimasi; 3.84 hanya patokan kasar. Rasio f(365)/f(28)
# yang mendekati 1 berarti kurva umur sudah jenuh pada 28 hari.

# 5.4 Box-Cox pada respons, dengan dan tanpa variance function
# Log-likelihood dikembalikan ke skala MPa lewat suku Jacobian (lambda - 1) * sum(log y),
# sehingga sebanding dengan tabel 5.1. Pada lambda = 1 respons menjadi y - 1, jadi hasilnya
# hampir, tetapi tidak persis, sama dengan fit skala MPa.
sum_log_y <- sum(log(y))
lambda_grid <- c(0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1)
B4 <- do.call(rbind, lapply(lambda_grid, function(lam) {
  yb <- bc(y, lam); st <- fitD$th; st[1] <- st[1] * mean(yb) / mean(y)
  f0 <- fit_aman(MODEL$D, yb, dat$wb, dat$Age, st, "const")
  f1 <- fit_aman(MODEL$D, yb, dat$wb, dat$Age, if (is.null(f0)) st else f0$th, "power")
  data.frame(lambda = lam,
             loglik_varians_konstan = if (is.null(f0)) NA else f0$loglik + (lam - 1) * sum_log_y,
             loglik_varPower = if (is.null(f1)) NA else f1$loglik + (lam - 1) * sum_log_y,
             pangkat_varians = if (is.null(f1)) NA else f1$psi,
             p_varians_konstan = if (is.null(f0)) NA else f0$th[4])
}))
cat("\n== 5.4 PROFIL BOX-COX (Model D) ==\n"); print(B4, digits = 7)
cat("Pembanding skala MPa: varians konstan =", fitD_const$loglik, "| varPower =", fitD$loglik, "\n")
tulis(B4, "05_4_profil_boxcox")
# Cara membaca: (a) lambda terbaik pada kolom varians konstan menunjukkan transformasi yang
# diminta data bila varians dipaksa konstan; (b) bila maksimum kolom varPower berada dekat
# lambda = 1, transformasi tidak lagi diperlukan setelah varians dimodelkan; (c) maksimum
# kolom (a) dibandingkan dengan varPower skala MPa: jumlah parameternya sama.

# 5.5 Diagnostik residual. Residual Pearson = residual / (sigma f^delta).
diag_tab <- function(fit, label) data.frame(
  dat[c("baris", "Age", "wb", "age_grp", "wb_grp", "Strength")], model = label,
  fitted = fit$fitted, residual = y - fit$fitted, pearson = fit$es / fit$sigma)
DG <- rbind(diag_tab(fitD_const, "D, varians konstan"), diag_tab(fitD, "D, varPower"),
            diag_tab(fitC_const, "C, varians konstan"), diag_tab(fitC, "C, varPower"))
DG$kuintil <- ave(DG$fitted, DG$model, FUN = function(v) ceiling(5 * rank(v, ties.method = "first") / length(v)))
B5a <- as.data.frame(DG %>% group_by(model, kuintil) %>%
  summarise(n = n(), rata_fitted = mean(fitted), sd_residual = sd(residual), sd_pearson = sd(pearson), .groups = "drop"))
B5b <- as.data.frame(bind_rows(
  DG %>% group_by(model, strata = as.character(age_grp)) %>%
    summarise(n = n(), rata_residual = mean(residual), sd_residual = sd(residual), sd_pearson = sd(pearson), .groups = "drop") %>%
    mutate(menurut = "Age"),
  DG %>% group_by(model, strata = as.character(wb_grp)) %>%
    summarise(n = n(), rata_residual = mean(residual), sd_residual = sd(residual), sd_pearson = sd(pearson), .groups = "drop") %>%
    mutate(menurut = "w/b")))
B5c <- as.data.frame(DG %>% group_by(model) %>%
  summarise(skewness = mean((pearson - mean(pearson))^3) / sd(pearson)^3,
            excess_kurtosis = mean((pearson - mean(pearson))^4) / sd(pearson)^4 - 3,
            q025 = quantile(pearson, 0.025), q10 = quantile(pearson, 0.10),
            q90 = quantile(pearson, 0.90), q975 = quantile(pearson, 0.975), .groups = "drop"))
# Statistik Koenker n*R^2 (Breusch & Pagan, 1979; Koenker, 1981): DESKRIPSI besaran, bukan kriteria
nR2 <- function(e2, x) length(e2) * summary(lm(e2 ~ x))$r.squared
dk <- DG[DG$model == "D, varians konstan", ]
B5d <- data.frame(model = "D, varians konstan", terhadap = c("fitted", "log Age", "w/b"),
                  koenker_nR2 = c(nR2(dk$residual^2, dk$fitted), nR2(dk$residual^2, log(dk$Age)), nR2(dk$residual^2, dk$wb)))
cat("\n== 5.5 DIAGNOSTIK RESIDUAL ==\n"); print(B5a, digits = 4); print(B5b, digits = 4); print(B5c, digits = 4); print(B5d, digits = 4)
tulis(B5a, "05_5a_sebaran_per_kuintil"); tulis(B5b, "05_5b_sebaran_per_strata")
tulis(B5c, "05_5c_bentuk_residual"); tulis(B5d, "05_5d_koenker")
# Kriteria: variance function memadai bila sd_pearson dekat 1 di semua kuintil dan strata.
dp <- DG[DG$model == "D, varPower", ]
ggsave(file.path(dir_out, "05_D_residual_vs_fitted_varians_konstan.png"),
       ggplot(dk, aes(fitted, residual)) + geom_point(alpha = 0.4) + geom_hline(yintercept = 0, linetype = 2) +
         labs(title = "Model D, varians konstan: residual vs fitted", x = "Fitted (MPa)", y = "Residual (MPa)") +
         theme_minimal(), width = 7, height = 5, dpi = 150)
ggsave(file.path(dir_out, "05_D_pearson_vs_fitted_varPower.png"),
       ggplot(dp, aes(fitted, pearson)) + geom_point(alpha = 0.4) + geom_hline(yintercept = 0, linetype = 2) +
         labs(title = "Model D, varPower: residual Pearson vs fitted", x = "Fitted (MPa)", y = "Residual Pearson") +
         theme_minimal(), width = 7, height = 5, dpi = 150)
ggsave(file.path(dir_out, "05_D_pearson_per_age.png"),
       ggplot(dp, aes(factor(Age), pearson)) + geom_boxplot(outlier.alpha = 0.4) + geom_hline(yintercept = 0, linetype = 2) +
         labs(title = "Model D, varPower: residual Pearson per Age", x = "Age (hari)", y = "Residual Pearson") +
         theme_minimal(), width = 8, height = 5, dpi = 150)
ggsave(file.path(dir_out, "05_D_qq_pearson.png"),
       ggplot(dp, aes(sample = pearson)) + stat_qq(alpha = 0.4) + stat_qq_line() +
         labs(title = "Model D, varPower: QQ residual Pearson", x = "Kuantil normal", y = "Kuantil residual") +
         theme_minimal(), width = 6, height = 5, dpi = 150)

# ---- 6. ALAT UQ: PI, BOOTSTRAP, VALIDASI ------------------------------------
# A. Normal-theory PI: Var(Y_new - fhat) ~ sigma^2 f^(2 delta) + g' V g
pi_normal <- function(model, fit, V, nd, levels = LEVELS) {
  f0 <- model$f(fit$th, nd$wb, nd$Age); J0 <- model$J(fit$th, nd$wb, nd$Age)
  se_mean <- sqrt(pmax(rowSums((J0 %*% V) * J0), 0))
  sd_new  <- fit$sigma * s_fun(fit$vform, fit$psi, f0)
  sp <- sqrt(sd_new^2 + se_mean^2)
  out <- data.frame(f = f0, se_mean = se_mean, sd_new = sd_new)
  for (L in levels) {
    z <- qnorm(1 - (1 - L) / 2)
    out[[paste0("lo", lab(L))]] <- f0 - z * sp
    out[[paste0("hi", lab(L))]] <- f0 + z * sp
  }
  out
}
# B/C. Parametric dan residual bootstrap PI (percentile predictive interval).
# Tiap replikasi: bangkitkan data dari model terestimasi -> fit ulang -> f pada x0 ->
# observasi baru dari model hasil fit ulang. Varian "err" memakai galat prediksi
# D* = Y*_new - f*_b(x0) sebagai cek. cutoff = NULL berarti tanpa pemotongan.
pi_bootstrap <- function(model, fit, wb, age, nd, jenis = c("param", "resid"), B, n_new,
                         seed, levels = LEVELS, cutoff = NULL) {
  jenis <- match.arg(jenis); set.seed(seed); n <- length(wb); m0 <- nrow(nd)
  e_std <- fit$es / fit$sigma_ml; e_std <- (e_std - mean(e_std)) / sd_pop(e_std)
  sd_fit <- fit$sigma * fit$s
  f0  <- model$f(fit$th, nd$wb, nd$Age)
  sd0 <- fit$sigma * s_fun(fit$vform, fit$psi, f0)
  tarik <- function(m) if (jenis == "param") rnorm(m) else sample(e_std, m, replace = TRUE)
  Ypred <- matrix(NA_real_, m0, B * n_new); Derr <- Ypred
  Fb <- matrix(NA_real_, B, m0); Par <- matrix(NA_real_, B, length(fit$th) + 1)
  gagal <- 0L; ditarik_ulang <- 0L; n_negatif <- 0L; y_min <- Inf; status <- rep(NA_integer_, B)
  for (b in seq_len(B)) {
    ystar <- fit$fitted + sd_fit * tarik(n)
    if (!is.null(cutoff)) {
      buruk <- ystar <= cutoff
      while (any(buruk)) {
        ditarik_ulang <- ditarik_ulang + sum(buruk)
        ystar[buruk] <- (fit$fitted + sd_fit * tarik(n))[buruk]
        buruk <- ystar <= cutoff
      }
    }
    y_min <- min(y_min, ystar); n_negatif <- n_negatif + sum(ystar < 0)
    fb <- fit_aman(model, ystar, wb, age, fit$th, fit$vform)
    if (is.null(fb)) { gagal <- gagal + 1L; next }
    status[b] <- fb$status
    f_b  <- model$f(fb$th, nd$wb, nd$Age)
    sd_b <- fb$sigma * s_fun(fb$vform, fb$psi, f_b)
    eb   <- fb$es / fb$sigma_ml
    kol  <- (b - 1) * n_new + seq_len(n_new)
    E1 <- matrix(if (jenis == "param") rnorm(m0 * n_new) else sample(eb, m0 * n_new, replace = TRUE),
                 m0, n_new)
    Ypred[, kol] <- f_b + sd_b * E1
    E2 <- matrix(tarik(m0 * n_new), m0, n_new)
    Derr[, kol] <- (f0 + sd0 * E2) - f_b
    Fb[b, ] <- f_b; Par[b, ] <- c(repar(fb$th), fb$psi)
  }
  q <- function(M, p) apply(M, 1, quantile, probs = p, na.rm = TRUE, names = FALSE)
  pred <- data.frame(f = f0); err <- data.frame(f = f0)
  for (L in levels) {
    a <- (1 - L) / 2
    pred[[paste0("lo", lab(L))]] <- q(Ypred, a);      pred[[paste0("hi", lab(L))]] <- q(Ypred, 1 - a)
    err[[paste0("lo", lab(L))]]  <- f0 + q(Derr, a);  err[[paste0("hi", lab(L))]]  <- f0 + q(Derr, 1 - a)
  }
  colnames(Par) <- c(names(repar(fit$th)), "delta")
  list(pred = pred, err = err, Fb = Fb, Par = Par, Ypred = Ypred, status = status, gagal = gagal,
       ditarik_ulang = ditarik_ulang, n_negatif = n_negatif, y_min = y_min, total_nilai = B * n)
}
# Galat Monte Carlo sebuah batas PI: simpangan baku antar-10 kelompok replikasi / sqrt(10)
mc_se_batas <- function(Ypred_baris, p, n_batch = 10) {
  ok <- Ypred_baris[is.finite(Ypred_baris)]
  kel <- cut(seq_along(ok), n_batch, labels = FALSE)
  sd(tapply(ok, kel, quantile, probs = p, names = FALSE)) / sqrt(n_batch)
}
# Bootstrap dengan fit ulang untuk parameter dan mean function: row atau kelompok
boot_param <- function(nama, unit = NULL, B, seed, data = dat) {
  model <- MODEL[[nama]]; fit0 <- FIT[[nama]]; set.seed(seed); n <- nrow(data)
  iu <- if (is.null(unit)) NULL else split(seq_len(n), unit)
  nm <- c(names(repar(fit0$th)), "delta", "sigma_ml", PTS$label)
  draws <- matrix(NA_real_, B, length(nm), dimnames = list(NULL, nm)); status <- rep(9L, B)
  for (b in seq_len(B)) {
    idx <- if (is.null(unit)) sample.int(n, n, replace = TRUE) else
      unlist(iu[sample.int(length(iu), replace = TRUE)], use.names = FALSE)
    fb <- fit_aman(model, data$Strength[idx], data$wb[idx], data$Age[idx], fit0$th)
    if (is.null(fb)) next
    status[b] <- fb$status
    draws[b, ] <- c(repar(fb$th), fb$psi, fb$sigma_ml, model$f(fb$th, PTS$wb, PTS$Age))
  }
  list(draws = draws, status = status)
}
# BCa (hanya untuk row dan identical composition): jackknife hapus-satu-unit
bca_ci <- function(est, boot, jack, alpha = 0.05) {
  boot <- boot[is.finite(boot)]
  z0 <- qnorm((sum(boot < est) + 0.5 * sum(boot == est)) / length(boot))
  jm <- mean(jack); a <- sum((jm - jack)^3) / (6 * sum((jm - jack)^2)^1.5)
  adj <- function(p) pnorm(z0 + (z0 + qnorm(p)) / (1 - a * (z0 + qnorm(p))))
  c(lo = unname(quantile(boot, adj(alpha / 2))), hi = unname(quantile(boot, adj(1 - alpha / 2))),
    z0 = z0, a = a)
}
jackknife <- function(nama, unit) {
  model <- MODEL[[nama]]; fit0 <- FIT[[nama]]; u <- unique(unit)
  out <- t(vapply(u, function(v) {
    m <- unit != v
    fb <- fit_aman(model, dat$Strength[m], dat$wb[m], dat$Age[m], fit0$th)
    c(repar(fb$th), fb$psi, fb$sigma_ml, model$f(fb$th, PTS$wb, PTS$Age))
  }, numeric(length(fit0$th) + 2 + nrow(PTS))))
  out
}
# Lipatan validasi: seluruh baris dari satu unit berada di lipatan yang sama
buat_lipatan <- function(unit, K, seed) {
  set.seed(seed); u <- unique(unit)
  lip_u <- sample(rep_len(seq_len(K), length(u)))
  lip_u[match(unit, u)]
}
# Validasi di luar sampel. Model, variance function, dan PI HANYA dari data latih.
validasi <- function(nama, unit, nama_skema, R = R_REP, K = K_FOLD, seed, nested_rep = 1,
                     data = dat, vform = "power", bootstrap = TRUE) {
  model <- MODEL[[nama]]; fit0 <- FIT[[nama]]; n <- nrow(data)
  if (is.null(unit)) unit <- seq_len(n)
  potong <- list(); ip <- 0L
  tambah <- function(te, r, k, f, sd_new, metode, L, lo, hi) {
    ip <<- ip + 1L
    potong[[ip]] <<- data.frame(skema = nama_skema, model = nama, rep = r, lipatan = k,
                                baris = data$baris[te], y = data$Strength[te], f = f,
                                sd_new = sd_new, metode = metode, level = L, lo = lo, hi = hi)
  }
  for (r in seq_len(R)) {
    lip <- buat_lipatan(unit, K, seed + r)
    for (k in seq_len(K)) {
      te <- which(lip == k); tr <- which(lip != k)
      ftr <- fit_aman(model, data$Strength[tr], data$wb[tr], data$Age[tr], fit0$th, vform)
      if (is.null(ftr)) stop("Fit gagal pada data latih: ", nama_skema, " ulangan ", r, " lipatan ", k)
      Vtr <- cov_gnls(model, ftr, data$wb[tr], data$Age[tr])$model
      nd  <- data[te, c("wb", "Age")]
      pn  <- pi_normal(model, ftr, Vtr, nd)
      for (L in LEVELS) tambah(te, r, k, pn$f, pn$sd_new, "A_normal", L,
                               pn[[paste0("lo", lab(L))]], pn[[paste0("hi", lab(L))]])
      if (bootstrap && r <= nested_rep) for (jenis in c("param", "resid")) {
        pb <- pi_bootstrap(model, ftr, data$wb[tr], data$Age[tr], nd, jenis, B_nested, 5,
                           seed + 1000 * r + 10 * k + (jenis == "resid"))
        for (L in LEVELS) tambah(te, r, k, pn$f, pn$sd_new,
                                 if (jenis == "param") "B_parametric" else "C_residual", L,
                                 pb$pred[[paste0("lo", lab(L))]], pb$pred[[paste0("hi", lab(L))]])
      }
    }
    cat("  validasi", nama, nama_skema, ": ulangan", r, "selesai\n")
  }
  do.call(rbind, potong)
}
# Ringkasan coverage. 'by' harus memuat "level".
ringkas_cov <- function(df, by) {
  df$cover <- as.numeric(df$y >= df$lo & df$y <= df$hi)
  df$low <- as.numeric(df$y < df$lo); df$up <- as.numeric(df$y > df$hi)
  df$width <- df$hi - df$lo
  df$is <- df$width + (2 / (1 - df$level)) * (pmax(df$lo - df$y, 0) + pmax(df$y - df$hi, 0))
  kunci_by <- interaction(df[by], drop = TRUE, sep = " | ")
  out <- do.call(rbind, lapply(split(df, kunci_by), function(g) data.frame(
    g[1, by, drop = FALSE], n = length(unique(g$baris)),
    coverage = mean(g$cover), coverage_error = mean(g$cover) - g$level[1],
    mean_width = mean(g$width), median_width = median(g$width),
    lower_miss = mean(g$low), upper_miss = mean(g$up),
    interval_score = mean(g$is), mean_error = mean(g$y - g$f),
    sd_std_resid = sqrt(mean(((g$y - g$f) / g$sd_new)^2)))))
  rownames(out) <- NULL
  out
}
# Galat baku sebuah rata-rata per baris bila baris dalam satu unit boleh berkorelasi
se_unit <- function(nilai, unit) {
  d <- nilai - mean(nilai); s <- tapply(d, unit, sum); G <- length(s)
  sqrt(G / (G - 1) * sum(s^2)) / length(d)
}

# ---- M. MODEL-SELECTION UNCERTAINTY -----------------------------------------
# Tujuan: mengulang langkah seleksi yang dapat diulang secara mekanis (C lawan D,
# dan bentuk variance function) di dalam tiap replikasi bootstrap, lalu melihat
# apakah interval "model terpilih" berbeda dari interval Model D.
seleksi_boot <- function(unit, B, seed) {
  set.seed(seed); n <- nrow(dat); iu <- if (is.null(unit)) NULL else split(seq_len(n), unit)
  res <- vector("list", B)
  for (b in seq_len(B)) {
    idx <- if (is.null(unit)) sample.int(n, n, replace = TRUE) else
      unlist(iu[sample.int(length(iu), replace = TRUE)], use.names = FALSE)
    y <- dat$Strength[idx]; w <- dat$wb[idx]; a <- dat$Age[idx]
    fD  <- fit_aman(MODEL$D, y, w, a, fitD$th, "power")
    fC  <- fit_aman(MODEL$C, y, w, a, fitC$th, "power")
    fD0 <- fit_aman(MODEL$D, y, w, a, fitD$th, "const")
    fDe <- fit_aman(MODEL$D, y, w, a, fitD$th, "exp")
    if (is.null(fD) || is.null(fC) || is.null(fD0) || is.null(fDe)) next
    pilih <- if (aic(fD) <= aic(fC)) "D" else "C"
    aic_var <- c(const = aic(fD0), power = aic(fD), exp = aic(fDe))
    fref_D <- MODEL$D$f(fD$th, W0, A0)
    res[[b]] <- data.frame(b = b, pilih = pilih, deviance_kappa = 2 * (fD$loglik - fC$loglik),
                           var_pilih = names(which.min(aic_var)), kappa = fD$th[5],
                           fref_D = fref_D,
                           fref_terpilih = if (pilih == "D") fref_D else MODEL$C$f(fC$th, W0, A0))
  }
  do.call(rbind, res)
}
skema_M <- list(row = NULL, identical = dat$g_ident, linked05 = dat$g_l05, linked10 = dat$g_l10)
M_raw <- cache("M_seleksi", lapply(seq_along(skema_M), function(i)
  cbind(skema = names(skema_M)[i], seleksi_boot(skema_M[[i]], B_sel, seed_utama + 100 + i))))
M_raw <- do.call(rbind, M_raw)
M_tab <- do.call(rbind, lapply(split(M_raw, M_raw$skema), function(g) data.frame(
  skema = g$skema[1], replikasi_berhasil = nrow(g),
  D_terpilih = mean(g$pilih == "D"), deviance_kappa_min = min(g$deviance_kappa),
  kappa_min = min(g$kappa),
  var_const = mean(g$var_pilih == "const"), var_power = mean(g$var_pilih == "power"),
  var_exp = mean(g$var_pilih == "exp"),
  ci_D_lo = quantile(g$fref_D, 0.025), ci_D_hi = quantile(g$fref_D, 0.975),
  ci_terpilih_lo = quantile(g$fref_terpilih, 0.025), ci_terpilih_hi = quantile(g$fref_terpilih, 0.975))))
rownames(M_tab) <- NULL
cat("\n== M. SELEKSI DIULANG DI DALAM BOOTSTRAP ==\n"); print(M_tab, digits = 4)
tulis(M_tab, "M_seleksi")
# Selisih antar-model pada lima titik (ketidakpastian pilihan kurva, tidak ada di CI)
M_selisih <- data.frame(titik = PTS$label, f_D = f_D(fitD$th, PTS$wb, PTS$Age),
                        f_C = f_C(fitC$th, PTS$wb, PTS$Age))
M_selisih$selisih <- M_selisih$f_D - M_selisih$f_C
print(M_selisih, digits = 4); tulis(M_selisih, "M_selisih_antar_model")
# Kepekaan terhadap variance function (data penuh, Model D)
M_var <- do.call(rbind, lapply(c("const", "power", "exp"), function(vf) {
  ft <- fit_aman(MODEL$D, dat$Strength, dat$wb, dat$Age, fitD$th, vf)
  V  <- cov_gnls(MODEL$D, ft, dat$wb, dat$Age)$model
  pn <- pi_normal(MODEL$D, ft, V, PTS)
  data.frame(variance_function = vf, psi = ft$psi, loglik = ft$loglik, AIC = aic(ft),
             titik = PTS$label, f = pn$f, se_mean = pn$se_mean, sd_new = pn$sd_new)
}))
print(M_var, digits = 4); tulis(M_var, "M_variance_function")

# ---- N. PREDICTION INTERVAL PADA LIMA TITIK ---------------------------------
N_A <- pi_normal(MODEL$D, fitD, VD$model, PTS)
N_B <- cache("N_param", pi_bootstrap(MODEL$D, fitD, dat$wb, dat$Age, PTS, "param", B_boot, N_NEW,
                                     seed_utama + 201))
N_C <- cache("N_resid", pi_bootstrap(MODEL$D, fitD, dat$wb, dat$Age, PTS, "resid", B_boot, N_NEW,
                                     seed_utama + 202))
N_tab <- do.call(rbind, lapply(LEVELS, function(L) data.frame(
  level = L, titik = PTS$label, f = N_A$f, sd_new = N_A$sd_new, se_mean = N_A$se_mean,
  bagian_parameter = N_A$se_mean^2 / (N_A$sd_new^2 + N_A$se_mean^2),
  A_lo = N_A[[paste0("lo", lab(L))]], A_hi = N_A[[paste0("hi", lab(L))]],
  B_lo = N_B$pred[[paste0("lo", lab(L))]], B_hi = N_B$pred[[paste0("hi", lab(L))]],
  C_lo = N_C$pred[[paste0("lo", lab(L))]], C_hi = N_C$pred[[paste0("hi", lab(L))]],
  C_err_lo = N_C$err[[paste0("lo", lab(L))]], C_err_hi = N_C$err[[paste0("hi", lab(L))]],
  mcse_B_lo = vapply(seq_len(nrow(PTS)), function(i) mc_se_batas(N_B$Ypred[i, ], (1 - L) / 2), 0),
  mcse_C_lo = vapply(seq_len(nrow(PTS)), function(i) mc_se_batas(N_C$Ypred[i, ], (1 - L) / 2), 0))))
cat("\n== N. PI PADA LIMA TITIK (A normal-theory, B parametric, C residual) ==\n")
print(N_tab, digits = 4); tulis(N_tab, "N_pi_lima_titik")
cat("Replikasi gagal B, C:", N_B$gagal, N_C$gagal, "| nilai simulasi negatif B, C:",
    N_B$n_negatif, N_C$n_negatif, "dari", N_B$total_nilai, "\n")
# Bentuk sebaran residual terstandar (menentukan layak-tidaknya asumsi normal)
e_std <- fitD$es / fitD$sigma_ml
N_bentuk <- data.frame(skewness = mean((e_std - mean(e_std))^3) / sd_pop(e_std)^3,
                       excess_kurtosis = mean((e_std - mean(e_std))^4) / sd_pop(e_std)^4 - 3,
                       q025 = quantile(e_std, 0.025), q975 = quantile(e_std, 0.975),
                       q10 = quantile(e_std, 0.10), q90 = quantile(e_std, 0.90))
print(N_bentuk, digits = 4); tulis(N_bentuk, "N_bentuk_residual")
# Grid untuk R Markdown dan dashboard: mean, SE di bawah empat covariance, dan PI
GRID <- expand.grid(wb = seq(0.25, 0.85, by = 0.025), Age = c(3, 7, 14, 28, 56, 90, 180, 365))
G_A <- pi_normal(MODEL$D, fitD, VD$model, GRID)
G_C <- cache("N_grid_resid", pi_bootstrap(MODEL$D, fitD, dat$wb, dat$Age, GRID, "resid", B_boot, N_NEW_GRID,
                                          seed_utama + 203)$pred)
J_grid <- J_D(fitD$th, GRID$wb, GRID$Age)
se_dari <- function(V) sqrt(pmax(rowSums((J_grid %*% V) * J_grid), 0))
grid_tab <- data.frame(GRID, f = G_A$f, sd_new = G_A$sd_new, se_model = se_dari(VD$model),
                       se_sandwich = se_dari(VD$sandwich), se_identical = se_dari(VD$identical),
                       se_linked05 = se_dari(VD$linked05), f_model_C = f_C(fitC$th, GRID$wb, GRID$Age),
                       warning_age_di_atas_100 = GRID$Age > 100)
for (L in LEVELS) {
  grid_tab[[paste0("pi_normal_lo", lab(L))]] <- G_A[[paste0("lo", lab(L))]]
  grid_tab[[paste0("pi_normal_hi", lab(L))]] <- G_A[[paste0("hi", lab(L))]]
  grid_tab[[paste0("pi_resid_lo", lab(L))]]  <- G_C[[paste0("lo", lab(L))]]
  grid_tab[[paste0("pi_resid_hi", lab(L))]]  <- G_C[[paste0("hi", lab(L))]]
}
tulis(grid_tab, "N_grid_prediksi")

# ---- O dan P. OUT-OF-SAMPLE VALIDATION DAN COVERAGE -------------------------
skema_V <- list("random-row" = NULL, "identical" = dat$g_ident,
                "linked 0.5" = dat$g_l05, "linked 1.0" = dat$g_l10)
OP_raw <- cache("OP_validasi", do.call(rbind, lapply(c("D", "C"), function(nm)
  do.call(rbind, lapply(seq_along(skema_V), function(i)
    validasi(nm, skema_V[[i]], names(skema_V)[i], seed = seed_utama + 300 + 10 * i))))))
P_tab <- ringkas_cov(OP_raw, c("model", "skema", "metode", "level"))
cat("\n== P. COVERAGE DI LUAR SAMPEL ==\n")
print(P_tab[P_tab$model == "D", ], digits = 4); tulis(P_tab, "P_coverage")
# Akurasi titik (RMSE, MAE) per model dan skema, dari metode A (5 ulangan)
dA <- OP_raw[OP_raw$metode == "A_normal" & OP_raw$level == 0.95, ]
O_akurasi <- do.call(rbind, lapply(split(dA, interaction(dA$model, dA$skema, drop = TRUE)), function(g)
  data.frame(model = g$model[1], skema = g$skema[1],
             RMSE = sqrt(mean((g$y - g$f)^2)), MAE = mean(abs(g$y - g$f)))))
rownames(O_akurasi) <- NULL; print(O_akurasi, digits = 4); tulis(O_akurasi, "O_akurasi")
# Galat baku coverage: baris independen, dan dengan unit lipatan tiap skema
unit_skema <- list("random-row" = dat$baris, "identical" = dat$g_ident,
                   "linked 0.5" = dat$g_l05, "linked 1.0" = dat$g_l10)
P_se <- do.call(rbind, lapply(names(unit_skema), function(sk) do.call(rbind, lapply(LEVELS, function(L) {
  g <- OP_raw[OP_raw$model == "D" & OP_raw$skema == sk & OP_raw$metode == "A_normal" & OP_raw$level == L, ]
  cov_baris <- tapply(as.numeric(g$y >= g$lo & g$y <= g$hi), g$baris, mean)
  data.frame(skema = sk, level = L, coverage = mean(cov_baris),
             se_baris_independen = sqrt(L * (1 - L) / length(cov_baris)),
             se_per_unit_skema = se_unit(cov_baris, unit_skema[[sk]][as.integer(names(cov_baris))]))
}))))
print(P_se, digits = 4); tulis(P_se, "P_galat_baku_coverage")

# ---- Q, R, S. COVERAGE MENURUT AGE, w/b, DAN AGE x w/b ----------------------
QRS <- merge(OP_raw[OP_raw$model == "D" & OP_raw$skema == "linked 0.5", ],
             dat[c("baris", "age_grp", "wb_grp")], by = "baris")
Q_tab <- ringkas_cov(QRS, c("metode", "level", "age_grp"))
R_tab <- ringkas_cov(QRS, c("metode", "level", "wb_grp"))
S_tab <- ringkas_cov(QRS, c("metode", "level", "age_grp", "wb_grp"))
S_tab$dinilai <- S_tab$n >= 30            # sel dengan n < 30 tidak ditafsirkan
cat("\n== Q. COVERAGE MENURUT AGE (Model D, linked 0.5) ==\n"); print(Q_tab, digits = 3)
cat("\n== R. COVERAGE MENURUT w/b ==\n"); print(R_tab, digits = 3)
cat("\n== S. COVERAGE AGE x w/b (hanya 95%, metode A) ==\n")
print(S_tab[S_tab$level == 0.95 & S_tab$metode == "A_normal", ], digits = 3)
tulis(Q_tab, "Q_coverage_age"); tulis(R_tab, "R_coverage_wb"); tulis(S_tab, "S_coverage_age_wb")
# Model C pada strata yang sama, untuk bagian X
QRS_C <- merge(OP_raw[OP_raw$model == "C" & OP_raw$skema == "linked 0.5" & OP_raw$metode == "A_normal", ],
               dat[c("baris", "age_grp", "wb_grp")], by = "baris")
S_tab_C <- ringkas_cov(QRS_C, c("metode", "level", "age_grp", "wb_grp"))
tulis(S_tab_C, "S_coverage_age_wb_modelC")

# ---- T. CALIBRATION DAN SHARPNESS -------------------------------------------
# Interval score sudah ada di P_tab. Dua pembanding: (i) Model D dengan varians
# konstan, (ii) interval tanpa prediktor (kuantil Strength data latih).
T_const <- cache("T_varians_konstan", validasi("D", dat$g_l05, "linked 0.5", seed = seed_utama + 330,
                                               vform = "const", bootstrap = FALSE))
T_const$metode <- "D_varians_konstan"
baseline <- function(unit, R, K, seed) {
  out <- list(); ip <- 0L
  for (r in seq_len(R)) { lip <- buat_lipatan(unit, K, seed + r)
    for (k in seq_len(K)) { te <- which(lip == k); tr <- which(lip != k)
      for (L in LEVELS) { ip <- ip + 1L
        qq <- quantile(dat$Strength[tr], c((1 - L) / 2, 1 - (1 - L) / 2))
        out[[ip]] <- data.frame(skema = "linked 0.5", model = "tanpa_prediktor", rep = r, lipatan = k,
                                baris = te, y = dat$Strength[te], f = mean(dat$Strength[tr]),
                                sd_new = sd(dat$Strength[tr]), metode = "tanpa_prediktor",
                                level = L, lo = unname(qq[1]), hi = unname(qq[2])) } } }
  do.call(rbind, out)
}
T_base <- baseline(dat$g_l05, R_REP, K_FOLD, seed_utama + 330)
T_tab <- rbind(
  ringkas_cov(OP_raw[OP_raw$skema == "linked 0.5", ], c("model", "metode", "level")),
  ringkas_cov(T_const, c("model", "metode", "level")),
  ringkas_cov(T_base, c("model", "metode", "level")))
cat("\n== T. CALIBRATION DAN SHARPNESS (linked 0.5) ==\n")
print(T_tab[c("model", "metode", "level", "coverage", "mean_width", "interval_score")], digits = 4)
tulis(T_tab, "T_calibration_sharpness")
# Varians konstan per w/b: menunjukkan coverage keseluruhan bisa menyembunyikan salah kalibrasi
T_const_wb <- ringkas_cov(merge(T_const, dat[c("baris", "wb_grp")], by = "baris"),
                          c("metode", "level", "wb_grp"))
print(T_const_wb[T_const_wb$level == 0.95, ], digits = 3); tulis(T_const_wb, "T_varians_konstan_per_wb")

# ---- U. SENSITIVITY TO DEPENDENCE ASSUMPTIONS -------------------------------
skema_U <- list(row = NULL, identical = dat$g_ident, linked05 = dat$g_l05,
                linked10 = dat$g_l10, block = dat$g_blok)
U_boot <- cache("U_boot", lapply(c(D = "D", C = "C"), function(nm)
  lapply(seq_along(skema_U), function(i) boot_param(nm, skema_U[[i]], B_boot, seed_utama + 400 + i))))
U_tab <- do.call(rbind, lapply(c("D", "C"), function(nm) do.call(rbind, lapply(seq_along(skema_U), function(i) {
  b <- U_boot[[nm]][[i]]; ok <- b$status != 9L; dr <- b$draws[ok, , drop = FALSE]
  est <- c(repar(FIT[[nm]]$th), FIT[[nm]]$psi, FIT[[nm]]$sigma_ml,
           MODEL[[nm]]$f(FIT[[nm]]$th, PTS$wb, PTS$Age))
  data.frame(model = nm, skema = names(skema_U)[i], besaran = colnames(dr), estimasi = est,
             replikasi = length(ok), berhasil = sum(ok), gagal = sum(!ok), batas_iterasi = sum(b$status == 1L),
             rata2_boot = colMeans(dr), bias = colMeans(dr) - est, se = apply(dr, 2, sd),
             pct_lo = apply(dr, 2, quantile, 0.025), pct_hi = apply(dr, 2, quantile, 0.975))
}))))
rownames(U_tab) <- NULL
cat("\n== U. BOOTSTRAP MENURUT SKEMA (sensitivity to dependence assumptions) ==\n")
print(U_tab[U_tab$model == "D" & U_tab$besaran %in% c("S_ref", "kappa", "p", "delta"), ], digits = 4)
tulis(U_tab, "U_dependence_bootstrap")
# BCa untuk row dan identical composition (Model D)
U_bca <- cache("U_bca", {
  jk_row <- jackknife("D", dat$baris); jk_id <- jackknife("D", dat$g_ident)
  est <- c(repar(fitD$th), fitD$psi, fitD$sigma_ml, f_D(fitD$th, PTS$wb, PTS$Age))
  rbind(data.frame(skema = "row", besaran = colnames(U_boot$D[[1]]$draws),
                   t(vapply(seq_along(est), function(j) bca_ci(est[j], U_boot$D[[1]]$draws[, j], jk_row[, j]), numeric(4)))),
        data.frame(skema = "identical", besaran = colnames(U_boot$D[[2]]$draws),
                   t(vapply(seq_along(est), function(j) bca_ci(est[j], U_boot$D[[2]]$draws[, j], jk_id[, j]), numeric(4)))))
})
print(U_bca, digits = 4); tulis(U_bca, "U_bca")
# Seberapa besar bagian varians prediktif yang berasal dari parameter, per asumsi
U_bagian <- do.call(rbind, lapply(c("model", "identical", "linked05", "linked10"), function(nmV) {
  pn <- pi_normal(MODEL$D, fitD, VD[[nmV]], PTS)
  data.frame(covariance = nmV, titik = PTS$label, se_mean = pn$se_mean, sd_new = pn$sd_new,
             bagian_parameter = pn$se_mean^2 / (pn$sd_new^2 + pn$se_mean^2),
             pi95_lo = pn$lo95, pi95_hi = pn$hi95)
}))
print(U_bagian, digits = 4); tulis(U_bagian, "U_bagian_parameter_dalam_PI")
# Coverage per kelompok komposisi identik dan ICC residual terstandar di luar sampel
gU <- OP_raw[OP_raw$model == "D" & OP_raw$skema == "linked 0.5" & OP_raw$metode == "A_normal" &
               OP_raw$level == 0.95, ]
z_baris   <- tapply((gU$y - gU$f) / gU$sd_new, gU$baris, mean)
cov_baris <- tapply(as.numeric(gU$y >= gU$lo & gU$y <= gU$hi), gU$baris, mean)
id_baris  <- dat$g_ident[as.integer(names(z_baris))]
icc_anova <- function(e, g) {
  ng <- tapply(e, g, length); mg <- tapply(e, g, mean); k <- length(ng); N <- length(e)
  msb <- sum(ng * (mg - mean(e))^2) / (k - 1); msw <- sum((e - mg[as.character(g)])^2) / (N - k)
  n0 <- (N - sum(ng^2) / N) / (k - 1)
  c(icc = (msb - msw) / (msb + (n0 - 1) * msw), sd_dalam_kelompok = sqrt(msw))
}
ukuran <- tapply(cov_baris, id_baris, length); cov_g <- tapply(cov_baris, id_baris, mean)
besar <- ukuran >= 3
U_komposisi <- data.frame(
  kelompok_3_baris_atau_lebih = sum(besar),
  bagian_coverage_100 = mean(cov_g[besar] >= 0.999),
  bagian_coverage_di_bawah_80 = mean(cov_g[besar] < 0.80),
  icc_residual_terstandar = icc_anova(z_baris, id_baris)["icc"],
  sd_dalam_komposisi = icc_anova(z_baris, id_baris)["sd_dalam_kelompok"])
print(U_komposisi, digits = 3); tulis(U_komposisi, "U_coverage_per_komposisi")

# ---- V. SENSITIVITY 25 IDENTICAL ROWS ---------------------------------------
dat1005 <- dat[!dat$identik_tambahan, ]
stopifnot(nrow(dat1005) == 1005)
V_fit <- lapply(c(D = "D", C = "C"), function(nm)
  fit_aman(MODEL[[nm]], dat1005$Strength, dat1005$wb, dat1005$Age, FIT[[nm]]$th))
V_par <- do.call(rbind, lapply(c("D", "C"), function(nm) {
  a <- FIT[[nm]]; b <- V_fit[[nm]]
  data.frame(model = nm, besaran = c(names(repar(a$th)), "delta", "sigma", PTS$label),
             n1030 = c(repar(a$th), a$psi, a$sigma, MODEL[[nm]]$f(a$th, PTS$wb, PTS$Age)),
             n1005 = c(repar(b$th), b$psi, b$sigma, MODEL[[nm]]$f(b$th, PTS$wb, PTS$Age)))
}))
V_par$selisih <- V_par$n1005 - V_par$n1030
V_cv <- cache("V_validasi_1005", do.call(rbind, lapply(c("D", "C"), function(nm)
  validasi(nm, dat1005$g_l05, "linked 0.5 (n=1005)", seed = seed_utama + 500,
           data = dat1005, bootstrap = FALSE))))
V_cov <- ringkas_cov(V_cv, c("model", "skema", "metode", "level"))
V_rmse <- do.call(rbind, lapply(split(V_cv[V_cv$level == 0.95, ], V_cv$model[V_cv$level == 0.95]), function(g)
  data.frame(model = g$model[1], RMSE = sqrt(mean((g$y - g$f)^2)), MAE = mean(abs(g$y - g$f)))))
V_pi <- pi_normal(MODEL$D, V_fit$D, cov_gnls(MODEL$D, V_fit$D, dat1005$wb, dat1005$Age)$model, PTS)
cat("\n== V. n = 1030 vs n = 1005 ==\n"); print(V_par, digits = 4); print(V_rmse, digits = 4)
print(V_cov, digits = 4)
V_pi_tab <- data.frame(titik = PTS$label, pi95_lo_1030 = N_A$lo95, pi95_hi_1030 = N_A$hi95,
                       pi95_lo_1005 = V_pi$lo95, pi95_hi_1005 = V_pi$hi95)
print(V_pi_tab, digits = 4); tulis(V_pi_tab, "V_pi_1005")
tulis(V_par, "V_parameter"); tulis(V_cov, "V_coverage_1005"); tulis(V_rmse, "V_rmse_1005")

# ---- X. TABEL FINAL MODEL C VS MODEL D --------------------------------------
ambil <- function(tab, ...) { k <- list(...); for (nm in names(k)) tab <- tab[tab[[nm]] == k[[nm]], ]; tab }
x_baris <- function(nm) {
  cv <- ambil(P_tab, model = nm, skema = "linked 0.5", metode = "A_normal")
  ak <- ambil(O_akurasi, model = nm, skema = "linked 0.5")
  ub <- U_tab[U_tab$model == nm, ]
  lebar <- function(sk, bes) { z <- ub[ub$skema == sk & ub$besaran == bes, ]; z$pct_hi - z$pct_lo }
  sel <- if (nm == "D") ringkas_cov(QRS[QRS$metode == "A_normal", ], c("metode", "level", "age_grp", "wb_grp")) else S_tab_C
  sel <- sel[sel$level == 0.95 & sel$n >= 30, ]
  data.frame(model = nm, RMSE = ak$RMSE, MAE = ak$MAE,
             fit_bootstrap_gagal = sum(ub$gagal[ub$besaran == "S_ref"]),
             lebar_CI_fref_row = lebar("row", PTS$label[1]), lebar_CI_fref_linked05 = lebar("linked05", PTS$label[1]),
             cov80 = cv$coverage[cv$level == 0.80], cov90 = cv$coverage[cv$level == 0.90],
             cov95 = cv$coverage[cv$level == 0.95], lebar_PI95 = cv$mean_width[cv$level == 0.95],
             IS80 = cv$interval_score[cv$level == 0.80], IS95 = cv$interval_score[cv$level == 0.95],
             galat_sel_min = min(sel$mean_error), galat_sel_maks = max(sel$mean_error),
             cov95_sel_min = min(sel$coverage), cov95_sel_maks = max(sel$coverage),
             prediksi_wb0235_Age365 = MODEL[[nm]]$f(FIT[[nm]]$th, 0.235, 365))
}
X_tab <- rbind(x_baris("D"), x_baris("C"))
cat("\n== X. MODEL C VS MODEL D ==\n"); print(t(X_tab), digits = 4); tulis(X_tab, "X_model_C_vs_D")
# Selisih berpasangan D - C dengan galat baku per kelompok linked 0.5
pas <- function(kolom) {
  g <- OP_raw[OP_raw$skema == "linked 0.5" & OP_raw$metode == "A_normal" & OP_raw$level == 0.95, ]
  g$mse <- (g$y - g$f)^2
  g$is  <- (g$hi - g$lo) + 40 * (pmax(g$lo - g$y, 0) + pmax(g$y - g$hi, 0))
  d <- tapply(g[[kolom]][g$model == "D"], g$baris[g$model == "D"], mean) -
       tapply(g[[kolom]][g$model == "C"], g$baris[g$model == "C"], mean)
  unit <- dat$g_l05[as.integer(names(d))]; s <- tapply(d, unit, sum); G <- length(s)
  c(selisih = mean(d), se = sqrt(G / (G - 1) * sum(s^2)) / length(d))
}
X_pas <- rbind(MSE = pas("mse"), IS95 = pas("is"))
print(X_pas, digits = 4); tulis(data.frame(ukuran = rownames(X_pas), X_pas), "X_selisih_berpasangan")

# ---- PENUTUP ----------------------------------------------------------------
# Objek untuk R Markdown dan dashboard
saveRDS(list(fitD = fitD, fitC = fitC, VD = VD, VC = VC, tab_fit = tab_fit, PTS = PTS, LEVELS = LEVELS,
             W0 = W0, A0 = A0, grid_tab = grid_tab, N_tab = N_tab,
             e_std = (fitD$es / fitD$sigma_ml - mean(fitD$es / fitD$sigma_ml)) / sd_pop(fitD$es / fitD$sigma_ml),
             data = dat[c("baris", KOMP, "Age", "Strength", "wb", "g_ident", "g_l05", "g_l10", "age_grp", "wb_grp",
                          "identik_tambahan")],
             MODE_UJI = MODE_UJI, seed_utama = seed_utama),
        file.path(dir_out, paste0("objek_final", akhiran, ".rds")))
writeLines(capture.output(sessionInfo()), file.path(dir_out, paste0("sessionInfo", akhiran, ".txt")))
cat("\nSelesai. MODE_UJI =", MODE_UJI, "| berkas ada di folder", dir_out, "\n")
cat("Angka laporan hanya diambil dari run dengan MODE_UJI = FALSE.\n")
sink()

# ================================================================
# DAFTAR PUSTAKA (periksa rincian bibliografi di sumber asli sebelum dikutip)
# ================================================================
# Abrams, D. A. (1918). Design of Concrete Mixtures. Bulletin 1, Structural Materials
#   Research Laboratory, Lewis Institute, Chicago.
# Akaike, H. (1974). A new look at the statistical model identification. IEEE Transactions
#   on Automatic Control, 19(6), 716-723.
# Bates, D. M., & Watts, D. G. (1988). Nonlinear Regression Analysis and Its Applications. Wiley.
# Box, G. E. P., & Cox, D. R. (1964). An analysis of transformations. Journal of the Royal
#   Statistical Society, Series B, 26(2), 211-252.
# Breusch, T. S., & Pagan, A. R. (1979). A simple test for heteroscedasticity and random
#   coefficient variation. Econometrica, 47(5), 1287-1294.
# Burnham, K. P., & Anderson, D. R. (2002). Model Selection and Multimodel Inference (2nd ed.). Springer.
# Carroll, R. J., & Ruppert, D. (1984). Power transformations when fitting theoretical models
#   to data. Journal of the American Statistical Association, 79(386), 321-328.
# Carroll, R. J., & Ruppert, D. (1988). Transformation and Weighting in Regression. Chapman & Hall.
# Chatfield, C. (1995). Model uncertainty, data mining and statistical inference. Journal of
#   the Royal Statistical Society, Series A, 158(3), 419-466.
# Cleveland, W. S. (1979). Robust locally weighted regression and smoothing scatterplots.
#   Journal of the American Statistical Association, 74(368), 829-836.
# Cook, R. D., & Tsai, C.-L. (1985). Residuals in nonlinear regression. Biometrika, 72(1), 23-29.
# Davidian, M., & Carroll, R. J. (1987). Variance function estimation. Journal of the American
#   Statistical Association, 82(400), 1079-1091.
# Duan, N. (1983). Smearing estimate: a nonparametric retransformation method. Journal of the
#   American Statistical Association, 78(383), 605-610.
# Jennrich, R. I. (1969). Asymptotic properties of non-linear least squares estimators.
#   Annals of Mathematical Statistics, 40(2), 633-643.
# Koenker, R. (1981). A note on studentizing a test for heteroscedasticity. Journal of
#   Econometrics, 17(1), 107-112.
# Marquardt, D. W. (1963). An algorithm for least-squares estimation of nonlinear parameters.
#   Journal of the Society for Industrial and Applied Mathematics, 11(2), 431-441.
# Neville, A. M. (2011). Properties of Concrete (5th ed.). Pearson.
# Pinheiro, J. C., & Bates, D. M. (2000). Mixed-Effects Models in S and S-PLUS. Springer.
# Ratkowsky, D. A. (1990). Handbook of Nonlinear Regression Models. Marcel Dekker.
# Seber, G. A. F., & Wild, C. J. (1989). Nonlinear Regression. Wiley.
# Shapiro, S. S., & Wilk, M. B. (1965). An analysis of variance test for normality (complete
#   samples). Biometrika, 52(3/4), 591-611.
# Tukey, J. W. (1977). Exploratory Data Analysis. Addison-Wesley.
# Venzon, D. J., & Moolgavkar, S. H. (1988). A method for computing profile-likelihood-based
#   confidence intervals. Applied Statistics, 37(1), 87-94.
# Yang, R. C., Kozak, A., & Smith, J. H. G. (1978). The potential of Weibull-type functions as
#   flexible growth curves. Canadian Journal of Forest Research, 8(4), 424-431.
# Yeh, I.-C. (1998). Modeling of strength of high-performance concrete using artificial neural
#   networks. Cement and Concrete Research, 28(12), 1797-1808.
