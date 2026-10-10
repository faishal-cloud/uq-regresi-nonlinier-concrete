# ==============================================================================
# Menyimpan parameter Model 1 dan Model 2 (model lama) untuk kurva di dashboard
#
# Jalankan SATU KALI dari folder yang memuat "hasil_final" (folder yang sama dengan app.R).
# Skrip ini hanya menambah satu berkas: hasil_final/02_parameter_model_lama.csv.
# Tidak ada berkas lain yang diubah dan tidak ada bootstrap, jadi selesai dalam beberapa detik.
#
# Kedua model di-fit persis seperti di analisis_final_tugas_anareg.R (bagian 4):
# rumus, starting values, dan pengaturan nlsLM yang sama.
# ==============================================================================
library(minpack.lm)

dir_hasil <- "hasil_final"
dat <- as.data.frame(readRDS(file.path(dir_hasil, "objek_final.rds"))$data)
stopifnot(all(c("Strength", "wb", "Age") %in% names(dat)))

ctrl_lm <- nls.lm.control(maxiter = 500, ftol = 1e-10, ptol = 1e-10)
fit_M1 <- nlsLM(Strength ~ alpha / wb^beta * (1 - exp(-gamma * Age)), data = dat,
                start = list(alpha = 30, beta = 1, gamma = 0.05), control = ctrl_lm)
fit_M2 <- nlsLM(Strength ~ alpha * exp(-beta * wb) * log(Age), data = dat,
                start = list(alpha = 30, beta = 1), control = ctrl_lm)

par_lama <- rbind(
  data.frame(model = "M1", parameter = names(coef(fit_M1)), estimasi = unname(coef(fit_M1))),
  data.frame(model = "M2", parameter = names(coef(fit_M2)), estimasi = unname(coef(fit_M2))))
write.csv(par_lama, file.path(dir_hasil, "02_parameter_model_lama.csv"), row.names = FALSE)

# Pemeriksaan: RMSE dari fit ini harus sama dengan RMSE yang sudah tersimpan
sil <- read.csv(file.path(dir_hasil, "02_silsilah_model.csv"), stringsAsFactors = FALSE)
rmse <- function(fit) sqrt(mean(residuals(fit)^2))
cek <- data.frame(
  model          = c("Model 1", "Model 2"),
  RMSE_fit_ini   = c(rmse(fit_M1), rmse(fit_M2)),
  RMSE_tersimpan = c(sil$RMSE[sil$model == "Model 1: asimtotik, Age^1"],
                     sil$RMSE[sil$model == "Model 2: logaritmik"]))
cek$sama <- abs(cek$RMSE_fit_ini - cek$RMSE_tersimpan) < 0.01

cat("\nParameter yang disimpan:\n"); print(par_lama, digits = 6)
cat("\nPemeriksaan RMSE (MPa):\n"); print(cek, digits = 6)
if (all(cek$sama)) {
  cat("\nSelesai. Berkas tersimpan di", file.path(dir_hasil, "02_parameter_model_lama.csv"), "\n")
} else {
  cat("\nPERHATIAN: RMSE tidak sama dengan yang tersimpan. Dashboard tidak akan menggambar kurva model itu.\n")
}
