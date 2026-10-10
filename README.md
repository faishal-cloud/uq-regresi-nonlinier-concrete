# Kuantifikasi Ketidakpastian pada Regresi Nonlinier Sederhana

Studi pada data *Concrete Compressive Strength* (Yeh, 1998)

**Penyusun**

- M. Faishal Amirulloh (NPM 140720260024)
- Irma Setiyani Rahayu (NPM 140720260025)

Tugas mata kuliah Analisis Regresi Tingkat Lanjut, Magister Statistika Terapan, Universitas Padjadjaran.

## Ringkasan

Repository ini memuat sintaks R untuk memodelkan *compressive strength* beton (Strength) dengan regresi nonlinier yang hanya memakai dua prediktor, yaitu umur beton (Age) dan *water-to-binder ratio* (w/b), lalu mengukur *uncertainty* dari estimasi dan prediksinya.

Model akhir (Model D):

```
Strength = alpha / wb^beta * [1 - exp(-gamma * wb^(-kappa) * Age^p)] + error
sd(error) = sigma * f^delta        (f = mean function)
wb = Water / (Cement + Blast Furnace Slag + Fly Ash)
```

Ringkasan metode:

- Estimasi dengan *generalized least squares* yang menggabungkan *weighted nonlinear least squares* (Levenberg-Marquardt) dan *variance function*.
- Dua parameter dilaporkan di *reference point* (w/b 0,45; Age 28 hari): S_ref dan gamma_28.
- CI untuk rata-rata: Wald (model-based dan sandwich) serta *percentile bootstrap* per kelompok komposisi campuran.
- PI untuk satu observasi baru: *residual bootstrap* (utama) dan *normal-theory* (pembanding).
- Validasi *out-of-sample*: 10-*fold cross-validation* dengan beberapa cara membentuk *fold*.
- Seluruh analisis frekuentis; tidak ada bagian yang Bayesian.

## Isi repository

| Berkas atau folder | Isi |
|---|---|
| `analisis_final_tugas_anareg.R` | Seluruh analisis, dari membaca data sampai semua keluaran di `hasil_final/` |
| `simpan_parameter_model_lama.R` | Menyimpan parameter Model 1 dan Model 2 untuk kurva di dashboard |
| `app.R` | Dashboard Shiny; hanya membaca keluaran di `hasil_final/`, tidak menghitung ulang model |
| `hasil_final/` | Keluaran analisis (CSV, RDS) dari run dengan `MODE_UJI <- FALSE` |
| `Concrete_Data.xls` | Data asli dari UCI Machine Learning Repository |
| `laporan_uq_regresi_nonlinier_3d_rev_parafrase.docx` | Laporan dalam format Word |

## Cara menjalankan

Paket yang diperlukan:

```r
install.packages(c("readxl", "minpack.lm", "dplyr", "tidyr", "ggplot2", "shiny"))
install.packages("plotly")   # opsional: grafik 3D yang bisa diputar di dashboard
```

1. Letakkan semua berkas di satu folder dan jadikan folder itu *working directory*.
2. **Analisis penuh.** Buka `analisis_final_tugas_anareg.R` dan ubah `MODE_UJI <- TRUE` menjadi `MODE_UJI <- FALSE`, lalu jalankan seluruh skrip. Mode `TRUE` hanya uji alur dengan replikasi kecil; keluarannya berakhiran `_uji` dan angkanya tidak dipakai di laporan. Analisis penuh memakan waktu lama karena *bootstrap* dan validasi silang. Langkah ini boleh dilewati karena hasilnya sudah ada di `hasil_final/`.
3. **Parameter model lama (sekali saja).** Jalankan `simpan_parameter_model_lama.R` bila berkas `hasil_final/02_parameter_model_lama.csv` belum ada.
4. **Dashboard.** Jalankan `shiny::runApp()` dari folder yang sama.

Seed utama ditetapkan di skrip (`seed_utama <- 20261006`), sehingga hasil dapat direproduksi.

Dashboard daring: https://ciyayane.shinyapps.io/dashboard-regresi-nonlinear/

## Data

Yeh, I.-C. (1998). Concrete Compressive Strength [Dataset]. UCI Machine Learning Repository. https://doi.org/10.24432/C5PK67

Data dipakai di bawah lisensi Creative Commons Attribution 4.0 International (CC BY 4.0).

Yeh, I.-C. (1998). Modeling of strength of high-performance concrete using artificial neural networks. *Cement and Concrete Research*, 28(12), 1797-1808.
