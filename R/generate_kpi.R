#' Menghasilkan Laporan KPI Stasiun Pengisian
#'
#' Fungsi ini menghitung dan menampilkan Key Performance Indicators
#' (KPI) utama untuk setiap stasiun pengisian berdasarkan data
#' operasional periode yang ditentukan, meliputi pendapatan per
#' sesi, energi per sesi, dan status operasional.
#'
#' @param bulan Nama bulan dalam Bahasa Indonesia
#'   (default: "Juni")
#' @param tahun Tahun pelaporan numerik (default: 2025)
#' @return Data frame berisi KPI yang telah dihitung (invisible)
#' @export
#' @examples
#' generate_kpi(bulan = "Juni", tahun = 2025)
#' generate_kpi(bulan = "Juli", tahun = 2025)
generate_kpi <- function(bulan = "Juni", tahun = 2025) {

  # Bangkitkan data dengan seed yang sama seperti .Rmd
  data <- buat_data_stasiun(bulan = bulan, tahun = tahun)

  kpi <- data
  kpi$pendapatan_per_sesi <- round(
    kpi$pendapatan / kpi$total_sesi, 0)
  kpi$energi_per_sesi <- round(
    kpi$energi_kwh / kpi$total_sesi, 2)
  kpi$status <- ifelse(
    kpi$utilisasi_pct >= 75, "Optimal",
    ifelse(kpi$utilisasi_pct >= 50, "Normal",
           "Perlu Perhatian"))

  cat("===== KPI STASIUN PENGISIAN =====\n")
  cat("Periode:", bulan, tahun, "\n")
  cat("---------------------------------\n")
  print(kpi[, c("stasiun", "utilisasi_pct",
                "pendapatan_per_sesi",
                "energi_per_sesi", "status")])

  invisible(kpi)
}
