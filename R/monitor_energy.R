#' Monitor Penggunaan Energi Stasiun
#'
#' Fungsi ini digunakan oleh Operator Stasiun untuk memantau
#' total konsumsi energi dan menampilkan ringkasan statistiknya
#' secara terstruktur berdasarkan data operasional periode
#' yang ditentukan.
#'
#' @param bulan Nama bulan dalam Bahasa Indonesia
#'   (default: "Juni")
#' @param tahun Tahun pelaporan numerik (default: 2025)
#' @return Ringkasan statistik penggunaan energi (invisible)
#' @export
#' @examples
#' monitor_energy(bulan = "Juni", tahun = 2025)
#' monitor_energy(bulan = "Juli", tahun = 2025)
monitor_energy <- function(bulan = "Juni", tahun = 2025) {

  # Bangkitkan data dengan seed yang sama seperti .Rmd
  data <- buat_data_stasiun(bulan = bulan, tahun = tahun)

  cat("===== MONITORING PENGGUNAAN ENERGI =====\n")
  cat("Periode              :", bulan, tahun, "\n")
  cat("-----------------------------------------\n")
  cat("Total Stasiun Aktif  :", nrow(data), "\n")
  cat("Total Energi (kWh)   :",
      format(sum(data$energi_kwh),
             big.mark = "."), "\n")
  cat("Rata-rata per Stasiun:",
      format(round(mean(data$energi_kwh), 2),
             big.mark = "."), "kWh\n")
  cat("Stasiun Tertinggi    :",
      data$stasiun[which.max(data$energi_kwh)],
      "(", max(data$energi_kwh), "kWh)\n")
  cat("Stasiun Terendah     :",
      data$stasiun[which.min(data$energi_kwh)],
      "(", min(data$energi_kwh), "kWh)\n")

  invisible(data)
}
