#' Melihat Statistik Penggunaan Energi
#'
#' Menampilkan statistik deskriptif penggunaan energi seluruh
#' stasiun pengisian pada periode yang ditentukan, mencakup
#' nilai minimum, maksimum, rata-rata, dan total.
#'
#' @param bulan Nama bulan dalam Bahasa Indonesia
#'   (default: "Juni")
#' @param tahun Tahun pelaporan numerik (default: 2025)
#' @return Data frame statistik energi (invisible)
#' @export
#' @examples
#' lihat_statistik(bulan = "Juni", tahun = 2025)
lihat_statistik <- function(bulan = "Juni", tahun = 2025) {

  data <- buat_data_stasiun(bulan = bulan, tahun = tahun)

  statistik <- data.frame(
    Metrik = c(
      "Total Energi (kWh)",
      "Rata-rata per Stasiun (kWh)",
      "Energi Tertinggi (kWh)",
      "Energi Terendah (kWh)",
      "Standar Deviasi (kWh)",
      "Total Sesi Pengisian",
      "Rata-rata Sesi per Stasiun",
      "Total Pendapatan (Rp)",
      "Rata-rata Utilisasi (%)"
    ),
    Nilai = c(
      format(round(sum(data$energi_kwh), 1),
             big.mark = "."),
      format(round(mean(data$energi_kwh), 2),
             big.mark = "."),
      format(max(data$energi_kwh),
             big.mark = "."),
      format(min(data$energi_kwh),
             big.mark = "."),
      format(round(sd(data$energi_kwh), 2),
             big.mark = "."),
      format(sum(data$total_sesi),
             big.mark = "."),
      format(round(mean(data$total_sesi), 1),
             big.mark = "."),
      format(sum(data$pendapatan),
             big.mark = "."),
      paste0(round(mean(data$utilisasi_pct), 1), "%")
    )
  )

  cat("===== STATISTIK PENGGUNAAN ENERGI =====\n")
  cat("Periode:", bulan, tahun, "\n")
  cat("----------------------------------------\n")
  print(statistik, row.names = FALSE)
  cat("----------------------------------------\n")
  cat("Stasiun dengan konsumsi tertinggi:",
      data$stasiun[which.max(data$energi_kwh)], "\n")
  cat("Stasiun dengan konsumsi terendah :",
      data$stasiun[which.min(data$energi_kwh)], "\n")

  invisible(statistik)
}
