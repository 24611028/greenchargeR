#' Generate Laporan Operasional Lengkap
#'
#' Fungsi ini menghasilkan laporan operasional ringkas dalam
#' format teks terstruktur dan secara opsional mengekspor
#' data ke file CSV, sesuai use case Generate Laporan pada
#' Sistem Pengisian Kendaraan Listrik.
#'
#' @param bulan Nama bulan dalam Bahasa Indonesia
#'   (default: "Juni")
#' @param tahun Tahun pelaporan numerik (default: 2025)
#' @param export_csv Logical, apakah mengekspor data ke CSV
#'   (default: TRUE)
#' @param output_dir Folder tujuan export CSV
#'   (default: "output")
#' @return Data frame data operasional (invisible)
#' @export
#' @examples
#' generate_report(bulan = "Juni", tahun = 2025)
#' generate_report(bulan = "Juni", tahun = 2025,
#'                 export_csv = FALSE)
generate_report <- function(bulan      = "Juni",
                            tahun      = 2025,
                            export_csv = TRUE,
                            output_dir = "output") {

  data <- buat_data_stasiun(bulan = bulan, tahun = tahun)

  cat("================================================\n")
  cat("   LAPORAN OPERASIONAL BULANAN\n")
  cat("   PT GREEN CHARGE INDONESIA\n")
  cat("   Periode:", bulan, tahun, "\n")
  cat("================================================\n\n")

  cat("RINGKASAN EKSEKUTIF\n")
  cat("-------------------\n")
  cat("Total Stasiun Aktif  :", nrow(data), "\n")
  cat("Total Sesi Pengisian :",
      format(sum(data$total_sesi),
             big.mark = "."), "\n")
  cat("Total Energi (kWh)   :",
      format(round(sum(data$energi_kwh), 1),
             big.mark = "."), "\n")
  cat("Total Pendapatan     : Rp",
      format(sum(data$pendapatan),
             big.mark = "."), "\n")
  cat("Rata-rata Utilisasi  :",
      round(mean(data$utilisasi_pct), 1), "%\n\n")

  cat("PERFORMA PER STASIUN\n")
  cat("-------------------\n")
  for (i in 1:nrow(data)) {
    cat(sprintf(
      "%-12s | Sesi: %4d | Energi: %7.0f kWh | Utilisasi: %.1f%%\n",
      data$stasiun[i],
      data$total_sesi[i],
      data$energi_kwh[i],
      data$utilisasi_pct[i]
    ))
  }

  # Export CSV jika diminta
  if (export_csv) {
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    nama_file <- paste0(
      output_dir, "/data_operasional_",
      bulan, "_", tahun, ".csv")

    write.csv(data,
              file      = nama_file,
              row.names = FALSE,
              fileEncoding = "UTF-8")

    cat("\n[CSV] Data diekspor ke:", nama_file, "\n")
  }

  cat("\n================================================\n")
  cat("Laporan dihasilkan pada:",
      format(Sys.time(), "%d %B %Y %H:%M"), "\n")
  cat("================================================\n")

  invisible(data)
}
