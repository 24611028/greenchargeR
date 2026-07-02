#' Analisis Konsumsi Energi Stasiun
#'
#' Melakukan analisis konsumsi energi per stasiun pengisian
#' meliputi efisiensi energi, kontribusi per stasiun terhadap
#' total, dan klasifikasi performa stasiun.
#'
#' @param bulan Nama bulan dalam Bahasa Indonesia
#'   (default: "Juni")
#' @param tahun Tahun pelaporan numerik (default: 2025)
#' @param tampilkan_plot Logical, apakah menampilkan
#'   visualisasi (default: TRUE)
#' @return Data frame hasil analisis energi (invisible)
#' @export
#' @examples
#' analisis_energi(bulan = "Juni", tahun = 2025)
#' analisis_energi(bulan = "Juni", tahun = 2025,
#'                 tampilkan_plot = FALSE)
analisis_energi <- function(bulan = "Juni",
                            tahun = 2025,
                            tampilkan_plot = TRUE) {

  data <- buat_data_stasiun(bulan = bulan, tahun = tahun)

  total_energi <- sum(data$energi_kwh)

  hasil <- data.frame(
    stasiun         = data$stasiun,
    kota            = data$kota,
    energi_kwh      = data$energi_kwh,
    kontribusi_pct  = round(
      data$energi_kwh / total_energi * 100, 2),
    efisiensi       = round(
      data$energi_kwh / data$total_sesi, 2),
    kategori        = ifelse(
      data$energi_kwh >= mean(data$energi_kwh),
      "Di Atas Rata-rata",
      "Di Bawah Rata-rata")
  )

  cat("===== ANALISIS KONSUMSI ENERGI =====\n")
  cat("Periode:", bulan, tahun, "\n")
  cat("Total Energi Keseluruhan:",
      format(round(total_energi, 1),
             big.mark = "."), "kWh\n")
  cat("-------------------------------------\n")
  print(hasil, row.names = FALSE)
  cat("-------------------------------------\n")
  cat("Rata-rata konsumsi    :",
      round(mean(data$energi_kwh), 2), "kWh\n")
  cat("Rata-rata efisiensi   :",
      round(mean(hasil$efisiensi), 2),
      "kWh/sesi\n")

  if (tampilkan_plot) {
    print(
      ggplot2::ggplot(
        hasil,
        ggplot2::aes(
          x    = reorder(stasiun, -kontribusi_pct),
          y    = kontribusi_pct,
          fill = kategori)) +
        ggplot2::geom_bar(stat = "identity",
                          width = 0.6) +
        ggplot2::geom_text(
          ggplot2::aes(
            label = paste0(kontribusi_pct, "%")),
          vjust = -0.5, size = 3.5) +
        ggplot2::scale_fill_manual(
          values = c(
            "Di Atas Rata-rata"  = "steelblue",
            "Di Bawah Rata-rata" = "tomato")) +
        ggplot2::labs(
          title = "Kontribusi Konsumsi Energi per Stasiun",
          x     = "Stasiun Pengisian",
          y     = "Kontribusi (%)",
          fill  = "Kategori") +
        ggplot2::theme_minimal()
    )
  }

  invisible(hasil)
}
