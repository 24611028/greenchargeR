#' Prediksi Kebutuhan Energi Bulan Berikutnya
#'
#' Menggunakan model regresi linear sederhana untuk memprediksi
#' permintaan energi bulan berikutnya. Data historis 5 bulan
#' sebelumnya dibangkitkan secara otomatis, dan bulan keenam
#' diambil dari data operasional periode yang ditentukan.
#'
#' @param bulan Nama bulan dalam Bahasa Indonesia sebagai
#'   bulan ke-6 (bulan berjalan) (default: "Juni")
#' @param tahun Tahun pelaporan numerik (default: 2025)
#' @param tampilkan_plot Logical, apakah menampilkan plot
#'   prediksi (default: TRUE)
#' @return Nilai prediksi energi bulan berikutnya dalam kWh
#'   (invisible)
#' @importFrom stats lm predict
#' @export
#' @examples
#' predict_demand(bulan = "Juni", tahun = 2025)
#' predict_demand(bulan = "Juni", tahun = 2025,
#'                tampilkan_plot = FALSE)
predict_demand <- function(bulan    = "Juni",
                           tahun    = 2025,
                           tampilkan_plot = TRUE) {

  # Bangkitkan data bulan berjalan
  data <- buat_data_stasiun(bulan = bulan, tahun = tahun)
  total_energi_berjalan <- sum(data$energi_kwh)

  # Data historis 5 bulan sebelumnya + bulan berjalan
  # (sama persis dengan chunk prediksi-energi di .Rmd)
  historis <- data.frame(
    bulan_ke = 1:6,
    energi   = c(42000, 45000, 48500, 51000, 53500,
                 round(total_energi_berjalan, 0))
  )

  model    <- lm(energi ~ bulan_ke, data = historis)
  prediksi <- predict(model,
                      newdata = data.frame(bulan_ke = 7))
  pct_naik <- round(
    (prediksi - historis$energi[6]) /
      historis$energi[6] * 100, 1)

  cat("===== PREDIKSI KEBUTUHAN ENERGI =====\n")
  cat("Periode berjalan       :", bulan, tahun, "\n")
  cat("Energi bulan berjalan  :",
      format(round(total_energi_berjalan, 0),
             big.mark = "."), "kWh\n")
  cat("Prediksi bulan ke-7    :",
      format(round(prediksi, 0),
             big.mark = "."), "kWh\n")
  cat("Perubahan prediksi     :", pct_naik, "%\n")

  if (tampilkan_plot) {
    df_pred <- data.frame(bulan_ke = 7,
                          energi   = prediksi)
    print(
      ggplot2::ggplot(historis,
                      ggplot2::aes(x = bulan_ke,
                                   y = energi)) +
        ggplot2::geom_line(color = "steelblue",
                           linewidth = 1.2) +
        ggplot2::geom_point(color = "steelblue",
                            size = 3) +
        ggplot2::geom_point(
          data  = df_pred,
          ggplot2::aes(x = bulan_ke, y = energi),
          color = "red", size = 4, shape = 17) +
        ggplot2::geom_smooth(
          method = "lm", se = TRUE,
          alpha = 0.15, color = "gray50",
          linetype = "dashed") +
        ggplot2::scale_x_continuous(
          breaks = 1:7,
          labels = c("Jan","Feb","Mar","Apr",
                     "Mei","Jun","Jul (Pred)")) +
        ggplot2::labs(
          title    = "Tren dan Prediksi Konsumsi Energi",
          subtitle = paste0(
            "Prediksi Juli: ",
            format(round(prediksi, 0),
                   big.mark = "."), " kWh"),
          x       = "Bulan",
          y       = "Total Energi (kWh)",
          caption = "Segitiga merah = nilai prediksi") +
        ggplot2::theme_minimal()
    )
  }
  invisible(round(prediksi, 0))
}
