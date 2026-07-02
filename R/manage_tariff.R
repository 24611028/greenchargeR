#' Mengelola dan Menghitung Tarif Pengisian
#'
#' Fungsi ini digunakan Operator untuk menghitung biaya pengisian
#' berdasarkan energi yang digunakan dan tarif yang berlaku,
#' menampilkan rincian biaya dasar dan pajak secara transparan.
#'
#' @param energi_kwh Jumlah energi yang digunakan dalam kWh
#' @param tarif_per_kwh Harga per kWh dalam Rupiah (default: 2500)
#' @param pajak_pct Persentase pajak dalam persen (default: 11)
#' @return List berisi rincian biaya pengisian (invisible)
#' @export
#' @examples
#' manage_tariff(energi_kwh = 30, tarif_per_kwh = 2500)
#' manage_tariff(energi_kwh = 45.5, tarif_per_kwh = 2500,
#'               pajak_pct = 11)
manage_tariff <- function(energi_kwh,
                          tarif_per_kwh = 2500,
                          pajak_pct     = 11) {
  if (energi_kwh <= 0) {
    stop("Energi yang digunakan harus bernilai positif")
  }
  biaya_dasar <- energi_kwh * tarif_per_kwh
  pajak       <- biaya_dasar * (pajak_pct / 100)
  total_biaya <- biaya_dasar + pajak

  hasil <- list(
    energi_kwh  = energi_kwh,
    tarif_kwh   = tarif_per_kwh,
    biaya_dasar = biaya_dasar,
    pajak_rp    = pajak,
    total_biaya = total_biaya
  )

  cat("===== RINCIAN BIAYA PENGISIAN =====\n")
  cat("Energi Terpakai  :", energi_kwh, "kWh\n")
  cat("Tarif per kWh    : Rp",
      format(tarif_per_kwh, big.mark = "."), "\n")
  cat("Biaya Dasar      : Rp",
      format(biaya_dasar, big.mark = "."), "\n")
  cat("Pajak", pajak_pct, "%       : Rp",
      format(round(pajak, 0), big.mark = "."), "\n")
  cat("Total Biaya      : Rp",
      format(round(total_biaya, 0), big.mark = "."), "\n")

  invisible(hasil)
}
