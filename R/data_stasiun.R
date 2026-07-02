#' Membangkitkan Data Simulasi Operasional Stasiun
#'
#' Fungsi ini membangkitkan data simulasi operasional stasiun
#' pengisian PT Green Charge Indonesia berdasarkan parameter
#' bulan dan tahun, menggunakan seed yang konsisten sehingga
#' data yang dihasilkan selalu sama untuk periode yang sama.
#'
#' @param bulan Nama bulan dalam Bahasa Indonesia
#'   (default: "Juni")
#' @param tahun Tahun pelaporan numerik (default: 2025)
#' @return Data frame berisi data operasional 5 stasiun dengan
#'   kolom stasiun, kota, total_sesi, energi_kwh, pendapatan,
#'   dan utilisasi_pct
#' @export
#' @examples
#' data <- buat_data_stasiun(bulan = "Juni", tahun = 2025)
#' head(data)
buat_data_stasiun <- function(bulan = "Juni", tahun = 2025) {

  bulan_num <- match(
    bulan,
    c("Januari","Februari","Maret","April",
      "Mei","Juni","Juli","Agustus",
      "September","Oktober","November","Desember")
  )

  if (is.na(bulan_num)) {
    stop("Nama bulan tidak valid. Gunakan nama bulan ",
         "dalam Bahasa Indonesia.")
  }

  # Seed identik dengan chunk data-simulasi di file .Rmd
  set.seed(bulan_num + tahun)

  data_stasiun <- data.frame(
    stasiun       = paste("Stasiun", LETTERS[1:5]),
    kota          = c("Jakarta","Bandung","Surabaya",
                      "Medan","Makassar"),
    total_sesi    = sample(800:1500, 5),
    energi_kwh    = round(runif(5, 5000, 15000), 2),
    pendapatan    = round(runif(5, 50000000, 150000000), 0),
    utilisasi_pct = round(runif(5, 55, 95), 1)
  )

  return(data_stasiun)
}
