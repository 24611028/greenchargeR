# Test buat_data_stasiun
test_that("buat_data_stasiun menghasilkan data frame", {
  hasil <- buat_data_stasiun("Juni", 2025)
  expect_s3_class(hasil, "data.frame")
  expect_equal(nrow(hasil), 5)
  expect_true(all(c("stasiun","kota","total_sesi",
                    "energi_kwh","pendapatan",
                    "utilisasi_pct") %in% names(hasil)))
})

test_that("buat_data_stasiun konsisten dengan seed sama", {
  data1 <- buat_data_stasiun("Juni", 2025)
  data2 <- buat_data_stasiun("Juni", 2025)
  expect_identical(data1, data2)
})

# Test monitor_energy
test_that("monitor_energy berjalan tanpa error", {
  expect_invisible(monitor_energy("Juni", 2025))
})

# Test manage_tariff
test_that("manage_tariff menghasilkan list komponen benar", {
  hasil <- manage_tariff(energi_kwh = 30)
  expect_true(is.list(hasil))
  expect_true(all(c("biaya_dasar","pajak_rp",
                    "total_biaya") %in% names(hasil)))
})

test_that("manage_tariff error jika energi nol", {
  expect_error(manage_tariff(energi_kwh = 0))
})

# Test generate_kpi
test_that("generate_kpi menghasilkan data frame dengan kolom status", {
  hasil <- generate_kpi("Juni", 2025)
  expect_s3_class(hasil, "data.frame")
  expect_true("status" %in% names(hasil))
})

# Test predict_demand
test_that("predict_demand menghasilkan nilai numerik", {
  hasil <- predict_demand("Juni", 2025,
                          tampilkan_plot = FALSE)
  expect_true(is.numeric(hasil))
  expect_true(hasil > 0)
})

# Test lihat_statistik
test_that("lihat_statistik menghasilkan data frame", {
  hasil <- lihat_statistik("Juni", 2025)
  expect_s3_class(hasil, "data.frame")
  expect_true("Metrik" %in% names(hasil))
  expect_true("Nilai" %in% names(hasil))
})

# Test analisis_energi
test_that("analisis_energi menghasilkan kolom kontribusi", {
  hasil <- analisis_energi("Juni", 2025,
                           tampilkan_plot = FALSE)
  expect_s3_class(hasil, "data.frame")
  expect_true("kontribusi_pct" %in% names(hasil))
  expect_equal(round(sum(hasil$kontribusi_pct), 0), 100)
})

# Test generate_report
test_that("generate_report berjalan tanpa error", {
  expect_invisible(
    generate_report("Juni", 2025, export_csv = FALSE)
  )
})
