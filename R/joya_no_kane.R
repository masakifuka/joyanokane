#' 鐘の音を合成する
#'
#' @param freq 基本周波数
#' @param duration 持続時間
#' @param sample_rate サンプリングレート
#' @return audioSampleオブジェクト
#' @keywords internal
synthesize_bell <- function(freq = 150, duration = 4, sample_rate = 44100) {
  t <- seq(0, duration, 1/sample_rate)

  harmonics <- c(1.0, 2.0, 3.0, 4.2, 5.4)
  amplitudes <- c(1.0, 0.6, 0.4, 0.2, 0.1)
  decays     <- c(0.8, 1.2, 2.0, 3.0, 4.0)

  wave <- numeric(length(t))

  for (i in seq_along(harmonics)) {
    component <- amplitudes[i] * sin(2 * pi * freq * harmonics[i] * t) * exp(-decays[i] * t)
    wave <- wave + component
  }

  wave <- wave / max(abs(wave))
  return(audio::audioSample(wave, rate = sample_rate))
}

#' 除夜の鐘を実行する
#'
#' @param interval 鐘をつく間隔
#' @export
joya_no_kane <- function(interval = 3) {
  message("--- 除夜の鐘を実行します。 ---")

  bell_sound <- synthesize_bell(freq = 150, duration = 4)

  for (i in 1:108) {
    message(sprintf("\r[%3d / 108] 除夜の鐘 実行中... (%.1f%%)", i, (i/108)*100), appendLF = FALSE)
    audio::play(bell_sound)
    Sys.sleep(interval)
  }

  message("\n\n--- 良いお年を。 ---")
}
