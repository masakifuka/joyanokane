#' 鐘の音のスペクトル解析
#'
#' 合成された鐘の音をFFT解析し、周波数特性（パワースペクトル）をプロットします。
#' 非整数次倍音がどのように含まれているかを確認できます。
#' 
#' @export
analyze_bell_spectrum <- function() {
  sr <- 44100
  sound <- synthesize_bell(freq = 150, duration = 2, sample_rate = sr)
  wave_data <- as.numeric(sound)
  message("周波数解析中...")
  
  stats::spectrum(wave_data, Fs = sr, log = "dB", 
           main = "除夜の鐘 パワースペクトル密度",
           xlab = "周波数 (Hz)", ylab = "スペクトル (dB)",
           col = "darkblue", lwd = 2,
           xlim = c(0, 2000)) # 2000Hzまでを表示（主要成分）
  
  graphics::abline(v = 150 * c(1, 2, 3, 4.2, 5.4), col = "red", lty = 2)
  graphics::legend("topright", legend = c("スペクトル", "理論上の倍音位置"),
         col = c("darkblue", "red"), lty = c(1, 2), lwd = c(2, 1))
         
  message("赤い点線が、プログラムで指定した「倍音成分」の位置です。")
}
