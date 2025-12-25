#' マニュアル除夜の鐘（メモリ解放付き）
#'
#' 実行するたびに鐘を一回つき、同時にメモリをクリアします。
#' @export
joya_gc <- function() {
  sound <- synthesize_bell(freq = 150, duration = 4)
  audio::play(sound)

  before <- gc(verbose = FALSE)
  val_before <- sum(before[, 2])

  Sys.sleep(0.5)

  after <- gc(verbose = FALSE)
  val_after <- sum(after[, 2])

  diff <- val_before - val_after

  message(sprintf("ゴーン (煩悩（メモリ）をクリアしました: %s cells 解放)", format(diff, big.mark = ",")))
}
