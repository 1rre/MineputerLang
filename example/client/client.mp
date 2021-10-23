'loop' -> self
totest <= 0
valofx <= 0

handle 'loop' {
  totest <- 'server' # ('set x', 'secret', 32)
  valofx <- 'server' # 'get x'
  ('set x', 'secret', 5) -> 'server'
  (4,5,6,7.5) -> 'server'
}