'loop' -> self

x: Int <= 0

handle 'loop' {
  x <- 'server2'#('hook me up', self)
}
handle ('you got it', sender: Node) {
  [x + 5] -> sender
}
