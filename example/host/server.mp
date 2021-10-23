name 'server'

x: Int <= 0

handle (4, 5, 6, 7.5) {
  x <- 49
}

handle ('get x', sender: Node) {
  x -> sender
}

handle ('set x', 'secret', newX: Int) {
  x <- newX
}

handle (('set x', password: Atom, newX: Int), sender: Node)
  when password = 'secret' {
    (4, x) <- (4, newX)
    'ok' -> sender
  }
  otherwise {
    ('error', 'wrong password') -> sender
  }
