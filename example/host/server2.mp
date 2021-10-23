name 'server2'

handle (('hook me up', sender1: Atom), sender2: Node) {
  ('you got it', sender2) -> sender1
}