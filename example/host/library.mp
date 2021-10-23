! This is an example of what the library would look like
! In actuality, it is dealt with by the interpreter
!- this is a block comment test!
-!
mode host
! This usually would be '<name>', but library is a reserved name
name 'library'

handle ('set time', new_time: Int, sender: Player)
  when server#('permissions', sender) = 'admin' {
    x <- server#('set time', time)
    x -> sender
  }
  otherwise {
    ('error', 'unauthorised') -> sender
  }

handle ('get time', sender: Player) {
  'get time' -> server -> sender
}
