import /server/timeofday:int as time.
import /library/gettimeofday:int.
repeat checkTime.

fun (int as x, int as y) checkTime: int
  if [time >= 12000]
    time <- 0
  otherwise
    timeToWait <= 12000 - time
    wait timeToWait
    time <- 0
  end.
