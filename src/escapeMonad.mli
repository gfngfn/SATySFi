val ( >>= ) : [< `Continue of 'a | `Escape of 'b ] -> ('a -> ([> `Escape of 'b ] as 'c)) -> 'c
val continue : 'a -> [> `Continue of 'a ]
val escape : 'a -> [> `Escape of 'a ]
val force : [< `Escape of 'a ] -> 'a
