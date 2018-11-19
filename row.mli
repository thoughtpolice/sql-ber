type t
type column = string

val fetch : column -> t ref -> string
