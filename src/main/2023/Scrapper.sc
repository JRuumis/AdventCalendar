

"123".indexOf("6")


val xxx = "abcone2one777threexyz".indexOf("one")
println(xxx)

val yyy = "abcone2one777threexyz".lastIndexOf("one")
println(yyy)


Vector('a','b','c').mkString




var myStr = "abc12ef4567gh90ijkl789"

("""\d+""".r findAllIn myStr).toList
myStr.split("\\D+").filter(_.nonEmpty).toList