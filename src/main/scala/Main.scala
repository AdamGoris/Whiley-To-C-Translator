package demo

import org.bitbucket.inkytonik.kiama.util.{Messaging, PositionStore}

object Main extends Messaging with PositionStore {

    import javak.java.javai.JavaI
    import javak.java.javai.JavaIPrettyPrinter._
    import javak.java.javai.JavaISyntax.Program
    import org.bitbucket.inkytonik.kiama.util.FileSource

    def main(args : Array[String]) {
        val p = new JavaI(FileSource(args(0)), positions)
        val pr = p.pProgram(0)
        if (pr.hasValue) {
            val v = p.value(pr).asInstanceOf[Program]
            println(show(v, 1))
            println(pretty(any(v)).layout)
        } else {
            val m = p.errorToMessage(pr.parseError)
            println(s"parse failed\n${formatMessage(m)}")
        }
    }

}
