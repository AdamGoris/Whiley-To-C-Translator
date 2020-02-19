package runtime

import org.bitbucket.inkytonik.kiama.util.{Messaging, PositionStore}

object Main extends Messaging with PositionStore {

    import whiley.WhileyI
    import whiley.WhileyIPrettyPrinter._
    import whiley.WhileyISyntax.Program
    import whiley.CI
    //import whiley.CIPrettyPrinter._
    import whiley.CISyntax.CProgram
    import org.bitbucket.inkytonik.kiama.util.FileSource
    import translator.Translator

    def main(args : Array[String]) {
        val p = new WhileyI(FileSource(args(0)), positions)
        val translator = new Translator
        //val p = new CI(FileSource(args(0)), positions)
        val pr = p.pProgram(0)
        if (pr.hasValue) {
            val v = p.value(pr).asInstanceOf[Program]
            println(show(v, 1))
            println(pretty(any(v)).layout)
            println("")
            println(translator.translate(v))
        } else {
            val m = p.errorToMessage(pr.parseError)
            println(s"parse failed\n${formatMessage(m)}")
        }
    }
}
