import arrow.core.*

typealias Result<A> = Either<String, Pair<A, String>>

class Parser<A> constructor(private val p: (String) -> Result<A>) {

    fun start(): (String) -> Result<A> = p

    infix fun <B> andThen(that: Parser<B>): Parser<Pair<A, B>> = Parser { str ->
        this.p(str).fold(
            { l -> Either.Left(l) },
            { r -> that.p(r.second).map { v -> Pair(Pair(r.first, v.first), v.second) } }
        )
    }

    infix fun <B : A> or(that: Parser<B>): Parser<B> = Parser { str ->
        this.p(str).fold(
            { l -> that.p(str) },
            { r -> Either.Right(r) as Result<B> }
        )
    }

    fun <B> map(f: (A) -> B): Parser<B> = Parser { str ->
        this.p(str).map { v -> Pair(f(v.first), v.second) }
    }

    val rep: Parser<List<A>> by lazy {
        Parser { str ->
            if (str.isEmpty()) {
                Either.Right(Pair(emptyList<A>(), ""))
            } else {
                (this.map { listOf(it) } andThen rep).map { v -> v.first + v.second }.start()(str)
            }
        }
    }
}

sealed class Expr
data class Number(val value: Int) : Expr()
data class Variable(val name: String) : Expr()
data class Operator(val op: Char): Expr()
data class Assignment(val variable: String, val expr: Expr) : Expr()
data class BinOp(val op: Expr, val left: Expr, val right: Expr) : Expr()

object ArithmeticParser {
    private fun parseChar(inStr: String, ch: Char): Result<Char> =
        inStr.firstOrNull()?.let { value ->
            if (value == ch) {
                Either.Right(Pair(value, inStr.drop(1)))
            } else {
                Either.Left("Parsing failure, got $value expected $ch")
            }
        } ?: Either.Left("Parsing failure, empty string")

    private fun parseLetter(inStr: String): Result<Char> =
        inStr.firstOrNull()?.let { letter ->
            if (letter.isLetter()) {
                Either.Right(Pair(letter, inStr.drop(1)))
            } else {
                Either.Left("Parsing failure, no letter found")
            }
            } ?: Either.Left("Parsing failure, empty string")

    private fun parseDigital(inStr: String): Result<Char> =
        inStr.firstOrNull()?.let { digit ->
            if (digit.isDigit()) {
                Either.Right(Pair(digit, inStr.drop(1)))
            } else {
                Either.Left("Parsing failure, no digit found")
            }
        } ?: Either.Left("Parsing failure, empty string")


    private fun parseVariable(inStr: String): Result<String> {
        val name = StringBuilder()
        fun parseNext(remaining: String): Result<String> {
            if (remaining.isNotEmpty() && remaining.firstOrNull()
                    ?.isLetterOrDigit() == true || remaining.firstOrNull() == '_'
            ) {
                val letterOrDigitParser = letterParser() or digitalParser()
                return letterOrDigitParser.start()(remaining).flatMap { result ->
                    name.append(result.first)
                    parseNext(result.second)
                }
            } else {
                return Either.Right(Pair(name.toString(), remaining))
            }
        }
        return parseNext(inStr).map { Pair(it.first, it.second) }
    }


    fun factor(): Parser<Expr> {
        return numberParser() or variableNameParser()
    }

    fun term(): Parser<Expr>  {
       return factor().andThen(operatorParser() andThen factor()).map { BinOp(it.second.first, it.first, it.second.second) }
    }

    fun expr(): Parser<Expr> {
        return (term() andThen (operatorParser() or term()).rep).map { it ->
            println(it)
            it.first
        } or parseAssignment()
    }

    fun parseAssignment(): Parser<Expr> {
        return (variableNameParser() andThen charParser('=') andThen expr()).map {
            Assignment(it.first.first.toString(), it.second)
        }
    }

    private fun numberParser(): Parser<Expr> {
        return (digitalParser().rep.map { it.joinToString("").trim().toInt() }).map { Number(it) }
    }

    private fun variableNameParser(): Parser<Expr> = Parser { str -> parseVariable(str) }.map { name -> Variable(name) }
    private fun digitalParser(): Parser<Char> = Parser { str -> parseDigital(str) }
    private fun letterParser(): Parser<Char> = Parser { str -> parseLetter(str) }
    private fun charParser(ch: Char): Parser<Char> = Parser { str -> parseChar(str, ch) }

    private fun additionParser(): Parser<Char> = charParser('+')
    private fun subtractionParser(): Parser<Char> = charParser('-')
    private fun multiplicationParser(): Parser<Char> = charParser('*')
    private fun divisionParser(): Parser<Char> = charParser('/')
    private fun openBracketParser(): Parser<Char> = charParser('(')
    private fun closeBracketParser(): Parser<Char> = charParser(')')

    private fun operatorParser(): Parser<Expr> = (additionParser() or subtractionParser() or multiplicationParser() or divisionParser()).map { Operator(it) }

}

fun main() {
    println(ArithmeticParser.parseAssignment().start()("x=3"))
}


