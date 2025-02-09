enum JsonValue:
  case JNull
  case JString(value: String)
  case JNumber(value: Int)
  case JBoolean(value: Boolean)
  case JArray(values: List[JsonValue])
  case JObject(fields: Map[String, JsonValue])

  override def toString: String = this match {
    case JNull => "null"
    case JString(value) => s"""$value"""
    case JNumber(value) => value.toString
    case JBoolean(value) => value.toString
    case JArray(values) => values.map(_.toString).mkString("[", ", ", "]")
    case JObject(fields) =>
      fields.map {
        case (key, value) => s""""$key": ${value.toString}"""
      }.mkString("{", ", ", "}")
   }

case class Parser[A](runParser: String => Option[(String, A)]) {
  // Functor: map
  def map[B](f: A => B): Parser[B] = Parser { input =>
    runParser(input).map {
      case (rest, value) => (rest, f(value))
    }
  }

  // Monad: flatMap (necessaire pour que le Parser soit un Monad)
  def flatMap[B](f: A => Parser[B]): Parser[B] = Parser { input =>
    runParser(input).flatMap {
      case (rest, value) => f(value).runParser(rest)
    }
  }

  // applicative, applique un Parser[A => B] à un Parser[A]
  def ap[B](pf: Parser[A => B]): Parser[B] = Parser { input =>
    for {
      (x, a) <- runParser(input)    // Execute le parser qui fournit `a`
      (y, f) <- pf.runParser(input) // Execute le parser qui fournit `f: A => B`
    } yield (y, f(a)) // applique `f` à `a`
  }

  def orElse(other: Parser[A]): Parser[A] = Parser { input =>
    runParser(input).orElse(other.runParser(input))
  }
}

def charP(x: Char): Parser[Char] = {
  def f(input: String): Option[(String, Char)] = input.toList match {
    case y :: ys if y == x => Some((ys.mkString, x))
    case _ => None
  }
  Parser(f)
}

def sequence[A](parsers: List[Parser[A]]): Parser[List[A]] = parsers match {
  case Nil => Parser(input => Some((input, Nil)))
  case p :: ps => for {
    first <- p
    rest <- sequence(ps)
  } yield first :: rest
}

def stringP(input: String): Parser[String] = {
  sequence(input.toList.map(charP)).map(_.mkString)
}

def spanP(f: Char => Boolean): Parser[String] = Parser { input =>
  val (token, rest) = input.span(f)
  if (token.nonEmpty) Some((rest, token)) else None
}

// Combinateur pour parser zero ou plusieurs occurences d'un parser donné
def many[A](p: Parser[A]): Parser[List[A]] =
  p.flatMap(a => many(p).map(as => a :: as))
    .orElse(Parser(input => Some((input, Nil))))

// Combinateur pour parser une liste d'elements `p` séparés par un séparateur `sep`
def sepBy[A, B](p: Parser[A], sep: Parser[B]): Parser[List[A]] =
  p.flatMap(a => many(sep.flatMap(_ => p)).map(as => a :: as))
    .orElse(Parser(input => Some((input, Nil))))

val stringLiteral: Parser[String] = spanP(_ != '"')

val jsonKey: Parser[String] = for {
  _ <- charP('"')
  key <- spanP(_ != '"')
  _ <- charP('"')
} yield key

// parser pour les espaces
val ws: Parser[String] = Parser { input =>
  val (spaces, rest) = input.span(_.isSpaceChar)
  Some((rest, spaces))
}

val jsonNull: Parser[JsonValue] = stringP("null").map(_ => JsonValue.JNull)

val jsonBool: Parser[JsonValue] =
  stringP("true").orElse(stringP("false")).map {
    case "true" => JsonValue.JBoolean(true)
    case "false" => JsonValue.JBoolean(false)
  }

val jsonNumber: Parser[JsonValue] =
  spanP(_.isDigit).map(ds => JsonValue.JNumber(ds.toInt))

val jsonString: Parser[JsonValue] =
  charP('"').flatMap(_ =>
    stringLiteral.flatMap(content =>
      charP('"').map(_ => JsonValue.JString(content))
    )
  )

val jsonArray: Parser[JsonValue] = for {
  _ <- charP('[')
  _ <- ws
  elements <- sepBy(jsonValue, for {
    _ <- ws
    _ <- charP(',')
    _ <- ws
  } yield ())
  _ <- ws
  _ <- charP(']')
} yield JsonValue.JArray(elements)

val jsonObject: Parser[JsonValue] = for {
  _ <- charP('{')
  _ <- ws
  pairs <- sepBy(for {
    key <- jsonKey
    _ <- ws
    _ <- charP(':')
    _ <- ws
    value <- jsonValue
  } yield (key, value), for {
    _ <- ws
    _ <- charP(',')
    _ <- ws
  } yield ())
  _ <- ws
  _ <- charP('}')
} yield JsonValue.JObject(pairs.toMap)

val jsonValue: Parser[JsonValue] = List(jsonNull, jsonBool, jsonNumber, jsonString, jsonArray, jsonObject)
  .reduce(_ orElse _)

@main
def main(): Unit = {
  println("Hello world!")
}