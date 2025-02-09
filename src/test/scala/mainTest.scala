class mainTest extends munit.FunSuite {
  def run[A](p: Parser[A], input: String): Option[A] =
    p.runParser(input).flatMap {
      case (rest, value) if rest.forall(_.isSpaceChar) => Some(value)
      case _ => None
    }

  test("Parser jsonNull") {
    val input = "null"
    assertEquals(run(jsonNull, input), Some(JsonValue.JNull))
  }

  test("Parser jsonBool - true") {
    val input = "true"
    assertEquals(run(jsonBool, input), Some(JsonValue.JBoolean(true)))
  }

  test("Parser jsonBool - false") {
    val input = "false"
    assertEquals(run(jsonBool, input), Some(JsonValue.JBoolean(false)))
  }

  test("Parser jsonNumber") {
    val input = "12345"
    assertEquals(run(jsonNumber, input), Some(JsonValue.JNumber(12345)))
  }

  test("Parser jsonString") {
    val input = "\"hello world\""
    assertEquals(run(jsonString, input), Some(JsonValue.JString("hello world")))
  }

  test("Parser jsonArray") {
    val input = "[1 , 2 , 3]"
    val expected = JsonValue.JArray(List(
      JsonValue.JNumber(1),
      JsonValue.JNumber(2),
      JsonValue.JNumber(3)
    ))
    assertEquals(run(jsonArray, input), Some(expected))
  }

  test("Parser jsonObject") {
    val input = "{\"a\":1,\"b\":2}"
    val expected = JsonValue.JObject(Map(
      "a" -> JsonValue.JNumber(1),
      "b" -> JsonValue.JNumber(2)
    ))
    assertEquals(run(jsonObject, input), Some(expected))
  }

  test("Parser jsonValue - structure imbriquée") {
    // Structure imbriquée avec tableau et objet
    val input =
      """{
        |  "a": [1, 2, 3],
        |  "b": { "c": "hello", "d": null },
        |  "e": false
        |}""".stripMargin
    val expected = JsonValue.JObject(Map(
      "a" -> JsonValue.JArray(List(
        JsonValue.JNumber(1),
        JsonValue.JNumber(2),
        JsonValue.JNumber(3)
      )),
      "b" -> JsonValue.JObject(Map(
        "c" -> JsonValue.JString("hello"),
        "d" -> JsonValue.JNull
      )),
      "e" -> JsonValue.JBoolean(false)
    ))
    assertEquals(run(jsonValue, input.replaceAll("\\s", "")), Some(expected))
  }

}
