package fitnesse.idea.lexer

class MiscLexerSuite extends LexerSuite {
  test("Regular text followed by Wiki Word") {
    assertResult(
      List(
        (FitnesseTokenType.WORD, "Some"),
        (FitnesseTokenType.WHITE_SPACE, " "),
        (FitnesseTokenType.WORD, "text"),
        (FitnesseTokenType.WHITE_SPACE, " "),
        (FitnesseTokenType.WORD, "and"),
        (FitnesseTokenType.WHITE_SPACE, " "),
        (FitnesseTokenType.WIKI_WORD, "WikiWord")
      )) {
      lex("Some text and WikiWord")
    }
  }

  test("Wiki Word followed by a LF") {
    assertResult(
      List(
        (FitnesseTokenType.WIKI_WORD, "WikWord"),
        (FitnesseTokenType.LINE_TERMINATOR, "\n")
      )) {
      lex("WikWord\n")
    }
  }

  test("Wiki Word followed by a CR LF") {
    assertResult(
      List(
        (FitnesseTokenType.WIKI_WORD, "WikWord"),
        (FitnesseTokenType.LINE_TERMINATOR, "\n")
      )) {
      lex("WikWord\n")
    }
  }

  test("Wiki Word that ends in a capital letter followed by a LF") {
    assertResult(
      List(
        (FitnesseTokenType.WIKI_WORD, "WikiWordThisIsA"),
        (FitnesseTokenType.LINE_TERMINATOR, "\n")
      )) {
      lex("WikiWordThisIsA\n")
    }
  }

  test("Wiki Word that ends in a capital letter followed by a CR LF") {
    assertResult(
      List(
        (FitnesseTokenType.WIKI_WORD, "WikiWordThisIsA"),
        (FitnesseTokenType.LINE_TERMINATOR, "\r\n")
      )) {
      lex("WikiWordThisIsA\r\n")
    }
  }

  test("Table that has regular text right after it") {
    assertResult(
      List(
        (FitnesseTokenType.TABLE_START, "|"),
        (FitnesseTokenType.WORD, "abc"),
        (FitnesseTokenType.ROW_END, "|\n|"),
        (FitnesseTokenType.WORD, "xyz"),
        (FitnesseTokenType.TABLE_END, "|\n"),
        (FitnesseTokenType.WORD, "some"),
        (FitnesseTokenType.WHITE_SPACE, " "),
        (FitnesseTokenType.WORD, "text")
      )) {
      lex("|abc|\n|xyz|\nsome text")
    }
  }

  test("Table that has spaces regular text right after it") {
    assertResult(
      List(
        (FitnesseTokenType.TABLE_START, "|"),
        (FitnesseTokenType.WHITE_SPACE, " "),
        (FitnesseTokenType.WORD, "abc"),
        (FitnesseTokenType.WHITE_SPACE, " "),
        (FitnesseTokenType.ROW_END, "|\n|"),
        (FitnesseTokenType.WORD, "xyz"),
        (FitnesseTokenType.TABLE_END, "|\n"),
        (FitnesseTokenType.WORD, "some"),
        (FitnesseTokenType.WHITE_SPACE, " "),
        (FitnesseTokenType.WORD, "text")
      )) {
      lex("| abc |\n|xyz|\nsome text")
    }
  }

  test("Table that has a WikiWord right after it") {
    assertResult(
      List(
        (FitnesseTokenType.TABLE_START, "|"),
        (FitnesseTokenType.WORD, "abc"),
        (FitnesseTokenType.ROW_END, "|\n|"),
        (FitnesseTokenType.WORD, "xyz"),
        (FitnesseTokenType.TABLE_END, "|\n"),
        (FitnesseTokenType.WIKI_WORD, "WikiWord")
      )) {
      lex("|abc|\n|xyz|\nWikiWord")
    }
  }

  test("Collapsible section") {
    assertResult(
      List(
        (FitnesseTokenType.COLLAPSIBLE_START, "!* "),
        (FitnesseTokenType.WORD, "abc"),
        (FitnesseTokenType.LINE_TERMINATOR, "\n"),
        (FitnesseTokenType.WORD, "def"),
        (FitnesseTokenType.LINE_TERMINATOR, "\n"),
        (FitnesseTokenType.COLLAPSIBLE_END, "*!"),
        (FitnesseTokenType.WHITE_SPACE, " "),
        (FitnesseTokenType.WORD, "word")
      )) {
      lex("!* abc\ndef\n*! word")
    }
  }

  test("Collapsed section") {
    assertResult(
      List(
        (FitnesseTokenType.COLLAPSIBLE_START, 0, 7, "!****< "),
        (FitnesseTokenType.WORD, 7, 10, "bbc"),
        (FitnesseTokenType.LINE_TERMINATOR, 10, 11, "\n"),
        (FitnesseTokenType.WORD, 11, 14, "def"),
        (FitnesseTokenType.LINE_TERMINATOR, 14, 15, "\n"),
        (FitnesseTokenType.COLLAPSIBLE_END, 15, 17, "*!"),
        (FitnesseTokenType.WHITE_SPACE, 17, 18, " "),
        (FitnesseTokenType.WORD, 18, 22, "word")
      )) {
      lexWithOffset("!****< bbc\ndef\n*! word")
    }
  }

  test("Collapsible section multi word title") {
    assertResult(
      List(
        (FitnesseTokenType.COLLAPSIBLE_START, 0, 4, "!*> "),
        (FitnesseTokenType.WORD, 4, 9, "Extra"),
        (FitnesseTokenType.WHITE_SPACE, 9, 10, " "),
        (FitnesseTokenType.WORD, 10, 16, "Import"),
        (FitnesseTokenType.LINE_TERMINATOR, 16, 17, "\n"),
        (FitnesseTokenType.LINE_TERMINATOR, 17, 18, "\n"),
        (FitnesseTokenType.WORD, 18, 20, "Hi"),
        (FitnesseTokenType.LINE_TERMINATOR, 20, 21, "\n"),
        (FitnesseTokenType.COLLAPSIBLE_END, 21, 24, "*!\n"),
        (FitnesseTokenType.LINE_TERMINATOR, 24, 25, "\n")
      )) {
      lexWithOffset("!*> Extra Import\n\nHi\n*!\n\n")
    }
  }

  test("Collapsible section containing a table") {
    assertResult(
      List(
        (FitnesseTokenType.COLLAPSIBLE_START, 0, 3, "!* "),
        (FitnesseTokenType.WORD, 3, 8, "title"),
        (FitnesseTokenType.LINE_TERMINATOR, 8, 9, "\n"),
        (FitnesseTokenType.TABLE_START, 9, 10, "|"),
        (FitnesseTokenType.WORD, 10, 13, "abc"),
        (FitnesseTokenType.ROW_END, 13, 16, "|\n|"),
        (FitnesseTokenType.WORD, 16, 19, "xyz"),
        (FitnesseTokenType.TABLE_END, 19, 21, "|\n"),
        (FitnesseTokenType.COLLAPSIBLE_END, 21, 23, "*!")
      )) {
      lexWithOffset("!* title\n|abc|\n|xyz|\n*!")
    }
  }

  test("Collapsible section containing a table followed by newline") {
    assertResult(
      List(
        (FitnesseTokenType.COLLAPSIBLE_START, 0, 3, "!* "),
        (FitnesseTokenType.WORD, 3, 8, "title"),
        (FitnesseTokenType.LINE_TERMINATOR, 8, 9, "\n"),
        (FitnesseTokenType.TABLE_START, 9, 10, "|"),
        (FitnesseTokenType.WORD, 10, 13, "abc"),
        (FitnesseTokenType.ROW_END, 13, 16, "|\n|"),
        (FitnesseTokenType.WORD, 16, 19, "xyz"),
        (FitnesseTokenType.TABLE_END, 19, 21, "|\n"),
        (FitnesseTokenType.COLLAPSIBLE_END, 21, 24, "*!\n")
      )) {
      lexWithOffset("!* title\n|abc|\n|xyz|\n*!\n")
    }
  }

}
