namespace Triton

module Error =

    exception LexerError of {| message: string |}

    exception CstParserError of {| expected: string; got: Lexeme.Lexeme option; |}
        with
            override this.Message =
                sprintf "Error while parsing to CST: Expected %s but got %O"
                    this.Data0.expected this.Data0.got

    exception CstParserInternalError
        with
            override this.Message = "Error while parsing to CST: internal error"

    exception AstConvertError of {| message: string |}

    exception TypeError of message: string

    exception ExeOutputError of {| message: string |}

    exception DriverError of {| message: string |}

    exception CommandLineParserError of string
