namespace Triton

[<RequireQualifiedAccess>]
module Cst =

    type Expr =
        | IntVal of int64
        | CharVal of char
        | BoolVal of bool
        | StringVal of string
        | Ref of string
        | Null
        | AddressOf of Expr
        | Negate of Expr
        | FunCall of FunCall
        | Operator of OperatorCall
    and FunCall = { func: Expr; arguments: Expr list; }
    and OperatorCall = { left: Expr; op: string; right: Expr; }

    type Type =
        | TypeRef of string
        | Array of ArrayType
        | Pointer of Type
        | Fun of FunType
        | Tuple of TypeTuple
    and TypeTupleSlot = { name: string option; type_: Type }
    and TypeTuple = TypeTuple of TypeTupleSlot list
    and FunType = { arguments: TypeTuple; result: TypeTuple }
    and ArrayType = { type_: Type; size: Expr option }

    type AttrValue =
        | None
        | String of string
        | Int of int64
    type AttrDefinition = { name: string; value: AttrValue }
    type AttrList = AttrList of AttrDefinition list
    type AttrLists = AttrLists of AttrList list

    type ConstDefinition = { name: string; type_: Type; value: Expr; }
    type VarDefinition = { name: string; type_: Type; value: Expr option; }

    type Assignment = { name: string; value: Expr; }

    type Statement =
        | ConstStatement of ConstDefinition
        | VarStatement of VarDefinition
        | Assignment of Assignment
        | Expression of Expr
    type FunBody = FunBody of Statement list

    type FunDefinition =
        { name: string
          type_: FunType
          body: FunBody
          attributes: AttrLists }

    type ExternFunDefinition =
        { name: string
          type_: FunType
          attributes: AttrLists }

    type ModuleItem =
        | ConstDefinition of ConstDefinition
        | VarDefinition of VarDefinition
        | FunDefinition of FunDefinition
        | ExternFunDefinition of ExternFunDefinition

    type ModuleTopLevel = ModuleTopLevel of ModuleItem list

    type Module = { name: string; definitions: ModuleTopLevel }

    type TopLevel = TopLevel of Module list

module ParserHelper =

    type ParseResult<'value, 'err> =
        | Match of 'value
        | NoMatch
        | Error of 'err

    type Parser<'lex, 'value, 'err> = 'lex list -> ParseResult<'lex list * 'value, 'err>

    type ExpectedButGot<'lex> = { expected: string; got: 'lex option }

    type ParseSeqBuilder() =

        member this.Bind(x: Parser<'lex, 'x, 'err>,
                         f: 'x -> Parser<'lex, 'y, 'err>)
                         : Parser<'lex, 'y, 'err> =
            fun lexemes ->
                match x lexemes with
                | Match (lexemes, value) -> f value lexemes
                | NoMatch -> NoMatch
                | Error err -> Error err

        member this.Return(x: 'x) : Parser<'lex, 'x, 'err> =
            fun lexemes -> Match (lexemes, x)

        member this.ReturnFrom(x: Parser<'lex, 'x, 'err>) : Parser<'lex, 'x, 'err> = x

    let parseSeq = ParseSeqBuilder()

    let failWith error = Error error

    let expectedButGot expected lexemes =
        match lexemes with 
        | lexeme :: _ -> { expected = expected; got = Some lexeme }
        | [] -> { expected = expected; got = None }

    let failExpectedButGot expected lexemes = Error <| expectedButGot expected lexemes

    let matchEq value expected lexemes =
        match lexemes with
        | x :: rest when x = value -> Match (rest, ())
        | other -> failWith (expectedButGot expected other)

    let tryMatchEq value lexemes =
        match lexemes with
        | x :: rest when x = value -> Match (rest, ())
        | _ -> NoMatch

    let matchIdentifier expected lexemes =
        match lexemes with
        | (Lexeme.Identifier identifier) :: rest -> Match (rest, identifier)
        | other -> failWith (expectedButGot expected other)

    let tryMatchIdentifier lexemes =
        match lexemes with
        | (Lexeme.Identifier identifier) :: rest -> Match (rest, identifier)
        | _ -> NoMatch

    let matchInt expected lexemes =
        match lexemes with
        | (Lexeme.Int intValue) :: rest -> Match (rest, intValue)
        | other -> failWith (expectedButGot expected other)

    let tryMatchInt lexemes =
        match lexemes with
        | (Lexeme.Int intValue) :: rest -> Match (rest, intValue)
        | _ -> NoMatch

    let tryMatchString lexemes =
        match lexemes with
        | (Lexeme.StringLiteral stringValue) :: rest -> Match (rest, stringValue)
        | _ -> NoMatch

    let matchEof expected lexemes =
        match lexemes with
        | [] -> Match ([], ())
        | other -> failWith (expectedButGot expected other)

    let matchFilter filter expected lexemes =
        match lexemes with
        | (x :: rest) as lexemes ->
            match filter x with
            | Some x -> Match (rest, x)
            | None -> failWith (expectedButGot expected lexemes)
        | other -> failWith (expectedButGot expected other)

    let maybeParse defaultValue p lexemes =
        match p lexemes with
        | Match m -> Match m
        | NoMatch -> Match (lexemes, defaultValue)
        | Error err -> Error err

    let rec tryParsers parsers lexemes =
        match parsers with
        | parser :: rest ->
            match parser lexemes with
            | Match m -> Match m
            | NoMatch -> tryParsers rest lexemes
            | Error err -> Error err
        | [] -> NoMatch

    let failIfNoMatch parser expected lexemes =
        match parser lexemes with
        | Match m -> Match m
        | NoMatch -> Error <| expectedButGot expected lexemes
        | Error e -> Error e

    let fail expected lexemes =
        Error <| expectedButGot expected lexemes

    let zeroOrMore parser =
        let rec zeroOrMoreImpl acc = tryParsers [
            parseSeq {
                let! result = parser
                return! zeroOrMoreImpl (result :: acc)
            }
            parseSeq { return List.rev acc }
        ]
        zeroOrMoreImpl []

    let oneOrMore parser =
        let rec oneOrMoreImpl acc = tryParsers [
            parseSeq {
                let! result = parser
                return! oneOrMoreImpl (result :: acc)
            }
            parseSeq { return List.rev acc }
        ]
        parseSeq {
            let! first = parser
            return! oneOrMoreImpl [first]
        }


module CstParser =

    open ParserHelper

    exception CstParserError of {| expected: string; got: Lexeme option; |}
        with
            override this.Message =
                sprintf "Error while parsing to CST: Expected %s but got %O"
                    this.Data0.expected this.Data0.got

    exception CstParserInternalError
        with
            override this.Message = "Error while parsing to CST: internal error"

    let errorExpectedButGot err =
        let { expected = expected; got = got } = err
        CstParserError {| expected = expected; got = got; |}

    module ParseType =
        let private typeTupleItem typeParser = tryParsers [
            parseSeq {
                let! name = tryMatchIdentifier
                do! tryMatchEq (Lexeme.Operator ":")
                let! type_ = typeParser
                return ({ name = Some name; type_ = type_; } : Cst.TypeTupleSlot) }
            parseSeq {
                let! type_ = typeParser
                return ({ name = None; type_ = type_; } : Cst.TypeTupleSlot) } ]

        let rec private typeName = parseSeq {
            let! typeName = tryMatchIdentifier
            return Cst.TypeRef typeName }

        and private pointerType lexemes = (parseSeq {
            do! tryMatchEq Lexeme.Caret
            let! pointerType = failIfNoMatch anyType "type in pointer type"
            return Cst.Pointer pointerType } ) lexemes

        and private arrayType lexemes = (parseSeq {
            do! tryMatchEq Lexeme.LeftSquare
            let! size = maybeParse None (parseSeq {
                let! intValue = tryMatchInt
                return Some <| Cst.IntVal intValue } )
            do! matchEq Lexeme.RightSquare "closing square bracket in array type"
            let! arrayType = failIfNoMatch anyType "array element type"
            return Cst.Array { type_ = arrayType; size = size } } ) lexemes

        and private typeCloseBrackets elements =
            tryParsers [
                parseSeq {
                    do! tryMatchEq Lexeme.Comma
                    let! nextType = failIfNoMatch anyType "type in tuple type"
                    let newElements : Cst.TypeTupleSlot list = { name = None; type_ = nextType; } :: elements
                    return! typeCloseBrackets newElements }
                parseSeq {
                    do! matchEq Lexeme.RightBracket "closing bracket in tuple type"
                    return elements |> List.rev |> Cst.TypeTuple |> Cst.Type.Tuple }
            ]

        and private typeBrackets lexeme = (parseSeq {
            do! tryMatchEq Lexeme.LeftBracket
            let! typeItems = tryParsers [
                parseSeq {
                    let! first = typeTupleItem anyType
                    let! rest = zeroOrMore (parseSeq {
                        do! tryMatchEq Lexeme.Comma
                        return! typeTupleItem anyType } )
                    do! tryParsers [
                        tryMatchEq Lexeme.Comma
                        parseSeq { return () } ]
                    return first :: rest }
                parseSeq { return [] }] 
            do! matchEq Lexeme.RightBracket "next type or right bracket in type tuple"
            return typeItems |> Cst.TypeTuple |> Cst.Type.Tuple } ) lexeme

        and private nonFunType = tryParsers [
            typeName
            pointerType
            arrayType
            typeBrackets ]

        and private toTupleType = function
            | Cst.Type.Tuple tt -> tt
            | t -> Cst.TypeTuple [ { name = None; type_ = t; } ]

        and private anyType lexemes = (parseSeq {
            let! arguments = nonFunType
            return! tryParsers [
                parseSeq {
                    do! tryMatchEq (Lexeme.Operator "->")
                    let! result = failIfNoMatch nonFunType "return type"
                    return Cst.Fun { arguments = toTupleType arguments
                                     result = toTupleType result } }
                parseSeq { return arguments } ] } ) lexemes

        let tryType (lexemes: Lexeme list) : ParseResult<Lexeme list * Cst.Type, ExpectedButGot<Lexeme>> =
            anyType lexemes

        let funType = parseSeq {
            let! arguments = failIfNoMatch nonFunType "type in function arguments type"
            do! matchEq (Lexeme.Operator "->") "-> in function type"
            let! result = failIfNoMatch nonFunType "type in function result type"
            return ( { arguments = toTupleType arguments
                       result = toTupleType result } : Cst.FunType) }

    module ParseExpression =

        let intConst = parseSeq { let! intVal = tryMatchInt in return Cst.IntVal intVal }

        let stringConst = parseSeq { let! value = tryMatchString in return Cst.StringVal value }

        let reference = parseSeq { let! identifier = tryMatchIdentifier in return Cst.Ref identifier }

        let brackets innerParser = parseSeq {
            do! tryMatchEq Lexeme.LeftBracket
            let! result = failIfNoMatch innerParser "expression inside brackets"
            do! matchEq Lexeme.RightBracket "matching closing bracket in expression"
            return result }

        let negate innerParser = parseSeq {
            do! tryMatchEq (Lexeme.Operator "-")
            let! inner = failIfNoMatch innerParser "expression after '-'"
            let result = Cst.Negate inner
            return result }

        let addressOf innerParser = parseSeq {
            do! tryMatchEq Lexeme.AtSign
            let! inner = failIfNoMatch innerParser "expression after '@'"
            let result = Cst.AddressOf inner
            return result }

        let minimalExpression innerParser = tryParsers [
            intConst
            stringConst
            reference
            negate innerParser
            addressOf innerParser
            brackets innerParser ]

        let funArgs innerParser = tryParsers [
            parseSeq {
                let! first = innerParser
                let! rest = zeroOrMore (parseSeq {
                    do! tryMatchEq Lexeme.Comma
                    return! failIfNoMatch innerParser "function argument after comma" })
                return first :: rest }
            parseSeq { return [] } ]

        let continueFunCall innerParser func = parseSeq {
            do! tryMatchEq Lexeme.LeftBracket
            let! args = funArgs innerParser
            do! matchEq Lexeme.RightBracket "right bracket after arguments list in function call"
            let result = Cst.FunCall { func = func; arguments = args; }
            return result }

        let rec tryContinueExpression innerParser result = tryParsers [
            parseSeq {
                let! funCallResult = continueFunCall innerParser result
                return! tryContinueExpression innerParser funCallResult }
            parseSeq { return result } ]

        let expressionRec innerParser = parseSeq {
            let! minimal = minimalExpression innerParser
            return! tryContinueExpression innerParser minimal }

        let rec tryExpression lexemes = expressionRec tryExpression lexemes

        let expression expected = failIfNoMatch tryExpression expected

    module ParseModule = 

        let constDefinition = parseSeq {
            do! tryMatchEq (Lexeme.Identifier "const")
            let! name = matchIdentifier "constant name after const keyword"
            do! matchEq (Lexeme.Operator ":") "colon after constant name in constant definition"
            let! constType = failIfNoMatch ParseType.tryType "constant type after colon in constant definition"
            do! matchEq (Lexeme.Operator ":=") "assignment operator after constant type in constant definition"
            let! value = ParseExpression.expression "constant value after assignment operator in constant definition"
            do! matchEq Lexeme.Semicolon "semicolon after constant definition"

            let constantDefinition =
                { Cst.ConstDefinition.name = name
                  Cst.ConstDefinition.type_ = constType
                  Cst.ConstDefinition.value = value }
            return constantDefinition }

        let varDefinition = parseSeq {
            do! tryMatchEq (Lexeme.Identifier "var")
            let! name = matchIdentifier "variable name after var keyword"
            do! matchEq (Lexeme.Operator ":") "colon after varibale name in variable definition"
            let! varType = failIfNoMatch ParseType.tryType "variable type after colon in variable definition"
            do! matchEq Lexeme.Semicolon "semicolon after variable definition"

            let variableDefinition : Cst.VarDefinition =
                { name = name
                  type_ = varType
                  value = None }
            return variableDefinition }

        let assignment = parseSeq {
            let! name = tryMatchIdentifier
            do! tryMatchEq (Lexeme.Operator ":=")
            let! value = ParseExpression.expression "expression after assignment operator"
            do! matchEq Lexeme.Semicolon "semicolon after assignment"

            let assignment =
                { Cst.Assignment.name = name
                  Cst.Assignment.value = value }
            return Cst.Assignment assignment }

        let expr = parseSeq {
            let! result = ParseExpression.tryExpression
            do! matchEq Lexeme.Semicolon "semicolon after expression"
            return Cst.Expression result }

        let funBodyItem = tryParsers [
            parseSeq { let! varDef = varDefinition in return Cst.VarStatement varDef }
            assignment
            expr ]

        let funDefinition = parseSeq {
            do! tryMatchEq (Lexeme.Identifier "fun")
            let! name = matchIdentifier "function name after fun keyword"
            do! matchEq (Lexeme.Operator ":") "colon after function name in function definition"
            let! funType = ParseType.funType
            do! matchEq Lexeme.LeftCurly "function body after function type in function definition"
            let! bodyItems = zeroOrMore funBodyItem
            do! matchEq Lexeme.RightCurly "closing curly bracket after function body in function definition"

            let functionDefinition : Cst.FunDefinition =
                { name = name
                  type_ = { arguments = funType.arguments; result = funType.result }
                  body = Cst.FunBody bodyItems
                  attributes = Cst.AttrLists [] }
            return functionDefinition }

        let externFunDefinition = parseSeq {
            do! tryMatchEq (Lexeme.Identifier "extern")
            do! matchEq (Lexeme.Identifier "fun") "fun keyword after extern"
            let! name = matchIdentifier "function name after fun keyword"
            do! matchEq (Lexeme.Operator ":") "colon after function name in external function definition"
            let! funType = ParseType.funType
            do! matchEq Lexeme.Semicolon "semicolon after external function definition"

            let externDefinition : Cst.ExternFunDefinition =
                { name = name
                  type_ = { arguments = funType.arguments; result = funType.result }
                  attributes = Cst.AttrLists [] }
            return externDefinition }

        let funAttr = parseSeq {
            let! name = tryMatchIdentifier
            let! value = maybeParse Cst.None (parseSeq {
                do! tryMatchEq (Lexeme.Operator "=")
                return! failIfNoMatch (tryParsers [
                    parseSeq { let! value = tryMatchInt in return Cst.Int value }
                    parseSeq { let! value = tryMatchString in return Cst.String value }
                ] ) "attribute value"
            } )
            return ({ name = name; value = value; } : Cst.AttrDefinition) }

        let funAttrList = parseSeq {
            do! tryMatchEq Lexeme.LeftSquare
            let! first = funAttr
            let! rest = zeroOrMore (parseSeq {
                do! tryMatchEq Lexeme.Comma
                return! funAttr } )
            do! matchEq Lexeme.RightSquare "attribute or right square bracket"
            return Cst.AttrList (first :: rest) }

        let withAttrLists attributes = tryParsers [
            parseSeq { let! funDef = funDefinition in return Cst.FunDefinition { funDef with attributes = attributes } }
            parseSeq { let! externDef = externFunDefinition in return Cst.ExternFunDefinition { externDef with attributes = attributes } }
            parseSeq { return! fail "function definition after attributes" }
        ]

        let moduleBodyItem = tryParsers [
            parseSeq { let! constDef = constDefinition in return Cst.ConstDefinition constDef }
            parseSeq { let! funDef = funDefinition in return Cst.FunDefinition funDef }
            parseSeq { let! externDef = externFunDefinition in return Cst.ExternFunDefinition externDef }
            parseSeq { let! attrLists = oneOrMore funAttrList in return! withAttrLists (Cst.AttrLists attrLists) }
        ]

        let moduleDefinition = parseSeq {
            do! tryMatchEq (Lexeme.Identifier "module")
            let! name = matchIdentifier "module name"
            do! matchEq Lexeme.LeftCurly "module body"
            let! items = zeroOrMore moduleBodyItem
            do! matchEq Lexeme.RightCurly "module definition or closing curly bracket"
            return { Cst.Module.name = name; Cst.Module.definitions = Cst.ModuleTopLevel items } }

        let topLevel = parseSeq {
            let! modules = zeroOrMore moduleDefinition
            do! matchEof "module definition"
            return Cst.TopLevel modules }

    let parse (lexemes: Lexeme list) : Cst.TopLevel =
        match ParseModule.topLevel lexemes with
        | Match (_, result) -> result
        | NoMatch -> raise CstParserInternalError
        | Error err -> raise <| errorExpectedButGot err
