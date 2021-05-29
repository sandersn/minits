open System
type Token =
 | IntLiteral of text: string * value: int
 | Identifier of text: string
 | Whitespace
 | Plus
 | Minus
 | Star
 | ForwardSlash
 | Unknown
 | EOF
let lex (s : string) = 
    let mutable pos =  0
    let scan () = 
        if pos = s.Length then EOF else
        let scanForward pred =
            while pos < s.Length && pred s.[pos] do
              pos <- pos + 1
        let start = pos
        pos <- pos + 1
        match s.[pos] with
        | '+' -> Plus
        | '-' -> Minus
        | '*' -> Star
        | '/' -> ForwardSlash
        | c when Char.IsWhiteSpace c ->
          scanForward Char.IsWhiteSpace
          Whitespace
        | c when Char.IsNumber c-> 
          scanForward Char.IsNumber
          IntLiteral(s.[start..pos - 1], int s.[start..pos - 1]) // TODO: Catch too-large exceptions and whatnot
        | c when Char.IsLetter c ->
          scanForward Char.IsLetterOrDigit
          Identifier s.[start..pos - 1]
        | _ -> Unknown
    let rec scanLoop acc = // TODO: Mutable state probs
        match scan () with
        | EOF -> List.rev acc
        | t -> scanLoop (t :: acc)
    scanLoop []
[<EntryPoint>]
let main argv =
    //let message = lex " " // Call the function
    lex " 1200Hello    World1! 14d" |> printfn "%A" 
    0 // return an integer exit code