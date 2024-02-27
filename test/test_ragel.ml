open OUnit2
open Test_support
open Markup__Common
module Error = Markup__Error
module Kstream = Markup__Kstream

let tests = [
  ("sanity" >:: fun _ ->
    let html = {|<!doctype html>
    <html>
    <head>
        <title>Example Domain</title>
    
        <meta charset="utf-8" />
        <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
    </head>
    
    <body>
    <div>
        <h1>Example Domain</h1>
        <p>This domain is for use in illustrative examples in documents. You may use this
        domain in literature without prior coordination or asking for permission.</p>
        <p><a href="https://www.iana.org/domains/example">More information...</a></p>
    </div>
    </body>
    </html>
    |} in
    let stream = Markup__Html_tokenizer.Ragel.tokenize html in
    (* let () = 
        let print_token (_, token) _throw k = print_endline @@ String.escaped @@ token_to_string token; k () in
        print_endline "tokens:";
        Kstream.iter print_token stream (wrong_k "failed") ignore;
    in *)
    let report _location err _throw k  = print_endline (Error.to_string err) ; k () in
    let signals = Markup__Html_parser.parse (Some `Document) report (stream, ignore, ignore) in
    let () = 
        print_endline "signals:";
        Kstream.to_list signals (wrong_k "failed") (fun l -> 
            (* if List.length l = 0 then failwith "empty list" *)
                List.iter (fun (_, signal) -> print_endline @@ String.escaped @@ signal_to_string signal) l
        );
    in
    ()

    );
]