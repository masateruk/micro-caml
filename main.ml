let lexbuf outchan l = (* バッファをコンパイルしてチャンネルへ出力する (caml2html: main_lexbuf) *)
  Id.counter := 0;
  output_string outchan
    (CFormat.f
	(Optimize.f
	    (C.f 
		(Closure.f
		    (Assoc.f 
			(Alpha.f 
			    (KNormal.f
				(Wrap.f
				    (Typing.f 
					(Parser.impl Lexer.token l))))))))))

let file input output = (* ファイルをコンパイルしてファイルに出力する (caml2html: main_file) *)
  let inchan = open_in input in
  let outchan = if output = "" then stdout else open_out output in
  try
    lexbuf outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e -> (close_in inchan; close_out outchan; raise e)

let () = (* ここからコンパイラの実行が開始される (caml2html: main_entry) *)
  let infile = ref "" in
  let outfile = ref "" in
    Arg.parse [
      ("-o", Arg.String(fun o -> outfile := o), "output file");
      ("-v", Arg.Unit(fun _ -> D.verbose := true), "verbose mode");
      ("--gc", Arg.Unit(fun _ -> C.enable_gc := true), "enable Boehm GC")
    ]
      (fun s -> infile := s)
      (Printf.sprintf "usage: %s [-o file] [-v] filename" Sys.argv.(0));
    ignore (file !infile !outfile)
