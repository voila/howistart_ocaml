open Lwt.Infix
open Websocket_lwt

let request uri =
  let open Frame in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  Conduit_lwt_unix.(endp_to_client ~ctx:default_ctx endp >>= fun client ->
  with_connection ~ctx:default_ctx client uri) >>= fun (recv, send) ->
  let rec handle_frame fr =
    let open Opcode in
    match fr.opcode with
    | Ping -> send @@ Frame.create ~opcode:Pong ()
    | Pong -> Lwt.return_unit
    | Binary 
    | Text -> print_frame fr
    | Close
    | _ -> send @@ Frame.close 1002 >> Lwt.fail Exit
  and print_frame fr =
    Yojson.Basic.from_string fr.content
    |> Yojson.Basic.pretty_to_string
    |> Lwt_io.printl 
  in
  let rec loop () = 
    send @@ Frame.create ~opcode:Opcode.Text ~content:"tick" () >> 
    recv () >>= handle_frame >> Lwt.return (Unix.sleep 1) >>= loop
  in loop ()


let () = 
  Lwt_main.run (request @@ Uri.of_string "http://127.0.0.1:8000/ticker") 
  |> Lwt.ignore_result






