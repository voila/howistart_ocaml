open Lwt
open Websocket_lwt
(** Defines the ticker type: a record aggregating a timestamp and 2 prices, ask 
    and bid.   
    [[@@deriving yojson]], uses ppx_deriving_yojson to generate 
    [ticker_to_yojson], a JSON serializer for our ticker type.
*)    

type ticker = {timestamp: float; bid: float; ask: float} [@@deriving yojson]

let one_tick () = 
  let timestamp = Unix.time () in
  let bid = 408. +. Random.float 5. in
  (* ask >= bid *)
  let ask  = bid +. Random.float 2. in 
  (* serialize our record to a JSON string *)
  let json = ticker_to_yojson {timestamp; bid; ask} in
  Yojson.Safe.to_string json


let server uri =
  let ticks id req recv send =
    let open Frame in
    let rec loop () =
      recv () >>= fun fr ->
      match fr.opcode with
      | Opcode.Ping ->
        send @@ 
        Frame.create ~opcode:Opcode.Pong ~content:fr.content () >>= loop
      | Opcode.Pong
      | Opcode.Binary -> loop ()
      | Opcode.Text -> 
        let json_str = one_tick () in
        send @@
          Frame.create ~opcode:Opcode.Text ~content:json_str () >>= loop
      | _ ->
          send @@ Frame.close 1002
    in
    try%lwt loop () with exn ->
      Lwt.fail exn
  in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  Conduit_lwt_unix.(
    endp_to_server ~ctx:default_ctx endp >>= fun server ->
    establish_server ~ctx:default_ctx ~mode:server ticks
  )

(* Main entry point *)
let () = 
    (* initialize the random number generator *)
    Random.self_init (); 
    let uri = (Uri.of_string "http://localhost:8000/") in
    (* run the server thread *)
    Lwt_main.run (ignore(server uri); fst (Lwt.wait ()))
  

