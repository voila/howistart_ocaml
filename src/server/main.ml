open Lwt
open Cohttp
open Cohttp_lwt_unix

type ticker = {timestamp: float; bid: float; ask: float} [@@deriving yojson]

let server =
  let callback _conn req _body =
    match Uri.path (Request.uri req)  with
    | "/ticker" -> 
      let headers = 
        Header.init_with "content-type" "application/json; charset=utf-8" in
      let timestamp = Unix.time () in
      let bid = 408. +. Random.float 5. in
      let ask  = bid +. Random.float 2. in
      let json = ticker_to_yojson {timestamp; bid; ask} in
      let json_str = Yojson.Safe.to_string json in
      Server.respond_string ~headers ~status:`OK ~body:json_str ()
    | _ -> Server.respond_string ~status:`Not_found ~body:"Page not found" ()
  in
  Lwt_io.printf "Server started - http://localhost:8000/ticker\n" >>= fun _ ->
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

let () = 
  ignore (Random.self_init (); Lwt_main.run server)




