open Lwt
open Cohttp
open Cohttp_lwt_unix

(** Defines the ticker type: a record aggregating a timestamp and 2 prices, ask 
    and bid.   
    [[@@deriving yojson]], uses ppx_deriving_yojson to generate 
    [ticker_to_yojson], a JSON serializer for our ticker type.
*)    

type ticker = {timestamp: float; bid: float; ask: float} [@@deriving yojson]


let server =
  (* the callback defining the server's behaviour *)
  let callback _conn req _body =
    match Uri.path (Request.uri req)  with
    (* we only serve JSON at the '/ticker' path *)
    | "/ticker" -> 
      let headers = 
        Header.init_with "content-type" "application/json; charset=utf-8" in
      let timestamp = Unix.time () in
      let bid = 408. +. Random.float 5. in
      (* ask >= bid *)
      let ask  = bid +. Random.float 2. in 
      (* serialize our record to a JSON string *)
      let json = ticker_to_yojson {timestamp; bid; ask} in
      let json_str = Yojson.Safe.to_string json in
      (* send the server's response *)
      Server.respond_string ~headers ~status:`OK ~body:json_str ()
    (* for any other path, matched by '_', the server responds with a 404 *)
    | _ -> Server.respond_string ~status:`Not_found ~body:"Page not found" ()
  in
  (* print to the console *)
  Lwt_io.printf "Server started - http://localhost:8000/ticker\n" >>= fun _ ->
  (* create an HTTP server with the callback defined above *)
  Server.create ~mode:(`TCP (`Port 8000)) (Server.make ~callback ())

(* Main entry point *)
let () = 
  ignore (
    (* initialize the random number generator *)
    Random.self_init (); 
    (* run the server thread *)
    Lwt_main.run server)




