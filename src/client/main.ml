open Lwt.Infix
open Cohttp_lwt_unix


let rec request () =
  (* Some bogus ask amd bid price *)
  Client.get (Uri.of_string "http://127.0.0.1:8000/ticker")
  (** Bind, or as some affectionally call shove: 'a Lwt.t -> (a' -> b'
      Lwt.t) -> 'b Lwt.t What we're doing here is shoving the thing inside
      the monad on the left of the >>= into the right side of the >>= then
      wrapping that result in a monad.  Another way to think about this
      might be: "I don't know when the left side will happen, but when it
      does, do this with it." *)
  >>= fun (resp, body) -> (
  (** Some debugging code for you to play with, technically bad since
      its using plain printf and not Lwt's Lwt_io.print functions *)
  (* let code = resp |> Response.status |> Cohttp.Code.code_of_status in *)
  (* Printf.printf "Response code: %d\n" code; *)
  (* Printf.printf "Headers: %s\n" (resp |> Response.headers |> Cohttp.Header.to_string); *)
  body |> Cohttp_lwt_body.to_string >|= (fun as_str ->
  Yojson.Basic.from_string as_str
  |> Yojson.Basic.pretty_to_string
  |> Lwt_io.printl ))

let () =
  (** Lwt is a cooperative threading library in OCaml. Cooperative
      threads (A.K.A. non-premtive threads) typically yield control to
      another thread before they finish.  People use Lwt for I/O, events,
      and ease of use. See also, async. *)

  (* Lwt_main.run : 'a Lwt.t -> 'a *)
  (** run t calls the Lwt scheduler repeatedly until t terminates,
      then returns the value returned by the thread.  If t fails with
      an exception the exception is raised. *)
  Lwt_main.run (request ()) |> Lwt.ignore_result



