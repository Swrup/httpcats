type error

val pp_error : error Fmt.t

module Method = H2.Method
module Headers = H2.Headers
module Status = H2.Status

type stop

val stop : unit -> stop
val switch : stop -> unit

type request = {
    meth: Method.t
  ; target: string
  ; scheme: string
  ; headers: Headers.t
}

type response = { status: Status.t; headers: Headers.t }
type body = [ `V1 of H1.Body.Writer.t | `V2 of H2.Body.Writer.t ]
type reqd = [ `V1 of H1.Reqd.t | `V2 of H2.Reqd.t ]

type error_handler =
  [ `V1 | `V2 ] -> ?request:request -> error -> (Headers.t -> body) -> unit

type handler =
  [ `Tcp of Miou_unix.file_descr | `Tls of Tls_miou_unix.t ] -> reqd -> unit

type websocket_frame_kind =
  [ `Connection_close
  | `Msg of H1_ws.Websocket.Opcode.standard_non_control * bool
  | `Other
  | `Ping
  | `Pong ]

type websocket_frame = (websocket_frame_kind * bytes) option

module Websocket_stream : sig
  type t

  val put : t -> websocket_frame -> unit
  val get : t -> websocket_frame
end

type websocket_handler =
  in_stream:Websocket_stream.t -> out_stream:Websocket_stream.t -> unit

val clear :
     ?parallel:bool
  -> ?stop:stop
  -> ?config:H1.Config.t
  -> ?backlog:int
  -> ?ready:unit Miou.Computation.t
  -> ?error_handler:error_handler
  -> ?upgrade:(Miou_unix.file_descr -> unit)
  -> handler:handler
  -> Unix.sockaddr
  -> unit

val with_tls :
     ?parallel:bool
  -> ?stop:stop
  -> ?config:
       [ `Both of H1.Config.t * H2.Config.t
       | `HTTP_1_1 of H1.Config.t
       | `H2 of H2.Config.t ]
  -> ?backlog:int
  -> ?ready:unit Miou.Computation.t
  -> ?error_handler:error_handler
  -> Tls.Config.server
  -> ?upgrade:(Tls_miou_unix.t -> unit)
  -> handler:handler
  -> Unix.sockaddr
  -> unit

val websocket_upgrade :
  handler:websocket_handler -> Miou_unix.file_descr -> unit
