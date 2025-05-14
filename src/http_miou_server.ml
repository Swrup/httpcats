open Http_miou_unix
module A = Runtime.Make (Tls_miou_unix) (H1.Server_connection)

module TCP_and_H1 = struct
  include TCP

  (* NOTE(dinosaure): an early [shutdown `read] is not really appreciated by
     http/1.1 servers. We do nothing in this case. However, we do make sure that
     as soon as the process has finished, we close the socket.

     See [http_1_1_server_connection] for the [Unix.close]. *)
  let shutdown flow = function `read -> () | value -> shutdown flow value
end

module H2_Server_connection = struct
  include H2.Server_connection

  let next_read_operation t =
    (next_read_operation t :> [ `Close | `Read | `Yield | `Upgrade ])

  let next_write_operation t =
    (next_write_operation t
      :> [ `Close of int
         | `Write of Bigstringaf.t Faraday.iovec list
         | `Yield
         | `Upgrade ])
end

module B = Runtime.Make (TCP_and_H1) (H1.Server_connection)
module C = Runtime.Make (TLS) (H2_Server_connection)

type error =
  [ `V1 of H1.Server_connection.error
  | `V2 of H2.Server_connection.error
  | `Protocol of string ]

type stop = Miou.Mutex.t * Miou.Condition.t * bool ref

let pp_error ppf = function
  | `V1 `Bad_request -> Fmt.string ppf "Bad HTTP/1.1 request"
  | `V1 `Bad_gateway -> Fmt.string ppf "Bad HTTP/1.1 gateway"
  | `V1 `Internal_server_error | `V2 `Internal_server_error ->
      Fmt.string ppf "Internal server error"
  | `V1 (`Exn exn) | `V2 (`Exn exn) ->
      Fmt.pf ppf "Unknown exception: %s" (Printexc.to_string exn)
  | `V2 `Bad_request -> Fmt.string ppf "Bad H2 request"
  | `Protocol msg -> Fmt.string ppf msg

let src = Logs.Src.create "http-miou-server"

module Log = (val Logs.src_log src : Logs.LOG)
module Method = H2.Method
module Headers = H2.Headers
module Status = H2.Status

type flow = [ `Tls of Tls_miou_unix.t | `Tcp of Miou_unix.file_descr ]

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

type handler = flow -> reqd -> unit

let request_from_H1 ~scheme { H1.Request.meth; target; headers; _ } =
  let headers = Headers.of_list (H1.Headers.to_list headers) in
  { meth; target; scheme; headers }

let request_from_h2 { H2.Request.meth; target; scheme; headers } =
  { meth; target; scheme; headers }

let errf = Fmt.str "<h1>500 Internal server error</h1><p>Error: %a</p>" pp_error

let default_error_handler version ?request:_ err respond =
  let str = errf err in
  let hdrs =
    match version with
    | `V1 ->
        [
          ("content-type", "text/html; charset=utf-8")
        ; ("content-length", string_of_int (String.length str))
        ; ("connection", "close")
        ]
    | `V2 ->
        [
          ("content-type", "text/html; charset=utf-8")
        ; ("content-length", string_of_int (String.length str))
        ]
  in
  let hdrs = H2.Headers.of_list hdrs in
  match respond hdrs with
  | `V1 body ->
      H1.Body.Writer.write_string body str;
      let fn () =
        if H1.Body.Writer.is_closed body = false then H1.Body.Writer.close body
      in
      H1.Body.Writer.flush body fn
  | `V2 body ->
      Log.debug (fun m -> m "respond with a h2 error");
      H2.Body.Writer.write_string body str;
      let fn = function
        | `Closed -> ()
        | `Written ->
            Log.debug (fun m -> m "close the h2 body");
            H2.Body.Writer.close body
      in
      Log.debug (fun m -> m "flush the errored h2 response");
      H2.Body.Writer.flush body fn

let http_1_1_server_connection ~config ~user's_error_handler ?upgrade
    ~user's_handler flow =
  let scheme = "http" in
  let read_buffer_size = config.H1.Config.read_buffer_size in
  let error_handler ?request err respond =
    let request = Option.map (request_from_H1 ~scheme) request in
    let err = `V1 err in
    let respond hdrs =
      let hdrs = H1.Headers.of_list (Headers.to_list hdrs) in
      let body = respond hdrs in
      `V1 body
    in
    user's_error_handler `V1 ?request err respond
  in
  let request_handler reqd = user's_handler (`Tcp flow) (`V1 reqd) in
  let conn =
    H1.Server_connection.create ~config ~error_handler request_handler
  in
  (* NOTE(dinosaure): see the module [TCP_and_H1] and the fake shutdown. We must
     finalize the process with [Miou_unix.close flow] — and avoid a fd leak. At
     the end, the flow is only shutdown on the write side. *)
  let finally () = Miou_unix.close flow in
  Fun.protect ~finally @@ fun () ->
  Miou.await_exn (B.run conn ~read_buffer_size ?upgrade flow)

let https_1_1_server_connection ~config ~user's_error_handler ?upgrade
    ~user's_handler flow =
  let scheme = "https" in
  let read_buffer_size = config.H1.Config.read_buffer_size in
  let error_handler ?request err respond =
    let request = Option.map (request_from_H1 ~scheme) request in
    let err = `V1 err in
    let respond hdrs =
      let hdrs = H1.Headers.of_list (Headers.to_list hdrs) in
      let body = respond hdrs in
      `V1 body
    in
    user's_error_handler `V1 ?request err respond
  in
  let request_handler reqd = user's_handler (`Tls flow) (`V1 reqd) in
  let conn =
    H1.Server_connection.create ~config ~error_handler request_handler
  in
  Miou.await_exn (A.run conn ~read_buffer_size ?upgrade flow)

let h2s_server_connection ~config ~user's_error_handler ?upgrade ~user's_handler
    flow =
  let read_buffer_size = config.H2.Config.read_buffer_size in
  let error_handler ?request err respond =
    let request = Option.map request_from_h2 request in
    let err = `V2 err in
    let respond hdrs = `V2 (respond hdrs) in
    user's_error_handler `V2 ?request err respond
  in
  let request_handler reqd = user's_handler (`Tls flow) (`V2 reqd) in
  let conn =
    H2.Server_connection.create ~config ~error_handler request_handler
  in
  Miou.await_exn (C.run conn ~read_buffer_size ?upgrade flow)

let rec clean_up orphans =
  match Miou.care orphans with
  | None | Some None -> ()
  | Some (Some prm) -> (
      match Miou.await prm with
      | Ok () -> clean_up orphans
      | Error exn ->
          Log.err (fun m ->
              m "unexpected exception: %s" (Printexc.to_string exn));
          clean_up orphans)

exception Stop

let rec wait ((m, c, v) as stop) () =
  let value =
    Miou.Mutex.protect m @@ fun () ->
    while not !v do
      Miou.Condition.wait c m
    done;
    !v
  in
  if value then raise Stop else wait stop ()

let stop () = (Miou.Mutex.create (), Miou.Condition.create (), ref false)

let switch (m, c, v) =
  Miou.Mutex.protect m @@ fun () ->
  v := true;
  Miou.Condition.broadcast c

let accept_or_stop ?stop file_descr =
  match stop with
  | None -> Some (Miou_unix.accept file_descr)
  | Some stop -> (
      let accept = Miou.async @@ fun () -> Miou_unix.accept file_descr in
      let stop = Miou.async (wait stop) in
      Log.debug (fun m -> m "waiting for a client");
      match Miou.await_first [ accept; stop ] with
      | Ok (fd, sockaddr) -> Some (fd, sockaddr)
      | Error Stop -> None
      | Error exn ->
          Log.err (fun m ->
              m "unexpected exception: %S" (Printexc.to_string exn));
          raise exn)

let pp_sockaddr ppf = function
  | Unix.ADDR_UNIX str -> Fmt.pf ppf "<%s>" str
  | Unix.ADDR_INET (inet_addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port

let clear ?(parallel = true) ?stop ?(config = H1.Config.default) ?backlog ?ready
    ?error_handler:(user's_error_handler = default_error_handler) ?upgrade
    ~handler:user's_handler sockaddr =
  let domains = Miou.Domain.available () in
  let call ~orphans fn =
    if parallel && domains >= 2 then ignore (Miou.call ~orphans fn)
    else ignore (Miou.async ~orphans fn)
  in
  let rec go orphans file_descr =
    match accept_or_stop ?stop file_descr with
    | None ->
        Log.debug (fun m -> m "stop the server");
        Runtime.terminate orphans;
        Miou_unix.close file_descr
    | Some (fd', sockaddr) ->
        Log.debug (fun m ->
            m "receive a connection from: %a" pp_sockaddr sockaddr);
        clean_up orphans;
        call ~orphans
          begin
            fun () ->
              http_1_1_server_connection ~config ~user's_error_handler ?upgrade
                ~user's_handler fd'
          end;
        go orphans file_descr
  in
  let socket =
    match sockaddr with
    | Unix.ADDR_UNIX _ -> invalid_arg "Impossible to create a Unix socket"
    | Unix.ADDR_INET (inet_addr, _) ->
        if Unix.is_inet6_addr inet_addr then Miou_unix.tcpv6 ()
        else Miou_unix.tcpv4 ()
  in
  Miou_unix.bind_and_listen ?backlog socket sockaddr;
  Option.iter (fun c -> ignore (Miou.Computation.try_return c ())) ready;
  go (Miou.orphans ()) socket

let alpn tls =
  match Tls_miou_unix.epoch tls with
  | Some { Tls.Core.alpn_protocol= protocol; _ } ->
      Log.debug (fun m ->
          m "protocol of the incoming client: %a"
            Fmt.(Dump.option string)
            protocol);
      protocol
  | None -> None

let with_tls ?(parallel = true) ?stop
    ?(config = `Both (H1.Config.default, H2.Config.default)) ?backlog ?ready
    ?error_handler:(user's_error_handler = default_error_handler) tls_config
    ?upgrade ~handler:user's_handler sockaddr =
  let domains = Miou.Domain.available () in
  let call ~orphans fn =
    if parallel && domains >= 2 then ignore (Miou.call ~orphans fn)
    else ignore (Miou.async ~orphans fn)
  in
  let rec go orphans file_descr =
    match accept_or_stop ?stop file_descr with
    | None -> Runtime.terminate orphans; Miou_unix.close file_descr
    | Some (fd', _sockaddr) ->
        clean_up orphans;
        let fn () =
          try
            let tls_flow = Tls_miou_unix.server_of_fd tls_config fd' in
            begin
              match (config, alpn tls_flow) with
              | `Both (_, h2), Some "h2" | `H2 h2, (Some "h2" | None) ->
                  Log.debug (fun m -> m "Start a h2 request handler");
                  h2s_server_connection ~config:h2 ~user's_error_handler
                    ?upgrade ~user's_handler tls_flow
              | `Both (config, _), Some "http/1.1"
              | `HTTP_1_1 config, (Some "http/1.1" | None) ->
                  Log.debug (fun m -> m "Start a http/1.1 request handler");
                  https_1_1_server_connection ~config ~user's_error_handler
                    ?upgrade ~user's_handler tls_flow
              | `Both _, None -> assert false
              | _, Some _protocol -> assert false
            end
          with exn ->
            Log.err (fun m ->
                m "got a TLS error during the handshake: %s"
                  (Printexc.to_string exn));
            Miou_unix.close fd'
        in
        call ~orphans fn; go orphans file_descr
  in
  let socket =
    match sockaddr with
    | Unix.ADDR_UNIX _ -> invalid_arg "Impossible to create a Unix socket"
    | Unix.ADDR_INET (inet_addr, _) ->
        if Unix.is_inet6_addr inet_addr then Miou_unix.tcpv6 ()
        else Miou_unix.tcpv4 ()
  in
  Miou_unix.bind_and_listen ?backlog socket sockaddr;
  Option.iter (fun c -> ignore (Miou.Computation.try_return c ())) ready;
  go (Miou.orphans ()) socket

module Websocket = struct
  module H1_ws_server_connection = struct
    (* make it match Runtime.S signature *)
    include H1_ws.Server_connection

    let next_read_operation t =
      (next_read_operation t :> [ `Read | `Close | `Upgrade | `Yield ])

    let next_write_operation t =
      (next_write_operation t
        :> [ `Write of Bigstringaf.t Faraday.iovec list
           | `Close of int
           | `Yield
           | `Upgrade ])

    let yield_reader _t _k = assert false

    let report_exn _t exn =
      (* TODO
       implement report_exn in H1_ws, just need to close wsd? *)
      Log.err (fun m -> m "websocket runtime: report_exn");
      raise exn
  end

  module D = Runtime.Make (TCP_and_H1) (H1_ws_server_connection)
  module E = Runtime.Make (Tls_miou_unix) (H1_ws_server_connection)

  type frame = H1_ws.Websocket.Opcode.t * bool * bytes

  let connection_close_frame = (`Connection_close, true, Bytes.empty)

  module Bounded_stream = struct
    module Bstream = struct
      type 'a t = {
          buffer: 'a array
        ; mutable rd_pos: int
        ; mutable wr_pos: int
        ; mutable closed: bool
        ; mutable halted: bool
        ; lock: Miou.Mutex.t
        ; non_empty: Miou.Condition.t
        ; non_full: Miou.Condition.t
      }

      let create size =
        let lock = Miou.Mutex.create () in
        let non_empty = Miou.Condition.create () in
        let non_full = Miou.Condition.create () in
        {
          buffer= Array.make size None
        ; lock
        ; rd_pos= 0
        ; wr_pos= 0
        ; closed= false
        ; halted= false
        ; non_empty
        ; non_full
        }

      let close t =
        Miou.Mutex.protect t.lock @@ fun () ->
        t.closed <- true;
        Miou.Condition.signal t.non_empty

      let halt t =
        Miou.Mutex.protect t.lock @@ fun () ->
        t.halted <- true;
        Miou.Condition.signal t.non_empty

      let put t data =
        Miou.Mutex.protect t.lock @@ fun () ->
        if not (t.closed || t.halted) then (
          while (t.wr_pos + 1) mod Array.length t.buffer = t.rd_pos do
            Miou.Condition.wait t.non_full t.lock
          done;
          if not (t.closed || t.halted) then (
            t.buffer.(t.wr_pos) <- Some data;
            t.wr_pos <- (t.wr_pos + 1) mod Array.length t.buffer;
            Miou.Condition.signal t.non_empty))

      let get t =
        Miou.Mutex.protect t.lock @@ fun () ->
        while t.wr_pos = t.rd_pos && not (t.closed || t.halted) do
          Miou.Condition.wait t.non_empty t.lock
        done;
        if t.halted || (t.closed && t.rd_pos = t.wr_pos) then None
        else
          let data = t.buffer.(t.rd_pos) in
          t.buffer.(t.rd_pos) <- None;
          t.rd_pos <- (t.rd_pos + 1) mod Array.length t.buffer;
          Miou.Condition.signal t.non_full;
          data
    end

    include Bstream

    type nonrec t = frame option t
    type ic = t
    type oc = t

    let to_ic : t -> ic = Fun.id
    let to_oc : t -> oc = Fun.id
    let create : unit -> t = fun () -> create 0x100
  end

  type handler = Bounded_stream.(ic) -> Bounded_stream.(oc) -> unit

  module Close_state = struct
    (* TODO do we need the lock? *)
    type t = {
        lock: Miou.Mutex.t
      ; cond: Miou.Condition.t
      ; mutable received: bool
      ; mutable emitted: bool
      ; mutable eof: bool
    }

    let create () =
      {
        lock= Miou.Mutex.create ()
      ; cond= Miou.Condition.create ()
      ; received= false
      ; emitted= false
      ; eof= false
      }

    let set_received t =
      Miou.Mutex.protect t.lock @@ fun () ->
      t.received <- true;
      Miou.Condition.signal t.cond

    let set_emmited t =
      Miou.Mutex.protect t.lock @@ fun () ->
      t.emitted <- true;
      Miou.Condition.signal t.cond

    let set_eof t =
      Miou.Mutex.protect t.lock @@ fun () ->
      t.eof <- true;
      Miou.Condition.signal t.cond

    let on_close t f =
      Miou.Mutex.protect t.lock @@ fun () ->
      while not ((t.received && t.emitted) || t.eof) do
        Miou.Condition.wait t.cond t.lock
      done;
      f t.received t.emitted
  end

  let websocket_handler comp user_handler's wsd =
    let open H1_ws in
    Log.debug (fun m -> m "Websocket.handler");
    let ic = Bounded_stream.create () in
    let oc = Bounded_stream.create () in
    let close_state = Close_state.create () in
    let input_handlers =
      let frame_handler ~opcode ~is_fin bstr ~off ~len =
        Log.debug (fun m ->
            m "Websocket frame: (%a, is_fin=%b)" Websocket.Opcode.pp_hum opcode
              is_fin);
        let data =
          let s = Bigstringaf.substring bstr ~off ~len in
          String.to_bytes s
        in
        Bounded_stream.put ic (opcode, is_fin, data);
        match opcode with
        | `Connection_close ->
            Bounded_stream.close ic;
            Close_state.set_received close_state;
            ()
        | _ -> ()
      in
      let eof () =
        Log.debug (fun m -> m "Websocket frame: EOF");
        Close_state.set_eof close_state;
        ()
      in
      Websocket.{ frame_handler; eof }
    in
    let rec write () =
      match Bounded_stream.get oc with
      | None ->
          Log.debug (fun m -> m "Websocket write loop stoped");
          ()
      | Some (opcode, is_fin, data) ->
          Log.debug (fun m ->
              m "Websocket write loop: (opcode: %a, is_fin: %b)"
                Websocket.Opcode.pp_hum opcode is_fin);
          begin
            match opcode with
            | `Other _i -> failwith "unsuported frame of kind `Other"
            | `Ping -> Wsd.send_ping wsd
            | `Pong -> Wsd.send_pong wsd
            | (`Continuation | `Text | `Binary) as kind ->
                let len = Bytes.length data in
                Wsd.send_bytes wsd ~kind ~is_fin data ~off:0 ~len;
                ()
            | `Connection_close ->
                (* TODO
                   Wsd.close write a final `Connection_close frame and close the conn
                   to have the server be able to start a close handcheck
                   we need a `Wsd.send_close` that write a close frame without actually closing the conn *)
                Bounded_stream.close oc;
                Close_state.set_emmited close_state;
                ()
          end;
          write ()
    in
    let close () =
      Close_state.on_close close_state (fun received emitted ->
          Wsd.close wsd;
          if received && emitted then (
            Log.debug (fun m -> m "websocket clean close");
            ())
          else (
            Log.debug (fun m -> m "websocket unclean close");
            Bounded_stream.halt ic;
            Bounded_stream.halt oc;
            ()))
    in
    let user's_handler =
      let ic = Bounded_stream.to_ic ic in
      let oc = Bounded_stream.to_oc oc in
      fun () -> user_handler's ic oc
    in
    let tasks = [ write; close; user's_handler ] in
    ignore (Miou.Computation.try_return comp tasks);
    input_handlers

  let upgrade ~handler:user's_handler flow =
    let comp = Miou.Computation.create () in
    let websocket_handler wsd = websocket_handler comp user's_handler wsd in
    let conn = H1_ws.Server_connection.create ~websocket_handler in
    let run_prm =
      match flow with
      | `Tcp flow -> D.run conn flow
      | `Tls flow -> E.run conn flow
    in
    let tasks = Miou.Computation.await_exn comp in
    let prms = run_prm :: List.map (fun task -> Miou.async task) tasks in
    Miou.await_all prms
    |> List.iter (function Error exn -> Miou.reraise exn | Ok () -> ());
    ()
end
