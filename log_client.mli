type t

val create : unit -> t
(** [create unit] creates a new log_client *)

val start : t -> string -> int -> unit
(** [start log_client pid start_time] marks process [pid] as started at [start_time] *)

val stop : t -> string -> unit
(** [stop log_client pid] marks process [pid] as ended *)

val poll : t -> string option
(** [poll log_client] returns the first completed process sorted by start time *)
