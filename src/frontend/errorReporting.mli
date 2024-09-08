
open LoggingUtil
open ConfigError

val make_config_error_message : logging_spec -> config_error -> string

val error_log_environment : logging_spec -> (unit -> ('a, config_error) result) -> ('a, config_error) result

val report_and_exit : string -> unit
