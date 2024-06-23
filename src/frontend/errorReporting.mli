
open ConfigError


val make_config_error_message : Logging.config -> config_error -> string


val error_log_environment : Logging.config -> (unit -> ('a, config_error) result) -> ('a, config_error) result


val report_and_exit : string -> unit
