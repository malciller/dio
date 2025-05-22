(lang dune 3.13)
(name dio)
(generate_opam_files true)

(license MIT)
(authors "Cal Miller")
(maintainers "malciller@gmail.com")
(source (github malciller/dio))

(package
 (name dio)
 (synopsis "Dio – trading engine")
 (depends
  ;; TLS back-end first, then conduit
  lwt_ssl
  tls-lwt
  conduit-lwt-unix

  dune
  decimal
  lwt lwt_ppx
  lwt_log
  yojson ppx_deriving_yojson
  sqlite3
  websocket-lwt-unix cohttp-lwt-unix
  alcotest alcotest-lwt
  dotenv
  cstruct
  notty))
