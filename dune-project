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

  ;; rest of your deps
  dune
  decimal
  lwt lwt_ppx
  yojson ppx_deriving_yojson
  caqti caqti-driver-sqlite3
  websocket-lwt-unix cohttp-lwt-unix
  cmdliner
  prometheus
  alcotest alcotest-lwt
  dotenv
  cstruct))
