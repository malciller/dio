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
  dune
  decimal
  lwt lwt_ppx
  yojson ppx_deriving_yojson
  caqti caqti-driver-sqlite3
  websocket-lwt cohttp-lwt-unix
  cmdliner
  prometheus
  alcotest))
