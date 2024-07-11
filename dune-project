(lang dune 3.15)

(using menhir 3.0)

(name mazeppa)

(version 0.1.0)

(generate_opam_files true)

(source
 (github Hirrolot/mazeppa))

(authors "Tima Kinsart")

(maintainers "Tima Kinsart")

(license MIT)

(documentation https://github.com/Hirrolot/mazeppa)

(package
 (name mazeppa)
 (synopsis "A modern supercompiler for call-by-value functional languages")
 (depends
  ocaml
  ocamlformat
  dune
  pprint
  (checked_oint
   (= "0.2.0"))
  ppx_deriving
  ppx_string_interpolation
  ppx_yojson_conv
  bisect_ppx
  menhir
  (spectrum
   (= "0.6.0"))
  clap
  alcotest))
