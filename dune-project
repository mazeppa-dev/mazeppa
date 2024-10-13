(lang dune 3.15)

(using menhir 3.0)

(name mazeppa)

(version 0.3.4)

(generate_opam_files true)

(source
 (github mazeppa-dev/mazeppa))

(authors "hirrolot <hirrolot@gmail.com>")

(maintainers "hirrolot <hirrolot@gmail.com>")

(license MIT)

(documentation https://github.com/mazeppa-dev/mazeppa)

(package
 (name mazeppa)
 (synopsis "A modern supercompiler for call-by-value functional languages")
 (depends
  (ocaml
   (>= 5.1))
  dune
  pprint
  (checked_oint
   (>= 0.2.0))
  ppx_deriving
  ppx_string_interpolation
  ppx_yojson_conv
  (ppx_blob
   (>= 0.9.0))
  bisect_ppx
  menhir
  (spectrum
   (>= 0.6.0))
  clap
  (alcotest
   (>= 1.7.0))))
