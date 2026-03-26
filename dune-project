(lang dune 3.21)

(name aiyu_cbor)

(package
 (name aiyu_cbor)
 (depends
  ocaml
  (ppx_deriving :with-test)
  (alcotest :with-test)
  (hex :with-test)))
