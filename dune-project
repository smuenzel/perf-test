(lang dune 3.7)

(name perf_test)

(generate_opam_files true)

(source
 (github smuenzel/perf-test))

(authors "Author Name")

(maintainers "Maintainer Name")

(license MPL-2.0)

(documentation https://url/to/documentation)

(package
 (name perf_test)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (allow_empty true)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
