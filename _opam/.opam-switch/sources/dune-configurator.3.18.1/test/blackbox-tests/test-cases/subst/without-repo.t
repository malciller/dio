Demonstrate $ dune subst without a git repository

  $ cat > dune-project << EOF
  > (lang dune 3.17)
  > (name test)
  > EOF

  $ echo "dune" > README.md

  $ dune subst

  $ cat README.md
  test
