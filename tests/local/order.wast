(assert_malformed
  (module quote
    "(func)"
    "(func (import \"\" \"\"))")
  "import after function")

(assert_malformed
  (module quote
    "(func)"
    "(elem func)"
    "(func (import \"\" \"\"))")
  "import after function")
