;; --enable-gc

(module
  (func (param funcref (ref func) (ref null func)))
  (func (param externref (ref extern) (ref null extern)))
  (func (param anyref (ref any) (ref null any)))
  (func (param eqref (ref eq) (ref null eq)))
  (func (param i31ref (ref i31) (ref null i31)))
  (func (param structref (ref struct) (ref null struct)))
  (func (param arrayref (ref array) (ref null array)))
  (func (param nullfuncref (ref nofunc) (ref null nofunc)))
  (func (param nullexternref (ref noextern) (ref null noextern)))
  (func (param nullref (ref none) (ref null none)))
)
