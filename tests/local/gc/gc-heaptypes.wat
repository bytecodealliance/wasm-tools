;; --enable-gc

(module
  (func (param funcref (ref func) (ref null func)))
  (func (param externref (ref extern) (ref null extern)))
  (func (param anyref (ref any) (ref null any)))
  (func (param eqref (ref eq) (ref null eq)))
  (func (param i31ref (ref i31) (ref null i31)))
  (func (param dataref (ref data) (ref null data)))
  (func (param arrayref (ref array) (ref null array)))
)
