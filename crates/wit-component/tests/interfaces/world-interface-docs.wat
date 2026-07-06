(component
  (type (;0;)
    (component
      (type (;0;)
        (instance
          (type (;0;) (func))
          (export (;0;) "f" (func (type 0)))
        )
      )
      (export (;0;) "foo:foo/imported" (instance (type 0)))
    )
  )
  (export (;1;) "imported" (type 0))
  (type (;2;)
    (component
      (type (;0;)
        (instance
          (type (;0;) (func))
          (export (;0;) "g" (func (type 0)))
        )
      )
      (export (;0;) "foo:foo/exported" (instance (type 0)))
    )
  )
  (export (;3;) "exported" (type 2))
  (type (;4;)
    (component
      (type (;0;)
        (component
          (type (;0;)
            (instance
              (type (;0;) (func))
              (export (;0;) "f" (func (type 0)))
            )
          )
          (import "foo:foo/imported" (instance (;0;) (type 0)))
          (type (;1;)
            (instance
              (type (;0;) (func))
              (export (;0;) "g" (func (type 0)))
            )
          )
          (export (;1;) "foo:foo/exported" (instance (type 1)))
        )
      )
      (export (;0;) "foo:foo/the-world" (component (type 0)))
    )
  )
  (export (;5;) "the-world" (type 4))
  (@custom "package-docs" "\01{\22docs\22:\22doc comments on by-reference interface imports/exports\5cninside a world (attached to the `import`/`export` statement itself, as\5cnopposed to the interface's own definition) must survive the binary WIT\5cnpackage round-trip via the `package-docs` custom section.\22,\22worlds\22:{\22the-world\22:{\22interface_import_docs\22:{\22foo:foo/imported\22:\22docs on the by-reference interface import statement\22},\22interface_export_docs\22:{\22foo:foo/exported\22:\22docs on the by-reference interface export statement\22}}},\22interfaces\22:{\22imported\22:{\22docs\22:\22the imported interface's own definition docs\22},\22exported\22:{\22docs\22:\22the exported interface's own definition docs\22}}}")
  (@producers
    (processed-by "wit-component" "$CARGO_PKG_VERSION")
  )
)
