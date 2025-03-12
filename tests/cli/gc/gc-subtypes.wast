;; RUN: wast --assert default --snapshot tests/snapshots %

;; --enable-gc

(module
  ;; nullability matching for field types
  (type $non_null (sub (array (ref null any))))
  (type $nullable (sub $non_null (array (ref any))))

  ;; struct width and depth subtyping
  (type $struct_base  (sub               (struct)))
  (type $struct_width (sub $struct_base  (struct (field (ref null $struct_base)))))
  (type $struct_depth (sub $struct_width (struct (field (ref $struct_base)))))

  ;; function param and result subtyping
  (type $func_base (sub (func (param eqref) (result eqref))))
  (type $func_contravariant_params (sub $func_base (func (param anyref) (result eqref))))
  (type $func_covariant_results (sub $func_base (func (param eqref) (result nullref))))

  ;; any refs
  (type $any (sub (array anyref)))
  (type $any_2 (sub $any (array anyref)))
  (type $any_mut (sub (array (mut anyref))))
  (type $any_mut_2 (sub $any_mut (array (mut anyref))))

  ;; eq refs
  (type $eq (sub $any (array eqref)))
  (type $eq_2 (sub $eq (array eqref)))
  (type $eq_mut (sub (array (mut eqref))))
  (type $eq_mut_2 (sub $eq_mut (array (mut eqref))))

  ;; i31 refs
  (type $i31 (sub $eq (array i31ref)))
  (type $i31_2 (sub $i31 (array i31ref)))
  (type $i31_mut (sub (array (mut i31ref))))
  (type $i31_mut_2 (sub $i31_mut (array (mut i31ref))))

  ;; array refs
  (type $array (sub $eq (array arrayref)))
  (type $array_2 (sub $array (array arrayref)))
  (type $array_mut (sub (array (mut arrayref))))
  (type $array_mut_2 (sub $array_mut (array (mut arrayref))))

  ;; concrete array refs
  (type $concrete_array (sub $array (array (ref null $array))))
  (type $concrete_array_2 (sub $concrete_array (array (ref null $array))))
  (type $concrete_array_mut (sub (array (ref null $array_mut))))
  (type $concrete_array_mut_2 (sub $concrete_array_mut (array (ref null $array_mut))))

  ;; struct refs
  (type $struct (sub $eq (array structref)))
  (type $struct_2 (sub $struct (array structref)))
  (type $struct_mut (sub (array (mut structref))))
  (type $struct_mut_2 (sub $struct_mut (array (mut structref))))

  ;; concrete struct refs
  (type $my_struct (struct))
  (type $concrete_struct (sub $struct (array (ref null $my_struct))))
  (type $concrete_struct_2 (sub $concrete_struct (array (ref null $my_struct))))
  (type $concrete_struct_mut (sub (array (mut (ref null $my_struct)))))
  (type $concrete_struct_mut_2 (sub $concrete_struct_mut (array (mut (ref null $my_struct)))))

  ;; none refs
  (type $none (sub (array nullref)))
  (type $none_2 (sub $none (array nullref)))
  (type $none_any (sub $any (array nullref)))
  (type $none_any_2 (sub $none_any (array nullref)))
  (type $none_eq (sub $eq (array nullref)))
  (type $none_eq_2 (sub $none_eq (array nullref)))
  (type $none_i31 (sub $i31 (array nullref)))
  (type $none_i31_2 (sub $none_i31 (array nullref)))
  (type $none_array (sub $array (array nullref)))
  (type $none_array_2 (sub $none_array (array nullref)))
  (type $none_concrete_array (sub $concrete_array (array nullref)))
  (type $none_concrete_array_2 (sub $none_concrete_array (array nullref)))
  (type $none_struct (sub $struct (array nullref)))
  (type $none_struct_2 (sub $none_struct (array nullref)))
  (type $none_concrete_struct (sub $concrete_struct (array nullref)))
  (type $none_concrete_struct_2 (sub $none_concrete_struct (array nullref)))
  (type $none_mut (sub (array (mut nullref))))
  (type $none_mut_2 (sub $none_mut (array (mut nullref))))

  ;; func refs
  (type $func (sub (array funcref)))
  (type $func_2 (sub $func (array funcref)))
  (type $func_mut (sub (array (mut funcref))))
  (type $func_mut_2 (sub $func_mut (array (mut funcref))))

  ;; concrete func refs
  (type $my_func (func))
  (type $concrete_func (sub $func (array (ref null $my_func))))
  (type $concrete_func_2 (sub $concrete_func (array (ref null $my_func))))
  (type $concrete_func_mut (sub (array (mut (ref null $my_func)))))
  (type $concrete_func_mut_2 (sub $concrete_func_mut (array (mut (ref null $my_func)))))

  ;; nofunc
  (type $nofunc (sub $concrete_func (array nullfuncref)))
  (type $nofunc_2 (sub $nofunc (array nullfuncref)))
  (type $nofunc_mut (sub (array (mut nullfuncref))))
  (type $nofunc_mut_2 (sub $nofunc_mut (array (mut nullfuncref))))

  ;; extern refs
  (type $extern (sub (array externref)))
  (type $extern_2 (sub $extern (array externref)))
  (type $extern_mut (sub (array (mut externref))))
  (type $extern_mut_2 (sub $extern_mut (array (mut externref))))

  ;; noextern
  (type $noextern (sub $extern (array nullexternref)))
  (type $noextern_2 (sub $noextern (array nullexternref)))
  (type $noextern_mut (sub (array (mut nullexternref))))
  (type $noextern_mut_2 (sub $noextern_mut (array (mut nullexternref))))

  ;; field names
  (type $struct_with_named_field (sub (struct (field $field (mut i32)))))
  ;; same field, same name
  (type (sub $struct_with_named_field (struct (field $field (mut i32)))))
  ;; same field, different name
  (type (sub $struct_with_named_field (struct (field $different (mut i32)))))
  ;; different field, same name
  (type (sub $struct_with_named_field (struct (field (mut i32)) (field $field (mut i64)))))
)
