;; RUN: wast --assert default --snapshot tests/snapshots %

(module
       (type $t  (sub    (struct (field anyref))))
  (rec (type $r  (sub $t (struct (field (ref $r))))))
       (type $t' (sub $r (struct (field (ref $r) i32))))
)
