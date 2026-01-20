;; RUN: wast --assert default --snapshot tests/snapshots % -f cm-async,cm-fixed-length-lists

;; async lower
(component
  (core module $libc (memory (export "memory") 1))
  (core instance $libc (instantiate $libc))

  ;; func()
  (import "f1" (func $f1))
  (core func $f1 (canon lower (func $f1) async))
  (core module $m1 (func (import "" "f") (result i32)))
  (core instance (instantiate $m1 (with "" (instance (export "f" (func $f1))))))

  ;; func(x: u32)
  (import "f2" (func $f2 (param "x" u32)))
  (core func $f2 (canon lower (func $f2) async))
  (core module $m2 (func (import "" "f") (param i32) (result i32)))
  (core instance (instantiate $m2 (with "" (instance (export "f" (func $f2))))))

  ;; func() -> u32
  (import "f3" (func $f3 (result u32)))
  (core func $f3 (canon lower (func $f3) async (memory $libc "memory")))
  (core module $m3 (func (import "" "f") (param i32) (result i32)))
  (core instance (instantiate $m3 (with "" (instance (export "f" (func $f3))))))

  ;; func(x: u32, y: f32, z: string)
  (import "f4" (func $f4 (param "x" u32) (param "y" f32) (param "z" string)))
  (core func $f4 (canon lower (func $f4) async (memory $libc "memory")))
  (core module $m4 (func (import "" "f") (param i32 f32 i32 i32) (result i32)))
  (core instance (instantiate $m4 (with "" (instance (export "f" (func $f4))))))

  ;; func() -> f32
  (import "f5" (func $f5 (result f32)))
  (core func $f5 (canon lower (func $f5) async (memory $libc "memory")))
  (core module $m5 (func (import "" "f") (param i32) (result i32)))
  (core instance (instantiate $m5 (with "" (instance (export "f" (func $f5))))))

  ;; func(x: list<string; 4>) -> f32
  (import "f6" (func $f6 (param "x" (list string 6)) (result f32)))
  (core func $f6 (canon lower (func $f6) async (memory $libc "memory")))
  (core module $m6 (func (import "" "f") (param i32 i32) (result i32)))
  (core instance (instantiate $m6 (with "" (instance (export "f" (func $f6))))))
)

;; async lift, callback abi
(component
  ;; func()
  (core module $m1
    (func (export "cb") (param i32 i32 i32) (result i32) unreachable)
    (func (export "f") (result i32) unreachable))
  (core instance $m1 (instantiate $m1))
  (func
    (canon lift (core func $m1 "f") async (callback (func $m1 "cb"))))

  ;; func(x: u32)
  (core module $m2
    (func (export "cb") (param i32 i32 i32) (result i32) unreachable)
    (func (export "f") (param i32) (result i32) unreachable))
  (core instance $m2 (instantiate $m2))
  (func (param "x" u32)
    (canon lift (core func $m2 "f") async (callback (func $m2 "cb"))))

  ;; func() -> u32
  (core module $m3
    (func (export "cb") (param i32 i32 i32) (result i32) unreachable)
    (func (export "f") (result i32) unreachable))
  (core instance $m3 (instantiate $m3))
  (func (result u32)
    (canon lift (core func $m3 "f") async (callback (func $m3 "cb"))))

  ;; func(x: f32)
  (core module $m4
    (func (export "cb") (param i32 i32 i32) (result i32) unreachable)
    (func (export "f") (param f32) (result i32) unreachable))
  (core instance $m4 (instantiate $m4))
  (func (param "x" f32)
    (canon lift (core func $m4 "f") async (callback (func $m4 "cb"))))

  ;; func(x: f32, y: string)
  (core module $m5
    (memory (export "memory") 1)
    (func (export "cb") (param i32 i32 i32) (result i32) unreachable)
    (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
    (func (export "f") (param f32 i32 i32) (result i32) unreachable))
  (core instance $m5 (instantiate $m5))
  (func (param "x" f32) (param "y" string)
    (canon lift (core func $m5 "f") async (callback (func $m5 "cb"))
      (memory $m5 "memory") (realloc (func $m5 "realloc"))))

  ;; func(x: list<string; 4>)
  (core module $m6
    (memory (export "memory") 1)
    (func (export "cb") (param i32 i32 i32) (result i32) unreachable)
    (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
    (func (export "f") (param i32 i32 i32 i32 i32 i32 i32 i32) (result i32) unreachable))
  (core instance $m6 (instantiate $m6))
  (func (param "x" (list string 4))
    (canon lift (core func $m6 "f") async (callback (func $m6 "cb"))
      (memory $m6 "memory") (realloc (func $m6 "realloc"))))

  ;; func(x: list<string; 10>)
  (core module $m7
    (memory (export "memory") 1)
    (func (export "cb") (param i32 i32 i32) (result i32) unreachable)
    (func (export "realloc") (param i32 i32 i32 i32) (result i32) unreachable)
    (func (export "f") (param i32) (result i32) unreachable))
  (core instance $m7 (instantiate $m7))
  (func (param "x" (list string 10))
    (canon lift (core func $m7 "f") async (callback (func $m7 "cb"))
      (memory $m7 "memory") (realloc (func $m7 "realloc"))))
)
