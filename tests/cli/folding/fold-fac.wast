;; RUN: wast --assert default,snapshot-folded --snapshot tests/snapshots %

;;; WABT fold-fac.wat test (Copyright 2016- WebAssembly Community Group participants)
(module
  (func $fac-stack-raw (param $n i64) (result i64)
    (local $i i64)
    (local $res i64)
    local.get $n
    local.set $i
    i64.const 1
    local.set $res
    block $done
      loop $loop
        local.get $i
        i64.const 0
        i64.eq
        if $body
          br $done
        else $body
          local.get $i
          local.get $res
          i64.mul
          local.set $res
          local.get $i
          i64.const 1
          i64.sub
          local.set $i
        end $body
        br $loop
      end $loop
    end $done
    local.get $res))
