;; RUN: wast \
;;      --assert default \
;;      --snapshot tests/snapshots \
;;      --ignore-error-messages \
;;      --features=wasm3,component-model,cm-map,cm-async,cm-implements,cm-async-stackful,cm-threading,cm-more-async-builtins,cm-fixed-length-lists \
;;      tests/component-model/test/async/cancellable.wast
