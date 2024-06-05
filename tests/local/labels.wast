(module
  (func
    br 0
    try_table $a
      br $a
      br 1
      loop $b
        br $a
        br $b
        br 2
        block $c
          br $a
          br $b
          br $c
          br 3
          if $d
            br $a
            br $b
            br $c
            br $d
            br 4
          else
            br $a
            br $b
            br $c
            br $d
            br 4
          end
        end
      end
    end
  )
)
