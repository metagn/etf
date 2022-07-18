# etf

ETF (Erlang Term Format) library for Nim. 

```nim
import etf

block:
  let t = term({
    binary("op"): term(2u8),
    binary("d"): term({
      binary("token"): binary("abc"),
      binary("large_threshold"): term(250i32),
      binary("properties"): term({
        binary("$os"): binary(hostOS),
        binary("$browser"): binary("Nim"),
        binary("$device"): binary("Nim")
      })
    })
  })

  assert parseEtf(toEtf(t)) == t
```
