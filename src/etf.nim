type Tag* = enum
  ## ETF tag numbers
  tagNone = 0
  tagFloat64 = 70
  tagBitBinary = 77
  tagAtomCacheRef = 82
  tagUint8 = 97
  tagInt32 = 98
  tagFloatString = 99
  tagAtom = 100
  tagReference = 101
  tagPort = 102
  tagPid = 103
  tagSmallTuple = 104
  tagLargeTuple = 105
  tagNil = 106
  tagString = 107
  tagList = 108
  tagBinary = 109
  tagSmallBigInt = 110
  tagLargeBigInt = 111
  tagNewReference = 114
  tagSmallAtom = 115
  tagMap = 116
  tagAtomUtf8 = 118
  tagSmallAtomUtf8 = 119
  
const atomTags* = {tagAtomCacheRef, tagAtom, tagSmallAtom, tagAtomUtf8, tagSmallAtomUtf8}

type
  BigInt* = object
    negative*: bool
    data*: seq[byte]

  BitBinary* = object
    bits*: byte
    data*: seq[byte]

  Atom* = distinct string

  Reference* = object
    node*: Atom
    id*: uint32
    creation*: byte

  NewReference* = object
    node*: Atom
    ids*: seq[uint32]
    creation*: byte

  Term* {.acyclic.} = object
    case tag*: Tag
    of tagNone: discard
    of tagFloat64:
      f64*: float64
    of tagBitBinary:
      bb*: BitBinary
    of tagUint8:
      u8*: byte
    of tagInt32:
      i32*: int32
    of tagFloatString:
      flstr*: string
    of tagAtomCacheRef, tagAtom, tagSmallAtom, tagAtomUtf8, tagSmallAtomUtf8:
      atom*: Atom
    of tagReference:
      reference*: Reference
    of tagPort:
      port*: Reference
    of tagPid:
      pid*: Reference
      serial*: uint32
    of tagSmallTuple, tagLargeTuple:
      tup*: seq[Term]
    of tagMap:
      map*: seq[(Term, Term)]
    of tagNil:
      nil
    of tagString:
      str*: string
    of tagBinary:
      bin*: string
    of tagSmallBigInt, tagLargeBigInt:
      bigint*: BigInt
    of tagList:
      lst*: seq[Term]
    of tagNewReference:
      newRef*: NewReference

proc term*(x: float64): Term = Term(tag: tagFloat64, f64: x)
proc term*(x: byte): Term = Term(tag: tagUint8, u8: x)
proc term*(x: int32): Term = Term(tag: tagInt32, i32: x)
proc binary*(x: string): Term = Term(tag: tagBinary, bin: x)
proc term*(x: sink openarray[(Term, Term)]): Term = Term(tag: tagMap, map: @x)
proc term*(x: typeof(nil)): Term = Term(tag: tagNil) 
proc term*(x: sink openarray[Term]): Term = Term(tag: tagList, lst: @x)

proc `==`*(a, b: Atom): bool {.borrow.}

proc `==`*(a, b: Term): bool =
  if a.tag != b.tag: return false
  case a.tag
  of tagFloat64:
    a.f64 == b.f64
  of tagBitBinary:
    a.bb == b.bb
  of tagUint8:
    a.u8 == b.u8
  of tagInt32:
    a.i32 == b.i32
  of tagFloatString:
    a.flstr == b.flstr
  of tagAtomCacheRef, tagAtom, tagSmallAtom, tagAtomUtf8, tagSmallAtomUtf8:
    a.atom == b.atom
  of tagReference:
    a.reference == b.reference
  of tagPort:
    a.port == b.port
  of tagPid:
    a.pid == b.pid and a.serial == b.serial
  of tagSmallTuple, tagLargeTuple:
    a.tup == b.tup
  of tagMap:
    a.map == b.map
  of tagNil:
    true
  of tagString:
    a.str == b.str
  of tagBinary:
    a.bin == b.bin
  of tagSmallBigInt, tagLargeBigInt:
    a.bigint == b.bigint
  of tagList:
    a.lst == b.lst
  of tagNewReference:
    a.newRef == b.newRef
  else: false

proc `$`*(term: Term): string =
  var s: string
  case term.tag
  of tagFloat64:
    s = $term.f64
  of tagBitBinary:
    s = $term.bb
  of tagUint8:
    s = $term.u8
  of tagInt32:
    s = $term.i32
  of tagFloatString:
    s.addQuoted(term.flstr)
  of tagAtomCacheRef, tagAtom, tagSmallAtom, tagAtomUtf8, tagSmallAtomUtf8:
    s.addQuoted(term.atom.string)
  of tagReference:
    s = $term.reference
  of tagPort:
    s = $term.port
  of tagPid:
    s = "(serial: " & $term.serial & ", ref: " & $term.pid & ")" 
  of tagSmallTuple, tagLargeTuple:
    for i in 0 ..< term.tup.len:
      if i > 0:
        s.add(", ")
      s.add($term.tup[i])
  of tagMap:
    for i in 0 ..< term.map.len:
      if i > 0:
        s.add(", ")
      let (key, value) = term.map[i]
      s.add($key)
      s.add(": ")
      s.add($value)
  of tagNil: discard
  of tagString:
    s.addQuoted(term.str)
  of tagBinary:
    s.addQuoted(term.bin)
  of tagSmallBigInt, tagLargeBigInt:
    if term.bigint.negative:
      s.add('-')
    for i in 0..term.bigint.data.high:
      if i > 0:
        s.add(", ")
      s.add($term.bigint.data[i])
  of tagList:
    for i in 0 ..< term.lst.len:
      if i > 0:
        s.add(", ")
      s.add($term.lst[i])
  of tagNewReference:
    s = $term.newRef
  else: discard
  result = ($term.tag)[3..^1]
  if s.len != 0:
    result.add('(' & s & ')')

proc getStr*(term: Term): string =
  ## tries to get a string field out of `term`
  case term.tag
  of tagString:
    term.str
  of tagBinary:
    term.bin
  of atomTags:
    term.atom.string
  elif term.tag == tagSmallBigInt and term.bigint.data.len == 8:
    when defined(js):
      let d = term.bigint.data
      var cs: cstring
      {.emit: "`cs` = new DataView(new Uint8Array(`d`).buffer).getBigInt64().toString();".}
      $cs
    else:
      var u: uint64
      copyMem(addr u, unsafeAddr term.bigint.data[0], 8)
      $u
  else:
    raise newException(ValueError, "cannot getStr of term " & $term)

proc getInt*(term: Term): int =
  ## tries to get an integer field out of `term`
  case term.tag
  of tagUint8:
    term.u8.int
  of tagInt32:
    term.i32.int
  else:
    raise newException(ValueError, "cannot getInt of term " & $term)

proc getFloat*(term: Term): float =
  ## tries to get a float field out of `term`
  case term.tag
  of tagFloat64:
    term.f64.float
  else:
    raise newException(ValueError, "cannot getFloat of term " & $term)

proc `[]`*(term: Term, str: string): Term =
  ## tries to get the value of string key `str` out of a map `term`
  case term.tag
  of tagMap:
    for k, v in term.map.items:
      if k.tag in (atomTags + {tagString, tagBinary}) and k.getStr == str:
        return v
    raise newException(KeyError, "no key " & str & " in " & $term)
  else:
    raise newException(ValueError, "cannot access term " & $term & " with key " & str)

proc `[]`*(term: Term, i: int): Term =
  ## tries to get the value of index `i` out of a list or tuple `term`
  case term.tag
  of tagList:
    term.lst[i]
  of tagSmallTuple, tagLargeTuple:
    term.tup[i]
  else:
    raise newException(ValueError, "cannot access term " & $term & " with index " & $i)

type Header* = ref object
  atomCache*: seq[(byte, string)]
  longAtoms*: bool

proc parseSingleTerm*(data: openarray[char], index: var int, header: Header = nil): Term =
  ## parses a single term without checking for the `131` tag start or a header

  template next: byte =
    let last = data[index].byte
    inc index
    last

  template readU16(): uint16 =
    let a = next.uint16
    let b = next.uint16
    (a shl 8) or b

  template readU32(): uint32 =
    let a = next.uint32
    let b = next.uint32
    let c = next.uint32
    let d = next.uint32
    (a shl 24) or (b shl 16) or (c shl 8) or d

  template readI32(): int32 =
    let a = next.int32
    let b = next.int32
    let c = next.int32
    let d = next.int32
    (a shl 24) or (b shl 16) or (c shl 8) or d

  template readU64(): uint16 =
    let a = next.uint16
    let b = next.uint16
    let c = next.uint16
    let d = next.uint16
    let e = next.uint16
    let f = next.uint16
    let g = next.uint16
    let h = next.uint16
    (a shl 56) or (b shl 48) or (c shl 40) or (d shl 32) or (e shl 24) or (f shl 16) or (g shl 8) or h
  
  template getTerm(): Term = parseSingleTerm(data, index, header)

  # errors later for invalid tag
  when (NimMajor, NimMinor) >= (1, 6):
    {.warning[HoleEnumConv]: off.}:
      result = Term(tag: Tag(next))
  else:
    result = Term(tag: Tag(next))

  case result.tag
  of tagAtomCacheRef:
    result.atom = header.atomCache[next.int][1].Atom
  of tagUint8:
    result.u8 = next
  of tagInt32:
    result.i32 = readI32()
  of tagFloatString:
    result.flstr = newString(31)
    for m in result.flstr.mitems:
      m = next.char
  of tagReference:
    result.reference.node = getTerm().atom
    result.reference.id = readU32()
    result.reference.creation = next
  of tagPort:
    result.port.node = getTerm().atom
    result.port.id = readU32()
    result.port.creation = next
  of tagPid:
    result.pid.node = getTerm().atom
    result.pid.id = readU32()
    result.serial = readU32()
    result.pid.creation = next
  of tagSmallTuple:
    result.tup.newSeq(next.int)
    for m in result.tup.mitems:
      m = getTerm()
  of tagLargeTuple:
    result.tup.newSeq(readU32().int)
    for m in result.tup.mitems:
      m = getTerm()
  of tagMap:
    result.map.newSeq(readU32().int)
    for m in result.map.mitems:
      m = (getTerm(), getTerm())
  of tagNil:
    discard
  of tagString:
    result.str = newString(readU16().int)
    for m in result.str.mitems:
      m = next.char
  of tagList:
    result.lst.newSeq(readU32().int)
    for m in result.lst.mitems:
      m = getTerm()
    let tail = getTerm()
    if tail.tag != tagNil:
      result.lst.add(tail)
  of tagBinary:
    result.bin = newString(readU32().int)
    for m in result.bin.mitems:
      m = next.char
  of tagSmallBigInt:
    result.bigInt.data.newSeq(next.int)
    result.bigInt.negative = next.bool
    for m in result.bigInt.data.mitems:
      m = next
  of tagLargeBigInt:
    result.bigInt.data.newSeq(readU32().int)
    result.bigInt.negative = next.bool
    for m in result.bigInt.data.mitems:
      m = next
  of tagNewReference:
    result.newRef.ids.newSeq(readU16().int)
    result.newRef.node = getTerm().atom
    result.newRef.creation = next
    for m in result.newRef.ids.mitems:
      m = readU32()
  of tagBitBinary:
    result.bb.data.newSeq(readU32().int)
    result.bb.bits = next
    for m in result.bb.data.mitems:
      m = next
  of tagFloat64:
    var p = readU64()
    result.f64 = cast[ptr float64](addr p)[]
  of tagAtomUtf8, tagAtom:
    result.atom = newString(readU16().int).Atom
    for m in result.atom.string.mitems:
      m = next.char
  of tagSmallAtomUtf8, tagSmallAtom:
    result.atom = newString(next.int).Atom
    for m in result.atom.string.mitems:
      m = next.char
  else:
    raise newException(ValueError, "invalid tag number " & $result.tag.int)

proc parseEtf*(data: openarray[char], compressed = false): Term =
  ## parses full ETF string. requires `131` tag start and supports optional header.
  ## 
  ## compressed ETF is currently not supported

  if compressed:
    raise newException(ValueError, "compressed ETF is currently unsupported")

  if data.len == 0:
    return

  var
    index = 0
    last: byte
    header: Header
  
  template next: byte =
    last = data[index].byte
    inc index
    last
  
  template back =
    dec index

  if next != 131:
    raise newException(ValueError, "did not expect at start of term char: " & last.char)

  block parseHeader:
    if next != 68:
      back
      break parseHeader

    header = Header()

    let numAtomCacheRefs = next
    if numAtomCacheRefs == 0:
      break parseHeader

    var flags: seq[byte]
    flags.newSeq(numAtomCacheRefs div 2 + 1)
    template flag(ni: int): byte =
      flags[ni div 2] and (0b1111u8 shl ((1u8 - byte(ni mod 2)) * 4).byte)
    
    for m in flags.mitems:
      m = next

    header.atomCache.newSeq(numAtomCacheRefs)
    for ni in 0 ..< header.atomCache.len:
      header.atomCache[ni] = ((flag(ni), ""))
    header.longAtoms = bool(flag(numAtomCacheRefs.int) and 0b0001)

    for ni in 0 ..< header.atomCache.len:
      let m = header.atomCache[ni][0]
      let n = next
      assert n.int == (ni and 0b0111)
      if (m and 0b1000) != 0:
        var length = next.uint16
        if header.longAtoms:
          length = (length shl 8) and next
        header.atomCache[ni][1] = newString(length.int)
        for i in 0..<length.int:
          header.atomCache[ni][1][i] = next.char

  result = parseSingleTerm(data, index, header)

proc toEtf*(term: Term, first = true): string =
  ## converts `term` to binary ETF format
  
  template write(x: string) =
    result.add(x)

  template write(x: uint8) =
    result.add(char(x))

  template write(x: float64) =
    let u = cast[uint64](x)
    result.add(char((u shr 56) and 0xFF))
    result.add(char((u shr 48) and 0xFF))
    result.add(char((u shr 40) and 0xFF))
    result.add(char((u shr 32) and 0xFF))
    result.add(char((u shr 24) and 0xFF))
    result.add(char((u shr 16) and 0xFF))
    result.add(char((u shr 8) and 0xFF))
    result.add(char(u and 0xFF))

  template write(x: uint32) =
    result.add(char((x shr 24) and 0xFF))
    result.add(char((x shr 16) and 0xFF))
    result.add(char((x shr 8) and 0xFF))
    result.add(char(x and 0xFF))

  template write(x: uint16) =
    result.add(char((x shr 8) and 0xFF))
    result.add(char(x and 0xFF))

  if first:
    write(131u8)
  write(term.tag.byte)
  case term.tag
  of tagFloat64:
    write(term.f64)
  of tagUint8:
    write(term.u8)
  of tagInt32:
    write(term.i32.uint32)
  of tagAtom, tagAtomUtf8:
    write(term.atom.string.len.uint16)
    write(term.atom.string)
  of tagSmallAtom, tagSmallAtomUtf8:
    write(term.atom.string.len.uint8)
    write(term.atom.string)
  of tagSmallTuple:
    write(term.tup.len.uint8)
    for m in term.tup:
      write(toEtf(m, false))
  of tagLargeTuple:
    write(term.tup.len.uint32)
    for m in term.tup:
      write(toEtf(m, false))
  of tagMap:
    write(term.map.len.uint32)
    for m in term.map:
      write(toEtf(m[0], false))
      write(toEtf(m[1], false))
  of tagNil:
    discard
  of tagString:
    write(term.str.len.uint16)
    write(term.str)
  of tagBinary:
    write(term.bin.len.uint32)
    write(term.bin)
  of tagSmallBigInt:
    write(term.bigint.data.len.uint8)
    write(term.bigint.negative.byte)
    for m in term.bigint.data:
      write(m)
  of tagLargeBigInt:
    write(term.bigint.data.len.uint32)
    write(term.bigint.negative.byte)
    for m in term.bigint.data:
      write(m)
  of tagList:
    write(term.lst.len.uint32 - 1)
    for m in term.lst:
      write(toEtf(m, false))
  else: raise newException(Exception, "unsupported term output type " & $term.tag)
