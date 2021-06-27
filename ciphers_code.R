num.cipher = function(..., code) {
  if (code == "encript") {
    num = 1:26
    names(num) = letters
    low = tolower(...)
    rm.space = gsub(" ","", low)
    raw = unlist(strsplit(rm.space, ""))
    cyph = num[c(raw)]
    cyph = cyph[!is.na(cyph)]
    names(cyph) = c()
    return(as.numeric(cyph))
  }
  if (code == "decript") {
    al.raw = letters[c(...)]
    raw = paste(al.raw, collapse = "")
    return(raw)
  }
}
caesar.cipher = function(..., pos.shift, code) {
  low = tolower(...)
  rm.space = gsub(" ","", low)
  msg = unlist(strsplit(rm.space, ""))
  comb.let = paste(letters, collapse = "")
  if (code == "encript") {
    n0 = pos.shift - 1
    n = length(letters) - n0
    move = substring(comb.let, first = n)
    alt.lett = gsub(move, "", comb.let)
    code.pos = paste(move, alt.lett, sep = "")
    pos.code = unlist(strsplit(code.pos, ""))
    names(letters) = pos.code
    cyph.msg = letters[c(msg)]
    cyph = paste(cyph.msg, collapse = "")
    return(cyph)
  }
  if (code == "decript") {
    move = substring(comb.let, first = 1, last = pos.shift)
    alt.lett = gsub(move, "", comb.let)
    code.pos = paste(alt.lett, move, sep = "")
    pos.code = unlist(strsplit(code.pos, ""))
    names(letters) = pos.code
    raw.msg = letters[c(msg)]
    raw = paste(raw.msg, collapse = "")
    return(raw)
  }
}
polyb.cipher = function(..., code) {
  encryp = c(11:15, 21:24, 24, 25, 31:35, 41:45, 51:55) ##i/j are represented
  ##by the same symbol
  if (code == "encript") {
    low = tolower(...)
    rm.space = gsub(" ","", low)
    msg = unlist(strsplit(rm.space, ""))
    names(encryp) = letters
    cyph = encryp[c(msg)]
    names(cyph) = c()
    return(cyph)
  }
  if (code == "decript") {
    cyph = character()
    for (i in 1:26) {
      letter = letters[i]
      num = encryp[i]
      cyph[num] = letter
    }
    raw.msg = cyph[c(...)]
    raw = paste(raw.msg, collapse = "")
    return(raw)
  }
}
adfgvx.cipher = function(..., code) {
  encryp = c(
    "AA", "AD", "AF", "AG", "AV", "AX",
    "DA", "DD", "DF", "DG", "DV", "DX",
    "FA", "FD", "FF", "FG", "FV", "FX",
    "GA", "GD", "GF", "GG", "GV", "GX",
    "VA", "VD", "VF", "VG", "VV", "VX",
    "XA", "XD", "XF", "XG", "XV", "XX")
  char = c(
    "N", "A", "1", "C", "3", "H",
    "8", "T", "B", "2", "O", "M",
    "E", "5", "W", "R", "P", "D",
    "4", "F", "6", "G", "7", "I",
    "9", "J", "0", "K", "L", "Q",
    "S", "U", "V", "X", "Y", "Z")
  if (code == "encript") {
    up = toupper(...)
    rm.space = gsub(" ","", up)
    msg = unlist(strsplit(rm.space, ""))
    names(encryp) = char
    cyph.msg = encryp[c(msg)]
    cyph = paste(cyph.msg, collapse = " ")
    return(cyph)
  }
  if (code == "decript") {
    up = toupper(...)
    msg = unlist(strsplit(up, " "))
    names(char) = encryp
    raw.msg = char[c(msg)]
    raw = tolower(paste(raw.msg, collapse = ""))
    return(raw)
  }
}
bacon.cipher = function(..., code) { ##i/j and u/v share the same symbol,
  ##respectively
  lett = letters
  encryp = c("AAAAA", "AAAAB", "AAABA",
             "AAABB", "AABAA", "AABAB",
             "AABBA", "AABBB", "ABAAA",
             "ABAAA", "ABAAB", "ABABA",
             "ABABB", "ABBAA", "ABBAB",
             "ABBBA", "ABBBB", "BAAAA",
             "BAAAB", "BAABA", "BAABB",
             "BAABB", "BABAA", "BABAB",
             "BABBA", "BABBB")
  if (code == "encript") {
    low = tolower(...)
    rm.space = gsub(" ","", low)
    msg = unlist(strsplit(rm.space, ""))
    names(encryp) = lett
    cyph.msg = encryp[c(msg)]
    cyph = paste(cyph.msg, collapse = " ")
    return(cyph)
  }
  if (code == "decript") {
    msg = unlist(strsplit(..., " "))
    names(lett) = encryp
    raw.msg = lett[c(msg)]
    raw = paste(raw.msg, collapse = "")
    return(raw)
  }
}
beaufort.cipher = function(..., key, code) {
  up.raw = toupper(...)
  rm.space = gsub(" ","", up.raw)
  key.u = toupper(key)
  msg = unlist(strsplit(rm.space, ""))
  beaufort.code = data.frame(A = LETTERS, B = c(LETTERS[-1], LETTERS[1]),
                             C = c(LETTERS[-c(1:2)], LETTERS[1:2]),
                             D = c(LETTERS[-c(1:3)], LETTERS[1:3]),
                             E = c(LETTERS[-c(1:4)], LETTERS[1:4]),
                             F = c(LETTERS[-c(1:5)], LETTERS[1:5]),
                             G = c(LETTERS[-c(1:6)], LETTERS[1:6]),
                             H = c(LETTERS[-c(1:7)], LETTERS[1:7]),
                             I = c(LETTERS[-c(1:8)], LETTERS[1:8]),
                             J = c(LETTERS[-c(1:9)], LETTERS[1:9]),
                             K = c(LETTERS[-c(1:10)], LETTERS[1:10]),
                             L = c(LETTERS[-c(1:11)], LETTERS[1:11]),
                             M = c(LETTERS[-c(1:12)], LETTERS[1:12]),
                             N = c(LETTERS[-c(1:13)], LETTERS[1:13]),
                             O = c(LETTERS[-c(1:14)], LETTERS[1:14]),
                             P = c(LETTERS[-c(1:15)], LETTERS[1:15]),
                             Q = c(LETTERS[-c(1:16)], LETTERS[1:16]),
                             R = c(LETTERS[-c(1:17)], LETTERS[1:17]),
                             S = c(LETTERS[-c(1:18)], LETTERS[1:18]),
                             T = c(LETTERS[-c(1:19)], LETTERS[1:19]),
                             U = c(LETTERS[-c(1:20)], LETTERS[1:20]),
                             V = c(LETTERS[-c(1:21)], LETTERS[1:21]),
                             W = c(LETTERS[-c(1:22)], LETTERS[1:22]),
                             X = c(LETTERS[-c(1:23)], LETTERS[1:23]),
                             Y = c(LETTERS[-c(1:24)], LETTERS[1:24]),
                             Z = c(LETTERS[-c(1:25)], LETTERS[1:25]))
  row.names(beaufort.code) = LETTERS
  chr.pos = numeric()
  for (i in 1:length(msg)) {
    s.msg = msg[i]
    let.pos = grep(s.msg, LETTERS)
    chr.pos[i] = as.numeric(let.pos)
  }
  if (code == "encript") {
    pos = numeric()
    for (v in 1:length(chr.pos)) {
      p = chr.pos[v]
      inner.pos = grep(key.u, beaufort.code[, p])
      pos[v] = as.numeric(inner.pos)
    }
    cyp = character()
    for (z in 1:length(pos)) {
      chr.f = pos[z]
      chr.raw = row.names(beaufort.code[chr.f,])
      cyp[z] = chr.raw
    }
    cyph = paste(cyp, collapse = "")
    return(cyph)
  }
  if (code == "decript") {
    pos = numeric()
    for (v in 1:length(chr.pos)) {
      p = chr.pos[v]
      inner.pos = grep(key.u, beaufort.code[p,])
      pos[v] = as.numeric(inner.pos)
    }
    al.raw = character()
    for (z in 1:length(pos)) {
      chr.f = pos[z]
      chr.raw = names(beaufort.code[chr.f])
      al.raw[z] = chr.raw
    }
    raw.msg = tolower(al.raw)
    raw = paste(raw.msg, collapse = "")
    return(raw)
  }
}
