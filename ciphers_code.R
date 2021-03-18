num.encrip = function(raw.msg) {
  key = 1:26
  names(key) = letters
  low = tolower(raw.msg)
  raw = unlist(strsplit(low, ""))
  cyoh = key[c(raw)]
  print(cyph[!is.na(cyph)])
}
num.decrip = function(...) {
  raw = letters[c(...)]
  print(raw)
}
caesar.encrip = function(pos.shift, raw.msg) {
  low = tolower(raw.msg)
  raw = unlist(strsplit(low, ""))
  if(pos.shift == 1) {
    names(letters) = c("z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y")
    cyp = letters[c(raw)]
    print(cyp)
  } 
  if(pos.shift == 2) {
    names(letters) = c("y", "z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x")
    cyp = letters[c(raw)]
    print(cyp)
  }
  if(pos.shift == 3) {
    names(letters) = c("x", "y", "z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w")
    cyp = letters[c(raw)]
    print(cyp)
  }
  if(pos.shift == 4) {
    names(letters) = c("w", "x", "y", "z", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v")
    cyph = letters[c(raw)]
    print(cyph[!is.na(cyph)])
  }
}
caesar.decrip = function(pos.shift, encr.msg) {
  key = unlist(strsplit(encr.msg, ""))
  if(pos.shift == 1) {
    names(letters) = c("b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "a")
    raw = letters[c(key)]
    print(raw)
  }
  if(pos.shift == 2) {
    names(letters) = c("c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "a", "b")
    raw = letters[c(key)]
    print(raw)
  }
  if(pos.shift == 3) {
    names(letters) = c("d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "a", "b", "c")
    raw = letters[c(key)]
    print(raw)
  }
  if(pos.shift == 4) {
    names(letters) = c("e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", "a", "b", "c", "d")
    raw = letters[c(key)]
    print(raw)
  }
}
poly.encrip = function(raw.msg) { ##i/j deben ser escritos asi - el código las cuenta como igual
  low = tolower(raw.msg)
  raw = unlist(strsplit(low, ""))
  encryp = c(11:15, 21:24, 24, 25, 31:35, 41:45, 51:55)
  names(encryp) = letters
  cyph = encryp[c(raw)]
  print(cyph[!is.na(cyph)])
}
poly.decrip = function(...) {
  cyph = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, "a", "b", "c", "d", "e", 16, 17, 18, 19, 20, "f", "g", "h", "i/j", "k", 26, 27, 28, 29, 30, "l", "m", "n", "o", "p", 36, 37, 38, 39, 40, "q", "r", "s", "t", "u", 46, 47, 48, 49, 50, "v", "w", "x", "y", "z")
  raw = cyph[c(...)]
  print(raw)
}
adfgvx.encrip = function(raw.msg) { ## Este código permite usar números
  up = toupper(raw.msg)
  raw = unlist(strsplit(up, ""))
  encryp = c("AA", "AD", "AF", "AG", "AV", "AX", "DA", "DD", "DF", "DG", "DV", "DX", "FA", "FD", "FF", "FG", "FV", "FX", "GA", "GD", "GF", "GG", "GV", "GX", "VA", "VD", "VF", "VG", "VV", "VX", "XA", "XD", "XF", "XG", "XV", "XX")
  names(encryp) = c("N", "A", "1", "C", "3", "H", "8", "T", "B", "2", "O", "M", "E", "5", "W", "R", "P", "D", "4", "F", "6", "G", "7", "I", "9", "J", "0", "K", "L", "Q", "S", "U", "V", "X", "Y", "Z")
  cyph = encryp[c(raw)]
  print(cyph[!is.na(cyph)])
}
adfgvx.decrip = function(...) {
  cyph = c("N", "A", "1", "C", "3", "H", "8", "T", "B", "2", "O", "M", "E", "5", "W", "R", "P", "D", "4", "F", "6", "G", "7", "I", "9", "J", "0", "K", "L", "Q", "S", "U", "V", "X", "Y", "Z")
  names(cyph) = c("AA", "AD", "AF", "AG", "AV", "AX", "DA", "DD", "DF", "DG", "DV", "DX", "FA", "FD", "FF", "FG", "FV", "FX", "GA", "GD", "GF", "GG", "GV", "GX", "VA", "VD", "VF", "VG", "VV", "VX", "XA", "XD", "XF", "XG", "XV", "XX")
  raw = cyph[c(...)]
  print(raw)
}
bacon.encrip = function(raw.msg) { ##i/j y u/v deben ser escritos asi - el código los considera igual
  raw = unlist(strsplit(raw.msg, ""))
  encryp = c("AAAAA", "AAAAB", "AAABA", "AAABB", "AABAA", "AABAB", "AABBA", "AABBB", "ABAAA", "ABAAA", "ABAAB", "ABABA", "ABABB", "ABBAA", "ABBAB", "ABBBA", "ABBBB", "BAAAA", "BAAAB", "BAABA", "BAABB", "BAABB", "BABAA", "BABAB", "BABBA", "BABBB")
  names(encryp) = letters
  cyph = encryp[c(raw)]
  print(cyph[!is.na(cyph)])
}
bacon.decrip = function(...) {
  cyph = letters
  names(cyph) = c("AAAAA", "AAAAB", "AAABA", "AAABB", "AABAA", "AABAB", "AABBA", "AABBB", "ABAAA", "ABAAA", "ABAAB", "ABABA", "ABABB", "ABBAA", "ABBAB", "ABBBA", "ABBBB", "BAAAA", "BAAAB", "BAABA", "BAABB", "BAABB", "BABAA", "BABAB", "BABBA", "BABBB")
  decryp = cyph[c(...)]
  print(decryp)
}
