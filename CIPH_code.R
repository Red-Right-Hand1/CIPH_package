#' Numeric Cipher.
#'
#' @param ms The message to be encrypted/decrypted in a single character object. The Raw message can be written with upper cases and spaces, but no numbers. The encrypted message has to be written with numbers, each separated by a single space.
#' @param code It refers to the transformation that will be made to the message: "encrypt" and "decrypt".
#'
#' @return A character object with the transformed message, using the replacement of letters to numbers or vice versa.
#' @export
#'
#' @examples
#' num.cipher(ms = "Hello World", code = "encrypt")
#' num.cipher(ms = "8 5 12 12 15 23 15 18 12 4", code = "decrypt")
num.cipher = function(ms, code) {
    if (code == "encrypt") {
        num = 1:26
        names(num) = letters
        low = tolower(ms)
        rm.space = gsub(" ","", low)
        raw = unlist(strsplit(rm.space, ""))
        cyph.msg = num[c(raw)]
        cyph.msg = cyph.msg[!is.na(cyph.msg)]
        cyph = as.character(cyph.msg)
        cyph = paste(cyph, collapse = " ")
        return(cyph)
    }
    if (code == "decrypt") {
        raw = as.numeric(unlist(strsplit(ms, " ")))
        al.raw = letters[c(raw)]
        raw = paste(al.raw, collapse = "")
        return(raw)
    }
}


#' Caesar Cipher.
#'
#' @param ms The message to be encrypted/decrypted in a single character object. The Raw and encrypted message can be written with upper cases and spaces, but no numbers.
#' @param pos.shift Indicates the position the alphabet will be shifted. Only accepts an integer between 1:26.
#' @param code It refers to the transformation that will be made to the message: "encrypt" and "decrypt".
#'
#' @return A character object with the transformed message, using a position shift inside the alphabet to encrypt/decrypt the message.
#' @export
#'
#' @examples
#' caesar.cipher(ms = "Hello World", pos.shift = 1, code = "encrypt")
#' caesar.cipher(ms = "jgnnqyqtnf", pos.shift = 2, code = "decrypt")
caesar.cipher = function(ms, pos.shift, code) {
    low = tolower(ms)
    rm.space = gsub(" ","", low)
    msg = unlist(strsplit(rm.space, ""))
    comb.let = paste(letters, collapse = "")
    if (code == "encrypt") {
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
    if (code == "decrypt") {
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


#' Polybius Cipher.
#'
#' @param ms The message to be encrypted/decrypted in a single character object. The Raw message can be written with upper cases and spaces, but no numbers. The transformed message has to be written with numbers, each separated by a single space.
#' @param code It refers to the transformation that will be made to the message: "encrypt" and "decrypt".
#'
#' @return A character object with the transformed message, using the unique matrix of the polybius cipher. The letters "i" and "j" have the same symbol.
#' @export
#'
#' @examples
#' polyb.cipher(ms = "Hello World", code = "encrypt")
#' polyb.cipher(ms = "23 15 31 31 34 52 34 42 31 14", code = "decrypt")
polyb.cipher = function(ms, code) {
    encryp = c(11:15, 21:24, 24, 25, 31:35, 41:45, 51:55) ##i/j are represented
    ##by the same symbol
    if (code == "encrypt") {
        low = tolower(ms)
        rm.space = gsub(" ","", low)
        msg = unlist(strsplit(rm.space, ""))
        names(encryp) = letters
        cyph = encryp[c(msg)]
        names(cyph) = c()
        cyph = paste(cyph, collapse = " ")
        return(cyph)
    }
    if (code == "decrypt") {
        cyph = character()
        for (i in 1:26) {
            letter = letters[i]
            num = encryp[i]
            cyph[num] = letter
        }
        r = as.numeric(unlist(strsplit(ms, " ")))
        raw.msg = cyph[c(r)]
        raw = paste(raw.msg, collapse = "")
        return(raw)
    }
}


#' ADFGVX Cipher.
#'
#' @param ms The message to be encrypted/decrypted in a single character object. The Raw message can be written with upper cases and spaces and numbers can be used. The encrypted message needs a space between each two-letter symbol.
#' @param code It refers to the transformation that will be made to the message: "encrypt" and "decrypt".
#'
#' @return A character object with the transformed message, returning an encrypted message with a corresponding two-letter code for each letter-number input.
#' @export
#'
#' @examples
#' adfgvx.cipher(ms = "Hello World 1", code = "encrypt")
#' adfgvx.cipher(ms = "AX FA VV VV DV FF DV FG VV FX", code = "decrypt")
adfgvx.cipher = function(ms, code) {
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
    if (code == "encrypt") {
        up = toupper(ms)
        rm.space = gsub(" ","", up)
        msg = unlist(strsplit(rm.space, ""))
        names(encryp) = char
        cyph.msg = encryp[c(msg)]
        cyph = paste(cyph.msg, collapse = " ")
        return(cyph)
    }
    if (code == "decrypt") {
        up = toupper(ms)
        msg = unlist(strsplit(up, " "))
        names(char) = encryp
        raw.msg = char[c(msg)]
        raw = tolower(paste(raw.msg, collapse = ""))
        return(raw)
    }
}


#' Bacon Cipher.
#'
#' @param ms The message to be encrypted/decrypted in a single character object. The Raw message can be written in upper cases and spaces, but no numbers. The encrypted message needs a space between each five-letter symbol.
#' @param code It refers to the transformation that will be made to the message: "encrypt" and "decrypt".
#'
#' @return A character object with the transformed message, returning an encrypted message with a corresponding five-letter code for each letter input.
#' @export
#'
#' @examples
#' bacon.cipher(ms = "Hello World", code = "encrypt")
#' bacon.cipher(ms = "AABBB AABAA ABABA ABABA ABBAB BABAA ABBAB BAAAA ABABA AAABB", code = "decrypt")
bacon.cipher = function(ms, code) { ##i/j and u/v share the same symbol,
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
    if (code == "encrypt") {
        low = tolower(ms)
        rm.space = gsub(" ","", low)
        msg = unlist(strsplit(rm.space, ""))
        names(encryp) = lett
        cyph.msg = encryp[c(msg)]
        cyph = paste(cyph.msg, collapse = " ")
        return(cyph)
    }
    if (code == "decrypt") {
        msg = unlist(strsplit(ms, " "))
        names(lett) = encryp
        raw.msg = lett[c(msg)]
        raw = paste(raw.msg, collapse = "")
        return(raw)
    }
}


#' Beaufort Cipher.
#'
#' @param ms The message to be encrypted/decrypted in a single character object. The Raw and encrypted message can be written with spaces and upper cases, but no numbers.
#' @param key A single letter character object (in upper or lower case) which indicates the key in which the message will be encrypted or decrypted.
#' @param code It refers to the transformation that will be made to the message: "encrypt" and "decrypt".
#'
#' @return A character object with the transformed message, using the key to locate in the beaufort code the corresponding letter.
#' @export
#'
#' @examples
#' beaufort.cipher(ms = "Hello World", key = "Z", code = "encrypt")
#' beaufort.cipher(ms = "FIBBYQYVBJ", key = "m", code = "decrypt")
beaufort.cipher = function(ms, key, code) {
    up.raw = toupper(ms)
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
    if (code == "encrypt") {
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
    if (code == "decrypt") {
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
