#' Numeric Cipher.
#' @description Transforms by substitution a given message using the number in each letter of the alphabet (a = 1, b = 2, c = 3, ...).
#'
#' @param ms The message to be encrypted/decrypted in a single character object. The Raw message can be written with upper cases and spaces, but no numbers. The encrypted message has to be written with numbers, each separated by a single space.
#' @param code It refers to the transformation that will be made to the message: "encrypt" and "decrypt".
#'
#' @return A character object with the transformed message, using the replacement of letters to numbers or vice versa.
#' @export
#'
#' @examples
#' num.cipher(ms = "Hello World", code = "encrypt")
#'
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
#' @description Transforms by substitution a given message using a position shift of the alphabet, substituting each letter in the Raw message for a the letter that is a fixed number of positions ahead.
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
#'
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
#' @description Using the Polybius square, substituting each letter for a coordinate it holds inside the square.
#'
#' @param ms The message to be encrypted/decrypted in a single character object. The Raw message can be written with upper cases and spaces, but no numbers. The transformed message has to be written with numbers, each separated by a single space.
#' @param code It refers to the transformation that will be made to the message: "encrypt" and "decrypt".
#'
#' @return A character object with the transformed message, using the unique matrix of the polybius cipher. The letters "i" and "j" have the same symbol.
#' @export
#'
#' @examples
#' polyb.cipher(ms = "Hello World", code = "encrypt")
#'
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
#' @description A cipher with two transformations: Substitution and Transposition. The original message is replaced by the values inside the ADFGVX square, and then, using a given keyword, the transformed message is rearranged, obtaining a unique set of code.
#'
#' @param ms The message to be encrypted/decrypted in a single character object. The Raw message can be written with upper cases and spaces and numbers can be used. The encrypted message needs a space between each five-letter symbol.
#' @param key A single word, necessarily one where no letters are repeated i.e. "Airjet", which determines the unique pattern in which the message is encrypted or decrypted.
#' @param code It refers to the transformation that will be made to the message: "encrypt" and "decrypt".
#'
#' @return For the encrypted message, returns a list of two: "Encrypted Message", which is the set of five-letter transformation of the Raw message; and "Null (characters) Values Added", which is the number of characters added into the message - that is because additional letters need to be added to complete the Transposition matrix. For the decrypted message, returns a single character object with the decrypted message.
#' @export
#'
#' @examples
#' adfgvx.cipher(ms = "Hello World 1", key = "Post", code = "encrypt")
#'
#' adfgvx.cipher(ms = "DXXXA XXAGA DGAXD FAAGD AXGAD AXDFD", key = "Air", code = "decrypt")
adfgvx.cipher = function(ms, key, code) {
    up = toupper(ms)
    up.key = toupper(key)
    key.unl = unlist(strsplit(up.key, ""))
    sorted.key = sort(key.unl)
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
        rm.space = gsub(" ","", up)
        msg = unlist(strsplit(rm.space, ""))
        names(encryp) = char
        cyph = encryp[c(msg)]
        cyph.comb = paste(cyph, collapse = "")
        cyph.spl = unlist(strsplit(cyph.comb, ""))
        adfgvx.matrix = suppressWarnings(
            matrix(
                cyph.spl, ncol = length(key.unl), byrow = TRUE))
        code.df = as.data.frame(adfgvx.matrix)
        colnames(code.df) = key.unl
        sort.pos = numeric()
        for (i in 1:length(sorted.key)) {
            sort.lett = sorted.key[i]
            pos = grep(sort.lett, x = key.unl)
            sort.pos[i] = pos
        }
        code.df = code.df[, c(sort.pos)]
        comb.lett = character()
        for (v in 1:length(code.df)) {
            col = code.df[, v]
            comb.col = paste(col, collapse = "")
            comb.lett[v] = comb.col
        }
        comb.code = paste(comb.lett, collapse = "")
        final.cyph = sub(
            "\\s+$", "", gsub('(.{5})', '\\1 ', comb.code))
        nulls = (length(adfgvx.matrix) - length(cyph.spl))/2
        result = list(final.cyph, nulls)
        names(result) = c("Encrypted Message", "Null (characters) Values Added")
        return(result)
    }
    if (code == "decrypt") {
        msg = unlist(strsplit(up, " "))
        comb.msg = paste(msg, collapse = "")
        sep.lett = unlist(strsplit(comb.msg, ""))
        adfgvx.matrix = suppressWarnings(matrix(
            sep.lett, ncol = length(sorted.key)))
        sort.df = as.data.frame(adfgvx.matrix)
        names(sort.df) = sorted.key
        corr.pos = numeric()
        for (i in 1:length(key.unl)) {
            sort.lett = key.unl[i]
            pos = grep(sort.lett, x = sorted.key)
            corr.pos[i] = pos
        }
        sort.df = sort.df[, c(corr.pos)]
        comb.lett = character()
        for (v in 1:nrow(sort.df)) {
            row = sort.df[v,]
            comb.row = paste(row, collapse = "")
            comb.lett[v] = comb.row
        }
        comb.code = paste(comb.lett, collapse = "")
        raw.code = sub(
            "\\s+$", "", gsub('(.{2})', '\\1 ', comb.code))
        sep.raw.code = unlist(strsplit(raw.code, " "))
        names(char) = encryp
        raw.msg = char[c(sep.raw.code)]
        raw = tolower(paste(raw.msg, collapse = ""))
        return(raw)
    }
}

#' Bacon Cipher.
#' @description A substitution cipher, in which every letter has a special, five-letter, code, except for i-j and u-v, which share a single value respectively. The final message is conveyed in the upper (represents "B" in bacon code) and lower (represents "A" in bacon code) of a decoy message.
#'
#' @param ms The message to be encrypted/decrypted in a single character object. The Raw message can be written in upper cases and spaces, but no numbers. The encrypted message needs to be written in upper cases and without spaces.
#' @param ... Utilized for the decoy message, that is, the message in which the bacon code is hidden in the upper and lower cases. Needs to be purely characters, and have a length equal or less than the total of the bacon code.
#' @param code It refers to the transformation that will be made to the message: "encrypt" and "decrypt".
#'
#' @return For the encrypted message, returns a list of two: "Bacon Code" which is a single character object with all the bacon code; and "Decoy Message", which is a single character object with the hidden bacon code. For the decrypted message, a character vector is returned with the decrypted message.
#' @export
#'
#' @examples
#' #The decoy message can be shorter than the total of letters obtained from
#' #the bacon code. The rest of letters will be randomly generated.
#' bacon.cipher(ms = "Hello World", "This is not an encrypted Message", code = "encrypt")
#'
#' bacon.cipher(ms = "thISIsnOtanEnCryPtEdmESsAGeTtpgEHvLQatjerWxPpgfvMC", code = "decrypt")
bacon.cipher = function(ms, ..., code) {
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
        dec.low = tolower(...)
        dec.rm = gsub(" ", "", dec.low)
        dec.msg = unlist(strsplit(dec.rm, ""))
        low = tolower(ms)
        rm.space = gsub(" ","", low)
        msg = unlist(strsplit(rm.space, ""))
        names(encryp) = lett
        cyph.msg = encryp[c(msg)]
        cyph = paste(cyph.msg, collapse = "")
        sep.cyph = unlist(strsplit(cyph, ""))
        diff = length(sep.cyph) - length(dec.msg)
        r.num = floor(runif(diff, min = 1, max = 26))
        r.lett = letters[c(r.num)]
        comb.dec = c(dec.msg, r.lett)
        names(comb.dec) = sep.cyph
        B.pos = grep("B", names(comb.dec))
        B.up = toupper(comb.dec[c(B.pos)])
        comb.dec[c(B.pos)] = B.up
        dec = paste(comb.dec, collapse = "")
        result = list(cyph, dec)
        names(result) = c("Bacon Code", "Decoy Message Code")
        return(result)
    }
    if (code == "decrypt") {
        sep.ms = unlist(strsplit(ms, ""))
        msg.c = gsub(
            pattern = "([a-z]+)", replacement = "\\2", x = ms)
        msg = unlist(strsplit(msg.c, ""))
        upper.pos = numeric()
        for (i in 1:length(msg)) {
            upper.lett = msg[i]
            pos = paste(
                as.character(
                    grep(upper.lett, sep.ms)),
                collapse = " ")
            upper.pos[i] = pos
        }
        upp.pos = as.numeric(
            unlist(
                strsplit(upper.pos, " ")))
        upp.pos = upp.pos[!duplicated(upp.pos)]
        upp.pos = sort(upp.pos)
        num = 1:length(sep.ms)
        low.pos = num[-c(upp.pos)]
        A.lett = sep.ms[-c(upp.pos)]
        B.lett = sep.ms[c(upp.pos)]
        A = rep("A", length(A.lett))
        B = rep("B", length(B.lett))
        bacon.code = character()
        bacon.code[low.pos] = A
        bacon.code[upp.pos] = B
        complete.code = paste(bacon.code, collapse = "")
        com.code = sub(
            "\\s+$", "", gsub('(.{5})', '\\1 ', complete.code))
        sep.code = unlist(
            strsplit(com.code, " "))
        names(lett) = encryp
        raw.msg = lett[c(sep.code)]
        raw = paste(raw.msg, collapse = "")
        return(raw)
    }
}

#' Beaufort Cipher.
#' @description Substitution cipher, using the Beaufort square and a given letter as a Key.
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
