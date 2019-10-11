object Cipher{
  /** Bit-wise exclusive-or of two characters */
  def xor(a: Char, b: Char) : Char = (a.toInt ^ b.toInt).toChar

  /** Print ciphertext in octal */
  def showCipher(cipher: Array[Char]) =
    for(c <- cipher){ print(c/64); print(c%64/8); print(c%8); print(" ") }

  /** Read file into array */
  def readFile(fname: String) : Array[Char] =
    scala.io.Source.fromFile(fname).toArray

  /** Read from stdin in a similar manner */
  def readStdin() = scala.io.Source.stdin.toArray

  /* ----- Functions below here need to be implemented ----- */

  /** Encrypt plain using key; can also be used for decryption */
  def encrypt(key: Array[Char], plain: Array[Char]) : Array[Char] =
    {
      var i = 0
      val k = key.size
      val t = plain.size
      var result = new Array [Char] (t)
      // Invariant I: result [0..i) = plain[0..i) encrypted with "key" (notice the modulus) && 0<=i<t
      // variant (t-i)
      while (i < t)
      {
        // I
          result(i) = xor (plain(i),key(i%k))
        // result [0..(i+1)) = plain [0..(i+1)) encrypted with "key"
          i = i + 1
        // I && o<=i<=t
      }
      // the variant becomes 0, so i=t
      // I holds, so result [0..t) = plain [0..t) encrypted  with "key"
      result
    }

  /** Try to decrypt ciphertext, using crib as a crib */
  def tryCrib(crib: Array[Char], ciphertext: Array[Char]) =
    {
      val k = crib.size
      val ct = ciphertext.size
      var start = 0 // the position where we start placing the crib
      var done = 0 // Checks if we have found a solution so far
      while ((done == 0) && (start<ct-k+1))
      {
        var i = 0
        // Creating keyChars from applying xor to ciphertext and crib
        var keyChars = new Array[Char] (k)
        while (i<k) {keyChars (i) = xor (ciphertext(i+start),crib(i)); i+=1}
        // Finding the smallest j<k-1 for which keyChars[0..k-j)=keyChars[j..k) with a separate function keyCheck
        var j = 1
        while ((keyCheck(keyChars,j) == false) && (j<k-1)) j = j + 1
        // Extracting the key from keyChars in the case we found a valid value for j at the previous step
        if (j<k-1)
        {
          var key = new Array [Char] (j)
          val b = start % j // b is the number of letters we need from the key that are before the repetition
          val pos = (j-b)%j // pos is the position from keyChars where the first letter of the key is, if b is 0, then we need to start from the first position, which is 0, not j, so we apply %j
          // We create the key from the elements of keyChars
          var i = 0
          while (i<j)
          {
            if (pos+i>=k) key (i) = keyChars (pos+i-j) // if we get out of the keyChars index k we need to start j positions behind as that's where we will find the last part of the key
                else key (i) = keyChars (pos+i) // otherwise we simply add the letters from keyChars, which are to the right with pos positions
            i += 1
          }
          // We print the key and the decrypted message as strings
          println(new String(key))
          println(new String (encrypt(key.toArray,ciphertext)))
          done = 1
          // done = 1 means we decrypted the text, so we can stop
        }
        start = start + 1
      }
      // If we get out of the loop with done = 1 it means we decrypted the code and we will have the key
      // and the decrypted text printed, otherwise start will become ct-k and we will get no solution
      // which means we could not decrypt the code with the given "crib"
    }

  def keyCheck (keyChars: Array[Char], j: Int) : Boolean =
    {
      val k = keyChars.size
      var ok = true
      var i = 0
      // We start supposing that the first k-j letters match the last k-j letters of keyChars
      while ((i<k-j) && (ok==true))
      {
        // If we find two letters that do not correspond we make ok = false
        if (keyChars(i) != keyChars (i+j)) ok = false
        i += 1
      }
      // At the end of the loop, the value of ok will tell us whether the first (k-j) letters match the last (k-j) letters of keyChars
      ok
    }

  /** The first optional statistical test, to guess the length of the key */
  def crackKeyLen(ciphertext: Array[Char]) =
    {
      val c = ciphertext.size
      val maxShift = 30
      var shift = 1
      // We shift the text with "shift" positions to the right and count the number of matches
      while (shift<=maxShift)
      {
        var matches = 0
        var i = 0
        // We count the number of matches until we get out of the ciphertext array
        while (i<c-shift)
        {
            if (ciphertext(i) == ciphertext(i+shift)) matches += 1
            i += 1
        }
        println(shift+ ": "+matches)
        shift += 1
      }
      //For private1.txt we obtain the length of the key 9 and for private2.txt the length 8
    }

  /** The second optional statistical test, to guess characters of the key. */
  def crackKey(klen: Int, ciphertext: Array[Char]) =
    {
      var m = 1
      // m means the number of times we shift the ciphertext with "klen" positions to the right
      val c = ciphertext.size
      while (klen*m<c)
      {
        var i = 0
        // We see the matches as long as we don't get out of the ciphertext array
        while (i<c-m*klen)
        {
          if (ciphertext(i) == ciphertext(i+m*klen))
          {
            // When we get a match we see what position we currently are in the key
             var letter = i % klen
             // Then we calculate its value by applying xor to the "space" character and the current ciphertext
             val value = xor (' ',ciphertext(i))
             val checkValue = value.toInt
             // We only print it if is a printable character
             if ((32<=checkValue) && (checkValue<=127)) println(letter+ ": "+value)
          }
          i += 1
        }
        m += 1
      }
      // We obtain the first key : PEMBERLEY
      // And the second key : HOGWARTS
    }

/** The main method just selects which piece of functionality to run */
  def main(args: Array[String]) = {
    // string to print if error occurs
    val errString =
      "Usage: scala Cipher (-encrypt|-decrypt) key [file]\n"+
      "     | scala Cipher -crib crib [file]\n"+
      "     | scala Cipher -crackKeyLen [file]\n"+
      "     | scala Cipher -crackKey len [file]"

    // Get the plaintext, either from the file whose name appears in position
    // pos, or from standard input
    def getPlain(pos: Int) =
      if(args.length==pos+1) readFile(args(pos)) else readStdin

    // Check there are at least n arguments
    def checkNumArgs(n: Int) = if(args.length<n){println(errString); sys.exit}

    // Parse the arguments, and call the appropriate function
    checkNumArgs(1)
    val command = args(0)
    if(command== "-encrypt" || command== "-decrypt"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      print(new String (encrypt(key,plain)))
    }
    else if(command== "-crib"){
      checkNumArgs(2); val key = args(1).toArray; val plain = getPlain(2)
      tryCrib(key, plain)
    }
    else if(command== "-crackKeyLen"){
      checkNumArgs(1); val plain = getPlain(1)
      crackKeyLen(plain)
    }
    else if(command== "-crackKey"){
      checkNumArgs(2); val klen = args(1).toInt; val plain = getPlain(2)
      crackKey(klen, plain)
    }
    else println(errString)
  }
}
