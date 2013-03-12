package org.enmas.util

import java.io._, java.security.{MessageDigest, DigestInputStream}

object FileUtils {

  sealed case class FileData(data: Array[Byte], md5: Array[Byte])

  /** Reads a file in to memory and computes the MD5 hash over
    * the bytes all at once.
    *
    * Returns a Some[FileData] iff reading is successful and 
    * None otherwise.  The recipient of this case object
    * (if used as an actor message) should compute MD5 over
    * the data bytes and compare them to the checksum.
    *
    * A utility method is provided to do just that; see
    * the verifyFileData method.
    */
  def readFile(file: File): Option[FileData] = {
    var result: Option[FileData] = None
    val data = new Array[Byte](file.length.toInt)
    val md5 = MessageDigest.getInstance("MD5")
    var fin: InputStream = new FileInputStream(file)
    try {
      fin = new DigestInputStream(fin, md5)
      fin.read(data, 0, data.length)
      result = Some(FileData(data, md5.digest))
    }
    catch { case _:Throwable  => None }
    finally { fin.close }
    result
  }

  /** Returns a Some[Array[Byte]] iff computing MD5 over the
    * data part of the input FileData object yields an array
    * that is equal to its checksum part.
    *
    * Returns a None otherwise.
    */
  def verifyFileData(fd: FileData): Option[Array[Byte]] = {
    val FileData(data, md5) = fd // multiple assignment trick
    import java.util.Arrays
    val computed = MessageDigest.getInstance("MD5")
    if (Arrays.equals(computed.digest(data), md5)) Some(data)
    else None
  }

}
