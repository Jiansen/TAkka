package org.enmas.util

import java.io._

object IOUtils {
  def copy(in: InputStream, out: OutputStream) {
    val buffer = new Array[Byte](1024)
	  var keepReading = true
		while(keepReading) {
			val amountRead = in.read(buffer)
			if (amountRead == -1) keepReading = false
			else out.write(buffer, 0, amountRead)
		}
  }
}