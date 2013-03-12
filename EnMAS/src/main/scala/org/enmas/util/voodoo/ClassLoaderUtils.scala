package org.enmas.util.voodoo

import java.io._, java.net._, java.lang.reflect._, java.util.jar._

object ClassLoaderUtils {

  /** To be mixed in with classes that need to be able to receive
    * the raw byte content of a JAR file over the network.
    */
  trait Provisionable {
    import org.enmas.pomdp._, org.enmas.util.FileUtils._, java.io._
    /** Saves the incoming FileData bytes as a temp file
      * if the computed MD5 matches the attached checksum,
      * returning the optional File object.
      */
    def provision[T]
      (fileData: FileData)
      (implicit m: scala.reflect.Manifest[T])
    : Option[File] = {
      verifyFileData(fileData) match {
        case Some(data)  => {
          val file = File.createTempFile("provisioned", null)
          file.deleteOnExit
          val fout = new FileOutputStream(file)
          fout.write(data)
          fout.flush
          Some(file)
        }
        case None  => None
      }
    }
  }

  def findSubclasses[T](file: File)(implicit m: scala.reflect.Manifest[T]) = {
    val jarFile = new JarFile(file)
    val jarEntries = jarFile.entries
    var subclasses = List[java.lang.Class[_ <: T]]()
    while (jarEntries.hasMoreElements) {
      val entry = jarEntries.nextElement
      if (entry.getName.endsWith(".class")) {
        val name = entry.getName.replace(".class", "")
        try {
          val clazz = getClass(file, name)
          clazz.asSubclass(m.erasure)
          subclasses :+= clazz.asInstanceOf[java.lang.Class[_ <: T]]
        }
        catch { case t: Throwable  => () }
      }
    }
    subclasses
  }

  private def getClass(file: File, name: String): java.lang.Class[_] = {
    addURL(file.toURI.toURL)
    procureUrlLoader.loadClass(name)
  }

  private def addURL(u: URL): Unit = {
    val loader = procureUrlLoader()
    val urls = loader.getURLs
    var alreadyHasURL = urls.contains { url: URL  => url.toString.equalsIgnoreCase(u.toString) }
    if (! alreadyHasURL) {
      loader.getClass.getMethods.filter{ method  => {
        method.getName == "addURL" || method.getName == "attachURL"
      }}.headOption match {
        case Some(m)  => { m setAccessible true; m.invoke(loader, u) }
        case None  => println("Big problems... the class loader does not support addURL!!!")
      }
    }
  }

  /** Hack! Looks for the nearest ClassLoader in the hierarchy that looks like
    * it can be cast to an instance of URLClassLoader because we need desperately
    * to call the addURL method.  If this fails the whole app is basically hosed.
    *
    * This ugliness was introduced because of problems encountered while trying
    * to run the project under sbt, which does some custom loader stuff, presumably
    * to link all of the dependencies and build products.
    *
    * Akka also has some issues running under sbt.  I think those were introduced
    * as a side effect of trying to accomodate running in a container like tomcat
    * or jetty.  Akka gets its loader from the Thread context.
    *
    * ClassLoaders... Arrrgh!
    */
  private def procureUrlLoader(): URLClassLoader = {
    val loaders = getClassLoaderChain(getClass().getClassLoader)
    val urlLoaders = loaders filter { cl: ClassLoader  => {
      var accept = false
      try {
        val possibility = cl.asInstanceOf[URLClassLoader]
        accept = possibility.getClass.getName.contains("URL") &&
                 ! possibility.getClass.getMethods.filter{ _.getName == "addURL" }.isEmpty
      }
      catch { case _:Throwable  => () }
      accept
    }}
    (urlLoaders.headOption getOrElse DefaultLoader).asInstanceOf[URLClassLoader]
  }

  object DefaultLoader extends URLClassLoader(
    new scala.Array[URL](1), getClass().getClassLoader
  ) {
    def attachURL(url: URL) = addURL(url)
  }

  private def getClassLoaderChain(loader: ClassLoader): List[ClassLoader] = {
    loader.getParent match {
      case null  => List(loader)
      case p: ClassLoader  => loader :: getClassLoaderChain(p)
    }
  }

}