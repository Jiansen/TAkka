package org.enmas.bundler

import org.enmas.pomdp.POMDP,
       scala.tools.nsc._, scala.tools.nsc.reporters._,
       scala.swing._, scala.swing.event._, 
       scala.swing.BorderPanel.Position._,
       scala.actors.Futures._,
       java.io._, java.net._, java.util.jar._

class Bundler extends MainFrame {
  title = "EnMAS: Jar Bundler"
  contents = ui
  minimumSize = new Dimension(650, 400)
  centerOnScreen
  visible = true

  lazy val ui = new BorderPanel {

    val sourceChooser = new FileChooser {
      title = "Choose source directory"
      fileSelectionMode = FileChooser.SelectionMode.DirectoriesOnly
      multiSelectionEnabled = false
      fileHidingEnabled = true
      peer.setAcceptAllFileFilterUsed(false)
      fileFilter = new javax.swing.filechooser.FileFilter {
        def accept(f: File) = f.isDirectory
        def getDescription = "Only directories"
      }
    }

    object StatusBar extends Label {
      opaque = true
      noSource
      def noSource = {
        text = "Please choose a source directory."
        background = new Color(223, 223, 223)
      }
      def working = {
        text = "Working..."
        background = new Color(255, 239, 0)
      }
      def failure = {
        text = "Compilation Failed."
        background = new Color(255, 64, 64)
      }
      def success = {
        text = "Compilation Succeeded."
        background = new Color(32, 255, 32)
      }
    }

    val results = new TextArea
    results.editable = false

    val compileSourceButton = new Button {
      action = Action("Compile scala source files...") {
        results.text = ""
        val v = sourceChooser.showDialog(this, "Choose source directory")
        if (v != FileChooser.Result.Approve || ! sourceChooser.selectedFile.exists)
          StatusBar.noSource
        else {
          future { compile(sourceChooser.selectedFile) }
          StatusBar.working
        }
      }
    }

    def compile(sourceDir: File) = {
      results.text += "Compiling source files... "
      val settings = new Settings()
      settings.sourcepath.value = sourceDir.getPath
      settings.outdir.value = settings.sourcepath.value
      val errorLog = new StringWriter
      val reporter = new ConsoleReporter(settings, null, new PrintWriter(errorLog))
      object scalac extends Global(settings, reporter)
      val run = new scalac.Run
      run compile(
        sourceDir.listFiles.toList.filter(_.toString.endsWith(".scala")) map { _.getAbsolutePath }
      )
      if (reporter.hasErrors) {
        StatusBar.failure
        results.text += "\n" + errorLog.toString
      }
      else {
        results.text += "completed without errors.\n\n"
        try {
          makeJar(sourceDir)
          StatusBar.success
          results.text += "\nDone!"
        }
        catch { case t: Throwable  ⇒ 
          StatusBar.failure
          results.text += "\n%s:\n%s".format(t.getClass.getName, t.getMessage)
        }
      }
    }

    def makeJar(sourceDir: File) = {
      results.text += "Making JAR file... "
      import java.util.jar._
      val jarFile = new File(sourceDir.getParentFile, sourceDir.getName.replaceAll("\\s", "")+".jar")
      jarFile.delete
      jarFile.getParentFile.mkdirs
      val manifest = new java.util.jar.Manifest
      manifest.getMainAttributes.put(Attributes.Name.MANIFEST_VERSION, "1.0")
      val jar = new JarOutputStream(new FileOutputStream(jarFile), manifest)
      val classFiles = sourceDir.listFiles.toList.filter(_.toString.endsWith(".class"))
      results.text += classFiles.length + " class files found\n"
      for (f <- classFiles) {
        var className = f.getPath.replace("\\", "/")
        className = className.substring(className.lastIndexOf("/")+1, className.length)
        results.text += "Bundling class file: " + className + "\n"
        jar putNextEntry { new JarEntry(className) }
        val fin = new FileInputStream(f)
        org.enmas.util.IOUtils.copy(fin, jar)
        fin.close
      }
      jar.close
    }

    layout(new FlowPanel(compileSourceButton)) = North
    layout(new ScrollPane(results)) = Center
    layout(StatusBar) = South
  }
}

object Bundler extends App {
  val bundler = new Bundler()
}
