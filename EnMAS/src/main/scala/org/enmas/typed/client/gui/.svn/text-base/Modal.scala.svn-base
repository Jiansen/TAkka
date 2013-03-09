package org.enmas.client.gui

import scala.swing._

object Modal {
  def popup(headline: String, message: String) {
    val dialog = new Dialog {
      title = headline
      contents = new FlowPanel(new Label(message)) { vGap = 60; hGap = 60; }
      centerOnScreen
      resizable = false
      visible = true
      override def closeOperation = dispose
    }
  }

  def confirm(headline: String, message: String): Boolean = {
    // TODO: spawn a popup confirmation box and return
    //       the user decision as a Boolean
    true
  }
}