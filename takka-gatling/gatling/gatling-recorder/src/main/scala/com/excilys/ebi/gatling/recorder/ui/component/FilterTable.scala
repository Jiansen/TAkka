/**
 * Copyright 2011-2012 eBusiness Information, Groupe Excilys (www.excilys.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * 		http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.excilys.ebi.gatling.recorder.ui.component

import java.awt.{ Dimension, Component, Color, BorderLayout }
import java.awt.event.{ MouseListener, MouseEvent, MouseAdapter, ActionListener, ActionEvent }

import com.excilys.ebi.gatling.recorder.config.Pattern
import com.excilys.ebi.gatling.recorder.ui.enumeration.PatternType.{ PatternType, JAVA, ANT }

import grizzled.slf4j.Logging
import javax.swing.{ JTable, JScrollPane, JRadioButton, JPopupMenu, JPanel, JMenuItem, ButtonGroup, AbstractCellEditor }
import javax.swing.table.{ TableCellRenderer, TableCellEditor, DefaultTableModel }

class FilterTable extends JPanel with MouseListener {

	private val model = new DefaultTableModel
	private val table = new JTable

	model.addColumn("Filter")
	model.addColumn("Style")

	table.setModel(model)
	table.setRowHeight(30)
	table.getTableHeader.setReorderingAllowed(false)

	setLayout(new BorderLayout)

	val scrollPane = new JScrollPane(table)
	scrollPane.setPreferredSize(new Dimension(200, 300))
	add(scrollPane)
	scrollPane.addMouseListener(this)

	initPopupMenu

	val styleColumn = table.getColumn("Style")
	styleColumn.setCellRenderer(new RadioButtonRenderer)
	styleColumn.setCellEditor(new RadioButtonEditor)
	styleColumn.setMinWidth(150)
	styleColumn.setMaxWidth(150)

	def validateCells {
		stopCellEditing
		var toRemove = List[Int]()
		for (i <- 0 until model.getRowCount if (model.getValueAt(i, 0).toString.isEmpty))
			toRemove = i :: toRemove

		removeRows(toRemove)
		removeDuplicates
	}

	def removeRows(toRemove: List[Int]) {
		toRemove.sorted.reverse.foreach {
			model.removeRow(_)
		}
	}

	def stopCellEditing {
		if (table.isEditing && table.getSelectedRow != -1)
			table.getCellEditor.stopCellEditing
	}

	def removeDuplicates {
		for (
			i <- (0 until model.getRowCount);
			j <- (0 until model.getRowCount);
			if (i != j && getPattern(i) == getPattern(j))
		) model.removeRow(j)
	}

	override def setEnabled(enabled: Boolean) {
		table.setEnabled(enabled)
		table.setBackground(if (enabled) Color.WHITE else Color.LIGHT_GRAY)
	}

	def addRow {
		stopCellEditing
		model.addRow(Array[Object](""));
	}

	def addRow(pattern: Pattern) = model.addRow(Array[Object](pattern.pattern, new SelectPatternPanel(pattern.patternType)))

	def removeSelectedRow = removeRows(getSelectedRows.toList)

	def removeAllElements = removeRows((0 until model.getRowCount).toList)

	def getSelectedRows = table.getSelectedRows

	def getRowCount = model.getRowCount

	def getPattern(row: Int) = new Pattern(getPatternTypeAt(row), model.getValueAt(row, 0).toString)

	private def getPatternTypeAt(row: Int): PatternType = {
		table.getValueAt(row, 1).asInstanceOf[SelectPatternPanel].getPatternType
	}

	private def initPopupMenu {
		val popup = new JPopupMenu
		val menuItem = new JMenuItem("Delete")
		menuItem.addActionListener(new ActionListener {
			def actionPerformed(e: ActionEvent) {
				removeSelectedRow
			}
		})

		popup.add(menuItem)

		table.addMouseListener(new MouseAdapter() {

			override def mousePressed(e: MouseEvent) {
				maybeShowPopup(e)
			}

			override def mouseReleased(e: MouseEvent) {
				maybeShowPopup(e)
			}

			private def maybeShowPopup(e: MouseEvent) {
				if (e.isPopupTrigger)
					popup.show(e.getComponent, e.getX, e.getY)
			}
		})
	}

	override def mouseReleased(e: MouseEvent) {}

	override def mousePressed(e: MouseEvent) {}

	override def mouseExited(e: MouseEvent) {}

	override def mouseEntered(e: MouseEvent) {}

	override def mouseClicked(e: MouseEvent) {
		stopCellEditing
	}
}

class RadioButtonRenderer extends TableCellRenderer {

	def getTableCellRendererComponent(table: JTable, value: Object, isSelected: Boolean, hasFocus: Boolean, row: Int, column: Int): Component = {
		Option(value.asInstanceOf[SelectPatternPanel]).getOrElse {
			val newValue = SelectPatternPanel()
			table.setValueAt(newValue, row, 1)
			newValue
		}
	}
}

class RadioButtonEditor extends AbstractCellEditor with TableCellEditor with Logging {

	var customPanel = SelectPatternPanel()

	def getTableCellEditorComponent(table: JTable, value: Object, isSelected: Boolean, row: Int, column: Int): Component = {
		customPanel = value.asInstanceOf[SelectPatternPanel]
		value.asInstanceOf[SelectPatternPanel]
	}

	def getCellEditorValue = customPanel
}

object SelectPatternPanel {
	def apply() = new SelectPatternPanel(ANT)
}

class SelectPatternPanel(patternType: PatternType) extends JPanel {

	val radio1 = new JRadioButton("Ant", true);
	val radio2 = new JRadioButton("Java", false);

	val group = new ButtonGroup
	group.add(radio1)
	group.add(radio2)

	patternType match {
		case ANT => radio1.setSelected(true)
		case JAVA => radio2.setSelected(true)
	}

	add(radio1)
	add(radio2)

	def getPatternType = if (radio1.isSelected) ANT else JAVA
}