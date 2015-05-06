package spreadsheet.gui;

import java.awt.Component;

import javax.swing.DefaultCellEditor;
import javax.swing.JTable;
import javax.swing.JTextField;

import spreadsheet.SpreadsheetInterface;

public class SpreadsheetCellEditor extends DefaultCellEditor {

	private JTextField textField;
	private SpreadsheetInterface spreadsheet;
	
	protected SpreadsheetCellEditor(JTextField textField, SpreadsheetInterface spreadsheet) {
		super(textField);
		this.textField = textField;
		this.spreadsheet = spreadsheet;
	}

	/**
	 * 
	 */
	public Component getTableCellEditorComponent(JTable table, Object value, boolean isSelected, int row, int column) {

		String location = SpreadsheetTableModel.convertColumn(column) + (row+1);
		textField.setText(spreadsheet.getExpression(location));
		
		textField.selectAll();
		textField.setSelectionStart(0);
		textField.setSelectionEnd(textField.getText().length());
		textField.setCaretPosition(textField.getText().length());

		return textField;
	}


}