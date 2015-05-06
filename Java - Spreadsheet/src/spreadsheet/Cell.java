package spreadsheet;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import spreadsheet.parser.Expression;

public class Cell {

	private final Spreadsheet spreadsheet;
	private final String location;
	private final List<Cell> parents, children;
	private String expression;
	private Object value;
	private boolean isPartOfLoop;

	public Cell(String location, String expression, Spreadsheet spreadsheet) {
		this.spreadsheet = spreadsheet;
		this.location = location;
		
		parents = new ArrayList<Cell>();
		children = new ArrayList<Cell>();
		
		setExpression(expression);
	}

	public String getExpression() {
		return expression;
	}
	
	public void setExpression(String expression) {
		this.expression = expression;
		isPartOfLoop = false;

		for (Cell parent : parents) {
			parent.removeChild(this);
		}
		parents.clear();
		if (!Expression.isFormula(expression)) {
			return;
		}

		Set<String> parentsReferences = Expression.getReferences(expression);
		for (String parentReference : parentsReferences) {
			if (!spreadsheet.contains(parentReference) ){
				parents.clear();
				return;
			}
			parents.add(spreadsheet.getCell(parentReference));
		}
		for (Cell parent : parents) {
			parent.addChild(this);
		}
	}

	public Object getValue() {
		return isPartOfLoop ? SpreadsheetInterface.LOOP : value;
	}
	
	public void setValue(Object value) {
		this.value = value;
		if (SpreadsheetInterface.LOOP.equals(value)) {
			setToLoop();
		}
	}
	
	public String getLocation() {
		return location;
	}

	public List<Cell> getChildren() {
		return children;
	}
	
	public boolean isPartOfLoop() {
		return isPartOfLoop;
	}
	
	public void setToLoop() {
		isPartOfLoop = true;
	}
	
	public void setToNotLoop() {
		isPartOfLoop = false;
	}

	private void addChild(Cell child) {
		children.add(child);
	}

	private void removeChild(Cell child) {
		children.remove(child);
	}
}
