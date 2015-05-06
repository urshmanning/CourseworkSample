package spreadsheet;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

import spreadsheet.parser.Expression;

public class Spreadsheet implements SpreadsheetInterface {

	private final Map<String, Cell> cells;
	private final Queue<Cell> changedCells;
	private final Map<String, Double> values;
	private final int rows, columns;

	public Spreadsheet(int rows, int columns) {
		this.rows = rows;
		this.columns = columns;
		
		cells = new HashMap<String, Cell>();
		changedCells = new LinkedList<Cell>();
		values = new HashMap<String, Double>();
	}

	@Override
	public int getRowCount() {
		return rows;
	}

	@Override
	public int getColumnCount() {
		return columns;
	}

	@Override
	public String getExpression(String location) {
		return getCell(location).getExpression();
	}

	@Override
	// Pre: location is within the bounds of the spreadsheet.
	public void setExpression(String location, String expression) {
		Cell cell = getCell(location);
		cell.setExpression(expression);
		if (!loopCheck(cell, new ArrayList<Cell>())) {
			changedCells.add(cell);
		}
	}

	@Override
	public Object getValue(String location) {
		return cells.containsKey(location) ? getCell(location).getValue() : "";
	}

	@Override
	public void recompute() {
		while (!changedCells.isEmpty()) {
			Cell cell = changedCells.poll();
			computeValue(cell);
			if (!cell.isPartOfLoop()) {
				changedCells.addAll(cell.getChildren());
			}
		}
	}
	
	public void computeValue(Cell cell) {
		Object value = Expression.computeValue(cell.getExpression(), values);
		if (value instanceof Double) {
			values.put(cell.getLocation(), (Double) value);
		}
		cell.setValue(value);
	}

	protected boolean loopCheck(Cell cell, List<Cell> path) {
		cell.setToNotLoop();
		if (path.contains(cell)) {
			for (Cell cellOnPath : path) {
				cellOnPath.setToLoop();
				values.remove(cellOnPath.getLocation());
				changedCells.add(cellOnPath);
			}
			return true;
		}
		for (Cell child : cell.getChildren()) {
			List<Cell> newPath = new ArrayList<Cell>();
			newPath.addAll(path);
			newPath.add(cell);
			if (loopCheck(child, newPath)) {
				return true;
			}
		}
		return false;
	}

	// Post: returns the cell if it is within the bounds, null otherwise.
	protected Cell getCell(String location) {
		if (!cells.containsKey(location) && contains(location)) {
			cells.put(location, new Cell(location, "", this));
		}
		return cells.get(location);
	}

	protected boolean contains(String location) {
		int[] coords = Expression.getCoordinates(location);
		return coords[0] >= 0 && coords[0] < columns && coords[1] >= 0
				&& coords[1] < rows;
	}
}
