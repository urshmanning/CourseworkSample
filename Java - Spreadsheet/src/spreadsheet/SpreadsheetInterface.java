package spreadsheet;

public interface SpreadsheetInterface {

public static final String LOOP = "#LOOP";
// loop Error Value 

public int getColumnCount();
// returns the number of columns in the spreadsheet.

public int getRowCount();
// returns the number of rows in the spreadsheet.

public void setExpression(String location, String expression);
// sets the expression of the cell at location and attempts to
// compute its current value.  If the cell has parents on which
// it depends then it is no longer their child.

public String getExpression(String location);
// returns the expression stored at the cell at location.

public Object getValue(String location);
// returns the value associated with the computed stored expression.

public void recompute();
// computes the value of all cells whose expressions were changed since the
// last call to this method. This also requires the computation of the values 
// of any dependent cells.  Cells forming a loop are assigned the value 
// SpreadsheetInterface.LOOP.
}
