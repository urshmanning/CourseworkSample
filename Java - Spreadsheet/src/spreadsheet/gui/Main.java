package spreadsheet.gui;

// Uncomment if you do the unassessed part of the exercise
// import spreadsheet.IterableSpreadsheet;
// 
import spreadsheet.Spreadsheet;
import spreadsheet.SpreadsheetInterface;

public class Main {

	private static final int DEFAULT_NUM_ROWS = 25;

	private static final int DEFAULT_NUM_COLUMNS = 8;

	/**
	 * Run the spreadsheet application.
	 * 
	 * @param args
	 *            command-line arguments
	 */
	public static void main(String[] args) {

		int columns, rows;

		try {
			columns = Integer.parseInt(args[0]);
			rows = Integer.parseInt(args[1]);

			if (rows <= 0 || columns <= 0) {
				throw new IndexOutOfBoundsException(
						"Invalid Spreadsheet Dimensions.");
			}
		} catch (Exception e) {
			System.out.println("Using Default Spreadsheet Dimensions.");

			columns = DEFAULT_NUM_COLUMNS;
			rows = DEFAULT_NUM_ROWS;
		}

		System.out.println("Initializing Spreadsheet GUI with " + columns
				+ " columns and " + rows + " rows...");

		final SpreadsheetInterface spreadsheet = new Spreadsheet(rows, columns);
		
		javax.swing.SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				new SpreadsheetGUI("Spreadsheet GUI", spreadsheet); 
			}
		});
		

	}

}
