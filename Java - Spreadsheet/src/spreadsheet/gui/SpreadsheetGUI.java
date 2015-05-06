package spreadsheet.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.List;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JToolBar;
import javax.swing.WindowConstants;
import javax.swing.event.CellEditorListener;
import javax.swing.table.TableColumn;

import spreadsheet.IterableSpreadsheetInterface;
import spreadsheet.SpreadsheetInterface;
import spreadsheet.parser.Expression;
import spreadsheet.parser.ParseException;

/**
 * This is the graphical user interface class for the Spreadsheet.
 */
public final class SpreadsheetGUI extends JFrame {

	private static final Dimension FRAME_SIZE = new Dimension(640, 480);

	private static final int FIRST_COLUMN_WIDTH = 35;

	public static final double DEFAULT_EPSILON = 1e-10;

	public static final int DEFAULT_MAX_ITERATIONS = 1000;

	private JTable table;

	private JCheckBox autoComputeCheckBox;

	private SpreadsheetTableModel tableModel;

	private SpreadsheetInterface spreadsheet;

	private boolean isIterable;

	public SpreadsheetGUI(String title, SpreadsheetInterface spreadsheet) {
		super(title);

		this.spreadsheet = spreadsheet;
		isIterable = spreadsheet instanceof IterableSpreadsheetInterface;

		setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		// get content pane
		setLayout(new BorderLayout());

		JToolBar bar = new JToolBar("Input Expression");
		add(bar, BorderLayout.NORTH);

		bar.setLayout(new GridBagLayout());
		GridBagConstraints c = new GridBagConstraints();
		c.fill = GridBagConstraints.HORIZONTAL;

		JLabel label = new JLabel("Expression: ");
		c.weightx = 0;
		c.gridx = 0;
		c.gridy = 0;
		bar.add(label, c);

		final JTextField expressionInput = new JTextField();

		c.gridx = 1;
		c.weightx = 0.5;
		bar.add(expressionInput, c);

		JButton expressionButton = new JButton("OK");
		c.fill = GridBagConstraints.HORIZONTAL;
		c.gridx = 2;
		c.weightx = 0;
		bar.add(expressionButton, c);

		bar.addSeparator();

		if (isIterable) {
			JLabel maxIterationsLabel = new JLabel("N: ");
			final JTextField maxIterationsInput = new JTextField();
			maxIterationsInput.addFocusListener(new FocusListener() {
				public void focusGained(FocusEvent e) {
				};

				public void focusLost(FocusEvent event) {
					IterableSpreadsheetInterface iterableSpreadsheet = (IterableSpreadsheetInterface) SpreadsheetGUI.this.spreadsheet;
					try {
						int maxIterations = Integer.valueOf(maxIterationsInput
								.getText());
						if (maxIterations <= 0) {
							maxIterations = DEFAULT_MAX_ITERATIONS;
							maxIterationsInput.setText(String
									.valueOf(DEFAULT_MAX_ITERATIONS));
						}
						iterableSpreadsheet.setMaximumIterations(maxIterations);
					} catch (Exception e) {
						iterableSpreadsheet
								.setMaximumIterations(DEFAULT_MAX_ITERATIONS);
						maxIterationsInput.setText(String
								.valueOf(DEFAULT_MAX_ITERATIONS));
					}
				};
			});

			maxIterationsInput.setText(String.valueOf(DEFAULT_MAX_ITERATIONS));
			((IterableSpreadsheetInterface) spreadsheet)
					.setMaximumIterations(DEFAULT_MAX_ITERATIONS);

			int defaultHeight = maxIterationsInput.getPreferredSize().height;
			Dimension iterationBoxSize = new Dimension(50, defaultHeight);

			maxIterationsInput.setPreferredSize(iterationBoxSize);

			JLabel epsilonLabel = new JLabel("\u03b5: ");
			final JTextField epsilonInput = new JTextField();
			epsilonInput.addFocusListener(new FocusListener() {
				public void focusGained(FocusEvent e) {
				}

				public void focusLost(FocusEvent event) {
					IterableSpreadsheetInterface iterableSpreadsheet = (IterableSpreadsheetInterface) SpreadsheetGUI.this.spreadsheet;
					try {
						double epsilon = Double.parseDouble(epsilonInput.getText());
						iterableSpreadsheet.setMaximumChange(epsilon);
					} catch (Exception e) {
						iterableSpreadsheet.setMaximumChange(DEFAULT_EPSILON);
						epsilonInput.setText(String.valueOf(DEFAULT_EPSILON));
					}
				}
			});
			epsilonInput.setText(String.valueOf(DEFAULT_EPSILON));
			((IterableSpreadsheetInterface) spreadsheet)
					.setMaximumChange(DEFAULT_EPSILON);
			epsilonInput.setPreferredSize(iterationBoxSize);

			bar.add(maxIterationsLabel);
			bar.add(maxIterationsInput);
			bar.add(epsilonLabel);
			bar.add(epsilonInput);
			bar.addSeparator();

			final JButton computeButton = new JButton("Compute");
			computeButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					computeButton.setEnabled(false);
					SpreadsheetGUI.this.recompute();
					computeButton.setEnabled(true);
				}
			});
			bar.add(computeButton);
		} else {
			autoComputeCheckBox = new JCheckBox();
			autoComputeCheckBox
					.setToolTipText("Automatically Re-compute Spreadsheet");
			bar.add(autoComputeCheckBox);

			final JButton computeButton = new JButton("Compute");
			computeButton.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					SpreadsheetGUI.this.recompute();
				}
			});
			bar.add(computeButton);

			// the auto-compute check box listener
			autoComputeCheckBox.addActionListener(new ActionListener() {
				public void actionPerformed(ActionEvent e) {
					JCheckBox jcb = (JCheckBox) e.getSource();
					computeButton.setEnabled(!jcb.isSelected());
				}
			});

			autoComputeCheckBox.setSelected(true);
			computeButton.setEnabled(!autoComputeCheckBox.isSelected());
		}

		// the textbox listener
		ActionListener textListener = new ActionListener() {
			public void actionPerformed(ActionEvent event) {
				String text = expressionInput.getText();
				try {
					List<String[]> ops = Expression.extractOperations(text);
					for (String[] cmd : ops) {
						SpreadsheetGUI.this.spreadsheet.setExpression(cmd[0],
								cmd[1]);
					}
					expressionInput.setText("");
					if (isIterable || autoComputeCheckBox.isSelected()) {
						SpreadsheetGUI.this.recompute();
					}
					// computed one cell only - redraw it
					tableModel.fireTableDataChanged();
				} catch (ParseException e) {
					// error occured
					JOptionPane.showMessageDialog(SpreadsheetGUI.this,
							"Invalid Command Sequence Entered!", "Input Error",
							JOptionPane.ERROR_MESSAGE);
					expressionInput.selectAll();
				}
			}
		};
		expressionInput.addActionListener(textListener);
		expressionButton.addActionListener(textListener);

		// create the table
		tableModel = new SpreadsheetTableModel(spreadsheet);
		table = new JTable(tableModel);
		table.setAutoCreateColumnsFromModel(false);

		SpreadsheetCellEditor sce = new SpreadsheetCellEditor(new JTextField(),
				spreadsheet);

		sce.addCellEditorListener(new CellEditorListener() {
			public void editingCanceled(javax.swing.event.ChangeEvent e) {
			};

			public void editingStopped(javax.swing.event.ChangeEvent e) {
				if (isIterable || autoComputeCheckBox.isSelected()) {
					SpreadsheetGUI.this.recompute();
				}
			};
		});

		table.setDefaultEditor(Object.class, sce);
		table.setDefaultRenderer(Object.class, new SpreadsheetCellRenderer(
				spreadsheet, table.getFont()));

		TableColumn firstColumn = table.getColumnModel().getColumn(0);
		firstColumn.setMinWidth(FIRST_COLUMN_WIDTH);
		firstColumn.setPreferredWidth(FIRST_COLUMN_WIDTH);

		// set location and initial size
		table.setPreferredScrollableViewportSize(FRAME_SIZE);
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		setLocation((screenSize.width - FRAME_SIZE.width) / 2,
				(screenSize.height - FRAME_SIZE.height) / 2);

		// other preferences
		table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
		table.getTableHeader().setReorderingAllowed(false);

		// set up the scrollbars
		// setContentPane(new JScrollPane(table));
		add(new JScrollPane(table), BorderLayout.CENTER);

		pack();
		setVisible(true);
	}

	private synchronized void recompute() {
		try{
			if(isIterable){
				((IterableSpreadsheetInterface)spreadsheet).recomputeWithIteration();	
			}else{
				spreadsheet.recompute();
			}
			
			tableModel.fireTableDataChanged();
		}catch(Exception e){
			// error occured
			JOptionPane.showMessageDialog(SpreadsheetGUI.this,
					"Invalid Command Encountered!", "Input Error",
					JOptionPane.ERROR_MESSAGE);
			
		}
	}

}
