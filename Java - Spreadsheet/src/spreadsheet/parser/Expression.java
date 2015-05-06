package spreadsheet.parser;

import java.io.StringReader;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Vector;
import java.util.regex.Pattern;

public final class Expression {

	/** Empty Set */
	private static final Set<String> EMPTY_SET = new LinkedHashSet<String>();

	/** Regular expression to match row numbers */
	private static final Pattern REGEX_ROW = Pattern.compile("[0-9]+");

	/** Sole Parser instance. */
	private static Parser parser = null;

	/**
	 * Setup and return a parser instance using the specified StringReader
	 * object.
	 * 
	 * @param reader
	 *            a StringReader pointing to the String to be parsed.
	 * @return a Parser object initialised with the specified StringReader.
	 */
	private static Parser getParser(StringReader reader) {
		if (parser == null) {
			synchronized (Parser.class) {
				if (parser == null) {
					parser = new Parser(reader);
				}
			}
		} else {
			parser.ReInit(reader);
		}
		return parser;
	}

	/**
	 * 
	 * @param text
	 * @return a list of assignment-expression pairs {cell-reference,
	 *         expression}.
	 * 
	 * @throws ParseException
	 *             if an invalid command sequence is encountered.
	 */
	public static List<String[]> extractOperations(String text)
			throws ParseException {

		List<String[]> result = new Vector<String[]>();
		try {
			StringTokenizer stStatement = new StringTokenizer(text, ";");
			while (stStatement.hasMoreTokens()) {
				StringTokenizer stAssignment = new StringTokenizer(stStatement
						.nextToken(), "=");
				while (stAssignment.hasMoreTokens()) {
					String left = stAssignment.nextToken().trim();
					String right = stAssignment.nextToken().trim();

					if (isCellLocation(left)) {
						if (isFormula("=" + right)) {
							result.add(new String[] { left, "=" + right });
						} else {
							result.add(new String[] { left, right });
						}

					} else {
						throw new ParseException("Invalid Command: " + left
								+ "=" + right);
					}
				}

			}
		} catch (Exception e) {
			throw new ParseException(e.getMessage());

		} catch (Error e) {
			throw new ParseException(e.getMessage());
		}

		return result;
	}

	/**
	 * Tests if the specified String is a valid cell reference
	 * 
	 * @param text
	 * @return <tt>true</tt> if the specified text is a valid cell reference.
	 */
	public static boolean isCellLocation(String text) {
		if (text == null || text.length() < 2)
			return false;

		try {
			return getParser(new StringReader(text)).testLocation();

		} catch (Error e) {
			// do nothing
		} catch (ParseException e) {
			// do nothing
		}

		return false;
	}

	/**
	 * Tests if the specified String is a valid formula.
	 * 
	 * @param text
	 *            suspected numerical text
	 * @return <tt>true</tt>
	 */
	public static boolean isFormula(String text) {

		if (text == null || text.length() <= 0)
			return false;

		try {
			return getParser(new StringReader(text)).testNumerical();

		} catch (Error e) {
			// do nothing
		} catch (ParseException e) {
			// do nothing
		}

		return false;
	}

	/**
	 * Return a set (without duplicates) containing the names of cell-references
	 * used in the specified formula.
	 * 
	 * @param formula
	 *            the expression from which to extract the cell references.
	 * @return the set of cell-references needed to evaluate the specified
	 *         expression.
	 */
	public static Set<String> getReferences(String formula) {
		Set<String> dependencySet = EMPTY_SET;

		try {
			dependencySet = new LinkedHashSet<String>();

			if (getParser(new StringReader(formula)).build(dependencySet) == Parser.BUILD_OK) {
				return dependencySet;
			}
		} catch (Error e) {
			// error matching a token
		} catch (ParseException e) {
			// do nothing
		}

		return dependencySet;
	}

	/**
	 * If the supplied expression is a valid expression, compute its value (as a
	 * <tt>Double</tt>) using the specified map to substitute values in place
	 * of cell-locations. If for <i>any</i> reason, the expression cannot be
	 * computed e.g. it does conform to the grammar, the value of the
	 * computation is set to original input expression <tt>String</tt> and
	 * returned.
	 * 
	 * @param expression
	 *            expression to be computed.
	 * @param map
	 *            a map from each cell reference in the specified expression to
	 *            a numerical values.
	 * @return the result of the computation.
	 * 
	 * @throws MissingValueException
	 *             if a cell reference in the expression does not contain a
	 *             corresponding value in the specified Map.
	 */
	public static Object computeValue(String expression, Map<String, Double> map) {

		Object value = null;
		try {
			value = getParser(new StringReader(expression)).compute(map);
		} catch (Error e) {
			value = expression;
		} catch (ParseException e) {
			value = expression;
		} catch (MissingValueException e) {
			value = "{" + expression + "}";
		}

		return value;
	}

	/**
	 * Map a cell-reference to coordinates representing horizontal and vertical
	 * displacement (x,y) from the origin (0,0) in the positive quadrant of
	 * cartesian space. e.g. the location A4 is mapped to the point (0,3).
	 * 
	 * (Note: The use of this method is necessary only if your chosen internal
	 * spreadsheet representation resembles a multidimensional array).
	 * 
	 * @param location
	 *            the cell's name
	 * @return the cell's location, relative to the point (0,0).
	 */
	public static int[] getCoordinates(String location) {

		StringBuffer column = new StringBuffer(REGEX_ROW.split(location
				.toLowerCase())[0]);
		String row = location.substring(column.length());

		// to integer (x,y) pt:
		int rowidx = Integer.valueOf(row);
		int colidx = 0;
		for (int i = 0; i < column.length(); i++) {
			char c = column.charAt(i);
			int cidx = c - 'a' + 1; // starts from 1
			int pwr = (column.length() - i - 1);
			colidx += Math.pow(26, pwr) * cidx;
		}
		colidx--;
		rowidx--;

		return new int[] { colidx, rowidx };
	}

}
