package spreadsheet;

public interface IterableSpreadsheetInterface extends SpreadsheetInterface {

	/**
	 * Set the number of times to compute the value stored in each loop cell.
	 * 
	 * @param maxIterationCount
	 *            the maximum number of times to evaluate the loop
	 */
	public void setMaximumIterations(int maxIterationCount);

	/**
	 * Return the number of times to compute the value stored in each loop cell.
	 * 
	 * @return the maximum number of iterations to attempt.
	 */
	public int getMaximumIterations();

	/**
	 * Set the maximum change in value between successive loop iterations,
	 * before a loop is classed as stable.
	 * 
	 * @param epsilon
	 *            the maximum change between successive iterations for a loop to
	 *            be stable.
	 */
	public void setMaximumChange(double epsilon);

	/**
	 * Return the maximum change in value between successive loop iterations
	 * before a loop is classed as stable.
	 * 
	 * @return the maximum change in value between successive iterations for a
	 *         loop to be stable.
	 */
	public double getMaximumChange();

	/**
	 * Recompute the value of all cells whose expressions were changed since the
	 * last call to this method. Propagate the re-computation to ancestor cells
	 * as necessary. For each loop encountered, compute the value of each cell a
	 * maximum of getMaximumIterations() times or until the change in values is
	 * less than or equal to getMaximumChange().
	 */
	public void recomputeWithIteration();
}
