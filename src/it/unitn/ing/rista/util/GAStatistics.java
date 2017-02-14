/*
* @(#)GAStatistics.java created 29/10/2002 Mesiano
*
* Copyright (c) 2002 Luca Lutterotti All Rights Reserved.
*
* This software is the research result of Luca Lutterotti and it is
* provided as it is as confidential and proprietary information.
* You shall not disclose such Confidential Information and shall use
* it only in accordance with the terms of the license agreement you
* entered into with the author.
*
* THE AUTHOR MAKES NO REPRESENTATIONS OR WARRANTIES ABOUT THE SUITABILITY OF THE
* SOFTWARE, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
* IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
* PURPOSE, OR NON-INFRINGEMENT. THE AUTHOR SHALL NOT BE LIABLE FOR ANY DAMAGES
* SUFFERED BY LICENSEE AS A RESULT OF USING, MODIFYING OR DISTRIBUTING
* THIS SOFTWARE OR ITS DERIVATIVES.
*
*/

package it.unitn.ing.rista.util;

import ec.*;
import ec.util.Output;
import ec.util.Parameter;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.io.File;
import java.io.IOException;

/**
 *  The GAStatistics is a simple implementation of the statistical logging facility
 *
 *
 * @version $Revision: 1.12 $, $Date: 2005/03/10 13:50:40 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GAStatistics extends Statistics {
	/** log file parameter */
	public static final String P_STATISTICS_FILE = "file";

	/** The Statistics' log */
	public int statisticslog;

	/** The best individual we've found so far */
	public Individual[] best_of_run;
	public Individual[] worst_of_run;

//////////////////////////////////////////////

	private PropertyChangeSupport changes;
//	private GeneticAlgorithmPlot GAPlot;

//////////////////////////////////////////////

	public GAStatistics() {
		best_of_run = null;
		statisticslog = 0; /* stdout */
		changes = new PropertyChangeSupport(this);
	}

	public void addPropertyChangeListener(PropertyChangeListener l) {
		changes.addPropertyChangeListener(l);
	}

	public void removePropertyChangeListener(PropertyChangeListener l) {
		changes.removePropertyChangeListener(l);
	}

	public void setup(final EvolutionState state, final Parameter base) {
		super.setup(state, base);

		File statisticsFile = state.parameters.getFile(
			base.push(P_STATISTICS_FILE), null);

		if (statisticsFile != null)
			try {
				statisticslog = state.output.addLog(statisticsFile, Output.V_NO_GENERAL - 1, false, true);
			} catch (IOException i) {
				state.output.fatal("An IOException occurred while trying to create the log " + statisticsFile + ":\n" + i);
			}


	}

	public void postInitializationStatistics(final EvolutionState state) {
		super.postInitializationStatistics(state);

		// set up our best_of_run array -- can't do this in setup, because
		// we don't know if the number of subpopulations has been determined yet
		best_of_run = new Individual[state.population.subpops.length];
		worst_of_run = new Individual[state.population.subpops.length];


//////////////////////////////////////////////

//		GAPlot = new GeneticAlgorithmPlot(new Frame(), state);
		//GAPlot.setPlotRange(0, state.numGenerations, 0, 1.0);
//		addPropertyChangeListener(GAPlot);


//////////////////////////////////////////////

	}

	/** Logs the best individual of the generation. */
	public void postEvaluationStatistics(final EvolutionState state) {
		super.postEvaluationStatistics(state);

		// for now we just print the best fitness per subpopulation.
		Individual[] best_i = new Individual[state.population.subpops.length];  // quiets compiler complaints
		Individual[] worst_i = new Individual[state.population.subpops.length];
		//Individual[] avg_i = new Individual[state.population.subpops.length];
		//Individual[] all_i = new Individual[state.population.subpops.length][state.population.subpops[0].length];
		for (int x = 0; x < state.population.subpops.length; x++) {
			best_i[x] = state.population.subpops[x].individuals[0];
			worst_i[x] = state.population.subpops[x].individuals[0];
			for (int y = 1; y < state.population.subpops[x].individuals.length; y++) {
				if (state.population.subpops[x].individuals[y].fitness.betterThan(best_i[x].fitness))
					best_i[x] = state.population.subpops[x].individuals[y];
				if (worst_i[x].fitness.betterThan(state.population.subpops[x].individuals[y].fitness))
					worst_i[x] = state.population.subpops[x].individuals[y];
			}

			// now test to see if it's the new best_of_run
			if (best_of_run[x] == null || best_i[x].fitness.betterThan(best_of_run[x].fitness))
				best_of_run[x] = (Individual) best_i[x].protoCloneSimple();
			if (worst_of_run[x] == null || worst_of_run[x].fitness.betterThan(worst_i[x].fitness))
				worst_of_run[x] = (Individual) worst_i[x].protoCloneSimple();

//			double WSS = 1.0 / best_i[x].fitness.fitness() - 1.0; // get the real WSS
//			GAPlot.addPoint(state.generation, WSS);
//			changes.firePropertyChange("dataModified", new Integer(0), new Integer(1));
		}

	}

	/** Logs the best individual of the run. */
	public void finalStatistics(final EvolutionState state, final int result) {
		super.finalStatistics(state, result);

		// for now we just print the best fitness

		state.output.println("\nBest Individual of Run:", Output.V_NO_GENERAL, statisticslog);
		for (int x = 0; x < state.population.subpops.length; x++)
			best_of_run[x].printIndividualForHumans(state, statisticslog, Output.V_NO_GENERAL);
	}
}

