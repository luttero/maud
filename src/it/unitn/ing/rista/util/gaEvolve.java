/*
 * @(#)gaEvolve.java created Mar 25, 2003 Mesiano
 *
 * Copyright (c) 2003 Luca Lutterotti All Rights Reserved.
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

import ec.EvolutionState;
import ec.Evolve;
import ec.util.*;

/**
 *  The gaEvolve is a
 *
 *
 * @version $Revision: 1.3 $, $Date: 2004/08/12 09:36:10 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class gaEvolve extends Evolve {
	public static EvolutionState make() {

		EvolutionState state = null;
		ParameterDatabase parameters = null;
		Output output;
		MersenneTwisterFast[] random;
		int[] seeds;
		int breedthreads = 1;
		int evalthreads = 1;
		int verbosity;
		boolean store;
		int x;


		parameters = new ParameterDatabase();
		parameters.set(new Parameter("verbosity"), "0");
		parameters.set(new Parameter("nostore"), "true");
		parameters.set(new Parameter("flush"), "true");
		parameters.set(new Parameter("breedthreads"), "1");
		parameters.set(new Parameter("evalthreads"), "1");
		parameters.set(new Parameter("seed.0"), "4357");
		parameters.set(new Parameter("state"), "ec.simple.SimpleEvolutionState");


		// 1. create the output
		store = parameters.getBoolean(new Parameter(P_STORE), null, false);

		verbosity = parameters.getInt(new Parameter(P_VERBOSITY), null, 0);
		if (verbosity < 0)
			Output.initialError("Verbosity should be an integer >= 0.\n", new Parameter(P_VERBOSITY));

		output = new Output(store, verbosity);
		output.setFlush(parameters.getBoolean(new Parameter(P_FLUSH), null, false));


		// stdout is always log #0.  stderr is always log #1.
		// stderr accepts announcements, and both are fully verbose
		// by default.
		output.addLog(ec.util.Log.D_STDOUT, Output.V_VERBOSE, false);
		output.addLog(ec.util.Log.D_STDERR, Output.V_VERBOSE, true);



		// 2. set up thread values

		breedthreads = parameters.getInt(new Parameter(P_BREEDTHREADS), null, 1);

		if (breedthreads < 1)
			Output.initialError("Number of breeding threads should be an integer >0.", new Parameter(P_BREEDTHREADS));

		evalthreads = parameters.getInt(new Parameter(P_EVALTHREADS), null, 1);

		if (evalthreads < 1)
			Output.initialError("Number of eval threads should be an integer >0.", new Parameter(P_EVALTHREADS));


		// 3. create the Mersenne Twister random number generators,
		// one per thread

		random = new MersenneTwisterFast[breedthreads > evalthreads ? breedthreads : evalthreads];
		seeds = new int[breedthreads > evalthreads ? breedthreads : evalthreads];

		int time = (int) System.currentTimeMillis();  // safe because we're getting low-order bits
		for (x = 0; x < random.length; x++) {
			int seed = 1;
			String tmp_s = parameters.getString(new Parameter(P_SEED).push("" + x), null);
			if (tmp_s.equalsIgnoreCase(V_SEED_TIME)) {
				seed = (int) System.currentTimeMillis(); // safe because we're getting low-order bits
				if (seed == 0)
					Output.initialError("Whoa! This Java version is returning 0 for System.currentTimeMillis(), which ain't right.  This means you can't use '" + V_SEED_TIME + "' as a seed ", new Parameter(P_SEED).push("" + x));
			} else {
				seed = parameters.getIntWithDefault(new Parameter(P_SEED).push("" + x), null, 0);
				if (seed == 0)
					Output.initialError("Seed should be an integer not equal to 0.", new Parameter(P_SEED).push("" + x));
			}
			seeds[x] = seed;
		}

		for (x = 0; x < random.length; x++) {
			for (int y = x + 1; y < random.length; y++)
				if (seeds[x] == seeds[y]) {
					Output.initialError(P_SEED + "." + x + " (" + seeds[x] + ") and " + P_SEED + "." + y + " (" + seeds[y] + ") ought not be the same seed.");
				}
			random[x] = new MersenneTwisterFast(seeds[x]);
		}

		// 4.  Start up the evolution

		// what evolution state to use?
		state = (EvolutionState)
			parameters.getInstanceForParameter(new Parameter(P_STATE), null, EvolutionState.class);
		state.parameters = parameters;
		state.random = random;
		state.output = output;
		state.evalthreads = evalthreads;
		state.breedthreads = breedthreads;

		output.systemMessage(Version.message());

		return state;
	}
}
