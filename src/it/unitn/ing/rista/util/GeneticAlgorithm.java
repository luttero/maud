/*
 * @(#)GeneticAlgorithm.java created 19/08/2001 Riva Del Garda
 *
 * Copyright (c) 2001 Luca Lutterotti All Rights Reserved.
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

import ec.util.*;
import ec.*;
import ec.simple.*;
import ec.vector.*;

import java.io.IOException;

import it.unitn.ing.rista.interfaces.*;

/**
 *  The GeneticAlgorithm is a class to use Genetic Algorithms
 *
 *
 * @version $Revision: 1.7 $, $Date: 2004/08/12 09:36:10 $
 * @author Luca Lutterotti
 * @since JDK1.1
 */

public class GeneticAlgorithm extends Problem implements SimpleProblemForm {

  EvolutionState state;
  public static final String P_WHICH_PROBLEM = "type";
  public static final String P_GENPROB = "GAproblem";
  public static final int PROB_GENPROB = 0;
  public int problemType = PROB_GENPROB;  // defaults on sdpd
  static java.util.Vector problemToSolve = new java.util.Vector(0, 1);

  public static void evolve(GAProblem structure, String[] args) {

    int inputL = args.length;
    int defL = defaultArgs.length;
    String[] initArgs = new String[inputL + 2 * defL];
    for (int i = 0; i < defL; i++) {
      initArgs[2 * i] = "-p";
      initArgs[2 * i + 1] = defaultArgs[i];
    }
    defL *= 2;
    for (int i = 0; i < inputL; i++) {
      initArgs[defL + i] = args[i];
    }

    EvolutionState state = Evolve.make(initArgs);
    if (state != null) {
      try {
        problemToSolve.addElement(state);
        problemToSolve.addElement(structure);
        state.run(EvolutionState.C_STARTED_FRESH);
      } catch (IOException e) {
        Output.initialError(
                "An IO Exception was generated upon" +
                "starting up, probably in setting up a log" +
                "\nHere it is:\n" + e);
      }
    }
    return;
  }

  public static void cleanUp() {
    if (problemToSolve.size() > 0) {
      problemToSolve.removeAllElements();
      problemToSolve = new java.util.Vector(0, 1);
    }
  }

  // nothing....
  public void setup(final EvolutionState state_, final Parameter base) {
    state = state_;
    String wp = state.parameters.getStringWithDefault(base.push(P_WHICH_PROBLEM),
            null, "");
    if (wp.compareTo(P_GENPROB) == 0)
      problemType = PROB_GENPROB;
    else
      state.output.fatal(
              "Invalid value for parameter, or parameter not found.\n" +
              "Acceptable values are:\n" +
              "  " + P_GENPROB,
              base.push(P_WHICH_PROBLEM));
  }

  public void evaluate(final EvolutionState state,
                       final Individual ind,
                       final int threadnum) {

    if (!(ind instanceof AtomDoubleVectorIndividual))
      state.output.fatal(
              "The individuals for this problem should be AtomDoubleVectorIndividuals.");

    AtomDoubleVectorIndividual temp = (AtomDoubleVectorIndividual) ind;
    double value = 0;

    switch (problemType) {
      case PROB_GENPROB:
        GAProblem problToSolve = null;
        for (int i = 0; i < problemToSolve.size(); i += 2)
          if (state == problemToSolve.elementAt(i))
            problToSolve = (GAProblem) problemToSolve.elementAt(i + 1);
        if (problToSolve != null)
          value = problToSolve.getFitness(temp.genome);
        else
          value = 0.0;
        value = 1.0 / (1.0 + value);
        ((SimpleFitness) (ind.fitness)).setFitness(state, (float) value, value == 1.0);
        break;
      default:
        state.output.fatal(
                "Invalid problem -- how on earth did that happen?");
        break;
    }

    ind.evaluated = true;
  }

  public void describe(final Individual ind,
                       final EvolutionState state,
                       final int threadnum,
                       final int log,
                       final int verbosity) {
    return;
  }

  static String[] defaultArgs = {
// we want store announcements in memory, so...
    "nostore=false",

// totally verbose
    "verbosity=0",

// flush output immediately, don't buffer it
    "flush=true",

// one thread
    "evalthreads=1",
    "breedthreads=1",

// a good random seed for thread 0
    "seed.0=4357",

// ec.EvolutionState
//==============================

// We're not garbage collecting.  If we were, we'd do it every generation,
// and we'd be aggressive, that is, we'd garbage collect until we knew
// there's nothing left to GC
    "gc=false",
    "gc-modulo=1",
    "aggressive=true",

// We're not writing checkpoint files.  If we were, we'd do it every
// generation, and the prefix to all the files would be "ec.*"
    "checkpoint=false",
    "checkpoint-modulo=1",
    "prefix=ec",

    "eval.problem=it.unitn.ing.rista.util.GeneticAlgorithm",
    "eval.problem.type=GAproblem",

    "state=ec.simple.SimpleEvolutionState",
    "init=ec.simple.SimpleInitializer",
    "finish=ec.simple.SimpleFinisher",
    "exch=ec.simple.SimpleExchanger",
    "breed=ec.simple.SimpleBreeder",
    "eval=ec.simple.SimpleEvaluator",
//    "stat=ec.simple.SimpleStatistics",
    "stat=it.unitn.ing.rista.util.GAStatistics",
    "stat.file= $out.stat",
    "generations=100",
    "quit-on-run-complete=true",
    "pop=ec.Population",
    "pop.subpops=1",
    "pop.subpop.0=ec.Subpopulation",
    "pop.subpop.0.duplicate-retries=2",
    "pop.subpop.0.species=ec.vector.AtomFloatVectorSpecies",
    "pop.subpop.0.fitness=ec.simple.SimpleFitness",
    "pop.subpop.0.species.pipe=ec.breed.ForceBreedingPipeline",
    "pop.subpop.0.species.pipe.num-inds=2",
    "pop.subpop.0.species.pipe.source.0=ec.vector.breed.VectorPermutationPipeline",
    "pop.subpop.0.species.pipe.source.0.source.0=ec.vector.breed.VectorMutationPipeline",
    "pop.subpop.0.species.pipe.source.0.source.0.source.0=ec.vector.breed.VectorCrossoverPipeline",
    "pop.subpop.0.species.pipe.source.0.source.0.source.0.source.0=ec.select.TournamentSelection",
    "pop.subpop.0.species.pipe.source.0.source.0.source.0.source.1=same",

    "pop.subpop.0.species.ind=ec.vector.AtomDoubleVectorIndividual",

// you can change these to whatever tickles your fancy
    "pop.subpop.0.species.min-gene=0.0",
    "pop.subpop.0.species.max-gene=1.0",
    "pop.subpop.0.species.genome-size=100",
    "pop.subpop.0.species.chunk-size=3",


    "select.tournament.size=2",
    "pop.subpop.0.species.mutation-prob=0.01",
    "pop.subpop.0.species.permutation-prob=0.01",
    "pop.subpop.0.species.crossover-type=one",

    "pop.subpop.0.size=1000",
    "stat.file=$out.stat",

    "select.tournament.size=2"
  };

}

