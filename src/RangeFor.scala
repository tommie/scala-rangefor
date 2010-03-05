/*
 * Copyright (c) 2006-2010 Spotify AB
 *
 * @author Tommie Gannert
 * @date 2010-02-22
 */

package rangefor

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.transform


/**
 * Compiler plugin to transform
 *
 *   for (i <- A to|until B [by C]) BLOCK
 *
 * into
 *
 *   var i = A; while (i <|<= B) { BLOCK; i += C }
 *
 * if A happens to be an intWrapper-wrapped term. This is a simple
 * optimization for a special case that makes Scala code much easier to read.
 *
 * @author Tommie Gannert
 * @date 2010-02-23
**/
class RangeFor(val global: Global) extends Plugin {
	import global._

	val name = "rangefor"
	val description = "optimise integer for loops"
	val components = List[PluginComponent](Component)

	/**
	 * A compiler phase that does the actual transform.
	**/
	private object Component extends PluginComponent with transform.Transform with transform.TypingTransformers {
		import posAssigner.atPos

		val global: RangeFor.this.global.type = RangeFor.this.global
		val phaseName = RangeFor.this.name
		val runsAfter = "refchecks"

		def newTransformer(unit: CompilationUnit) = new RangeForTransformer(unit)

		/**
		 * This transformer rewrites Ident(X) to point to the newly created
		 * loop variable symbol.
		**/
		class IdentTransformer(unit: CompilationUnit, sym: Symbol) extends TypingTransformer(unit) {
			override def transform(tree: Tree): Tree = {
				import posAssigner.atPos
				val NAME = sym.name

				super.transform(tree match {
				case Ident(NAME) =>
					atPos(tree.pos) { localTyper.typed { Ident(sym) } }

				case _ =>
					tree
				})
			}
		}

		/**
		 * The core transformer, finding matches to the for loop and replacing
		 * it with an equivalent while loop.
		**/
		class RangeForTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
			override def transform(tree: Tree): Tree = {
				val NAME_BY = newTermName("by")
				val NAME_INT_WRAPPER = newTermName("intWrapper")

				/**
				 * Generate the actual while loop AST.
				 *
				 * @param a the start value.
				 * @param b the ending value (inclusive or exclusive).
				 * @param func the body of the for loop.
				 * @param cmp the comparator name (must be a function of scala.Int).
				 * @param mod the modifier name (incrementer, that must be a function of scala.Int).
				 * @param step the argument to the modifier function, usually the constant 1.
				 * @return the while tree
				**/
				def whileLoop(a: Tree, b: Tree, func: Function, cmp: Name, mod: Name, step: Tree) = {
					val endName = newTermName(unit.fresh.newName(tree.pos, "rangefor$end$"))
					val stepName = newTermName(unit.fresh.newName(tree.pos, "rangefor$step$"))
					val endSym = currentOwner.newVariable(tree.pos, endName).setInfo(b.tpe.underlying)
					val stepSym = currentOwner.newVariable(tree.pos, stepName).setInfo(step.tpe.underlying)
					val label = newTermName(unit.fresh.newName(tree.pos, "rangefor$"))
					val labelSym = currentOwner.newLabel(tree.pos, label).setInfo(MethodType(List(), definitions.UnitClass.tpe))
					val loopvarSym = currentOwner.newVariable(tree.pos, func.vparams(0).name).setInfo(func.vparams(0).symbol.info)
					val cmpSym = definitions.getMember(definitions.IntClass, cmp).suchThat(_.tpe.paramTypes(0) == a.tpe.underlying)
					val modSym = definitions.getMember(definitions.IntClass, mod).suchThat(_.tpe.paramTypes(0) == a.tpe.underlying)

					atPos(tree.pos) { localTyper.typed {
						Block(
							List(
								ValDef(endSym, b),
								ValDef(stepSym, step),
								ValDef(loopvarSym, a)
							),
							LabelDef(
								labelSym,
								List(),
								If(
									Apply(
										Select(Ident(loopvarSym), cmpSym.name).setSymbol(cmpSym),
										List(Ident(endSym))
									),
									Block(
										List(
											Block(
												List(
													new IdentTransformer(unit, loopvarSym).transform(func.body)
												),
												Assign(
													Ident(loopvarSym),
													Apply(
														Select(Ident(loopvarSym), modSym.name).setSymbol(modSym),
														List(Ident(stepSym))
													)
												)
											)
										),
										Apply(
											Ident(labelSym),
											List()
										)
									),
									Literal()
								)
							)
						)
					} }
				}

				/**
				 * Return a comparator name depending on the range builder.
				**/
				def nameToCmp(name: Name) = name.toString match {
					case "to" => nme.LE
					case "until" => nme.LT
					case _ => null
				}

				/**
				 * Return the while loop if the how is recognizedm otherwise return the original tree.
				**/
				def whileOrPass(a: Tree, b: Tree, func: Function, how: Name, step: Tree) = {
					val cmp = nameToCmp(how)

					if (cmp eq null) tree else whileLoop(a, b, func, cmp, nme.ADD, step)
				}

				super.transform(tree match {
				case Apply(Select(Apply(Select(Apply(Select(Apply(Select(Select(This(nme.scala_tn), nme.Predef), NAME_INT_WRAPPER), List(a)), how), List(b)), NAME_BY), List(c)), nme.foreach), List(func: Function)) =>
					whileOrPass(a, b, func, how, c)

				case Apply(Select(Apply(Select(Apply(Select(Select(This(nme.scala_tn), nme.Predef), NAME_INT_WRAPPER), List(a)), how), List(b)), nme.foreach), List(func: Function)) =>
					whileOrPass(a, b, func, how, atPos(tree.pos) { localTyper typed { Literal(Constant(1)) } })

				case _ =>
					tree
				})
			}
		}
	}
}
