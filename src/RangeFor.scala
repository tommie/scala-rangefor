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
import nsc.symtab.Flags
import nsc.transform


/**
 * Compiler plugin to transform
 *
 *   for (i <- X: scala.Range) BLOCK
 *
 * to
 *
 *   var i = X.start; while (i < X.end) { BLOCK; i += X.step }
 *
**/
class RangeFor(val global: Global) extends Plugin {
	import global._
	import posAssigner.atPos

	val name = "rangefor"
	val description = "optimise integer for loops"
	val components = List[PluginComponent](Component)

	/**
	 * A compiler phase that does the actual transform.
	**/
	private object Component extends PluginComponent with transform.Transform with transform.TypingTransformers {
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
				val NAME_FOREACH = nme.foreach
				val NAME_RANGE = newTypeName("Range")
				val NAME_INCLUSIVE = newTypeName("Inclusive")
				val NAME_START = newTermName("start")
				val NAME_END = newTermName("end")
				val NAME_STEP = newTermName("step")

				/**
				 * Generate the actual while loop AST.
				 *
				 * @param a the start value.
				 * @param b the ending value (inclusive or exclusive).
				 * @param func the body of the for loop.
				 * @param cmp the comparator name (must be a function of scala.Int).
				 * @param mod the modifier name (incrementer, that must be a function of scala.Int).
				 * @param modArg the argument to the modifier function, usually the constant 1.
				 * @return the while tree
				**/
				def whileLoopFromRange(range: Tree, func: Function, cmp: Name, mod: Name) = {
					val expr = newTermName(unit.fresh.newName(tree.pos, "rangefor$expr$"))
					val end = newTermName(unit.fresh.newName(tree.pos, "rangefor$end$"))
					val step = newTermName(unit.fresh.newName(tree.pos, "rangefor$step$"))
					val exprSym = currentOwner.newVariable(tree.pos, expr).setInfo(range.tpe)
					val endSym = currentOwner.newVariable(tree.pos, end).setInfo(func.vparams(0).symbol.info)
					val stepSym = currentOwner.newVariable(tree.pos, step).setInfo(func.vparams(0).symbol.info)
					val label = newTermName(unit.fresh.newName(tree.pos, "rangefor$"))
					val labelSym = currentOwner.newLabel(tree.pos, label).setInfo(MethodType(List(), definitions.UnitClass.tpe))
					val loopvarSym = currentOwner.newVariable(tree.pos, func.vparams(0).name).setInfo(func.vparams(0).symbol.info)
					val cmpSym = definitions.getMember(definitions.IntClass, cmp).suchThat(_.tpe.paramTypes(0) == func.vparams(0).symbol.info)
					val modSym = definitions.getMember(definitions.IntClass, mod).suchThat(_.tpe.paramTypes(0) == func.vparams(0).symbol.info)

					atPos(tree.pos) { localTyper.typed {
						Block(
							List(
								ValDef(exprSym, range),
								ValDef(loopvarSym, Select(Ident(exprSym), NAME_START)),
								ValDef(endSym, Select(Ident(exprSym), NAME_END)),
								ValDef(stepSym, Select(Ident(exprSym), NAME_STEP))
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

				def isScalaPackage(sym: Symbol) =
					sym.hasFlag(Flags.PACKAGE | Flags.MODULE) && sym.name == nme.scala_tn

				def isRangeClass(sym: Symbol) =
					sym.isClass && sym.name == NAME_RANGE && isScalaPackage(sym.owner)

				def isInclusiveClass(sym: Symbol) =
					sym.isClass && sym.name == NAME_INCLUSIVE && isRangeClass(sym.owner)

				super.transform(tree match {
				case Apply(Select(src, NAME_FOREACH), List(func: Function)) =>
					src.tpe match {
					case TypeRef(ThisType(scalaSym), rangeSym, List()) if (isScalaPackage(scalaSym) && isRangeClass(rangeSym)) =>
						println("range " + currentOwner)
						whileLoopFromRange(src, func, nme.LT, nme.ADD)

					case TypeRef(SingleType(ThisType(scalaSym), rangeSym), inclusiveSym, List()) if (isScalaPackage(scalaSym)) =>
						println("inclusive " + currentOwner + isRangeClass(rangeSym) + " " + isInclusiveClass(inclusiveSym))
						whileLoopFromRange(src, func, nme.LE, nme.ADD)

					case _ =>
						tree
					}

				case _ =>
					tree
				})
			}
		}
	}
}
