package translator

class Translator {

	import whiley.WhileyISyntax._

	def translate(p : Program) : String = {
		translateStm(p.optStms(0))
	}

	def translateStm(stm : Stm) : String = {
		stm match {
			case PackageDecl(loc, optDotLocs) =>

			case ImportDecl(locOrStar, loc, optDotLocOrStars) =>

			case Public(stm) =>

			case Private(stm) =>

			case Native(stm) => 

			case Export(stm) =>

			case DeclAsgn(typeField, lVal, optCommTypeLocs, exp, optCommExps) =>

			case Decl(typeField, lVal) =>

			case AsgnStm(assign) =>

			case TypeDecl(loc, typeField, optLoc, optWhereExprs) =>

			case ConstDecl(loc, exp) =>

			case If(exp, optStms, optElseIfs, optElse) =>

			case Switch(exp, optCaseStms) =>

			case While(exp, optWhereExprs, optStms) =>

			case DoWhile(optStms, exp, optWhereExprs) =>

			case FnDecl(loc, optParameters, optReturnType, optRequiresEnsuress, optStms) =>

			case MthdDecl(loc, optParameters, optReturnType, optRequiresEnsuress, optStms) =>

			case RtnStm(exp, optCommExps) =>

			case Assert(exp) =>
			
			case Assume(exp) =>

			case DebugExp(exp) =>

			case SkipStm() =>

			case BreakStm() =>

			case ContStm() =>

			case FailStm() =>
		}
	}

	def translateExp(exp : Exp) : String = {
		exp match {
			case Iff(leftExp, rightExp) =>

			case Implies(leftExp, rightExp) =>

			case Or(leftExp, rightExp) =>

			case Xor(leftExp, rightExp) =>

			case And(leftExp, rightExp) =>

			case BitWiseAnd(leftExp, rightExp) =>

			case EQ(leftExp, rightExp) =>

			case NE(leftExp, rightExp) =>

			case LT(leftExp, rightExp) =>

			case LE(leftExp, rightExp) =>

			case GT(leftExp, rightExp) =>

			case GE(leftExp, rightExp) =>

			case Lsh(leftExp, rightExp) =>

			case ARsh(leftExp, rightExp) =>

			case Add(leftExp, rightExp) =>

			case Sub(leftExp, rightExp) =>

			case Mul(leftExp, rightExp) =>

			case Div(leftExp, rightExp) =>

			case Rem(leftExp, rightExp) =>

			case Not(exp) =>

			case Neg(exp) =>

			case FunctionCall(loc, exp) =>

			case ArrAccess(exp1, exp2) =>

			case ArrGen(exp1, exp2) =>

			case ArrInit(optExps, exp) =>

			case ExpList(exp1, exp2) =>

			case QuantifierExp(quantExp) =>

			case Use(loc) =>
		}
	}
}