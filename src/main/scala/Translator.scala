package translator

class Translator {

	import whiley.WhileyISyntax._

	def translate(p : Program) : String = {
		//translateStm(p.optStms(0))

		var translated = "";
		for (stm <- p.optStms) {
			translated = translated + translateStm(stm) + "\n"
		}
		return translated
	}

	def translateStm(stm : Stm) : String = {
		stm match {
			//case PackageDecl(loc, optDotLocs) =>

			//case ImportDecl(locOrStar, loc, optDotLocOrStars) =>

			case Public(stm) =>
				translateStm(stm)

			case Private(stm) =>
				translateStm(stm)

			case Native(stm) => 
				translateStm(stm)

			case Export(stm) =>
				translateStm(stm)

			//case DeclAsgn(typeField, lVal, optCommTypeLocs, exp, optCommExps) =>

			//case Decl(typ, lVal) =>
			//	return translateType(typ) + translateLVal(lVal) + ';'

			case AsgnStm(assign) =>
				return translateExp(assign) + ';'
/*
			case TypeDecl(loc, typ, optLoc, optWhereExprs) =>

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
			*/
		}
	}
/*
	def translateType() : String = {

	}

	def translateUnionType(unionType : Type) : String = {

	}

	def translateIntrsctnType(intersectionType : Type, optIntersectionTypes : Vector[Type]) : String = {

	}

	def translateTermType() : String = {

	}

	def translateRecType(recordType : Type) : String = {

	}

	def translateRefType(referenceType : Type) : String = {

	}

	def translateArrType(arrayType : Type) : String = {
		
	}

	def translateNegType(negationType : Type) : String = {
		
	}

	def translateFuncType(functionType : Type) : String = {
		
	}

	def translateMthdType(methodType : Type) : String = {
		
	}

	def translateNmnlType(identifier : String) : String = {
		
	}

	def translateMixedType() : String = {

	}

	def translateMix(typeField : Type, loc : LVal) : String = {

	}

	def translateMixFunc(loc : LVal, parameters1 : Parameters, parameters2 : Parameters) : String = {

	}

	def translateMixMthd(loc : LVal, parameters1 : Parameters, parameters2 : Parameters) : String = {

	}

	def translateFunctionType(parameters1 : Parameters, parameters2 : Parameters) : String = {
		
	}

	def translateMethodType(parameters1 : Parameters, parameters2 : Parameters) : String = {
		
	}	
*/
	def translateExp(exp : Exp) : String = {
		exp match {
			//case Iff(leftExp, rightExp) =>

			//case Implies(leftExp, rightExp) =>

			case Or(leftExp, rightExp) =>
				return translateExp(leftExp) + "||" + translateExp(rightExp)

			case Xor(leftExp, rightExp) =>
				return translateExp(leftExp) + "^" + translateExp(rightExp)

			case And(leftExp, rightExp) =>
				return translateExp(leftExp) + "&&" + translateExp(rightExp)

			case BitWiseAnd(leftExp, rightExp) =>
				return translateExp(leftExp) + "&" + translateExp(rightExp)

			case EQ(leftExp, rightExp) =>
				return translateExp(leftExp) + "==" + translateExp(rightExp)

			case NE(leftExp, rightExp) =>
				return translateExp(leftExp) + "!=" + translateExp(rightExp)

			case LT(leftExp, rightExp) =>
				return translateExp(leftExp) + "<" + translateExp(rightExp)

			case LE(leftExp, rightExp) =>
				return translateExp(leftExp) + "<=" + translateExp(rightExp)

			case GT(leftExp, rightExp) =>
				return translateExp(leftExp) + ">" + translateExp(rightExp)

			case GE(leftExp, rightExp) =>
				return translateExp(leftExp) + ">=" + translateExp(rightExp)

			case Lsh(leftExp, rightExp) =>
				return translateExp(leftExp) + "<<" + translateExp(rightExp)

			case ARsh(leftExp, rightExp) =>
				return translateExp(leftExp) + ">>" + translateExp(rightExp)

			case Add(leftExp, rightExp) =>
				return translateExp(leftExp) + "+" + translateExp(rightExp)

			case Sub(leftExp, rightExp) =>
				return translateExp(leftExp) + "-" + translateExp(rightExp)

			case Mul(leftExp, rightExp) =>
				return translateExp(leftExp) + "*" + translateExp(rightExp)

			case Div(leftExp, rightExp) =>
				return translateExp(leftExp) + "/" + translateExp(rightExp)

			case Rem(leftExp, rightExp) =>
				return translateExp(leftExp) + "%" + translateExp(rightExp)

			case Not(exp) =>
				return "!" + translateExp(exp)

			case Neg(exp) =>
				return "-" + translateExp(exp)
/*
			case FunctionCall(loc, exp) =>

			case ArrAccess(exp1, exp2) =>

			case ArrGen(exp1, exp2) =>

			case ArrInit(optExps, exp) =>

			case ExpList(exp1, exp2) =>
*/
			case Assign(lVal, exp) =>
				return translateLVal(lVal) +  " = " + translateExp(exp)
/*
			case Length() =>

			case QuantifierExp(quantExp) =>
*/
			case Use(lVal) =>
				return translateLVal(lVal)

			case Lit(lit) =>
				return translateLit(lit)
		}
	}

	//def translateAssign(lVal, exp) : String = {
		
	//}
/*
	def translateWhereExp(exp : Exp) : String = {

	}

	def translateQuantExp(noSomeAll : NoSomeAll, loc : LVal, exp1 : Exp, optCommLocInExps : Vector[CommLocInExp], exp2 : Exp) : String = {

	}

	def translateNoSomeAll() : String = {

	}

	def translateNo() : String = {

	}

	def translateSome() : String = {
		
	}

	def translateAll() : String = {
		
	}

	def translateElseIf(exp : Exp, optStms : Vector[Stm]) : String = {

	}

	def translateElse(optStms : Vector[Stm]) : String = {

	}

	def translateCaseStm() : String = {

	}

	def translateClass(exp : Exp, optCommExps : Vector[CommExp], optStms : Vector[Stm]) : String = {

	}

	def translateDefaultCase(optStms : Vector[Stm]) : String = {

	}

	def translateRequiresEnsures() : String = {

	}

	def translateRequires(exp : Exp) : String = {

	}

	def translateEnsures(exp : Exp) : String = {

	}

	def translateParameters() : String = {

	}

	def translateParams(typeLoc : TypeLoc, optCommTypeLocs : Vector[CommTypeLoc]) : String = {

	}

	def translateTypeParam(typ : Type) : String = {

	}

	def translateReturnType() : String = {

	}

	def translateRtnParams(parameters : Parameters) : String = {

	}

	def translateRtnType(typ : Type) : String = {

	}
*/
	def translateLit(lit : Literal) : String = {
		lit match {
			case NullLit() =>
				return "NULL"

			case ByteLit(optBits) =>
				return optBits.mkString("")

			case IntLit(intLit) =>
				return translateIntLit(intLit)

			case BoolLit(boolLit) =>
				return translateBoolLit(boolLit)

			case CharLit(charLit) =>
				return translateCharLit(charLit)

			//case StringLit(stringLit) =>
			//	return translateStringLit(stringLit)
		}
	}

	def translateByteLit(byteLit : ByteLit) : String = {
		byteLit match {
			case ByteLit(optBits) =>
				return optBits.mkString("")
		}
	}
	
	def translateIntLit(intLit : Int) : String = {
		return intLit.toString
	}

	def translateBoolLit(boolLit : BooleanLiteral) : String = {
		boolLit match {
			case False() =>
				return "0"

			case True() =>
				return "1"
		}
	}
	
	//def translateFalse() : String = {

	//}

	//def translateTrue() : String = {
		
	//}

	def translateCharLit(char : String) : String = {
		return char
	}

	//def translateStringLit(strLit : StringLiteral) : String = {

	//}

	//def translateNullLit() : String = {

	//}

	def translateLVal(lVal : LVal) : String = {
		lVal match {
			//case FieldAsgn(loc, idn) =>
			//	return translateFieldAsgn(loc, idn)

			//case ListAsgn(loc, exp) =>
			//	return translateListAsgn(loc, exp)

			case Pointer(exp) =>
				return "*" + translateExp(exp)

			case Loc(idn) =>
				return translateLoc(idn)
		}
	}

	def translateLoc(identifier : String) : String = {
		return identifier
	}
/*
	def translateLen(loc : LVal) : String = {

	}

	def translateCommExp(exp : Exp) : String = {

	}

	def translateCommLoc(loc : LVal) : String = {

	}

	def translateCommLocInExp(loc : LVal, exp : Exp) : String = {

	}

	def translateCommLit(lit: Exp) : String = {

	}

	def translateCommTypeLoc(typeLoc : TypeLoc) : String = {

	}

	def translateCommTypeLoc(typeLoc : TypeLoc) : String ={

	}

	def translateDotLoc(loc: LVal) : String = {

	}

	def tranlsateLocOrStar() : String = {

	}

	def translateImpAll() : String = {

	}

	def translateSpcfc(loc : LVal) : String = {

	}

	def translateDotLocOrStar(locOrStar : LocOrStar) : String = {

	}

	def translateTypeLoc(typ : Type, loc : LVal) : String = {

	}
	*/
}