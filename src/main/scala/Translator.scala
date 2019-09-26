package translator

class Translator {

	import whiley.WhileyISyntax._

	def translate(p : Program) : String = {
        return translateStms(p.optStms)
	}

    def translateStms(stms : Vector[Stm]) : String = {
        var translated = "";
		for (stm <- stms) {
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

			//case DeclAsgn(typ, lVal, optCommTypeLocs, exp, optCommExps) =>

			case Decl(typ, loc) =>
				return translateType(typ) + " " + translateLoc(loc) + ';'

			case AsgnStm(assign) =>
				return translateExp(assign) + ';'
/*
			case TypeDecl(loc, typ, optLoc, optWhereExprs) =>

			case ConstDecl(loc, exp) =>
*/
			case If(exp, optStms, optElseIfs, optElse) =>
                return "if (" + translateExp(exp) + ")" + "\n{\n" + translateStms(optStms) + "}\n" + translateElseIfVector(optElseIfs) + translateElse(optElse)

			case Switch(exp, optCaseStms) =>
                return "switch (" + translateExp(exp) + ")" + "\n{\n" + translateCaseStmVector(optCaseStms) + "}\n" 

			case While(exp, optWhereExprs, optStms) =>
                return "while (" + translateExp(exp) + ")\n" + "{\n" + translateWhereExpVector(optWhereExprs) + translateStms(optStms) + "}\n" 

			case DoWhile(optStms, exp, optWhereExprs) =>
                return "do {\n" + translateWhereExpVector(optWhereExprs) + translateStms(optStms) + "} while (" + translateExp(exp) + ");\n"

            // Need to sort out the Requires and Ensures
			case FnDecl(loc, optParameters, optReturnType, optRequiresEnsuress, optStms) =>
                return translateReturnType(optReturnType) + " " + translateLoc(loc) + "(" + translateParameters(optParameters) + ")\n" + "{\n" + translateRequiresEnsures(optRequiresEnsuress, optStms) + "}" 

			case MthdDecl(loc, optParameters, optReturnType, optRequiresEnsuress, optStms) =>
                return translateReturnType(optReturnType) + " " + translateLoc(loc) + "(" + translateParameters(optParameters) + ")\n" + "{\n" + translateRequiresEnsures(optRequiresEnsuress, optStms) + "}"

            // Need to sort out the commExps
			case RtnStm(exp, optCommExps) =>
                return "return " + translateExp(exp) + ";"

			case Assert(exp) =>
                return "assert (" + translateExp(exp) + ");" 
/*			
			case Assume(exp) =>

			case DebugExp(exp) =>
*/
			case SkipStm() =>
                return ""

			case BreakStm() =>
                return "break;"

			case ContStm() =>
                return "continue;"

//			case FailStm() =>
		}
	}

	def translateType(typ : Type) : String = {
        typ match {
            case IntType() =>
                return "int"

            case ByteType() =>
                return "int"

            case BoolType() =>
                return "int"
        }
        return "void"
	}
/*
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
				return translateExp(leftExp) + " || " + translateExp(rightExp)

			case Xor(leftExp, rightExp) =>
				return translateExp(leftExp) + " ^ " + translateExp(rightExp)

			case And(leftExp, rightExp) =>
				return translateExp(leftExp) + " && " + translateExp(rightExp)

			case BitWiseAnd(leftExp, rightExp) =>
				return translateExp(leftExp) + " & " + translateExp(rightExp)

			case EQ(leftExp, rightExp) =>
				return translateExp(leftExp) + " == " + translateExp(rightExp)

			case NE(leftExp, rightExp) =>
				return translateExp(leftExp) + " != " + translateExp(rightExp)

			case LT(leftExp, rightExp) =>
				return translateExp(leftExp) + " < " + translateExp(rightExp)

			case LE(leftExp, rightExp) =>
				return translateExp(leftExp) + " <= " + translateExp(rightExp)

			case GT(leftExp, rightExp) =>
				return translateExp(leftExp) + " > " + translateExp(rightExp)

			case GE(leftExp, rightExp) =>
				return translateExp(leftExp) + " >= " + translateExp(rightExp)

			case Lsh(leftExp, rightExp) =>
				return translateExp(leftExp) + " << " + translateExp(rightExp)

			case ARsh(leftExp, rightExp) =>
				return translateExp(leftExp) + " >> " + translateExp(rightExp)

			case Add(leftExp, rightExp) =>
				return translateExp(leftExp) + " + " + translateExp(rightExp)

			case Sub(leftExp, rightExp) =>
				return translateExp(leftExp) + " - " + translateExp(rightExp)

			case Mul(leftExp, rightExp) =>
				return translateExp(leftExp) + " * " + translateExp(rightExp)

			case Div(leftExp, rightExp) =>
				return translateExp(leftExp) + " / " + translateExp(rightExp)

			case Rem(leftExp, rightExp) =>
				return translateExp(leftExp) + " % " + translateExp(rightExp)

			case Not(exp) =>
				return " !" + translateExp(exp)

			case Neg(exp) =>
				return " -" + translateExp(exp)
/*
			case FunctionCall(loc, exp) =>

			case ArrAccess(exp1, exp2) =>

			case ArrGen(exp1, exp2) =>

			case ArrInit(optExps, exp) =>

			case ExpList(exp1, exp2) =>
*/
			case Assign(lVal, exp) =>
				return translateLVal(lVal) +  " = " + translateExp(exp)

			case Len(loc) =>
                return "sizeof(" + translateLoc(loc) + ") / sizeof(int)"
/*
			case QuantifierExp(quantExp) =>
*/
			case Use(loc) =>
				return translateLoc(loc)

			case Lit(lit) =>
				return translateLit(lit)
		}
	}

	def translateWhereExpVector(vwe : Vector[WhereExp]) : String = {
        var translation = ""
        for (we <- vwe) {
            translation = translation + translateWhereExp(we.exp) + "\n"
        }
        return translation
	}

    def translateWhereExp(exp : Exp) : String = {
        return "assert (" + translateExp(exp) + ");"
    }
/*
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
*/
	def translateElseIfVector(vei : Vector[ElseIf]) : String = {
        //for all elseifs, translate else if
        var translation = ""
        for (ei <- vei) {
            translation = translation + translateElseIf(ei.exp, ei.optStms)
        }
        return translation
	}

    def translateElseIf(exp : Exp, optStms : Vector[Stm]) : String = {
        return "else if " + "(" + translateExp(exp) + ")" + "\n{\n" + translateStms(optStms) + "}\n"
    }

	def translateElse(oelse : Option[Else]) : String = {
        return "else\n" + "{\n" + translateStms(oelse.getOrElse(return "").optStms) + "}\n"
	}

	def translateCaseStmVector(ocaseStm : Vector[CaseStm]) : String = {
        var translation = ""
        for (cs <- ocaseStm) {
            translation = translation + translateCaseStm(cs)
        }
        return translation
	}

    def translateCaseStm(caseStm : CaseStm) : String = {
        caseStm match {
            case Case(exp, optCommExps, optStms) =>
                return "case " + translateExp(exp) + ":\n" + translateStms(optStms) 

            case DefaultCase(optStms) =>
                return "default:\n" + translateStms(optStms) 
        }
    }


	def translateRequiresEnsures(optRequiresEnsures : Vector[RequiresEnsures], optStms : Vector[Stm]) : String = {
        var requires = Vector[Exp]()
        var ensures = Vector[Exp]()
        var translation = ""

        for (re <- optRequiresEnsures) {
            re match {
                case Requires(exp) =>
                    requires = requires :+ exp
                
                case Ensures(exp) =>
                    ensures = ensures :+ exp
            }
        }

        for (r <- requires) {
            translation = translation + "assert (" + translateExp(r) + ");\n"
        }

        translation = translation + translateStms(optStms)

        for (e <- ensures) {
            translation = translation + "assert (" + translateExp(e) + ");\n"
        }

        return translation
	}

	def translateParameters(params : Option[Parameters]) : String = {
        params.getOrElse(return "") match {
            case Params(typeLoc, optCommTypeLocs) =>
                return translateTypeLoc(typeLoc) + translateCommTypeLocs(optCommTypeLocs)

            case TypeParam(typ, optCommTypes) =>
                return translateType(typ)
        }
	}


	def translateReturnType(rtn : Option[ReturnType]) : String = {
        rtn.getOrElse(return "void") match {
            case RtnParams(params) =>
                return translateParameters(Option(params))

            case RtnType(typ) =>
                return translateType(typ)
        }
	}

	def translateLit(lit : Literal) : String = {
		lit match {
			case NullLit() =>
				return "NULL"

			case ByteLit(optBits) =>
				return "0b" + optBits.mkString("")

			case IntLit(intLit) =>
				return intLit.toString

			case BoolLit(boolLit) =>
				return translateBoolLit(boolLit)

			case CharLit(charLit) =>
				return "'" + charLit + "'"

			case StringLit(strLit) =>
				return '"' + strLit.mkString + '"'
		}
	}

	def translateBoolLit(boolLit : BooleanLiteral) : String = {
		boolLit match {
			case False() =>
				return "0"

			case True() =>
				return "1"
		}
	}

	def translateLVal(lVal : LVal) : String = {
		lVal match {
			//case FieldAsgn(loc, idn) =>
			//	return translateFieldAsgn(loc, idn)

			//case ListAsgn(loc, exp) =>
			//	return translateListAsgn(loc, exp)

			case Pointer(exp) =>
				return "*" + translateExp(exp)

			case LocAsgn(loc) =>
				return translateLoc(loc)
		}
	}

	def translateLoc(loc : Loc) : String = {
		loc match {
            case Loc(idn) =>
                return idn
        }
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
*/

	def translateTypeLoc(typeLoc : TypeLoc) : String = {
        typeLoc match{
            case TypeLoc(typ, loc) =>
                return translateType(typ) + " " + translateLoc(loc)
        }
	}

	def translateCommTypeLocs(vctl : Vector[TypeLoc]) : String = {
        var translate = ""
        for (tl <- vctl) {
            translate = translate + ", " + translateTypeLoc(tl)
        }
        return translate
	}

/*
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
	*/
}