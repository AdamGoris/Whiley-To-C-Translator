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
                return "if (" + translateExp(exp) + ")" + "\n{\n" + translateStms(optStms) + "}\n" + translateElseIfVector(optElseIfs) + translateElse(optElse.get)

			case Switch(exp, optCaseStms) =>
                return "switch (" + translateExp(exp) + ")" + "\n{\n" + translateCaseStmVector(optCaseStms) + "}\n" 

			case While(exp, optWhereExprs, optStms) =>
                return "while (" + translateExp(exp) + ")\n" + "{\n" + translateWhereExpVector(optWhereExprs) + translateStms(optStms) + "}\n" 

			case DoWhile(optStms, exp, optWhereExprs) =>
                return "do {\n" + translateWhereExpVector(optWhereExprs) + translateStms(optStms) + "} while (" + translateExp(exp) + ");\n"

			case FnDecl(loc, optParameters, optReturnType, optRequiresEnsuress, optStms) =>
                return translateReturnType(optReturnType) + " " + translateLoc(loc) + "(" + translateParameters(optParameters) + ")\n" + "{\n" + translateStms(optStms) + "}" 

			case MthdDecl(loc, optParameters, optReturnType, optRequiresEnsuress, optStms) =>
                return translateReturnType(optReturnType) + " " + translateLoc(loc) + "(" + translateParameters(optParameters) + ")\n" + "{\n" + translateStms(optStms) + "}"
/*
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
/*
			case Length() =>

			case QuantifierExp(quantExp) =>
*/
			case Use(loc) =>
				return translateLoc(loc)

			case Lit(lit) =>
				return translateLit(lit)
		}
	}

	def translateWhereExpVector(vwe : Vector[WhereExpr]) : String = {
        var translation = ""
        for (we <- vwe) {
            translation = translation + translateWhereExp(we) + "\n"
        }
        return translation
	}

    def translateWhereExp(we : WhereExpr) : String = {
        we match {
            case WhereExp(exp) =>
                return "assert (" + translateExp(exp) + ");"
        }
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
            translation = translation + translateElseIf(ei)
        }
        return translation
	}

    def translateElseIf(ei : ElseIf) : String = {
        ei match {
            case ElseIf(exp, optStms) =>
                return "else if " + "(" + translateExp(exp) + ")" + "\n{\n" + translateStms(optStms) + "}\n"
        }
    }

	def translateElse(e : Else) : String = {
        e match {
            case Else(optStms) =>
                return "else\n" + "{\n" + translateStms(optStms) + "}\n" 
        }
        return ""
	}

	def translateCaseStmVector(ocs : Vector[CaseStm]) : String = {
        var translation = ""
        for (cs <- ocs) {
            translation = translation + translateCaseStm(cs)
        }
        return translation
	}

    def translateCaseStm(cs : CaseStm) : String = {
        cs match {
            case Case(exp, optCommExps, optStms) =>
                return "case " + translateExp(exp) + ":\n" + translateStms(optStms) 

            case DefaultCase(optStms) =>
                return "default:\n" + translateStms(optStms) 
        }
    }

/*
	def translateRequiresEnsures() : String = {

	}

	def translateRequires(exp : Exp) : String = {

	}

	def translateEnsures(exp : Exp) : String = {

	}

	def translateParameters(params : Option[Parameters]) : String = {
        var translate = ""
        for (param <- params) {
            translate = translate + translateParameter(param)
        }
        return translate
	}
*/
	def translateParameters(params : Option[Parameters]) : String = {
        params.getOrElse(return "") match {
            case Params(typeLoc, optCommTypeLocs) =>
                return translateTypeLoc(typeLoc) + translateCommTypeLocs(optCommTypeLocs)

            case TypeParam(typ) =>
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

/*
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