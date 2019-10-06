package translator

class Translator {

	import whiley.WhileyISyntax._

	def translate(p : Program) : String = {
		var translated = "#include <stdio.h>\n#include <assert.h>\n\n"
        return translated + translateStms(p.optStms)
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

			// no public, private, native, export in C
			case Public(stm) =>
				translateStm(stm)

			case Private(stm) =>
				translateStm(stm)

			case Native(stm) => 
				translateStm(stm)

			case Export(stm) =>
				translateStm(stm)

			case DeclAsgn(typ, lVal, optCommTypeLocs, exp, optCommExps) =>
				return translateType(typ) + " " + translateLVal(lVal) + " = " + translateExp(exp) + ";" //+ translateMutliDeclAsgn(optCommTypeLVals, optCommExps)

			case Decl(typ, loc) =>
				return translateType(typ) + " " + translateLoc(loc) + ";"

			case AsgnStm(assign) =>
				return translateExp(assign) + ";"

			//FIXME:
			case TypeDecl(loc, typ, optLoc, optCommTypeLocs, optWhereExps) =>
				return "typedef " + translateType(typ) + " " + translateLoc(loc) + ";\n" + translateWhereExpVector(optWhereExps) 

			case ConstDecl(loc, exp) =>
				return "const int " + translateLoc(loc) + " = " + translateExp(exp) + ";"

			case If(exp, optStms, optElseIfs, optElse) =>
                return "if (" + translateExp(exp) + ")" + "\n{\n" + translateStms(optStms) + "}\n" + translateElseIfVector(optElseIfs) + translateElse(optElse)

			case Switch(exp, optCaseStms) =>
                return "switch (" + translateExp(exp) + ")" + "\n{\n" + translateCaseStmVector(optCaseStms) + "}" 

			case While(exp, optWhereExprs, optStms) =>
                return "while (" + translateExp(exp) + ")\n" + "{\n" + translateWhereExpVector(optWhereExprs) + translateStms(optStms) + "}" 

			case DoWhile(optStms, exp, optWhereExprs) =>
                return "do {\n" + translateWhereExpVector(optWhereExprs) + translateStms(optStms) + "} while (" + translateExp(exp) + ");"

            //FIXME: Ensures statement problem, can't assert before or after return
			case FnDecl(loc, optParameters, optReturnType, optRequiresEnsuress, optStms) =>
                return translateReturnType(optReturnType) + " " + translateLoc(loc) + "(" + translateParameters(optParameters) + ")\n" + "{\n" + translateRequiresEnsures(optRequiresEnsuress, optStms, optReturnType) + "}" 

			case MthdDecl(loc, optParameters, optReturnType, optRequiresEnsuress, optStms) =>
                return translateReturnType(optReturnType) + " " + translateLoc(loc) + "(" + translateParameters(optParameters) + ")\n" + "{\n" + translateRequiresEnsures(optRequiresEnsuress, optStms, optReturnType) + "}"

            // Need to sort out the commExps
			case RtnStm(exp, optCommExps) =>
                return "return " + translateExp(exp) + ";"

			case Assert(exp) =>
                return "assert (" + translateExp(exp) + ");" 
			
			//FIXME: currently treating assume like an assert
			case Assume(exp) =>
				return "assert (" + translateExp(exp) + ");" 
/*
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
			//FIXME:
			case RecType(mixedType, optCommMixedTypes) =>
				return ""

			//FIXME:
			case RefType(typ) =>
				return ""

			//FIXME:
			case NegType(typeField) =>
				return ""

			//FIXME:
			case ArrType(primitiveType) =>
				return translateType(primitiveType) + "[]"

			//FIXME:
			case FuncType(parameters1, parameters2) =>
				return ""

			//FIXME:
			case MthdType(parameters1, parameters2) =>
				return ""				

            case IntType() =>
                return "int"

            case ByteType() =>
                return "int"

            case BoolType() =>
                return "int"
			
			//FIXME:
			case NmnlType(loc) =>
				return ""
        }
        return "void"
	}
/*
	def translateMixedType() : String = {
		Mix(typeField : Type, loc : Loc)
		MixFunc(loc : Loc, parameters1 : Parameters, parameters2 : Parameters)
		MixMthd(loc : Loc, parameters1 : Parameters, parameters2 : Parameters)
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

			case FunctionCall(loc, exp) =>
				return translateLoc(loc) + "(" + translateExp(exp) + ")"

			case ArrAccess(exp1, exp2) =>
				return translateExp(exp1) + "[" + translateExp(exp2) + "]"

			//case ArrGen(exp1, exp2) =>

			case ArrInit(exp, optExps) =>
				return "{" + translateExp(exp) + translateCommExp(optExps) + "}"

			case Assign(lVal, exp) =>
				return translateLVal(lVal) +  " = " + translateExp(exp)

			case Len(loc) =>
                return "sizeof (" + translateLoc(loc) + ") / sizeof (int)"

			case QuantExp(noSomeAll, loc, exp1, optCommLocInExps, exp2) =>
				return translateQuantExp(noSomeAll, loc, exp1, optCommLocInExps, exp2)

			case Use(loc) =>
				return translateLoc(loc)

			case Lit(lit) =>
				return translateLit(lit)
		}
	}

	def translateWhereExpVector(vWhereExp : Vector[WhereExp]) : String = {
        var translation = ""
        for (whereExp <- vWhereExp) {
            translation = translation + translateWhereExp(whereExp.exp) + "\n"
        }
        return translation
	}

    def translateWhereExp(exp : Exp) : String = {
		exp match {
			case QuantExp(noSomeAll, loc, exp1, exp2, exp3) =>
				return translateQuantExp(noSomeAll, loc, exp1, exp2, exp3)
			
			case (_) =>
				return "assert (" + translateExp(exp) + ");"
		}
    }

	def translateQuantExp(noSomeAll : NoSomeAll, loc : Loc, exp1 : Exp, exp2 : Exp, exp3 : Exp) : String = {
		noSomeAll match {
			case All() =>
				return "for (int " + translateLoc(loc) + " = " + translateExp(exp1) + "; " + translateLoc(loc) + " < " + translateExp(exp2) + "; " + translateLoc(loc) + "++)\n" + "{\n" + "assert (" + translateExp(exp3) + ");\n" + "}"  

			case No() =>
				return "for (int " + translateLoc(loc) + " = " + translateExp(exp1) + "; " + translateLoc(loc) + " < " + translateExp(exp2) + "; " + translateLoc(loc) + "++)\n" + "{\n" + "assert (" + translateExpOppositeSign(exp3) + ");\n" + "}"
		}
	}

	def translateExpOppositeSign(exp : Exp) : String = {
		exp match {
			case EQ(exp1, exp2) =>
				return translateExp(exp1) +  " != " + translateExp(exp2)

			case NE(exp1, exp2) =>
				return translateExp(exp1) +  " == " + translateExp(exp2)

			case LT(exp1, exp2) =>
				return translateExp(exp1) +  " >= " + translateExp(exp2)
			
			case LE(exp1, exp2) =>
				return translateExp(exp1) +  " > " + translateExp(exp2)

			case GT(exp1, exp2) =>
				return translateExp(exp1) +  " <= " + translateExp(exp2)
			
			case GE(exp1, exp2) =>
				return translateExp(exp1) +  " < " + translateExp(exp2)

			case Not(exp) =>
				return translateExp(exp)

			case Neg(exp) =>
				return translateExp(exp)

			case And(exp1, exp2) =>
				return translateExp(exp1) + translateExp(exp2)

			case Or(exp1, exp2) =>
				return translateExp(exp1) + translateExp(exp2)				
		}
	}

/*
	def translateNoSomeAll() : String = {

	}

	def translateNo() : String = {

	}

	def translateSome() : String = {
		
	}

	def translateAll() : String = {
		
	}
*/
	def translateElseIfVector(vElseIf : Vector[ElseIf]) : String = {
        //for all elseifs, translate else if
        var translation = ""
        for (elseif <- vElseIf) {
            translation = translation + translateElseIf(elseif.exp, elseif.optStms)
        }
        return translation
	}

    def translateElseIf(exp : Exp, optStms : Vector[Stm]) : String = {
        return "else if (" + translateExp(exp) + ")" + "\n{\n" + translateStms(optStms) + "}\n"
    }

	def translateElse(optElse : Option[Else]) : String = {
        return "else\n" + "{\n" + translateStms(optElse.getOrElse(return "").optStms) + "}\n"
	}

	def translateCaseStmVector(vCaseStm : Vector[CaseStm]) : String = {
        var translation = ""
        for (cs <- vCaseStm) {
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


	def translateRequiresEnsures(optRequiresEnsures : Vector[RequiresEnsures], optStms : Vector[Stm], optReturnType : Option[ReturnType]) : String = {
        var requires = Vector[Exp]()
        var ensures = Vector[Exp]()
        var translation = ""

        for (requiresEnsures <- optRequiresEnsures) {
            requiresEnsures match {
                case Requires(exp) =>
                    requires = requires :+ exp
                
                case Ensures(exp) =>
                    ensures = ensures :+ exp
            }
        }

        for (require <- requires) {
            translation = translation + "assert (" + translateExp(require) + ");\n"
        }

		// If there is a specified value to be returned, i.e. -> (int r), that value first needs to be initialised in C
		optReturnType.getOrElse() match {
			case RtnParams(params) =>
				params match {
					case Params(typeLoc, optCommTypeLocs) =>
						translation = translation + translateTypeLoc(typeLoc) + ";\n"
				}
		}

		// Need to search for return statements in the body of the function
		// Once a return statement has been found, before that return statement:
		// first set the initialised return value to equal the expression at the return statment
		// next make the assertion, using the ensures expression.
		

        return translation
	}

	def translateParameters(optParams : Option[Parameters]) : String = {
        optParams.getOrElse(return "") match {
            case Params(typeLoc, optCommTypeLocs) =>
                return translateTypeLoc(typeLoc) + translateCommTypeLocs(optCommTypeLocs)

            case TypeParam(typ, optCommTypes) =>
                return translateType(typ)
        }
	}


	def translateReturnType(optReturnType : Option[ReturnType]) : String = {
        optReturnType.getOrElse(return "void") match {
            case RtnParams(params) =>
                params match {
					case Params(typeLoc, optCommTypeLocs) =>
						return translateType(typeLoc.typeField)
					
					case TypeParam(typ, optCommTypes) =>
                		return translateType(typ)
				}

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
        return loc.identifier
	}


	def translateCommExp(vExp : Vector[Exp]) : String = {
		var translate = ""
		for (exp <- vExp) {
			translate = translate + ", " + translateExp(exp)
		}
		return translate
	}

	def translateCommLoc(vLoc : Vector[Loc]) : String = {
		var translate = ""
		for (loc <- vLoc){
			translate = translate + ", " + translateLoc(loc)
		}
		return translate
	}
/*
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

	def translateCommTypeLocs(vTypeLoc : Vector[TypeLoc]) : String = {
        var translate = ""
        for (typeLoc <- vTypeLoc) {
            translate = translate + ", " + translateTypeLoc(typeLoc)
        }
        return translate
	}


	def translateDotLoc(lVal: LVal) : String = {
		return '.' + translateLVal(lVal)
	}
/*
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