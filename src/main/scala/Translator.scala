package translator

class Translator {

	import whiley.WhileyISyntax._

	def translate(p : Program) : String = {
		var translated = "#include <stdio.h>\n#include <assert.h>\n\n"
        return translated + translateStms(0, p.optStms)
	}

    def translateStms(ind : Int, stms : Vector[Stm]) : String = {
        var translated = "";
		for (stm <- stms) {
			translated = translated + translateStm(ind, stm) + "\n"
		}
		return translated
    }

	def translateStm(ind : Int, stm : Stm) : String = {
		stm match {
			//case PackageDecl(loc, optDotLocs) =>

			//case ImportDecl(locOrStar, loc, optDotLocOrStars) =>

			// no public, private, native, export in C
			case Public(stm) =>
				return translateStm(ind, stm)

			case Private(stm) =>
				return translateStm(ind, stm)

			case Native(stm) => 
				return translateStm(ind, stm)

			case Export(stm) =>
				return translateStm(ind, stm)

			case DeclAsgn(typ, lVal, optCommTypeLocs, exp, optCommExps) =>
				return ("\t" * ind) + translateType(typ) + " " + translateLVal(lVal) + " = " + translateExp(exp) + ";" //+ translateMutliDeclAsgn(optCommTypeLVals, optCommExps)

			case Decl(typ, loc) =>
				return ("\t" * ind) + translateType(typ) + " " + translateLoc(loc) + ";"

			case AsgnStm(assign) =>
				return ("\t" * ind) + translateExp(assign) + ";"

			//FIXME:
			case TypeDecl(loc, typ, optLoc, optCommTypeLocs, optWhereExps) =>
				return ("\t" * ind) + "typedef " + translateType(typ) + " " + translateLoc(loc) + ";\n" + translateWhereExpVector(ind, optWhereExps) 

			case ConstDecl(loc, exp) =>
				return ("\t" * ind) + "const int " + translateLoc(loc) + " = " + translateExp(exp) + ";"

			case If(exp, optStms, optElseIfs, optElse) =>
                return ("\t" * ind) + "if (" + translateExp(exp) + ")\n" + ("\t" * ind) + "{\n" + translateStms(ind + 1, optStms) + ("\t" * ind) + "}" + translateElseIfVector(ind, optElseIfs) + translateElse(ind, optElse)

			case Switch(exp, optCaseStms) =>
                return ("\t" * ind) + "switch (" + translateExp(exp) + ")" + "\n" + ("\t" * ind) + "{\n" + translateCaseStmVector(ind + 1, optCaseStms) + ("\t" * ind) + "}" 

			case While(exp, optWhereExprs, optStms) =>
                return ("\t" * ind) + translateWhereExpVector(0, optWhereExprs) + "while (" + translateExp(exp) + ")\n" + "{\n" + translateStms(ind + 1, optStms) + translateWhereExpVector(ind + 1, optWhereExprs) + "}\n" + translateWhereExpVector(0, optWhereExprs) 

			case DoWhile(optStms, exp, optWhereExprs) =>
                return ("\t" * ind) + translateWhereExpVector(0, optWhereExprs) + "do\n" + ("\t" * ind) + "{\n" + translateWhereExpVector(ind + 1, optWhereExprs) + translateStms(ind + 1, optStms) + ("\t" * ind) + "} while (" + translateExp(exp) + ");\n" + translateWhereExpVector(0, optWhereExprs)

			case FnDecl(loc, optParameters, optReturnType, optRequiresEnsuress, optStms) =>
                return ("\t" * ind) + translateReturnType(optReturnType) + " " + translateLoc(loc) + "(" + translateParameters(optParameters) + ")\n" + ("\t" * ind) + "{\n" + translateRequiresEnsures(ind + 1, optRequiresEnsuress, optStms, optReturnType) + ("\t" * ind) + "}" 

			case MthdDecl(loc, optParameters, optReturnType, optRequiresEnsuress, optStms) =>
                return ("\t" * ind) + translateReturnType(optReturnType) + " " + translateLoc(loc) + "(" + translateParameters(optParameters) + ")\n" + ("\t" * ind) + "{\n" + translateRequiresEnsures(ind + 1, optRequiresEnsuress, optStms, optReturnType) + ("\t" * ind) + "}"

			case RtnStm(exp, optCommExps) =>
				return ("\t" * ind) + "return " + translateExp(exp) + ";"

			case Assert(exp) =>
                return ("\t" * ind) + "assert (" + translateExp(exp) + ");" 
			
			case Assume(exp) =>
				return ("\t" * ind) + "assert (" + translateExp(exp) + ");" 
/*
			case DebugExp(exp) =>
*/
			case SkipStm() =>
                return ""

			case BreakStm() =>
                return ("\t" * ind) + "break;"

			case ContStm() =>
                return ("\t" * ind) + "continue;"

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
				return "!" + translateExp(exp)

			case Neg(exp) =>
				return "-" + translateExp(exp)

			case FunctionCall(loc, exp) =>
				return translateLoc(loc) + "(" + translateExp(exp) + ")"

			case RecordInitialiser(loc, exp, optCommLocColonExps) =>
				return "{" + translateExp(exp) + translateOptCommLocColonExps(optCommLocColonExps) + "}"

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

	def translateWhereExpVector(ind : Int, vWhereExp : Vector[WhereExp]) : String = {
        var translation = ""
        for (whereExp <- vWhereExp) {
            translation = translation + translateWhereExp(ind, whereExp.exp) + "\n"
        }
        return translation
	}

    def translateWhereExp(ind : Int, exp : Exp) : String = {
		exp match {
			case QuantExp(noSomeAll, loc, exp1, exp2, exp3) =>
				return ("\t" * ind) + translateQuantExp(noSomeAll, loc, exp1, exp2, exp3)
			
			case (_) =>
				return ("\t" * ind) + "assert (" + translateExp(exp) + ");"
		}
    }

	def translateOptCommLocColonExps(vCommLocColonExp : Vector[CommLocColonExp]) : String = {
		var translate = ""
		for (commLocColonExp <- vCommLocColonExp) {
			translate = translate + ", " + translateExp(commLocColonExp.exp)
		}
		return translate
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
	def translateElseIfVector(ind : Int, vElseIf : Vector[ElseIf]) : String = {
        //for all elseifs, translate else if
        var translation = ""
        for (elseif <- vElseIf) {
            translation = translation + translateElseIf(ind, elseif.exp, elseif.optStms)
        }
        return translation
	}

    def translateElseIf(ind : Int, exp : Exp, optStms : Vector[Stm]) : String = {
        return "\n" + ("\t" * ind) + "else if (" + translateExp(exp) + ")" + "\n" + ("\t" * ind) + "{\n" + translateStms(ind + 1, optStms) + ("\t" * ind) + "}"
    }

	def translateElse(ind : Int, optElse : Option[Else]) : String = {
        return "\n" + ("\t" * ind) + "else\n" + ("\t" * ind) + "{\n" + translateStms(ind + 1, optElse.getOrElse(return "").optStms) + ("\t" * ind) + "}"
	}

	def translateCaseStmVector(ind : Int, vCaseStm : Vector[CaseStm]) : String = {
        var translation = ""
        for (cs <- vCaseStm) {
            translation = translation + translateCaseStm(ind, cs)
        }
        return translation
	}

    def translateCaseStm(ind : Int, caseStm : CaseStm) : String = {
        caseStm match {
            case Case(exp, optCommExps, optStms) =>
                return ("\t" * ind) + "case " + translateExp(exp) + ":\n" + translateStms(ind + 1, optStms) 

            case DefaultCase(optStms) =>
                return ("\t" * ind) + "default:\n" + translateStms(ind + 1, optStms) 
        }
    }


	def translateRequiresEnsures(ind : Int, optRequiresEnsures : Vector[RequiresEnsures], optStms : Vector[Stm], optReturnType : Option[ReturnType]) : String = {
        var requires = Vector[Requires]()
        var ensures = Vector[Ensures]()
        var translation = ""

        for (requiresEnsures <- optRequiresEnsures) {
            requiresEnsures match {
                case Requires(exp) =>
                    requires = requires :+ Requires(exp)
                
                case Ensures(exp) =>
                    ensures = ensures :+ Ensures(exp)
            }
        }

        for (require <- requires) {
            translation = translation + ("\t" * ind) + "assert (" + translateExp(require.exp) + ");\n"
        }

		// If there is a specified value to be returned, i.e. -> (int r), that value first needs to be initialised in C
		optReturnType.getOrElse() match {
			case RtnParams(params) =>
				params match {
					case Params(typeLoc, optCommTypeLocs) =>
						translation = translation + ("\t" * ind) + translateTypeLoc(typeLoc) + ";\n"
				}
		}

		// Need to search for return statements in the body of the function
		// Once a return statement has been found, before that return statement:
		// first set the initialised return value to equal the expression at the return statment
		// next make the assertion, using the ensures expression.
		translation = translation + translateStmsSearchRtn(ind, optReturnType, ensures, optStms)

        return translation
	}

def translateStmsSearchRtn(ind : Int, optRtnType : Option[ReturnType], vEnsures : Vector[Ensures], stms : Vector[Stm]) : String = {
	var translate = ""
	for (stm <- stms) {
		translate = translate + translateStmSearchRtn(ind, optRtnType, vEnsures, stm) + "\n"
	}
	return translate
}

def translateStmSearchRtn(ind : Int, optRtnType : Option[ReturnType], vEnsures : Vector[Ensures], stm : Stm) : String = {
		stm match {
			//case PackageDecl(loc, optDotLocs) =>

			//case ImportDecl(locOrStar, loc, optDotLocOrStars) =>

			// no public, private, native, export in C
			case Public(stm) =>
				return translateStmSearchRtn(ind, optRtnType, vEnsures, stm)

			case Private(stm) =>
				return translateStmSearchRtn(ind, optRtnType, vEnsures, stm)

			case Native(stm) => 
				return translateStmSearchRtn(ind, optRtnType, vEnsures, stm)

			case Export(stm) =>
				return translateStmSearchRtn(ind, optRtnType, vEnsures, stm)

			case DeclAsgn(typ, lVal, optCommTypeLocs, exp, optCommExps) =>
				return ("\t" * ind) + translateType(typ) + " " + translateLVal(lVal) + " = " + translateExp(exp) + ";" //+ translateMutliDeclAsgn(optCommTypeLVals, optCommExps)

			case Decl(typ, loc) =>
				return ("\t" * ind) + translateType(typ) + " " + translateLoc(loc) + ";"

			case AsgnStm(assign) =>
				return ("\t" * ind) + translateExp(assign) + ";"

			//FIXME:
			case TypeDecl(loc, typ, optLoc, optCommTypeLocs, optWhereExps) =>
				return ("\t" * ind) + "typedef " + translateType(typ) + " " + translateLoc(loc) + ";\n" + translateWhereExpVector(ind, optWhereExps) 

			case ConstDecl(loc, exp) =>
				return ("\t" * ind) + "const int " + translateLoc(loc) + " = " + translateExp(exp) + ";"

			case If(exp, optStms, optElseIfs, optElse) =>
                return ("\t" * ind) + "if (" + translateExp(exp) + ")\n" + ("\t" * ind) + "{\n" + translateStmsSearchRtn(ind + 1, optRtnType, vEnsures, optStms) + ("\t" * ind) + "}" + translateElseIfVectorSearchRtn(ind, optRtnType, vEnsures, optElseIfs) + translateElseSearchRtn(ind, optRtnType, vEnsures, optElse)

			case Switch(exp, optCaseStms) =>
                return ("\t" * ind) + "switch (" + translateExp(exp) + ")\n" + ("\t" * ind) + "{\n" + translateCaseStmVectorSearchRtn(ind + 1, optRtnType, vEnsures, optCaseStms) + ("\t" * ind) + "}" 

			case While(exp, optWhereExprs, optStms) =>
                return ("\t" * ind) + "while (" + translateExp(exp) + ")\n" + ("\t" * ind) + "{\n" + translateWhereExpVector(ind + 1, optWhereExprs) + translateStmsSearchRtn(ind + 1, optRtnType, vEnsures, optStms) + ("\t" * ind) + "}" 

			case DoWhile(optStms, exp, optWhereExprs) =>
                return ("\t" * ind) + "do {\n" + translateWhereExpVector(ind + 1, optWhereExprs) + translateStmsSearchRtn(ind + 1, optRtnType, vEnsures, optStms) + ("\t" * ind) + "} while (" + translateExp(exp) + ");"

			case FnDecl(loc, optParameters, optReturnType, optRequiresEnsuress, optStms) =>
                return ("\t" * ind) + translateReturnType(optReturnType) + " " + translateLoc(loc) + "(" + translateParameters(optParameters) + ")\n" + ("\t" * ind) + "{\n" + translateRequiresEnsures(ind + 1, optRequiresEnsuress, optStms, optReturnType) + ("\t" * ind) + "}" 

			case MthdDecl(loc, optParameters, optReturnType, optRequiresEnsuress, optStms) =>
                return ("\t" * ind) + translateReturnType(optReturnType) + " " + translateLoc(loc) + "(" + translateParameters(optParameters) + ")\n" + ("\t" * ind) + "{\n" + translateRequiresEnsures(ind + 1, optRequiresEnsuress, optStms, optReturnType) + ("\t" * ind) + "}"

			case RtnStm(exp, optCommExps) =>
				var translate = "\n"
				translate = translate + ("\t" * ind) + translateReturnTypeLoc(optRtnType) + " = " + translateExp(exp) + ";\n"
				for (ensure <- vEnsures) {
					translate = translate + ("\t" * ind) + "assert (" + translateExp(ensure.exp) + ");\n"
				}
				translate = translate + ("\t" * ind) + "return " + translateExp(exp) + ";"
                return translate

			case Assert(exp) =>
                return ("\t" * ind) + "assert (" + translateExp(exp) + ");" 
			
			case Assume(exp) =>
				return ("\t" * ind) + "assert (" + translateExp(exp) + ");" 
/*
			case DebugExp(exp) =>
*/
			case SkipStm() =>
                return ""

			case BreakStm() =>
                return ("\t" * ind) + "break;"

			case ContStm() =>
                return ("\t" * ind) + "continue;"

//			case FailStm() =>
		}
	}

	def translateElseIfVectorSearchRtn(ind : Int, optRtnType : Option[ReturnType], vEnsures : Vector[Ensures], vElseIfs : Vector[ElseIf]) : String = {
		var translation = ""
        	for (elseif <- vElseIfs) {
            	translation = translation + translateElseIfSearchRtn(ind, optRtnType, vEnsures, elseif.exp, elseif.optStms)
        	}
        	return translation
	}

	def translateElseIfSearchRtn(ind : Int, optRtnType : Option[ReturnType], vEnsures : Vector[Ensures], exp : Exp, optStms : Vector[Stm]) : String = {
		return "\n" + ("\t" * ind) + "else if (" + translateExp(exp) + ")" + "\n" + ("\t" * ind) + "{\n" + translateStmsSearchRtn(ind + 1, optRtnType, vEnsures, optStms) + ("\t" * ind) + "}"
	}

	def translateElseSearchRtn(ind : Int, optRtnType : Option[ReturnType], vEnsures : Vector[Ensures], optElse : Option[Else]) : String = {
		return "\n" + ("\t" * ind) + "else\n" + ("\t" * ind) + "{\n" + translateStmsSearchRtn(ind + 1, optRtnType, vEnsures, optElse.getOrElse(return "").optStms) + ("\t" * ind) + "}"
	}

	def translateCaseStmVectorSearchRtn(ind : Int, optRtnType : Option[ReturnType], vEnsures : Vector[Ensures], vCaseStm : Vector[CaseStm]) : String = {
        var translation = ""
        for (cs <- vCaseStm) {
            translation = translation + translateCaseStmSearchRtn(ind, optRtnType, vEnsures, cs)
        }
        return translation
	}

    def translateCaseStmSearchRtn(ind : Int, optRtnType : Option[ReturnType], vEnsures : Vector[Ensures], caseStm : CaseStm) : String = {
        caseStm match {
            case Case(exp, optCommExps, optStms) =>
                return ("\t" * ind) + "case " + translateExp(exp) + ":\n" + translateStmsSearchRtn(ind + 1, optRtnType, vEnsures, optStms) 

            case DefaultCase(optStms) =>
                return ("\t" * ind) + "default:\n" + translateStmsSearchRtn(ind + 1, optRtnType, vEnsures, optStms) 
        }
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

	def translateReturnTypeLoc(optReturnType : Option[ReturnType]) : String = {
		optReturnType.getOrElse(return "") match {
			case RtnParams(params) =>
				params match {
					case Params(typeLoc, optCommTypeLocs) =>
						return translateLoc(typeLoc.loc)
				}
		}
		return ""
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