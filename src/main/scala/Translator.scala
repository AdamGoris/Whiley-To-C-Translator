package translator

class Translator {

	import whiley.WhileyISyntax._

	def translate(p : Program) : String = {
		translateStm(p.optStms(0))
	}

	def translateStm(stm : Stm) : String = {
		stm match {
			//case PackageDecl(loc, optDotLocs) =>

			//case ImportDecl(locOrStar, loc, optDotLocOrStars) =>

			//case Public(stm) =>

			//case Private(stm) =>

			//case Native(stm) => 

			//case Export(stm) =>

			//case DeclAsgn(typeField, lVal, optCommTypeLocs, exp, optCommExps) =>

			case Decl(typ, lVal) =>


			case AsgnStm(assign) =>

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
		}
	}

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

			case Assign(lVal, exp) =>

			case Length() =>

			case QuantifierExp(quantExp) =>

			case Use(loc) =>

			case Lit() =>

		}
	}

	def translateAssign(lval : Lval, exp : Exp) : String = {
		
	}

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

	def translateLit() : String = {

	}

	def translateNullLit(nullLit : NullLiteral) : String = {

	}

	def translateByteLit(byteLit : ByteLiteral) : String = {

	}

	def translateByteLiteral(optBits : Vector[String]) : String = {

	}
	
	def translateIntLit(intLit : Int) : String = {

	}

	def translateBoolLit(boolLit : Exp) : String = {

	}
	
	def translateFalse() : String = {

	}

	def translateTrue() : String = {
		
	}

	def translateCharLit(charLit : CharacterLiteral) : String = {

	}

	def translateStringLit(strLit : StringLiteral) : String = {

	}

	def translateNullLit() : String = {

	}

	def translateLVal() : String = {

	}

	def translateFieldAsgn(loc : LVal, identifier : String) = {

	}

	def translateListAsgn(loc : LVal, exp : Exp) : String = {

	}

	def translatePointer(exp : Exp) : String = {

	}

	def translateLoc(identifier : String) : String = {

	}

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
}