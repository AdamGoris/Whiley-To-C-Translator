// AUTOMATICALLY GENERATED by sbt-rats - EDIT AT YOUR OWN RISK

package whiley


object WhileyISyntax {

    sealed abstract class ASTNode extends Product
    
    case class Program (optStms : Vector[Stm]) extends ASTNode
     
    sealed abstract class Stm extends ASTNode
    case class DeclAsgn (typeField : Type, lVal : LVal, optCommTypeLocs : Vector[CommTypeLoc], exp : Exp, optCommExps : Vector[CommExp]) extends Stm  
    case class Decl (typeField : Type, lVal : LVal) extends Stm  
    case class Asgn (assign : Exp) extends Stm  
    case class TypeDeclType (loc : Loc, typeField : Type, optWhereExprs : Vector[WhereExpr]) extends Stm  
    case class TypeDeclLoc (loc1 : Loc, loc2 : Loc, optWhereExprs : Vector[WhereExpr]) extends Stm  
    case class ConstDecl (loc : Loc, exp : Exp) extends Stm  
    case class If (exp : Exp, stm : Stm, optElseIfs : Vector[ElseIf], optElse : Option[Else]) extends Stm  
    case class Switch (exp : Exp, caseStm : CaseStm) extends Stm  
    case class While (exp : Exp, stm : Stm) extends Stm  
    case class DoWhile (stm : Stm, exp : Exp, optWhereExprs : Vector[WhereExpr]) extends Stm  
    case class FnDecl (optModifier : Option[Modifier], loc : Loc, optParameters : Option[Parameters], optReturnType : Option[ReturnType], optRequiresEnsuress : Vector[RequiresEnsures], stm : Stm) extends Stm  
    case class MthdDecl (optModifier : Option[Modifier], loc : Loc, optParameters : Option[Parameters], optReturnType : Option[ReturnType], optRequiresEnsuress : Vector[RequiresEnsures], stm : Stm) extends Stm  
    case class RtnStm (exp : Exp, optCommExps : Vector[CommExp]) extends Stm  
    case class AssertExp (exp : Exp) extends Stm  
    case class AssumeExp (exp : Exp) extends Stm  
    case class SkipStm () extends Stm  
    case class BreakStm () extends Stm  
    case class ContStm () extends Stm  
    case class FailStm () extends Stm  
     
    sealed abstract class Type extends ASTNode
    case class IntType () extends Type  
    case class ByteType () extends Type  
    case class BoolType () extends Type  
     
    sealed abstract class Exp extends ASTNode with org.bitbucket.inkytonik.kiama.output.PrettyExpression
    case class Or (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 8
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.NonAssoc)
    }
    case class Xor (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 7
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.LeftAssoc)
    }
    case class And (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 6
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.LeftAssoc)
    }
    case class EQ (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 5
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.LeftAssoc)
    }
    case class NE (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 5
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.LeftAssoc)
    }
    case class LT (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 4
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.LeftAssoc)
    }
    case class LE (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 4
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.LeftAssoc)
    }
    case class GT (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 4
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.LeftAssoc)
    }
    case class GE (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 4
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.LeftAssoc)
    }
    case class Lsh (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 3
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.LeftAssoc)
    }
    case class ARsh (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 3
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.LeftAssoc)
    }
    case class Add (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 2
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.LeftAssoc)
    }
    case class Sub (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 2
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.LeftAssoc)
    }
    case class Mul (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 1
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.LeftAssoc)
    }
    case class Div (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 1
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.LeftAssoc)
    }
    case class Rem (exp1 : Exp, exp2 : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 1
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.LeftAssoc)
    }
    case class Not (exp : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 0
        val fixity = org.bitbucket.inkytonik.kiama.output.Prefix
    }
    case class Use (loc : Loc) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 0
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.NonAssoc)
    }
     
    case class Assign (lVal : LVal, exp : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 0
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.NonAssoc)
    }
     
    sealed abstract class WhereExpr extends ASTNode
    case class WhereExp (exp : Exp) extends WhereExpr  
     
    case class ElseIf (exp : Exp, stm : Stm) extends ASTNode
     
    case class Else (stm : Stm) extends ASTNode
     
    sealed abstract class CaseStm extends ASTNode
    case class Case (exp : Exp, optCommExps : Vector[CommExp], stm : Stm) extends CaseStm  
    case class DefaultCase (stm : Stm) extends CaseStm  
     
    sealed abstract class RequiresEnsures extends ASTNode
    case class Requires (exp : Exp) extends RequiresEnsures  
    case class Ensures (exp : Exp) extends RequiresEnsures  
     
    sealed abstract class Parameters extends ASTNode
    case class Params (typeLoc : TypeLoc, optCommTypeLocs : Vector[CommTypeLoc]) extends Parameters  
     
    sealed abstract class ReturnType extends ASTNode
    case class RtnParams (parameters : Parameters) extends ReturnType  
    case class RtnType (typeField : Type) extends ReturnType  
     
    sealed abstract class Modifier extends ASTNode
    case class Public () extends Modifier  
    case class Private () extends Modifier  
    case class Native () extends Modifier  
    case class Export () extends Modifier  
     
    case class NullLit (nullLiteral : NullLiteral) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 0
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.NonAssoc)
    }
    case class ByteLit (byteLiteral : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 0
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.NonAssoc)
    }
    case class IntLit (integerLiteral : Int) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 0
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.NonAssoc)
    }
    case class BoolLit (booleanLiteral : Exp) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 0
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.NonAssoc)
    }
     
    case class NullLiteral () extends ASTNode
     
    case class ByteLiteral (optBits : Vector[String]) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 0
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.NonAssoc)
    }
       
    case class False () extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 0
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.NonAssoc)
    }
    case class True () extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 0
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.NonAssoc)
    }
         
    sealed abstract class LVal extends ASTNode
    case class FieldAsgn (loc : Loc, identifier : String) extends LVal  
    case class ListAsgn (loc : Loc, exp : Exp) extends LVal  
    case class Pointer (exp : Exp) extends LVal  
    case class IdnAsgn (identifier : String) extends LVal  
     
    case class Loc (identifier : String) extends ASTNode
     
    case class Len (loc : Loc) extends Exp with org.bitbucket.inkytonik.kiama.output.PrettyNaryExpression {
        val priority = 0
        val fixity = org.bitbucket.inkytonik.kiama.output.Infix (org.bitbucket.inkytonik.kiama.output.NonAssoc)
    }
      
    case class CommLoc (loc : Loc) extends ASTNode
     
    case class TypeLoc (typeField : Type, loc : Loc) extends ASTNode
     
    case class CommTypeLoc (typeLoc : TypeLoc) extends ASTNode
     
    case class CommExp (exp : Exp) extends ASTNode
    
}
