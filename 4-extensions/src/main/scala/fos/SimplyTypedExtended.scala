package fos

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._

/** This object implements a parser and evaluator for the
 *  simply typed lambda calculus found in Chapter 9 of
 *  the TAPL book.
 */
object SimplyTypedExtended extends  StandardTokenParsers {
  lexical.delimiters ++= List("(", ")", "\\", ".", ":", "=", "->", "{", "}", ",", "*", "+",
                              "=>", "|")
  lexical.reserved   ++= List("Bool", "Nat", "true", "false", "if", "then", "else", "succ",
                              "pred", "iszero", "let", "in", "fst", "snd", "fix", "letrec",
                              "case", "of", "inl", "inr", "as")

  def parseNum(num:Int):Term={
    if(num==0)
      return Zero()
    else
      return Succ(parseNum(num-1))
  }

  /** Term     ::= SimpleTerm { SimpleTerm }
   */
  def Term: Parser[Term] = (
      SimpleTerm~rep(SimpleTerm)^^{ case s1~termList =>
        termList.foldLeft(s1){(z,i)=> App(z,i);}
      }
  )

  /** SimpleTerm ::= "true"
   *               | "false"
   *               | number
   *               | "succ" Term
   *               | "pred" Term
   *               | "iszero" Term
   *               | "if" Term "then" Term "else" Term
   *               | ident
   *               | "\" ident ":" Type "." Term
   *               | "(" Term ")"
   *               | "let" ident ":" Type "=" Term "in" Term
   *               | "{" Term "," Term "}"
   *               | "fst" Term
   *               | "snd" Term
   *               | "inl" Term "as" Type
   *               | "inr" Term "as" Type
   *               | "case" Term "of" "inl" ident "=>" Term "|" "inr" ident "=>" Term
   *               | "fix" Term
   *               | "letrec" ident ":" Type "=" Term "in" Term</pre>
   */
  def SimpleTerm: Parser[Term] = (
    "true" ^^^ True()
      | "false" ^^^ False()
      | "if" ~ Term ~ "then" ~ Term ~ "else" ~ Term ^^ {case _ ~cond ~ _ ~ s1 ~ "else" ~ s2 => If(cond,s1,s2)}
      | numericLit ^^ {case num => parseNum(num.toInt)}
      | "succ" ~ Term ^^ {case _ ~ s => Succ(s)}
      | "pred" ~ Term ^^ {case _ ~ s => Pred(s)}
      | "iszero" ~ Term ^^ {case _ ~ s => IsZero(s)}
      | ident ^^{case s => Var(s)}
      | "\\" ~ ident~ ":" ~ Type ~ "."~Term^^{case _~v~_~ty~_~te => Abs(v,ty,te)}
      | "("~ Term ~ ")"^^{case _~t~_ => t }
      | "let"~ ident ~ ":"~ Type ~ "=" ~ Term ~ "in" ~ Term^^{case _~v~_~ty~_~t1~_~t2 => App(Abs(v,ty,t2),t1)}
      | "{"~ Term ~ ","~ Term ~"}"^^{ case _~t1~_~t2~_ => TermPair(t1,t2)}
      | "fst"~ Term^^{case _~t1 => First(t1)}
      | "snd"~ Term^^{case _~t1 => Second(t1)}
      | "inl"~ Term~ "as"~ Type ^^{case _~t1~_~ty => Inl(t1,ty)}
      | "inr"~ Term~ "as"~ Type ^^{case _~t1~_~ty => Inr(t1,ty)}
      | "case"~ Term~ "of"~ "inl"~ ident~ "=>"~ Term~ "|"~ "inr"~ ident~ "=>"~ Term ^^{case _~t~_~_~s1~_~t1~_~_~s2~_~t2 => Case(t,s1,t1,s2,t2)}
      | "fix"~ Term ^^{case _~t => Fix(t)}
      | "letrec"~ ident~ ":"~ Type~ "="~ Term~ "in"~ Term ^^{ case _~s1~_~ty~_~t1~_~t2 => App(Abs(s1,ty,t2),Fix(Abs(s1,ty,t1)))}
  )

  /** Type       ::= SimpleType [ "->" Type ]
   */
  def Type: Parser[Type] = (
      rep1sep(SimpleType,"->")^^{ 
      case list=>list.reduceRight((t1,t2)=>TypeFun(t1,t2))
    }
  )

  /** SimpleType ::= BaseType [ ("*" SimpleType) | ("+" SimpleType) ]
   */
  def SimpleType: Parser[Type] = (
     BaseType~"+"~SimpleType^^{case ty1~_~ty2 => TypeSum(ty1,ty2)}
     | BaseType~"*"~SimpleType^^{case ty1~_~ty2 => TypePair(ty1,ty2)}
     | BaseType
  )

  /** BaseType ::= "Bool" | "Nat" | "(" Type ")"
   */
  def BaseType: Parser[Type] = (
    "Bool" ^^^ TypeBool
    | "Nat" ^^^ TypeNat
    | "("~Type~")" ^^{ case _~t~_ => t}
  )

  def isNumberic(t:Term):Boolean=t match{
    case Zero() => true
    case Succ(t) => isNumberic(t)
    case _ => false
  }
  
  def isValue(t:Term):Boolean = t match{
    case True() => true
    case False() => true
    case Abs(_,_,_) => true
    case TermPair(t1,t2) => isValue(t1) && isValue(t2)
    case Inl(t1,ty) => isValue(t1)
    case Inr(t1,ty) => isValue(t1)
    case _=> isNumberic(t)
  }
  
  /** Call by value reducer. */
  def reduce(t: Term): Term = t match{
    
    //Computation Rules
    case If(True(),t1,t2) => t1
    case If(False(),t1,t2) => t2
    case IsZero(Zero()) => True()
    case IsZero(Succ(t1)) if isNumberic(t1) => False()
    case Pred(Zero()) => Zero()
    case Pred(Succ(t1)) if isNumberic(t1) => t1
    case App(Abs(x,type1,t1),t2) if isValue(t2) => subst(t1,x,t2)
    
    //Congruence Rules
    case If(cond,t1,t2) => If(reduce(cond),t1,t2)
    case IsZero(t1) => IsZero(reduce(t1))
    case Succ(t1) => Succ(reduce(t1))
    case Pred(t1) => Pred(reduce(t1))
    
    //case App(Abs(x,type1,t1),t2) if !t2.isInstanceOf[App] => subst(t1,x,t2)
    //case App(t1@Abs(_,_,_),t2@App(_,_)) => App(t1,reduce(t2))
    case App(t1,t2) if !isValue(t1) => App(reduce(t1),t2)
    case App(t1,t2) if isValue(t1) && !isValue(t2) => App(t1,reduce(t2))
    
    //Pair Term
    case First(TermPair(t1,t2)) if isValue(t1)&&isValue(t2) => t1
    case Second(TermPair(t1,t2)) if isValue(t1)&&isValue(t2) => t2
    case First(t1) => First(reduce(t1))
    case Second(t2) => Second(reduce(t2))
    case TermPair(t1,t2) if !isValue(t1) => TermPair(reduce(t1),t2)
    case TermPair(t1,t2) if isValue(t1) && !isValue(t2) => TermPair(t1,reduce(t2))
    
    //Sum Term
    case Case(Inl(v1,ty),s1,t1,s2,t2) if isValue(v1) => subst(t1,s1,v1)
    case Case(Inr(v1,ty),s1,t1,s2,t2) if isValue(v1) => subst(t2,s2,v1)
    case Case(cond,s1,t1,s2,t2) => Case(reduce(cond),s1,t1,s2,t2)
    case Inl(t1,ty) => Inl(reduce(t1),ty)
    case Inr(t1,ty) => Inr(reduce(t1),ty)
    
    //Fix Term
    case Fix(Abs(x,ty,t1)) => subst(t1,x,t)
    case Fix(t1) => Fix(reduce(t1))
    case _=>throw NoRuleApplies(t)
  }

  
   /** Straight forward substitution method
   *  (see definition 5.3.5 in TAPL book).
   *  [x -> s]t
   *
   *  @param t the term in which we perform substitution
   *  @param x the variable name
   *  @param s the term we replace x with
   *  @return  ...
   */
  def subst(t: Term, x: String, s: Term): Term = t match{
      case Succ(t1) => Succ(subst(t1,x,s))
      case Pred(t1) => Pred(subst(t1,x,s))
      case IsZero(t1) => IsZero(subst(t1,x,s))
      case If(cons,t1,t2) => If(subst(cons,x,s),subst(t1,x,s),subst(t2,x,s))
      case First(t1) => First(subst(t1,x,s))
      case Second(t1) => Second(subst(t1,x,s))
      case TermPair(t1,t2) => TermPair(subst(t1,x,s),subst(t2,x,s))
      
      case Inl(t1,ty) => Inl(subst(t1,x,s),ty)
      case Inr(t1,ty) => Inr(subst(t1,x,s),ty)
      case Case(cond,s1,t1,s2,t2) => {
        var tmp1 = subst(Abs(s1,TypeNat,t1),x,s).asInstanceOf[Abs];
        var tmp2 = subst(Abs(s2,TypeNat,t2),x,s).asInstanceOf[Abs];
        Case(subst(cond,x,s),tmp1.v,tmp1.t,tmp2.v,tmp2.t);
      }
      case Fix(t1) => Fix(subst(t1,x,s))
      
      case Var(y) if y==x => s
      case Var(y) if y!=x => Var(y)
      case Abs(y,type1,t1) if y==x => Abs(y,type1,t1);
      case Abs(y,type1,t1) if y!=x && !(FV(s) contains y) => Abs(y,type1,subst(t1,x,s))
      case Abs(y,type1,t1) if y!=x && (FV(s) contains y) =>  subst(Abs(y+"3",type1,rename(t1,y)),x,s)
      case App(left,right) => App(subst(left,x,s),subst(right,x,s))
      case _=> t
  }
  
  def rename(t:Term,x:String):Term = t match{
    case Var(y) if y==x => Var(y+"3")
    case Abs(y,type1,t1) if y!=x => Abs(y,type1,rename(t1,x))
    case App(t1,t2) => App(rename(t1,x),rename(t2,x))
    
    case Inl(t1,ty) => Inl(rename(t1,x),ty)
    case Inr(t1,ty) => Inr(rename(t1,x),ty)
    case Case(cond,s1,t1,s2,t2) => Case(rename(cond,x),s1,if (s1==x) t1 else rename(t1,x),s2,if (s2==x) t2 else rename(t2,x))
    case Fix(t1) => Fix(rename(t1,x))
    
    case Succ(t1) => Succ(rename(t1,x))
    case Pred(t1) => Pred(rename(t1,x))
    case IsZero(t1) => IsZero(rename(t1,x))
    case If(cons,t1,t2) => If(rename(cons,x),rename(t1,x),rename(t2,x))
    case First(t1) => First(rename(t1,x))
    case Second(t1) => Second(rename(t1,x))
    case TermPair(t1,t2) => TermPair(rename(t1,x),rename(t2,x))
    
    case _=> t
  }
  
  def FV(t:Term):List[String] = { 
      val fv=List()
      t match{
        case Var(x) => x +: fv;
        case Abs(x,type1,t) => FV(t) diff List(x) 
        case App(t1,t2) => FV(t1) ++: FV(t2)
        
        case Inl(t1,ty) => FV(t1)
        case Inr(t1,ty) => FV(t1)
        case Case(cond,s1,t1,s2,t2) => FV(cond)++: (FV(t1) diff List(s1)) ++: (FV(t2) diff List(s1))
        case Fix(t1) => FV(t1)
        
        case Succ(t1) => FV(t1)
        case Pred(t1) => FV(t1)
        case IsZero(t1) => FV(t1)
        case If(cons,t1,t2) => FV(cons) ++: FV(t1) ++: FV(t2) 
        case First(t1) => FV(t1)
        case Second(t1) => FV(t1)
        case TermPair(t1,t2) => FV(t1) ++: FV(t2)
        case _=>Nil
    }
  }
  
  /** Thrown when no reduction rule applies to the given term. */
  case class NoRuleApplies(t: Term) extends Exception(t.toString)

  /** Print an error message, together with the position where it occured. */
  case class TypeError(t: Term, msg: String) extends Exception(msg) {
    override def toString = msg + "\n" + t
  }

  /** The context is a list of variable names paired with their type. */
  type Context = List[(String, Type)]

  /** Returns the type of the given term <code>t</code>.
   *
   *  @param ctx the initial context
   *  @param t   the given term
   *  @return    the computed type
   */
  def typeof(ctx: Context, t: Term): Type = t match{
    case True() => TypeBool
    case False() => TypeBool
    case Zero() => TypeNat
    case Pred(t1) if typeof(ctx,t1) == TypeNat => TypeNat
    case Succ(t1) if typeof(ctx,t1) == TypeNat => TypeNat
    case IsZero(t1) if typeof(ctx,t1) == TypeNat => TypeBool
    case If(cond,t1,t2) if typeof(ctx,cond) == TypeBool && typeof(ctx,t1)==typeof(ctx,t2) => typeof(ctx,t1)
    case Var(x) if ctx.exists(_._1 == x)=> getTypeFromList(ctx,x)
    case Abs(x,type1,t2) => { val type2 = typeof((x,type1) +: ctx, t2)
      TypeFun(type1,type2)
    }
    case App(t1,t2) => {
      typeof(ctx,t1) match{
        case TypeFun(type2,type3) if (type2 == typeof(ctx,t2)) => type3
        case TypeFun(type2,type3) if (type2 != typeof(ctx,t2)) =>throw TypeError(t,"parameter type mismatch: expected "+type2+", found "+typeof(ctx,t2))
        case _=> throw TypeError(t,"parameter type mismatch: expected TypeFun, found "+typeof(ctx,t1))
      }
    }
    case TermPair(t1,t2) =>  TypePair(typeof(ctx,t1),typeof(ctx,t2))
    case First(t3) => {
        typeof(ctx,t3) match{
        case TypePair(t1,t2) => t1
        case _=> throw TypeError(t,"pair type expected but "+typeof(ctx,t3)+" found")    
      }
    }
    case Second(t3) => {
        typeof(ctx,t3) match{
        case TypePair(t1,t2) => t2
        case _=> throw TypeError(t,"pair type expected but "+typeof(ctx,t3)+" found")
      }
    }
    
    
    //sum type
    case Case(cond1,x1,t1,x2,t2) => {
       typeof(ctx,cond1) match{
         case TypeSum(ty1,ty2) if typeof((x1,ty1)+:ctx,t1) == typeof((x2,ty2)+:ctx,t2)  => typeof((x2,ty2)+:ctx,t2)
         case _=> throw TypeError(t,"parameter type mismatch")
       }
    }    
    case Inl(t1,ty1@TypeSum(ty2,ty3)) if typeof(ctx,t1) == ty2 => ty1
    case Inr(t1,ty1@TypeSum(ty2,ty3)) if typeof(ctx,t1) == ty3 => ty1

    //fix type
    case Fix(t1) => {
      typeof(ctx,t1) match{
        case TypeFun(ty1,ty2) if ty1==ty2 => ty1
        case _=> throw TypeError(t,"parameter type mismatch")
      }
    }
    
    case _=> throw TypeError(t,"parameter type mismatch")
  }

  def getTypeFromList(ctx: Context,s: String):Type = {
    ctx.find(_._1 == s).get._2 
  }
  
  def typeof(t: Term): Type = try {
    typeof(Nil, t)
  } catch {
    case err @ TypeError(_, _) =>
      Console.println(err)
      null
  }

  /** Returns a stream of terms, each being one step of reduction.
   *
   *  @param t      the initial term
   *  @param reduce the evaluation strategy used for reduction.
   *  @return       the stream of terms representing the big reduction.
   */
  def path(t: Term, reduce: Term => Term): Stream[Term] =
    try {
      var t1 = reduce(t)
      Stream.cons(t, path(t1, reduce))
    } catch {
      case NoRuleApplies(_) =>
        Stream.cons(t, Stream.empty)
    }

  def main(args: Array[String]): Unit = {
    val stdin = new java.io.BufferedReader(new java.io.InputStreamReader(System.in))
    val tokens = new lexical.Scanner(stdin.readLine())
    phrase(Term)(tokens) match {
      case Success(trees, _) =>
        try {
          println("parsed: " + trees)
          println("typed: " + typeof(Nil, trees))
          for (t <- path(trees, reduce))
            println(t)
        } catch {
          case tperror: Exception => println(tperror.toString)
        }
      case e =>
        println(e)
    }
  }
}
