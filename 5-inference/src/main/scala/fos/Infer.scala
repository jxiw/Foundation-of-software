package fos

//Test \f.\x.f(f(x)) (\x.if x then true else false) false

object Infer {
  
  case class TypeScheme(params: List[TypeVar], tp: Type){
    
    
    def newFresh():TypeVar={
      instanceNum+=1
      TypeVar("NN"+instanceNum)
    }
    
     def newInstance(tp1:Type, mapSet:Map[TypeVar,TypeVar]):Type = tp1 match{
      case FunType(t1,t2) => FunType(newInstance(t1,mapSet),newInstance(t2,mapSet))
      case s@TypeVar(_) if mapSet.contains(s) => mapSet.get(s).get
      case _=> tp1
    }
     
    def instantiate:Type ={ 
      if(params.isEmpty) tp else newInstance(tp,params.map{x => x->newFresh()}.toMap)
    }
    
   
  }
  
  var instanceNum=0;
  var num=0;
  def freshType():TypeVar ={
    num+=1
    TypeVar("TT"+num)
  }
  
  
  type Env = List[(String, TypeScheme)]
  type Constraint = (Type, Type)

  case class TypeError(msg: String) extends Exception(msg)
  case class UnificationError(msg: String) extends Exception(msg)
  
  def collect(env: Env, t: Term): (Type, List[Constraint]) = t match{
    case True() => (BoolType,Nil)
    case False() => (BoolType,Nil)
    case Zero() => (NatType,Nil)
    case Pred(t1) => val (ty,constraint) = collect(env, t1)
    (NatType, (ty,NatType)::constraint)
    
    case Succ(t1) => val (ty,constraint) = collect(env, t1)
    (NatType, (ty,NatType)::constraint)
    
    case IsZero(t1) => val (ty,constraint) = collect(env, t1)
    (BoolType, (ty,NatType)::constraint)
    
    case If(t1,t2,t3) => val (ty1,constraint1) = collect(env, t1)
    val (ty2,constraint2) = collect(env, t2)
    val (ty3,constraint3) = collect(env, t3)
    (ty2, (ty1,BoolType) :: (ty2, ty3) ::
        constraint1 ++: constraint2 ++: constraint3)
    
    case Var(t1) if 
      env.exists(_._1 == t1) => (env.find(_._1 == t1).get._2.instantiate,Nil)    
    
    case Abs(t1,tp,t2) => val type1 = tp match{
      case EmptyTypeTree() => freshType()
      case _=> tp.tpe
    }
    val (ty,constraint) = collect((t1,TypeScheme(Nil,type1))::env,t2)
    (FunType(type1,ty),constraint)
    
    case App(t1,t2) => val type1 = freshType()
    val (ty1,constraint1) = collect(env, t1)
    val (ty2,constraint2) = collect(env, t2)
    (type1, constraint1 ++: constraint2 :+ (ty1, FunType(ty2,type1)))
 
    case Let(x,tp,v,t1) =>
     
      val (ty, constraint) = collect(env,v)
      val fun = unify(constraint)
      val T = fun(ty)
      val newEnv = env.map(p => (p._1, TypeScheme(p._2.params,fun(p._2.tp))))
      val (ty2,constraint2) = collect(newEnv :+ (x,TypeScheme(tv(T,newEnv).distinct,T)), t1)
      tp match {
        case EmptyTypeTree() => (ty2,constraint ++: constraint2)
        case _=> (ty2,constraint ++: constraint2 :+ (ty,tp.tpe))
      }
      
     case _=> throw TypeError("Type Error "+t)
  }
  
   def tv(tp: Type, env: Env): List[TypeVar] = tp match {
    case x@TypeVar(_) if !env.exists(s => isContain(s._2.tp, x)) => List(x)
    case FunType(a, b) => tv(a,env) ::: tv(b,env)
    case _ => Nil
  }
  
  def unify(c: List[Constraint]): Type => Type = {
    
    if (c.isEmpty){
       return ty => ty
    }
    
    c.head match {
      case (t1,t2) if t1==t2 => unify(c.tail)
      case (t1@TypeVar(_),t2) if !isContain(t2,t1) => 
        unify(c.tail.map{x => (substitution(x._1, t1, t2),substitution(x._2, t1, t2))}).compose { ty => substitution(ty, t1, t2)}
      case (t1,t2@TypeVar(_)) if !isContain(t1,t2) => 
        unify(c.tail.map{x => (substitution(x._1, t2, t1),substitution(x._2, t2, t1))}).compose { ty => substitution(ty, t2, t1)}
      case (FunType(s1,s2),FunType(t1,t2)) => unify((s2,t2)::(s1,t1)::c.tail) 
      case _=> throw TypeError("Unification Error")
    }
    
  }
  
  def isContain(ty:Type,testType:Type):Boolean = ty match{
    case t1 if t1 == testType => true
    case FunType(t1,t2) => isContain(t1, testType) || isContain(t2, testType)
    case _ => false
  }
  
  // x is the term we want to do substitution, t1 is the term need to be replaced, t2 is the term replace with.
  def substitution(x:Type,t1:Type,t2:Type):Type = {
    if (x==t1)
      return t2
    x match {
      case FunType(x1,x2) => FunType(substitution(x1, t1, t2),substitution(x2, t1, t2))
      case _=> x
    }
  }
}
