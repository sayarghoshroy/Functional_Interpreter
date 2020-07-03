import ASTParser

-- syntax of data
data Value = NumVal Int | BoolVal Bool | Proc [ID] AST Env deriving Show
type Env = [ (ID, Value) ]

mappings :: Env -> Binding -> (ID, Value)
mappings env (Bind id ast) = (id, (eval env ast))

-- lookup environment
lookup_env :: Env -> ID -> Value
lookup_env (car_e : cdr_e) key = (if (fst car_e) == key then (snd car_e) else (lookup_env cdr_e key) )
lookup_env _ key = error "Undefined"

extend_env :: [Binding] -> Env -> Env
extend_env (car_bindings : cdr_bindings) bindings = (mappings bindings car_bindings) : (extend_env cdr_bindings bindings)
extend_env _ env = env

get_function_tree :: Value -> AST
get_function_tree (Proc formals ast env) = ast

lookup_function :: ID -> Env -> AST
lookup_function id (car_bindings : cdr_bindings) = (if (fst car_bindings) == id then (get_function_tree (snd car_bindings)) else (lookup_function id cdr_bindings))

extract_Env :: Value -> Env
extract_Env (Proc ids ast env) = env

get_closures :: ID -> Env -> Env
get_closures id (carenv : cdrenv) = (if (fst carenv) == id then (extract_Env (snd carenv)) else (get_closures id cdrenv))

extract_Formals :: Value -> [ID]
extract_Formals (Proc ids ast env) = ids

get_Formals :: ID -> Env -> [ID]
get_Formals id (carenv : cdrenv) = (if (fst carenv) == id then (extract_Formals (snd carenv)) else (get_Formals id cdrenv))

-- get value
get_Int_val :: Value -> Int
get_Int_val (NumVal d) = d
get_Int_val (BoolVal b) = error "Expected an integer value" 

get_Bool_val :: Value -> Bool
get_Bool_val (BoolVal d) = d
get_Bool_val (NumVal n) = error "Expected a boolean value"

mapper :: Env -> [ID] -> [AST] -> Env
mapper env (carid:cdrid) (carast:cdrast) = (carid, (eval env carast)) : (mapper env cdrid cdrast)
mapper env _ _ = []

-- evaluator
eval :: Env -> AST -> Value
eval env (Number n) = NumVal n
eval env (Boolean b) = BoolVal b
eval env (Reference r) = (lookup_env env r)
eval env (Assume formals ast) = (eval (extend_env formals env) ast)
eval env (If x y z) = (if (get_Bool_val (eval env x)) == True then (eval env y) else (eval env z))

-- primitive pattern matchings
eval env (App [Reference "isZero", a]) = (BoolVal (isZero (eval env a)))
eval env (App [Reference "not", a]) = (BoolVal (not_bool (eval env a)))
eval env (App [Reference "and", a, b]) = (BoolVal (and_bool (eval env a) (eval env b)))
eval env (App [Reference "or", a, b]) = (BoolVal (or_bool (eval env a) (eval env b)))
eval env (App [Reference "*", a, b]) = (NumVal (prod (eval env a) (eval env b)))
eval env (App [Reference "+", a, b]) = (NumVal (add (eval env a) (eval env b)))
eval env (App [Reference "-", a, b]) = (NumVal (diff (eval env a) (eval env b)))
eval env (App [Reference "/", a, b]) = (NumVal (quotient (eval env a) (eval env b)))

-- to handle primitives
eval env (Function formals ast) = (Proc formals ast env)
eval env (App ((Reference procedure) : args)) = (eval ((mapper env (get_Formals procedure env) args) ++ (get_closures procedure env))  (lookup_function procedure env))
 
-- primitives
and_bool :: Value -> Value -> Bool
and_bool (BoolVal a) (BoolVal b) = (a && b)

or_bool :: Value -> Value -> Bool
or_bool (BoolVal a) (BoolVal b) = (a || b)

not_bool :: Value -> Bool
not_bool (BoolVal a)  = (not a)

add :: Value -> Value -> Int
add (NumVal a) (NumVal b) = (a + b)

prod :: Value -> Value -> Int
prod (NumVal a) (NumVal b) = (a * b)

diff :: Value -> Value -> Int
diff (NumVal a) (NumVal b) = (a - b)

quotient :: Value -> Value -> Int
quotient (NumVal a) (NumVal b) = (div a b)

isZero :: Value -> Bool
isZero (NumVal n) = (if n == 0 then True else False)

-- to Run
run :: String -> Value
run str = (eval [] (parseString str))

-- USAGE:
-- (run "(assume ((x 7) . (y 3)) (assume ((add (function(x)(+ x y)))) (add 3)))")