{-# LANGUAGE TemplateHaskell #-}
module Language.C.Simple.CType.Build.TH where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.C.Quote.Utils
import Data.List
import Control.Applicative
import Control.Arrow
import Language.C.Simple.CType
import Language.C hiding (Exp)

mk_simple_c_type_record :: Name -> Q [Dec]
mk_simple_c_type_record name = do
    info <- reify name
    let members = get_members info
    return $ [mk_simple_c_type_record' name members]

mk_simple_c_type_record' :: Name -> [VarStrictType] -> Dec
mk_simple_c_type_record' name members = result where
    result = InstanceD [] (AppT (ConT $ mkName "ToCType") (ConT name)) [
            FunD (mkName "toCType") [
                Clause [] (NormalB $ AppE (VarE $ mkName "const") $ mk_to_c_exp name members) []]]
                
 
mk_to_c_exp :: Name -> [VarStrictType] -> Exp
mk_to_c_exp name members = AppE (AppE (VarE $ mkName "Conversion.mk_record_ctype") 
                   (LitE (StringL $ show name)))
                   (ListE $ map make_member_arg members)
     
make_member_arg :: VarStrictType -> Exp
make_member_arg (name, _, typ) = InfixE (Just (LitE (StringL $ show name)))
                                (VarE $ mkName "Conversion.<::>")
                                (Just (SigE (VarE $ mkName "undefined") typ))
                                
get_members (TyConI dec) = get_members_dec dec
                                
get_members_dec (DataD _ _ _ [con] _) = get_members_con con 
get_members_dec x = error ("whoops " ++ show x)

get_members_con (RecC _ members) = members

mk_c_named_members :: Name -> Q [Dec]
mk_c_named_members name = do 
    (TyConI dec) <- reify name 
    (:[]) <$> mk_c_type_instance mk_c_type_named_typ [] [] dec

mk_c_type_instance' :: [(String, [String])] -> Name -> Q [Dec]
mk_c_type_instance' classes' name = do 
    (TyConI dec) <- reify name 
    let classes = map (first mkName) classes'
        cxts = map mk_cxt classes
        typs = map fst  classes
    (:[]) <$> mk_c_type_instance mk_c_type_type typs cxts dec
    
mk_cxt (var_n, class_names) = map (mk_pred var_n) class_names

mk_pred var_n class_name = ClassP (mkName class_name) [(VarT var_n)]

--mk_c_type_instance :: [Cxt] -> Dec -> Q Dec
mk_c_type_instance f typs cxts d@(DataD _ name ty_vars cons _) = do
    let instance_cxt = concat cxts
        header       = appT (conT $ mkName "ToCType") (foldl' appT (conT name) (map varT typs))
        
    instanceD (cxt $ map return instance_cxt) header [mk_c_type_fun f typs d]
    
get_cxt_types cxts = concatMap (map (\(ClassP _ (x:[])) -> (get_type_name x))) cxts

get_type_name (VarT a) = a
get_type_name (ConT a) = a


mk_c_type_fun f typs d = do
    let c = clause [] (normalB $ appE (varE $ mkName "const") $ mk_c_type f typs d) [] 
    funD (mkName "toCType") [c]


--mk_c_type :: [Type] -> Dec -> Q Exp
mk_c_type f typs (DataD [] name ty_vars cons _) = 
    [| TStruct $(stringE $ show name) 
        [   TEnum $(stringE enum_name) $(listE $ map stringE enum_options),
            TUnion $(stringE $ show name) $(listE $ map (mk_c_type_con f typs) cons)]|] where
    enum_name    = enum_name_from_name $ show name
    enum_options = map (name_to_enum_option (show name) . show) con_names
    con_names    = map get_con_name cons
    
get_con_name (NormalC x _) = x
get_con_name (RecC x _) = x





--mk_c_type_con :: [Types] -> Con -> Q Exp
mk_c_type_con f typs (NormalC name strict_types) = 
 [| TStruct $(stringE $ show name) $(listE $ zipWith (mk_strict_type_member f typs) [0..] strict_types) |]

mk_c_type_con f typs (RecC name var_strict_types) = 
 [| TStruct $(stringE $ show name) $(listE $ map (mk_var_strict_type_member f typs) var_strict_types) |]

--mk_strict_type_member :: Int -> StrictType -> Q Exp
mk_strict_type_member f typs index (_, typ) = [| TMember $(stringE name) $(f typs typ) |] where
 name = "x_" ++ (show index)

--mk_var_strict_type_member :: VarStrictType -> Q Exp
mk_var_strict_type_member f typs (name, _, typ) = [| TMember $(stringE $ show name) $(f typs typ) |] 


mk_c_type_type typs x = [| toCType (undefined :: $(return $ convert_type typs x)) |]

mk_c_type_named_typ typs (ConT x) = [| TNamed $(stringE $ show x) |]

convert_type typs (VarT x) = VarT $ head $ filter (\t -> head (show x) == head (show t)) typs
convert_type typs c@(ConT x) = c
convert_type typs (AppT x y) = AppT (convert_type typs x) (convert_type typs y)


--------------------------------------------------------------------------------------------------------------

--I need to make the definitions from the functions
--I need to also make the evalutator

type CommandHandler = (String, Func)

make_command_code :: Name -> [CommandHandler] -> Q ((String, [Definition]), (String, [Func]))
make_command_code cmd handlers = do
 let name          = undefined
 (TyConI (DataD [] _ _ cons _)) <- reify name 
 let result        = ((header_name, definitions), (source_name, function_defs))
     header_name   = name ++ ".h"
     source_name   = name ++ ".c"
     command_name  = name
     definitions   = undefined
     function_defs = undefined

 return undefined

























