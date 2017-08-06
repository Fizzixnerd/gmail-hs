{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Gmail.TH where

import Data.List
import qualified Data.Text as T
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as Syn

import Gmail.Types

class UrlEncode a where
  toUrlArguments :: a -> UrlArguments

class UrlShow a where
  urlShow :: a -> Maybe String

instance UrlShow T.Text where
  urlShow t = if t == T.empty then
                Nothing
              else
                Just $ T.unpack $ T.map (\c -> if c == ' ' then '+' else c) t

instance UrlShow a => UrlShow [a] where
  urlShow [] = Nothing
  urlShow (x:xs) = let listOfMaybes = intersperse (Just ",") $ fmap urlShow (x:xs) in
    foldl (\acc x -> case acc of
             Nothing -> x
             Just s  -> case x of
               Nothing -> acc
               Just v  -> Just (s ++ v)) Nothing listOfMaybes

instance UrlShow Bool where
  urlShow True = Just "true"
  urlShow False = Just "false"

instance UrlShow Int where
  urlShow = Just . show

getUrlArgumentsNames :: TH.Dec -> [TH.Name]
getUrlArgumentsNames (TH.DataD _ _ _ [(TH.RecC _ vars)] _) =
  fmap (\(name, _, _) -> name) vars
getUrlArgumentsNames _ = error "The declaration must be a data decl with a single record constructor."

getTypeConDec :: TH.Name -> TH.DecQ
getTypeConDec name = do
  tyCon <- TH.reify name
  case tyCon of
    TH.TyConI dec -> return dec
    _ -> error "Must be a 'plain' type constructor."

getTypeName :: TH.Type -> TH.Name
getTypeName (TH.ConT name) = name
getTypeName _ = error "Expected a ConT."

getInstanceFunctionsQ :: TH.Q [TH.Name] -> TH.ExpQ
getInstanceFunctionsQ namesQ = namesQ >>= TH.listE . (fmap (\name -> 
                                                           [| (\inst ->
                                                                 let value = urlShow $ $(TH.varE name) inst in
                                                                 ($(Syn.liftString $ TH.nameBase name), value)) |] ))

deriveUrlEncode :: TH.Name -> TH.DecsQ
deriveUrlEncode typeName =
  let decQ = getTypeConDec typeName 
      fieldNamesQ = fmap getUrlArgumentsNames decQ
      instanceFunctionsQ = getInstanceFunctionsQ fieldNamesQ

  in
    [d| instance UrlEncode $(return $ TH.ConT typeName)  where
          toUrlArguments x = map ($ x) $instanceFunctionsQ |]
