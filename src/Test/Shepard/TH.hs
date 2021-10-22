{-# LANGUAGE TemplateHaskell #-}

-- | Template Haskell derivation functions for the Shepard-related typeclasses.
module Test.Shepard.TH
  ( deriveShepard
  , deriveAddShrinks
  , deriveSeedShepard
  ) where

import           Control.Monad (foldM, forM)
import           Data.Typeable (Typeable)
import           Language.Haskell.TH
import           TH.ReifySimple

import Test.Shepard.Core


--------------------------------------------------------------------------------
-- Shepard instance derivation
--------------------------------------------------------------------------------

-- | Derive a `Shepard` instance for datatypes which have `Shepard` and `AddShrinks`
-- instances for their enclosed fields.
-- `tinker`s recursively with fields of a datatype, then uses `<$$>` and `<**>`
-- to map the constructor over the tinkered fields.
-- `conjure`s by using `<$>` and `<*>` over recursive calls to `conjure`.
--
-- @
--   deriveShepard ''(,)
--   ======>
--   instance (Shepard s a,
--             AddShrinks a,
--             Shepard s b,
--             AddShrinks b) =>
--            Shepard s ((,) a b) where
--     tinker gen
--       = tinkerRummagedOrConjureOrSave
--           (((\ a b -> ((,) a) b)
--              <$$> tinker ((\ ((,) a _) -> a) <$> gen))
--              <**> tinker ((\ ((,) _ b) -> b) <$> gen))
--     conjure = (saveInBagOfTricks =<< (((,) <$> conjure) <*> conjure))
-- @
deriveShepard :: Name -> Q [Dec]
deriveShepard name = do
  (DataType _dName dTyVars _dCtx dCons) <- reifyDataType name
  genomeVar <- VarT <$> newName "genome"
  classParamNames <- forM dTyVars (const (newName "arg"))
  let con = ensureSingleton dCons
  ctx <- if null classParamNames
            then (:[]) <$> [t| GeneOps $(pure genomeVar) |]
            else wrapWithShepardConstraints genomeVar classParamNames
  decTy <- makeInstanceType genomeVar classParamNames
  tink <- makeTinker con
  conj <- makeConjure con
  pure [InstanceD Nothing ctx decTy [tink, conj]]
 where
  ensureSingleton dCons =
    case dCons of
      []  -> error "deriveShepard: cannot derive Shepard for a void type"
      [x] -> x
      _   -> error "deriveShepard: cannot derive Shepard for a sum type"

  -- Create constraints `(Shepard s a, AddShrinks a ...) => ...` for the instance
  wrapWithShepardConstraints genomeVar = sequence .
    concatMap (\cpn -> [ [t| Shepard $(pure genomeVar) $(pure (VarT cpn)) |]
                       , [t| AddShrinks $(pure (VarT cpn)) |]
                       ])

  -- Make instance type `... => Shepard s (Foo a b ...)`
  makeInstanceType genomeVar classParamNames =
    [t| Shepard $(pure genomeVar)
               ( $(pure (foldl wrapTyVars (ConT name) classParamNames)) ) |]
  wrapTyVars acc cpn = AppT acc (VarT cpn)

  makeTinker con = do
    if length (dcFields con) <= 0
       then mkPureDec "tinker"
       else do
         argName <- newName "arg"
         let pat = VarP argName
         body <- mkBody argName
         pure (FunD (mkName "tinker") [Clause [pat] (NormalB body) []])
   where
    mkBody argName = do
      fieldNames <- forM (dcFields con) (const (newName "field"))
      let accessors = makeAccessors (dcName con) fieldNames
      start <- [| $(pure (makeConPacker (dcName con) fieldNames))
                    <$$> (tinker ($(pure (head accessors))
                            <$> $(pure (VarE argName)))) |]
      let tinkerBody =
            foldM (\acc getter -> [| $(pure acc)
                                       <**> (tinker ($(pure getter)
                                              <$> $(pure (VarE argName)))) |])
                  start
                  (tail accessors)
      [| tinkerRummagedOrConjureOrSave $(tinkerBody) |]

  makeConjure con = do
    if length (dcFields con) <= 0
       then mkNullaryDefn
       else do
         start <- [| $(pure (ConE (dcName con))) <$> conjure |]
         body <- foldM (\acc _ -> [| $(pure acc) <*> conjure |])
                       start
                       (tail (dcFields con))
         cb <- [| saveInBagOfTricks =<< $(pure body) |]
         pure (FunD (mkName "conjure") [Clause [] (NormalB cb) []])
   where
    mkNullaryDefn = do
      body <- [| pure $(pure (ConE (dcName con))) |]
      pure (FunD (mkName "conjure") [Clause [] (NormalB body) []])


--------------------------------------------------------------------------------
-- AddShrinks instance derivation
--------------------------------------------------------------------------------

-- | Derive an `AddShrinks` instance for datatypes which have `AddShrinks`
-- instances for their enclosed fields. Simply performs structural recursion
-- on fields, then uses `<$>` and `<*>` to apply the constructor over the
-- `addShrinks` of the fields.
--
-- @
--   deriveAddShrinks ''(,)
--   ======>
--   instance (AddShrinks a, AddShrinks b) =>
--            AddShrinks ((,) a b) where
--     addShrinks ((,) x y)
--       = (((\ x y -> ((,) x) y)
--            <$> addShrinks x)
--            <*> addShrinks y)
-- @
deriveAddShrinks :: Name -> Q [Dec]
deriveAddShrinks name = do
  (DataType _dName dTyVars _dCtx dCons) <- reifyDataType name
  classParamNames <- forM dTyVars (const (newName "arg"))
  ctx <- wrapWithConstraints classParamNames
  decTy <- makeInstanceType classParamNames
  addShrinkDec <- makeAddShrinks dCons
  pure [InstanceD Nothing ctx decTy [addShrinkDec]]
 where

  -- Create constraints `(AddShrinks a ...) => ...` for the instance
  wrapWithConstraints = sequence .
    map (\cpn -> [t| AddShrinks $(pure (VarT cpn)) |])

  -- Make instance type `... => AddShrinks (Foo a b ...)`
  makeInstanceType classParamNames =
    [t| AddShrinks
          ( $(pure (foldl wrapTyVars (ConT name) classParamNames)) ) |]
  wrapTyVars acc cpn = AppT acc (VarT cpn)

  makeAddShrinks :: [DataCon] -> Q Dec
  makeAddShrinks dcs = do
    clauses <- mapM makeAddShrinksClause dcs
    pure (FunD (mkName "addShrinks") clauses)

  makeAddShrinksClause :: DataCon -> Q Clause
  makeAddShrinksClause con =
    if length (dcFields con) <= 0

       then do
         field <- newName "field"
         body <- [| pure $(pure (VarE field)) |]
         pure (Clause [VarP field] (NormalB body) [])

       else do
         fieldNames <- forM (dcFields con) (const (newName "field"))
         let pat = ConP (dcName con) (map VarP fieldNames)

         start <- [| $(pure (makeConPacker (dcName con) fieldNames))
                       <$> addShrinks $(pure (VarE (head fieldNames))) |]
         body <- foldM (\acc v -> [| $(pure acc) <*> addShrinks $(pure (VarE v)) |])
                       start
                       (tail (fieldNames))
         pure (Clause [pat] (NormalB body) [])


--------------------------------------------------------------------------------
-- SeedShepard instance derivation
--------------------------------------------------------------------------------

-- | Derive a `SeedShepard` instance which calls `saveInBagOfTricks` on the
-- argument then recurs structurally on fields.
--
-- @
--   deriveSeedShepard ''(,)
--   ======>
--   instance (SeedShepard a,
--             Typeable a,
--             SeedShepard b,
--             Typeable b) =>
--            SeedShepard ((,) a b) where
--     seeder p@((,) x y)
--       = do (() <$ saveInBagOfTricks p)
--            seeder x
--            seeder y
-- @
deriveSeedShepard :: Name -> Q [Dec]
deriveSeedShepard name = do
  (DataType _dName dTyVars _dCtx dCons) <- reifyDataType name
  classParamNames <- forM dTyVars (const (newName "arg"))
  ctx <- wrapWithConstraints classParamNames
  decTy <- makeInstanceType classParamNames
  sdr <- makeSeeder dCons
  pure [InstanceD Nothing ctx decTy [sdr]]
 where

  -- Create constraints `(SeedShepard a ...) => ...` for the instance
  wrapWithConstraints = sequence .
    concatMap (\cpn -> [ [t| SeedShepard $(pure (VarT cpn)) |]
                       , [t| Typeable $(pure (VarT cpn)) |]
                       ])


  -- Make instance type `... => SeedShepard (Foo a b ...)`
  makeInstanceType classParamNames =
    [t| SeedShepard
          ( $(pure (foldl wrapTyVars (ConT name) classParamNames)) ) |]
  wrapTyVars acc cpn = AppT acc (VarT cpn)

  makeSeeder :: [DataCon] -> Q Dec
  makeSeeder dcs = do
    clauses <- mapM makeSeederClause dcs
    pure (FunD (mkName "seeder") clauses)

  makeSeederClause :: DataCon -> Q Clause
  makeSeederClause con = do
    asName <- newName "argAs"
    fieldNames <- forM (dcFields con) (const (newName "field"))
    let pat = AsP asName (ConP (dcName con) (map VarP fieldNames))

    seedAs <- [| () <$ saveInBagOfTricks $(pure (VarE asName)) |]
    seedRest <- forM fieldNames $ \vName -> do
                  [| seeder $(pure (VarE vName)) |]
    let stmts = map NoBindS (seedAs:seedRest)
    pure (Clause [pat] (NormalB (DoE stmts)) [])

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Take a constructor name and a list of arguments, and return a lambda
-- which receives the arguments and packs them into the constructor.
makeConPacker :: Name -> [Name] -> Exp
makeConPacker conName argNames =
  let body = foldl (\acc v -> AppE acc (VarE v))
                   (ConE conName)
                   argNames
   in LamE (map VarP argNames) body

-- | Take a constructor name and a list of arguments, and return a list
-- of lambdas which access each respective constructor field, in order.
makeAccessors :: Name -> [Name] -> [Exp]
makeAccessors conName argNames =
  [ let front = replicate i WildP
        back  = replicate (length argNames - i - 1) WildP
        arg   = argNames !! i
        pat = ConP conName (front ++ [VarP arg] ++ back)
     in LamE [pat] (VarE arg)
  | i <- [0 .. length argNames - 1]
  ]

-- | Create a decl of the form `<name> = pure`
mkPureDec :: String -> Q Dec
mkPureDec name = do
  body <- [| pure |]
  pure (FunD (mkName name) [Clause [] (NormalB body) []])
