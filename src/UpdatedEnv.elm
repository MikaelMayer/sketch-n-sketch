module UpdatedEnv exposing (UpdatedEnv, original, offset, merge, split, isUnmodified, show)
import Lang exposing (..)
import UpdateUtils
import Utils
import LangUtils exposing (envToString, valEqual)
import Set exposing (Set)
import Dict exposing (Dict)
import UpdateUnoptimized

-- Useful to merge environments faster.
-- Maybe will containn things like "insert a variable with these dependences"

type alias UpdatedEnv = { val: Env, changes: Maybe EnvDiffs }

-- Declares an environment as unmodified
original: Env -> UpdatedEnv
original env = UpdatedEnv env Nothing

-- Merges two modified environments
merge: Exp -> VDiffs -> Env -> UpdatedEnv -> UpdatedEnv -> UpdatedEnv
merge e vdiffs env env1 env2 =
  case vdiffs of
    VUnoptimizedDiffs ->
      let is = LangUtils.freeIdentifiers e in
      UpdatedEnv (UpdateUnoptimized.mergeEnvGeneral e is env env1.val env2.val) (Just <| Dict.fromList [(" unoptimized", [(0, VUnoptimizedDiffs)])])
    _ ->
      if isUnmodified env1 then env2 else if isUnmodified env2 then env1 else
      let (finalEnv, finalChanges) = UpdateUtils.mergeEnv env env1.val env1.changes env2.val env2.changes in
      UpdatedEnv finalEnv finalChanges

offset: Int -> TupleDiffs VDiffs -> TupleDiffs VDiffs
offset = UpdateUtils.offset

toTupleDiffs: List (Maybe a) -> TupleDiffs a
toTupleDiffs l =
  let aux: Int -> List (Maybe a) -> TupleDiffs a -> TupleDiffs a
      aux i l revAcc = case l of
    [] -> List.reverse revAcc
    head :: tail -> case head of
       Nothing ->
         aux (i + 1) tail revAcc
       Just a ->
         (i, a) :: revAcc |>
         aux (i + 1) tail
  in aux 0 l []

-- Given a list of identifiers that are being removed from an environment, builds two environment diffs.
splitIdentifiers: List Ident -> Maybe EnvDiffs -> (Maybe EnvDiffs, Maybe EnvDiffs)
splitIdentifiers idents mbEnvDiffs = case mbEnvDiffs of
  Nothing -> (Nothing, Nothing)
  Just envDiffs ->
    let aux: List String -> (Dict String (List (Maybe VDiffs)), EnvDiffs) -> (Maybe EnvDiffs, Maybe EnvDiffs)
        aux idents (diffs1, diffs2) = case idents of
      [] -> (
          if Dict.isEmpty diffs1 then Nothing else Just <| Dict.map (\k -> toTupleDiffs) diffs1,
          if Dict.isEmpty diffs2 then Nothing else Just diffs2
         )
      head::tail ->
         let (mbDiffs1, newDiffs2) = envFun.removeOneParametrized envFun.unconsTupleDiffs head diffs2 in
         let newDiff1 = case mbDiffs1 |> Maybe.andThen identity of
           Nothing -> Dict.update head (\mbd -> Just <| Nothing :: Maybe.withDefault [] mbd) diffs1
           Just diff -> Dict.update head (\mbd -> Just <| Just diff :: Maybe.withDefault [] mbd) diffs1
         in
         (newDiff1, newDiffs2) |>
         aux tail
    in aux idents (Dict.empty, envDiffs)

-- Returns the first n elements, and the remaining elements
split: Int -> UpdatedEnv -> (UpdatedEnv, UpdatedEnv)
split n uenv =
  let ((d, idents) as env) = uenv.val in
  let changes = uenv.changes in
  let (lenv1, (denv2, didents2) as env2) = envFun.extractLinear n env in
  let env1 = envFun.fromLinear lenv1 in
  case uenv.changes of
    Nothing -> (UpdatedEnv env1 Nothing, UpdatedEnv env2 Nothing)
    Just changes ->
       let (idents1, idents2) = Utils.split n idents in
       let (changes1, changes2) = splitIdentifiers idents1 uenv.changes in
       (UpdatedEnv env1 changes1, UpdatedEnv env2 changes2)

isUnmodified: UpdatedEnv -> Bool
isUnmodified menv = menv.changes |> Utils.maybeIsEmpty

show: UpdatedEnv -> String
show updatedEnv =
  let prunedEnv =
        Dict.foldl (\key change acc ->
           case Dict.get key (Tuple.first updatedEnv.val) of
            Just (v::_) -> Dict.insert key [v] acc
            _ -> acc
        ) Dict.empty (Maybe.withDefault Dict.empty updatedEnv.changes)
  in
  "modified:" ++ envToString ((,) prunedEnv <| Tuple.second updatedEnv.val) ++ "\n(" ++ toString updatedEnv.changes ++ ")"

-- When comparing VClosures, how to get the modifications
{-create: Set Ident -> Env -> Env -> UpdatedEnv
create ks oldEnv newEnv = --Very slow process, we need to optimize that
  let aux: Int -> Set Ident ->  List (Ident, Val) -> List Int -> Env -> Env -> UpdatedEnv
      aux  i      freeVariables accEnv               accModifs   oldEnv newEnv =
    if Set.isEmpty freeVariables then UpdatedEnv (List.reverse accEnv ++ newEnv) accModifs
    else case (oldEnv, newEnv) of
       ([], []) -> UpdatedEnv (List.reverse accEnv) (List.reverse accModifs)
       ((oldk, oldv)::oldtail, (newk, newv)::newtail) ->
         if oldk /= newk then Debug.crash <| "Comparing tow environments which do not have the same order of keys:" ++ oldk ++ " /= " ++ newk
         else if Set.member oldk freeVariables then
           let newModifs = if valEqual oldv newv then accModifs else i::accModifs in
           aux (i + 1) (Set.remove oldk freeVariables) ((newk, newv)::accEnv) newModifs oldtail newtail
         else aux (i + 1) freeVariables ((oldk, newv)::accEnv) accModifs oldtail newtail
       (_, _) -> Debug.crash <| "Comparing tow environments which do not have the same size" ++ envToString oldEnv ++ " /= " ++ envToString newEnv
  in aux 0 ks [] [] oldEnv newEnv
  -}
