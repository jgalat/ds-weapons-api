module API (weaponsStaticAPI) where

import qualified Data.Map.Lazy  as M
import qualified Data.Set       as DS (fromList, toList)
import           Data.Aeson
import           Web.StaticAPI

import           Weapon

unique :: Ord a => [a] -> [a]
unique = DS.toList . DS.fromList

weaponsStaticAPI :: Weapons -> StaticAPI
weaponsStaticAPI weapons =
  let
    weaponIds             = M.keys weapons
    weaponElems           = M.elems weapons
    weaponElemsSimplified = map simplifyWeapon weaponElems
    weaponTypes           = unique (map weapon_type weaponElems)
    normalWeapon          = weapons M.! "dagger"
    possibleUpgrades      = M.keys (upgrades normalWeapon)
    getWeapon e           = weapons M.! (get "id" e)

    weaponIdPath          = constant "weapon" ./ variable "id" weaponIds
  in do
    -- "/"
    route root $ \_ ->
      weaponElemsSimplified

    -- "/weapon_type/:weapon_type"
    route (constant "weapon_type" ./ variable "weaponType" weaponTypes) $ \e ->
      let weaponType = get "weaponType" e
      in filter ((weaponType ==) . weapon_type) weaponElemsSimplified

    -- "/weapon/:weapon_id"
    route weaponIdPath $
      getWeapon

    -- "/weapon/:weapon_id/simplify"
    route (weaponIdPath ./ constant "simplify") $ \e ->
      simplifyWeapon (getWeapon e)

    -- "/weapon/:weapon_id/name"
    route (weaponIdPath ./ constant "name") $ \e ->
      name (getWeapon e)

    -- "/weapon/:weapon_id/description"
    route (weaponIdPath ./ constant "description") $ \e ->
      description (getWeapon e)

    -- "/weapon/:weapon_id/special_features"
    route (weaponIdPath ./ constant "special_features") $ \e ->
      special_features (getWeapon e)

    -- "/weapon/:weapon_id/damage"
    route (weaponIdPath ./ constant "damage") $ \e ->
      damage (getWeapon e)

    -- "/weapon/:weapon_id/requirements"
    route (weaponIdPath ./ constant "requirements") $ \e ->
      requirements (getWeapon e)

    -- "/weapon/:weapon_id/auxiliary"
    route (weaponIdPath ./ constant "auxiliary") $ \e ->
      auxiliary (getWeapon e)

    -- "/weapon/:weapon_id/bonus"
    route (weaponIdPath ./ constant "bonus") $ \e ->
      bonus (getWeapon e)

    -- "/weapon/:weapon_id/upgrades"
    route (weaponIdPath ./ constant "upgrades") $ \e ->
      upgrades (getWeapon e)

    -- "/weapon/:weapon_id/upgrade/:upgrade_path"
    route (weaponIdPath ./ constant "upgrade" ./ variable "path" possibleUpgrades) $ \e ->
      let upgradePath     = get "path" e
          weaponUpgrades  = upgrades (getWeapon e)
      in M.lookup upgradePath weaponUpgrades
