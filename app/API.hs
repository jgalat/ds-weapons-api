module API (weaponsStaticAPI) where

import qualified Data.Map.Lazy  as M
import           Data.Aeson
import           Data.List
import           Web.StaticAPI

import           Weapon

weaponsStaticAPI :: Weapons -> StaticAPI
weaponsStaticAPI weapons =
  let
    weaponIds             = M.keys weapons
    weaponElems           = M.elems weapons
    weaponElemsSimplified = map simplifyWeapon weaponElems
    weaponTypes           = nub (map weapon_type weaponElems)
    normalWeapon          = weapons M.! "dagger"
    possibleUpgrades      = M.keys (upgrades normalWeapon)
    getWeapon             = (weapons M.!) <$> readVariable "id"

    weaponIdPath          = constant "weapon" ./ variable "id" weaponIds
  in do
    -- "/"
    route root (return weaponElemsSimplified)

    -- "/weapon_type/:weapon_type"
    route (constant "weapon_type" ./ variable "weaponType" weaponTypes) $ do
      weaponType <- readVariable "weaponType"
      return (filter ((weaponType ==) . weapon_type) weaponElemsSimplified)

    -- "/weapon/:weapon_id"
    route weaponIdPath getWeapon

    -- "/weapon/:weapon_id/simplify"
    route (weaponIdPath ./ constant "simplify") $
      simplifyWeapon <$> getWeapon

    -- "/weapon/:weapon_id/name"
    route (weaponIdPath ./ constant "name") $
      name <$> getWeapon

    -- "/weapon/:weapon_id/description"
    route (weaponIdPath ./ constant "description") $
      description <$> getWeapon

    -- "/weapon/:weapon_id/special_features"
    route (weaponIdPath ./ constant "special_features") $
      special_features <$> getWeapon

    -- "/weapon/:weapon_id/damage"
    route (weaponIdPath ./ constant "damage") $
      damage <$> getWeapon

    -- "/weapon/:weapon_id/requirements"
    route (weaponIdPath ./ constant "requirements") $
      requirements <$> getWeapon

    -- "/weapon/:weapon_id/auxiliary"
    route (weaponIdPath ./ constant "auxiliary") $
      auxiliary <$> getWeapon

    -- "/weapon/:weapon_id/bonus"
    route (weaponIdPath ./ constant "bonus") $
      bonus <$> getWeapon

    -- "/weapon/:weapon_id/upgrades"
    route (weaponIdPath ./ constant "upgrades") $
      upgrades <$> getWeapon

    -- "/weapon/:weapon_id/upgrade/:upgrade_path"
    route (weaponIdPath ./ constant "upgrade" ./ variable "path" possibleUpgrades) $ do
      upgradePath <- readVariable "path"
      weapon      <- getWeapon
      return (M.lookup upgradePath (upgrades weapon))
