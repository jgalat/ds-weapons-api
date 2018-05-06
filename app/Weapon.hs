{-# LANGUAGE OverloadedStrings #-}
module Weapon where

import           Data.Aeson
import           Data.Map.Lazy
import           Control.Applicative
import           Control.Monad
import           Data.Text

data Auxiliary = Auxiliary  { bleed   :: Int
                            , divine  :: Int
                            , occult  :: Int
                            , poison  :: Int
                            , toxic   :: Int
                            }

instance FromJSON Auxiliary where
  parseJSON (Object o) = Auxiliary  <$> o .: "bleed"
                                    <*> o .: "divine"
                                    <*> o .: "occult"
                                    <*> o .: "poison"
                                    <*> o .: "toxic"
  parseJSON _          = mzero

instance ToJSON Auxiliary where
  toJSON aux = object [ "bleed"   .= bleed aux
                      , "divine"  .= divine aux
                      , "occult"  .= occult aux
                      , "poison"  .= poison aux
                      , "toxic"   .= toxic aux
                      ]

data BonusValue = S | A | B | C | D | E | NONE

instance FromJSON BonusValue where
  parseJSON (String "S")  = pure S
  parseJSON (String "A")  = pure A
  parseJSON (String "B")  = pure B
  parseJSON (String "C")  = pure C
  parseJSON (String "D")  = pure D
  parseJSON (String "E")  = pure E
  parseJSON Null          = pure NONE
  parseJSON _             = mzero

instance ToJSON BonusValue where
  toJSON S    = String "S"
  toJSON A    = String "A"
  toJSON B    = String "B"
  toJSON C    = String "C"
  toJSON D    = String "D"
  toJSON E    = String "E"
  toJSON NONE = Null

data Bonus = Bonus  { bns_strength      :: BonusValue
                    , bns_dexterity     :: BonusValue
                    , bns_intelligence  :: BonusValue
                    , bns_faith         :: BonusValue
                    }

instance FromJSON Bonus where
  parseJSON (Object o) = Bonus  <$> o .: "strength"
                                <*> o .: "dexterity"
                                <*> o .: "intelligence"
                                <*> o .: "faith"
  parseJSON _          = mzero

instance ToJSON Bonus where
  toJSON b  = object  [ "strength"      .= bns_strength b
                      , "dexterity"     .= bns_dexterity b
                      , "intelligence"  .= bns_intelligence b
                      , "faith"         .= bns_faith b
                      ]

data Damage = Damage  { dmg_physical  :: Int
                      , dmg_magic     :: Int
                      , dmg_fire      :: Int
                      , dmg_lightning :: Int
                      }

instance FromJSON Damage where
  parseJSON (Object o) = Damage <$> o .: "physical"
                                <*> o .: "magic"
                                <*> o .: "fire"
                                <*> o .: "lightning"
  parseJSON _          = mzero

instance ToJSON Damage where
  toJSON d = object [ "physical"  .= dmg_physical d
                    , "magic"     .= dmg_magic d
                    , "fire"      .= dmg_fire d
                    , "lightning" .= dmg_lightning d
                    ]

data DamageReduction = DamageReduction  { red_physical  :: Float
                                        , red_magic     :: Float
                                        , red_fire      :: Float
                                        , red_lightning :: Float
                                        }

instance FromJSON DamageReduction where
  parseJSON (Object o) = DamageReduction  <$> o .: "physical"
                                          <*> o .: "magic"
                                          <*> o .: "fire"
                                          <*> o .: "lightning"
  parseJSON _          = mzero

instance ToJSON DamageReduction where
  toJSON d = object [ "physical"  .= red_physical d
                    , "magic"     .= red_magic d
                    , "fire"      .= red_fire d
                    , "lightning" .= red_lightning d
                    ]

data Requirements = Requirements  { req_strength      :: Int
                                  , req_dexterity     :: Int
                                  , req_intelligence  :: Int
                                  , req_faith         :: Int
                                  }

instance FromJSON Requirements where
  parseJSON (Object o) = Requirements <$> o .: "strength"
                                      <*> o .: "dexterity"
                                      <*> o .: "intelligence"
                                      <*> o .: "faith"
  parseJSON _          = mzero

instance ToJSON Requirements where
  toJSON r  = object  [ "strength"      .= req_strength r
                      , "dexterity"     .= req_dexterity r
                      , "intelligence"  .= req_intelligence r
                      , "faith"         .= req_faith r
                      ]

type UpgradePath = String

type UpgradeLevel = String

data UpgradeUpdate = UpgradeUpdate  { upu_damage            :: Damage
                                    , upu_damage_reduction  :: DamageReduction
                                    , upu_bonus             :: Bonus
                                    }

instance FromJSON UpgradeUpdate where
  parseJSON (Object o) = UpgradeUpdate  <$> o .: "damage"
                                        <*> o .: "damage_reduction"
                                        <*> o .: "bonus"
  parseJSON _          = mzero

instance ToJSON UpgradeUpdate where
  toJSON uu = object  [ "damage"            .= upu_damage uu
                      , "damage_reduction"  .= upu_damage_reduction uu
                      , "bonus"             .= upu_bonus uu
                      ]

type Upgrade = Map UpgradeLevel UpgradeUpdate

type Upgrades = Map UpgradePath Upgrade

data Weapon = WeaponFull  { name              :: String
                          , description       :: String
                          , special_features  :: Maybe String
                          , weapon_type       :: String
                          , attack_type       :: String
                          , weight            :: Float
                          , stability         :: Float
                          , durability        :: Int
                          , critial           :: Int
                          , damage            :: Damage
                          , damage_reduction  :: DamageReduction
                          , requirements      :: Requirements
                          , auxiliary         :: Auxiliary
                          , bonus             :: Bonus
                          , upgrades          :: Upgrades
                          }
            | WeaponSimple  { name              :: String
                            , weapon_type       :: String
                            , attack_type       :: String
                            , weight            :: Float
                            , durability        :: Int
                            , damage            :: Damage
                            , damage_reduction  :: DamageReduction
                            , requirements      :: Requirements
                            , bonus             :: Bonus
                            }

instance FromJSON Weapon where
  parseJSON (Object o)  = WeaponFull  <$> o .: "name"
                                      <*> o .: "description"
                                      <*> o .: "special_features"
                                      <*> o .: "weapon_type"
                                      <*> o .: "attack_type"
                                      <*> o .: "weight"
                                      <*> o .: "stability"
                                      <*> o .: "durability"
                                      <*> o .: "critical"
                                      <*> o .: "damage"
                                      <*> o .: "damage_reduction"
                                      <*> o .: "requirements"
                                      <*> o .: "auxiliary"
                                      <*> o .: "bonus"
                                      <*> o .: "upgrades"
                        <|> WeaponSimple  <$> o .: "name"
                                          <*> o .: "weapon_type"
                                          <*> o .: "attack_type"
                                          <*> o .: "weight"
                                          <*> o .: "durability"
                                          <*> o .: "damage"
                                          <*> o .: "damage_reduction"
                                          <*> o .: "requirements"
                                          <*> o .: "bonus"
  parseJSON _           = mzero

instance ToJSON Weapon where
  toJSON (WeaponFull n de sf wt at w s du c dm dr r a b u) =
    object  [ "name"              .= n
            , "description"       .= de
            , "special_features"  .= sf
            , "weapon_type"       .= wt
            , "weight"            .= w
            , "stability"         .= s
            , "durability"        .= du
            , "critical"          .= c
            , "damage"            .= dm
            , "damage_reduction"  .= dr
            , "requirements"      .= r
            , "auxiliary"         .= a
            , "bonus"             .= b
            , "attack_type"       .= at
            , "upgrades"          .= u
            ]
  toJSON (WeaponSimple n wt at w du dm dr r b) =
    object  [ "name"              .= n
            , "weapon_type"       .= wt
            , "attack_type"       .= at
            , "weight"            .= w
            , "durability"        .= du
            , "damage"            .= dm
            , "damage_reduction"  .= dr
            , "requirements"      .= r
            , "bonus"             .= b
            ]

type WeaponId = String

type Weapons = Map WeaponId Weapon

simplifyWeapon :: Weapon -> Weapon
simplifyWeapon (WeaponFull n _ _ wt at w _ du _ dm dr r _ b _) =
  WeaponSimple n wt at w du dm dr r b
simplifyWeapon _ = undefined
