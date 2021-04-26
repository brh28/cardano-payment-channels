{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Main.UniPaymentChannel where

import           Control.Monad                     (void)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import qualified Ledger.Ada                as Ada
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)

import           Plutus.Contract
--import qualified Data.ByteString.Lazy.Char8   as BSLC
import           Ledger
import           Wallet.Emulator                   (Wallet (..))
import qualified Wallet.Emulator                   as Emulator
import           Ledger.Typed.Scripts              (ScriptInstance)
import qualified Ledger.Typed.Scripts              as Scripts
import           Ledger.Value                      (Value)
import qualified Ledger.Value                      as Value
import qualified Ledger.Constraints                as Constraints
import           Prelude                            (Semigroup (..))
import qualified Ledger.Contexts                      as V
import qualified Ledger.Interval                      as Interval
import           Playground.Contract                (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))



data PaymentChannelParams = PaymentChannelParams {
    pcSender    :: PubKeyHash,
    pcReceiver  :: PubKeyHash,
    pcExpires   :: Slot
} deriving Show
PlutusTx.unstableMakeIsData ''PaymentChannelParams
PlutusTx.makeLift ''PaymentChannelParams

openChannelRange :: PaymentChannelParams -> SlotRange
openChannelRange pc = Interval.to (pcExpires pc)

refundRange :: PaymentChannelParams -> SlotRange
refundRange pc = Interval.from (pcExpires pc)

{-# INLINABLE validatePayment #-}
validatePayment :: PaymentChannelParams -> () -> () -> ScriptContext -> Bool
validatePayment m@PaymentChannelParams{pcSender, pcReceiver, pcExpires} _ _ ctx =
    let txInfo = scriptContextTxInfo ctx
    in 
        traceIfFalse "needs signature of receiver" (pcReceiver `elem` txInfoSignatories txInfo)
        && traceIfFalse "needs signature of sender" (pcSender `elem` txInfoSignatories txInfo)
        && traceIfFalse "not in valid range" ((openChannelRange m) `Interval.contains` (txInfoValidRange txInfo)) 

data PaymentChannel
instance Scripts.ScriptType PaymentChannel where
    type instance RedeemerType PaymentChannel = ()
    type instance DatumType PaymentChannel = ()

scrInstance :: PaymentChannelParams -> Scripts.ScriptInstance PaymentChannel
scrInstance p = Scripts.validator @PaymentChannel
    ($$(PlutusTx.compile [|| validatePayment ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @() @()

validator :: PaymentChannelParams -> Validator
validator = Scripts.validatorScript . scrInstance

scrAddress :: PaymentChannelParams -> Ledger.Address
scrAddress = scriptAddress . validator

data DepositParams = DepositParams
    { receiver  :: !PubKeyHash
    , amount    :: !Integer
    , expires   :: !Slot
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

--(HasBlockchainActions s, AsContractError e) => GiveParams -> Contract w s e ()
deposit :: (HasBlockchainActions s, AsContractError e) => DepositParams -> Contract w s e ()
deposit DepositParams{receiver, amount, expires} = do
    s <- pubKeyHash <$> ownPubKey
    let p = PaymentChannelParams {
            pcSender = s,
            pcReceiver = receiver,
            pcExpires = expires
        }
    logInfo $ "Open Channel range = " <> show (openChannelRange p)
    let tx = Constraints.mustPayToTheScript () $ Ada.lovelaceValueOf $ amount
    void $ submitTxConstraints (scrInstance p) tx
    

data WithdrawParams = WithdrawParams
    { sender  :: !PubKeyHash
    , expireSlot   :: !Slot
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

pks :: PaymentChannelParams -> (PubKeyHash, PubKeyHash)
pks pc = (pcSender pc, pcReceiver pc) 

withdraw :: AsContractError e => WithdrawParams -> Contract () PaymentChannelSchema e ()
withdraw WithdrawParams{sender, expireSlot} = do
    r <- pubKeyHash <$> ownPubKey
    let p = PaymentChannelParams {
            pcSender = sender,
            pcReceiver = r,
            pcExpires = expireSlot
        }
    -- utx <- utxoAt (Scripts.scriptAddress scrAddress)
    utx <- utxoAt $ scrAddress p

    let tx = collectFromScript utx ()
                -- <> foldMap Constraints.mustBeSignedBy $ pks p
    void $ submitTxConstraintsSpending (scrInstance p) utx tx

type PaymentChannelSchema =
    BlockchainActions
        .\/ Endpoint "deposit" DepositParams
        .\/ Endpoint "withdraw" WithdrawParams 

-- Plutus Playground
endpoints :: AsContractError e => Contract () PaymentChannelSchema e ()
endpoints = deposit' `select` withdraw' >> endpoints
    where
        deposit' = endpoint @"deposit" >>= deposit
        withdraw' = endpoint @"withdraw" >>= withdraw

mkSchemaDefinitions ''PaymentChannelSchema

$(mkKnownCurrencies [])
