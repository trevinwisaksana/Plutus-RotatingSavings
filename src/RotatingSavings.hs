{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE NumericUnderscores #-}

module RotatingSavings where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract      as Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import qualified PlutusTx.Builtins    as Builtins
import           Control.Monad.Freer.Extras as Extras
import           Data.Default               (Default (..))
import           Data.Functor               (void)
import           Ledger.TimeSlot
import           Plutus.Trace.Emulator  as Emulator
import           Wallet.Emulator.Wallet
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String, Int, fromIntegral)
import           Text.Printf          (printf)

minLovelace :: Integer
minLovelace = 2000000

data SavingsDatum = SavingsDatum
    { members        :: [PaymentPubKeyHash]
    , winners        :: [PaymentPubKeyHash]
    , stake          :: Integer
    , joinDeadline   :: POSIXTime
    } deriving Show

PlutusTx.unstableMakeIsData ''SavingsDatum

data SavingsRedeemer = JoinSession | MakePayment PaymentPubKeyHash | Leave PaymentPubKeyHash | Raffle PaymentPubKeyHash
    deriving Show

PlutusTx.unstableMakeIsData ''SavingsRedeemer

{-# INLINABLE  mkRotatingSavingsValidator #-}
mkRotatingSavingsValidator ::  SavingsDatum -> SavingsRedeemer -> ScriptContext -> Bool
mkRotatingSavingsValidator dat red ctx =
    case red of
        JoinSession ->
            traceIfFalse "Join deadline has passed"      deadlineNotPassed &&
            traceIfFalse "Minimum stake is 2 ADA"        stakeIsGreaterThanZero
        MakePayment pkh ->
            traceIfFalse "Stake cannot be zero"          stakeIsGreaterThanZero &&
            traceIfFalse "User not in session"           (isPartOfSession pkh dat)
        Raffle pkh ->
            traceIfFalse "Join deadline has not passed"  deadlineReached &&
            traceIfFalse "Stake cannot be zero"          stakeIsGreaterThanZero &&
            traceIfFalse "User not in session"           (isPartOfSession pkh dat)
        Leave pkh ->
            traceIfFalse "User not in session"           (isPartOfSession pkh dat)
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        deadlineReached :: Bool
        deadlineReached = contains (from $ joinDeadline dat) $ txInfoValidRange info

        deadlineNotPassed :: Bool
        deadlineNotPassed = contains (to $ joinDeadline dat) $ txInfoValidRange info

        stakeIsGreaterThanZero :: Bool
        stakeIsGreaterThanZero = stake dat > minLovelace

        isPartOfSession :: PaymentPubKeyHash -> SavingsDatum -> Bool
        isPartOfSession pkh dat = pkh `elem` members dat

data SavingsSession
instance Scripts.ValidatorTypes SavingsSession where
    type instance RedeemerType SavingsSession = SavingsRedeemer
    type instance DatumType SavingsSession = SavingsDatum

typedRotatingSavingsValidator :: Scripts.TypedValidator SavingsSession
typedRotatingSavingsValidator = Scripts.mkTypedValidator @SavingsSession
    $$(PlutusTx.compile [|| mkRotatingSavingsValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
      where
        wrap = Scripts.wrapValidator @SavingsDatum @SavingsRedeemer

validator :: Validator
validator = Scripts.validatorScript typedRotatingSavingsValidator

rotatingSavingsScript :: Ledger.ValidatorHash
rotatingSavingsScript = Scripts.validatorHash typedRotatingSavingsValidator

rotatingSavingsScriptAddress :: Ledger.Address
rotatingSavingsScriptAddress = scriptAddress validator

--OFF-CHAIN

alreadyInSession :: forall w s e. AsContractError e => PaymentPubKeyHash -> Contract w s e Bool
alreadyInSession pkh = do
    utxos <- Map.filter (containsPkh pkh) <$> utxosAt rotatingSavingsScriptAddress
    if Map.null utxos
        then return False
        else return True

containsPkh :: PaymentPubKeyHash -> ChainIndexTxOut -> Bool
containsPkh pkh o = case _ciTxOutDatum o of
    Left _          -> False
    Right (Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> False
        Just d  -> pkh `elem` members d

getDatum' :: ChainIndexTxOut -> SavingsDatum
getDatum' o = case _ciTxOutDatum o of
    Left  _ -> traceError "Datum does not exist"
    Right (Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> traceError "Unknown datum type"
        Just d  -> d

data StartSessionParams = StartSessionParams
    { ssStake            :: !Integer
    , ssJoinDeadline     :: !POSIXTime
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

startSession :: AsContractError e => StartSessionParams -> Contract w s e ()
startSession p = do
    pkh <- Contract.ownPaymentPubKeyHash
    let dat = SavingsDatum
                { stake         = ssStake p
                , members       = [pkh]
                , winners       = []
                , joinDeadline  = ssJoinDeadline p
                }
        tx   = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ ssStake p
    ledgerTx <- submitTxConstraints typedRotatingSavingsValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ "Session has started"

data JoinSessionParams = JoinSessionParams
    { jsStake            :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

joinSession :: AsContractError e => JoinSessionParams -> Contract w s e ()
joinSession p = do
    pkh <- Contract.ownPaymentPubKeyHash
    now   <- currentTime
    Contract.logInfo $ "Current time:" <> show now
    isInSession <- alreadyInSession pkh
    if isInSession then Contract.logInfo @String $ "Cannot join another session" else (do
        utxos <- Map.filter (isSuitable now) <$> utxosAt rotatingSavingsScriptAddress
        if Map.null utxos
            then Contract.logInfo @String $ "No saving session available"
            else do
                let sessions = snd <$> Map.toList utxos
                    oref = head $ fst <$> Map.toList utxos
                    most = hasMostMembers sessions
                    dat = getDatum' most
                    newDat = SavingsDatum
                        { stake    = stake dat + jsStake p
                        , members  = pkh : members dat
                        , winners  = []
                        , joinDeadline = joinDeadline dat
                        }
                    lookups = Constraints.unspentOutputs utxos <>
                              Constraints.otherScript validator <>
                              Constraints.typedValidatorLookups typedRotatingSavingsValidator
                    tx   = Constraints.mustPayToTheScript newDat (Ada.lovelaceValueOf $ stake newDat) <>
                           Constraints.mustValidateIn (to $ now + 1000) <>
                           Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData JoinSession)
                Contract.logInfo $ "Join Deadline:" <> show (joinDeadline dat)
                Contract.logInfo $ "New members:" <> show (members newDat)
                ledgerTx <- submitTxConstraintsWith @SavingsSession lookups tx
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                Contract.logInfo @String $ "Joined session")
    where
        isSuitable :: POSIXTime -> ChainIndexTxOut -> Bool
        isSuitable now o = case _ciTxOutDatum o of
            Left _          -> False
            Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                Nothing -> False
                Just d  -> joinDeadline d > now && length (members d) < 12

        hasMostMembers :: [ChainIndexTxOut] -> ChainIndexTxOut
        hasMostMembers [x] = x
        hasMostMembers (x:xs)
            | membersCount > maxTail = x
            | otherwise = hasMostMembers xs
            where
                membersCount = length $ members (getDatum' x)
                maxTail = length $ members (getDatum' $ hasMostMembers xs)

leaveSession :: AsContractError e => Contract w s e ()
leaveSession = do
    pkh <- Contract.ownPaymentPubKeyHash
    utxos <- Map.filter (containsPkh pkh) <$> utxosAt rotatingSavingsScriptAddress
    if Map.null utxos
        then Contract.logInfo @String $ "You have not joined a session"
        else do
            let dat = getDatum' (head $ snd <$> Map.toList utxos)
                oref = head $ fst <$> Map.toList utxos
                newDat = SavingsDatum
                    { stake    = stake dat
                    , members  = removePkh pkh $ members dat
                    , winners  = removePkh pkh $ winners dat
                    , joinDeadline = joinDeadline dat
                    }
                lookups = Constraints.unspentOutputs utxos <>
                          Constraints.otherScript validator <>
                          Constraints.typedValidatorLookups typedRotatingSavingsValidator
                tx   = Constraints.mustPayToTheScript newDat (Ada.lovelaceValueOf $ stake dat) <>
                       Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Leave pkh)
            Contract.logInfo $ "Members:" <> show (members newDat)
            ledgerTx <- submitTxConstraintsWith @SavingsSession lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ "Left session"
    where
        removePkh :: PaymentPubKeyHash -> [PaymentPubKeyHash] -> [PaymentPubKeyHash]
        removePkh _ [] = []
        removePkh x (y:ys) | x == y = removePkh x ys
                           | otherwise = y : removePkh x ys

makePayment :: AsContractError e => Integer -> Contract w s e ()
makePayment amount = do
    pkh <- Contract.ownPaymentPubKeyHash
    utxos <- Map.filter (containsPkh pkh) <$> utxosAt rotatingSavingsScriptAddress
    test <- utxosAt rotatingSavingsScriptAddress
    Contract.logInfo $ "UTXOs:" <> show test
    if Map.null utxos
        then Contract.logInfo @String $ "You have not joined a session"
        else do
            let dat = getDatum' (head $ snd <$> Map.toList utxos)
                oref = head $ fst <$> Map.toList utxos
                newDat = SavingsDatum
                    { stake    = stake dat + amount
                    , members  = members dat
                    , winners  = winners dat
                    , joinDeadline = joinDeadline dat
                    }
                lookups = Constraints.unspentOutputs utxos <>
                          Constraints.otherScript validator <>
                          Constraints.typedValidatorLookups typedRotatingSavingsValidator
                tx   = Constraints.mustPayToTheScript newDat (Ada.lovelaceValueOf $ stake newDat) <>
                       Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ MakePayment pkh)
            Contract.logInfo $ "Stake:" <> show (stake newDat)
            ledgerTx <- submitTxConstraintsWith @SavingsSession lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ "Made payment to script"

raffle :: AsContractError e => Contract w s e ()
raffle = do
    pkh <- Contract.ownPaymentPubKeyHash
    utxos <- Map.filter (containsPkh pkh) <$> utxosAt rotatingSavingsScriptAddress
    Contract.logInfo $ "UTXOs:" <> show utxos
    if Map.null utxos
        then Contract.logInfo @String $ "You have not joined a session"
        else do
            let session = head $ snd <$> Map.toList utxos
                oref = head $ fst <$> Map.toList utxos
                dat = getDatum' (head $ snd <$> Map.toList utxos)
                everyoneHasWonOnce = length (winners dat) == length (members dat)
            if everyoneHasWonOnce then resetSession dat utxos oref else continueSession dat utxos oref
    where
        shuffleMembers :: [PaymentPubKeyHash] -> [PaymentPubKeyHash]
        shuffleMembers [] = []
        shuffleMembers xs = let (ys,zs) = splitAlt xs
                            in ys ++ shuffleMembers zs

        splitAlt :: [a] -> ([a],[a])
        splitAlt = PlutusTx.Prelude.foldr (\x (ys, zs) -> (x:zs, ys)) ([],[])

        notWinners :: [PaymentPubKeyHash] -> [PaymentPubKeyHash] -> [PaymentPubKeyHash]
        notWinners members winners = PlutusTx.Prelude.filter (`notElem` winners) members

        resetSession :: AsContractError e => SavingsDatum -> Map TxOutRef ChainIndexTxOut -> TxOutRef -> Contract w s e ()
        resetSession dat utxos oref = do
            pkh <- Contract.ownPaymentPubKeyHash
            now   <- currentTime
            let sMembers = shuffleMembers (members dat)
                winner = head sMembers
                newDat = SavingsDatum {
                  stake    = minLovelace
                , members  = members dat
                , winners  = [winner]
                , joinDeadline = joinDeadline dat
                }
                lookups = Constraints.unspentOutputs utxos <>
                          Constraints.otherScript validator <>
                          Constraints.typedValidatorLookups typedRotatingSavingsValidator
                tx = Constraints.mustPayToPubKey winner (Ada.lovelaceValueOf (stake dat - minLovelace)) <>
                     Constraints.mustPayToTheScript newDat (Ada.lovelaceValueOf minLovelace) <>
                     Constraints.mustValidateIn (from now) <>
                     Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Raffle pkh)
            Contract.logInfo $ "Original members:" <> show (members dat)
            Contract.logInfo $ "Members shuffled:" <> show sMembers
            Contract.logInfo $ "Winner:" <> show winner
            ledgerTx <- submitTxConstraintsWith @SavingsSession lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ "Session has started over"

        continueSession :: AsContractError e => SavingsDatum -> Map TxOutRef ChainIndexTxOut -> TxOutRef -> Contract w s e ()
        continueSession dat utxos oref = do
            pkh <- Contract.ownPaymentPubKeyHash
            now   <- currentTime
            let sMembers = shuffleMembers (notWinners (members dat) (winners dat))
                winner = head sMembers
                newDat = SavingsDatum {
                  stake    = minLovelace
                , members  = members dat
                , winners  = winner : winners dat
                , joinDeadline = joinDeadline dat
                }
                lookups = Constraints.unspentOutputs utxos <>
                          Constraints.otherScript validator <>
                          Constraints.typedValidatorLookups typedRotatingSavingsValidator
                tx = Constraints.mustPayToPubKey winner (Ada.lovelaceValueOf (stake dat - minLovelace)) <>
                     Constraints.mustPayToTheScript newDat (Ada.lovelaceValueOf minLovelace) <>
                     Constraints.mustValidateIn (from now) <>
                     Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Raffle pkh)
            Contract.logInfo $ "Original members:" <> show (members dat)
            Contract.logInfo $ "Members shuffled:" <> show sMembers
            Contract.logInfo $ "Winner:" <> show winner
            ledgerTx <- submitTxConstraintsWith @SavingsSession lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ "Session has continued"

type RotatingSavingsSchema =
               Endpoint "startSession" StartSessionParams
           .\/ Endpoint "joinSession" JoinSessionParams
           .\/ Endpoint "leaveSession" ()
           .\/ Endpoint "makePayment" Integer
           .\/ Endpoint "raffle" ()

endpoints :: Contract () RotatingSavingsSchema Text ()
endpoints = awaitPromise (startSession' `select` joinSession' `select` leaveSession' `select` makePayment' `select` raffle') >> endpoints
  where
    startSession' = endpoint @"startSession" startSession
    joinSession' = endpoint @"joinSession" joinSession
    leaveSession' = endpoint @"leaveSession" $ const leaveSession
    makePayment' = endpoint @"makePayment" makePayment
    raffle' = endpoint @"raffle" $ const raffle

mkSchemaDefinitions ''RotatingSavingsSchema

mkKnownCurrencies []

-- TEST

-- Anyone can start a session but would have to pay the stake to do so
startSessionTest :: IO ()
startSessionTest = runEmulatorTraceIO startSessionTrace

startSessionTrace :: EmulatorTrace ()
startSessionTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    callEndpoint @"startSession" h1 $ StartSessionParams
        { ssStake = 10000000
        , ssJoinDeadline = slotToBeginPOSIXTime def 100
        }
    s <- Emulator.waitNSlots 10
    Extras.logInfo $ "Reached " ++ show s

-- Anyone can join the session before the deadline
-- Member will join a session that has the most members below 12 people
joinSessionTest :: IO ()
joinSessionTest = runEmulatorTraceIO joinSessionTrace

joinSessionTrace :: EmulatorTrace ()
joinSessionTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    h3 <- activateContractWallet (knownWallet 3) endpoints
    callEndpoint @"startSession" h1 $ StartSessionParams
        { ssStake = 10000000
        , ssJoinDeadline = slotToBeginPOSIXTime def 100
        }
    void $ Emulator.waitNSlots 10
    callEndpoint @"joinSession" h2 $ JoinSessionParams { jsStake = 2000000 }
    void $ Emulator.waitNSlots 10
    callEndpoint @"joinSession" h3 $ JoinSessionParams { jsStake = 5000000 }
    s <- Emulator.waitNSlots 10
    Extras.logInfo $ "Wallet 1: " ++ show (knownWallet 1)
    Extras.logInfo $ "Wallet 2: " ++ show (knownWallet 2)
    Extras.logInfo $ "Wallet 3: " ++ show (knownWallet 3)
    Extras.logInfo $ "Reached " ++ show s

-- A winner will be randomly selected from a list of members who haven't won
raffleTest :: IO ()
raffleTest = runEmulatorTraceIO raffleTrace

raffleTrace :: EmulatorTrace ()
raffleTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    h3 <- activateContractWallet (knownWallet 3) endpoints
    callEndpoint @"startSession" h1 $ StartSessionParams
        { ssStake = 10000000
        , ssJoinDeadline = slotToBeginPOSIXTime def 100
        }
    void $ Emulator.waitNSlots 10
    callEndpoint @"joinSession" h2 $ JoinSessionParams { jsStake = 2000000 }
    void $ Emulator.waitNSlots 10
    callEndpoint @"joinSession" h3 $ JoinSessionParams { jsStake = 5000000 }
    void $ Emulator.waitUntilSlot 110
    callEndpoint @"raffle" h1 ()
    s <- Emulator.waitNSlots 10
    Extras.logInfo $ "Wallet 1: " ++ show (knownWallet 1)
    Extras.logInfo $ "Wallet 2: " ++ show (knownWallet 2)
    Extras.logInfo $ "Wallet 3: " ++ show (knownWallet 3)
    Extras.logInfo $ "Reached " ++ show s

-- Only members participating in a session can make a payment
-- this is used so members can pay back after winning the raffle
makePaymentTest :: IO ()
makePaymentTest = runEmulatorTraceIO makePaymentTrace

makePaymentTrace :: EmulatorTrace ()
makePaymentTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    h3 <- activateContractWallet (knownWallet 3) endpoints
    callEndpoint @"startSession" h1 $ StartSessionParams
        { ssStake = 10000000
        , ssJoinDeadline = slotToBeginPOSIXTime def 100
        }
    void $ Emulator.waitNSlots 10
    callEndpoint @"joinSession" h2 $ JoinSessionParams { jsStake = 2000000 }
    void $ Emulator.waitNSlots 10
    callEndpoint @"joinSession" h3 $ JoinSessionParams { jsStake = 5000000 }
    void $ Emulator.waitNSlots 10
    callEndpoint @"raffle" h1 ()
    void $ Emulator.waitNSlots 10
    callEndpoint @"makePayment" h1 3000000
    void $ Emulator.waitNSlots 10
    callEndpoint @"makePayment" h2 3000000
    s <- Emulator.waitNSlots 10
    Extras.logInfo $ "Wallet 1: " ++ show (knownWallet 1)
    Extras.logInfo $ "Wallet 2: " ++ show (knownWallet 2)
    Extras.logInfo $ "Wallet 3: " ++ show (knownWallet 3)
    Extras.logInfo $ "Reached " ++ show s

-- Upon leaving a session, the member does not get his/her money back
leaveSessionTest :: IO ()
leaveSessionTest = runEmulatorTraceIO leaveSessionTrace

leaveSessionTrace :: EmulatorTrace ()
leaveSessionTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"startSession" h1 $ StartSessionParams
        { ssStake = 10000000
        , ssJoinDeadline = slotToBeginPOSIXTime def 100
        }
    void $ Emulator.waitNSlots 10
    callEndpoint @"joinSession" h2 $ JoinSessionParams { jsStake = 2000000 }
    void $ Emulator.waitNSlots 10
    callEndpoint @"leaveSession" h2 ()
    s <- Emulator.waitNSlots 10
    Extras.logInfo $ "Wallet 1: " ++ show (knownWallet 1)
    Extras.logInfo $ "Wallet 2: " ++ show (knownWallet 2)
    Extras.logInfo $ "Reached " ++ show s
