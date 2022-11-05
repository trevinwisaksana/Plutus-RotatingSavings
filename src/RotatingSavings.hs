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
{-# LANGUAGE NumericUnderscores #-}

module RotatingSavings where

import           Cardano.Api                          (PlutusScriptV2,
                                                       writeFileTextEnvelope)
import           Cardano.Api.Shelley                  (PlutusScript (PlutusScriptSerialised),
                                                       PlutusScriptV1)
import           Codec.Serialise
import qualified Data.ByteString.Lazy                 as LBS
import qualified Data.ByteString.Short                as SBS
import           Data.Functor                         (void)
import           Data.Map                             as Map
import           Data.Text                            (Text)
import           Data.Aeson                           (ToJSON, FromJSON)
import           Data.Void                            (Void)
import           Data.Default
import           GHC.Generics                         (Generic)
import           Ledger
import           Ledger.TimeSlot
import           Ledger.Ada                           as Ada
import           Ledger.Constraints                   as Constraints
import qualified Ledger.Typed.Scripts                 as Scripts
import           Plutus.Contract                      as Contract
import qualified Plutus.Script.Utils.V1.Scripts       as PSU.V1
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import           Plutus.Trace.Emulator                as Emulator
import qualified Plutus.V1.Ledger.Api                 as PlutusV1
import qualified Plutus.V2.Ledger.Api                 as PlutusV2
import qualified PlutusTx
import           PlutusTx.Prelude                     as P hiding (Semigroup (..), unless, (.))
import           Prelude                              (IO, Semigroup (..), Show (..), String, (.))
import           Wallet.Emulator.Wallet

minLovelace :: Integer
minLovelace = 2000000

data SavingsDatum = SavingsDatum
    { members        :: [PaymentPubKeyHash]
    , participants   :: [PaymentPubKeyHash]
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
        stakeIsGreaterThanZero = stake dat > 0

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
        wrap = PSU.V1.mkUntypedValidator

validator :: PlutusV2.Validator
validator = Scripts.validatorScript typedRotatingSavingsValidator

rotatingSavingsScript :: PSU.V1.ValidatorHash
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
    Right (PlutusV2.Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> False
        Just d  -> pkh `elem` members d

getDatum' :: ChainIndexTxOut -> SavingsDatum
getDatum' o = case _ciTxOutDatum o of
    Left  _ -> traceError "Datum does not exist"
    Right (PlutusV2.Datum e) -> case PlutusTx.fromBuiltinData e of
        Nothing -> traceError "Unknown datum type"
        Just d  -> d

removePkh :: PaymentPubKeyHash -> [PaymentPubKeyHash] -> [PaymentPubKeyHash]
removePkh element list = P.filter (\e -> e/=element) list

data StartSessionParams = StartSessionParams
    { ssStake            :: !Integer
    , ssJoinDeadline     :: !POSIXTime
    } deriving (Show, Generic, FromJSON, ToJSON)

startSession :: AsContractError e => StartSessionParams -> Contract w s e ()
startSession p = do
    pkh <- Contract.ownPaymentPubKeyHash
    let dat = SavingsDatum
                { stake         = ssStake p
                , members       = [pkh]
                , participants  = [pkh]
                , joinDeadline  = ssJoinDeadline p
                }
        tx   = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ ssStake p
    Contract.logInfo $ "Members:" <> show (members dat)
    ledgerTx <- submitTxConstraints typedRotatingSavingsValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String $ "Session has started"

data JoinSessionParams = JoinSessionParams
    { jsStake            :: !Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

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
                        , participants  = pkh : members dat
                        , joinDeadline = joinDeadline dat
                        }
                    lookups = Constraints.unspentOutputs utxos <>
                              Constraints.otherScript validator <>
                              Constraints.typedValidatorLookups typedRotatingSavingsValidator
                    tx   = Constraints.mustPayToTheScript newDat (Ada.lovelaceValueOf $ stake newDat) <>
                           Constraints.mustValidateIn (to $ now + 1000) <>
                           Constraints.mustSpendScriptOutput oref (PlutusV1.Redeemer $ PlutusTx.toBuiltinData JoinSession)
                Contract.logInfo $ "Join Deadline:" <> show (joinDeadline dat)
                Contract.logInfo $ "New members:" <> show (members newDat)
                ledgerTx <- submitTxConstraintsWith @SavingsSession lookups tx
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                Contract.logInfo @String $ "Joined session")
    where
        isSuitable :: POSIXTime -> ChainIndexTxOut -> Bool
        isSuitable now o = case _ciTxOutDatum o of
            Left _          -> False
            Right (PlutusV2.Datum e) -> case PlutusTx.fromBuiltinData e of
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
                    , participants  = removePkh pkh $ participants dat
                    , joinDeadline = joinDeadline dat
                    }
                lookups = Constraints.unspentOutputs utxos <>
                          Constraints.otherScript validator <>
                          Constraints.typedValidatorLookups typedRotatingSavingsValidator
                tx   = Constraints.mustPayToTheScript newDat (Ada.lovelaceValueOf $ stake dat) <>
                       Constraints.mustSpendScriptOutput oref (PlutusV1.Redeemer $ PlutusTx.toBuiltinData $ Leave pkh)
            Contract.logInfo $ "Members:" <> show (members newDat)
            ledgerTx <- submitTxConstraintsWith @SavingsSession lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ "Left session"

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
                    , participants  = participants dat
                    , joinDeadline = joinDeadline dat
                    }
                lookups = Constraints.unspentOutputs utxos <>
                          Constraints.otherScript validator <>
                          Constraints.typedValidatorLookups typedRotatingSavingsValidator
                tx   = Constraints.mustPayToTheScript newDat (Ada.lovelaceValueOf $ stake newDat) <>
                       Constraints.mustSpendScriptOutput oref (PlutusV1.Redeemer $ PlutusTx.toBuiltinData $ MakePayment pkh)
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
                everyoneHasWonOnce = length (participants dat) == 0
            if everyoneHasWonOnce then resetSession dat utxos oref else continueSession dat utxos oref
    where
        shuffleMembers :: [PaymentPubKeyHash] -> [PaymentPubKeyHash]
        shuffleMembers [] = []
        shuffleMembers xs = let (ys,zs) = splitAlt xs
                            in ys ++ shuffleMembers zs

        splitAlt :: [a] -> ([a],[a])
        splitAlt = P.foldr (\x (ys, zs) -> (x:zs, ys)) ([],[])

        resetSession :: AsContractError e => SavingsDatum -> Map TxOutRef ChainIndexTxOut -> TxOutRef -> Contract w s e ()
        resetSession dat utxos oref = do
            pkh <- Contract.ownPaymentPubKeyHash
            now   <- currentTime
            let sParticipants = shuffleMembers (members dat)
                winner = head sParticipants
                newDat = SavingsDatum {
                  stake    = minLovelace
                , members  = members dat
                , participants  = members dat
                , joinDeadline = joinDeadline dat
                }
                lookups = Constraints.unspentOutputs utxos <>
                          Constraints.otherScript validator <>
                          Constraints.typedValidatorLookups typedRotatingSavingsValidator
                tx = Constraints.mustPayToPubKey winner (Ada.lovelaceValueOf (stake dat - minLovelace)) <>
                     Constraints.mustPayToTheScript newDat (Ada.lovelaceValueOf minLovelace) <>
                     Constraints.mustValidateIn (from now) <>
                     Constraints.mustSpendScriptOutput oref (PlutusV1.Redeemer $ PlutusTx.toBuiltinData $ Raffle pkh)
            Contract.logInfo $ "Original members:" <> show (members dat)
            Contract.logInfo $ "Members shuffled:" <> show sParticipants
            Contract.logInfo $ "Participants:" <> show (participants newDat)
            Contract.logInfo $ "Winner:" <> show winner
            ledgerTx <- submitTxConstraintsWith @SavingsSession lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ "Session has started over"

        continueSession :: AsContractError e => SavingsDatum -> Map TxOutRef ChainIndexTxOut -> TxOutRef -> Contract w s e ()
        continueSession dat utxos oref = do
            pkh <- Contract.ownPaymentPubKeyHash
            now   <- currentTime
            Contract.logInfo $ "Participants Before Raffle:" <> show (participants dat)
            let sParticipants = shuffleMembers (participants dat)
                winner = head sParticipants
                updatedParticipants = removePkh winner $ participants dat
                newDat = SavingsDatum {
                  stake    = minLovelace
                , members  = members dat
                , participants  = updatedParticipants
                , joinDeadline = joinDeadline dat
                }
                lookups = Constraints.unspentOutputs utxos <>
                          Constraints.otherScript validator <>
                          Constraints.typedValidatorLookups typedRotatingSavingsValidator
                tx = Constraints.mustPayToPubKey winner (Ada.lovelaceValueOf (stake dat - minLovelace)) <>
                     Constraints.mustPayToTheScript newDat (Ada.lovelaceValueOf minLovelace) <>
                     Constraints.mustValidateIn (from now) <>
                     Constraints.mustSpendScriptOutput oref (PlutusV1.Redeemer $ PlutusTx.toBuiltinData $ Raffle pkh)
            Contract.logInfo $ "Original members:" <> show (members newDat)
            Contract.logInfo $ "Members shuffled:" <> show sParticipants
            Contract.logInfo $ "Participants:" <> show (participants newDat)
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
    void $ Emulator.waitNSlots 10

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
    void $ Emulator.waitNSlots 10

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
    void $ Emulator.waitNSlots 10

-- A winner will be randomly selected from a list of members who haven't won
raffleTwiceTest :: IO ()
raffleTwiceTest = runEmulatorTraceIO raffleTwiceTrace

raffleTwiceTrace :: EmulatorTrace ()
raffleTwiceTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"startSession" h1 $ StartSessionParams
        { ssStake = 10000000
        , ssJoinDeadline = slotToBeginPOSIXTime def 100
        }
    void $ Emulator.waitNSlots 10
    callEndpoint @"joinSession" h2 $ JoinSessionParams { jsStake = 2000000 }
    void $ Emulator.waitUntilSlot 110
    callEndpoint @"raffle" h1 ()
    void $ Emulator.waitNSlots 10
    callEndpoint @"makePayment" h1 3000000
    void $ Emulator.waitNSlots 10
    callEndpoint @"makePayment" h2 3000000
    void $ Emulator.waitNSlots 10
    callEndpoint @"raffle" h1 ()
    void $ Emulator.waitNSlots 10

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
        , ssJoinDeadline = slotToBeginPOSIXTime def 30
        }
    void $ Emulator.waitNSlots 5
    callEndpoint @"joinSession" h2 $ JoinSessionParams { jsStake = 2000000 }
    void $ Emulator.waitNSlots 10
    callEndpoint @"joinSession" h3 $ JoinSessionParams { jsStake = 5000000 }
    void $ Emulator.waitUntilSlot 35
    callEndpoint @"raffle" h1 ()
    void $ Emulator.waitNSlots 5
    callEndpoint @"makePayment" h1 3000000
    void $ Emulator.waitNSlots 10
    callEndpoint @"makePayment" h2 3000000
    void $ Emulator.waitNSlots 5

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
    void $ Emulator.waitNSlots 10

-- Once everyone wins, the participants list should reset to members list
everyoneWinsTest :: IO ()
everyoneWinsTest = runEmulatorTraceIO everyoneWinsTrace

everyoneWinsTrace :: EmulatorTrace ()
everyoneWinsTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    callEndpoint @"startSession" h1 $ StartSessionParams
        { ssStake = 10000000
        , ssJoinDeadline = slotToBeginPOSIXTime def 100
        }
    void $ Emulator.waitNSlots 10
    callEndpoint @"joinSession" h2 $ JoinSessionParams { jsStake = 2000000 }
    void $ Emulator.waitUntilSlot 110
    callEndpoint @"raffle" h1 ()
    void $ Emulator.waitNSlots 10
    callEndpoint @"makePayment" h1 3000000
    void $ Emulator.waitNSlots 10
    callEndpoint @"makePayment" h2 3000000
    void $ Emulator.waitNSlots 10
    callEndpoint @"raffle" h1 ()
    void $ Emulator.waitNSlots 10
    callEndpoint @"makePayment" h1 3000000
    void $ Emulator.waitNSlots 10
    callEndpoint @"makePayment" h2 3000000
    void $ Emulator.waitNSlots 10
    callEndpoint @"raffle" h1 ()
    void $ Emulator.waitNSlots 10

-- User not part of a session cannot raffle
failRaffleWithoutJoiningTest :: IO ()
failRaffleWithoutJoiningTest = runEmulatorTraceIO raffleWithoutJoiningTrace

raffleWithoutJoiningTrace :: EmulatorTrace ()
raffleWithoutJoiningTrace = do
    h1 <- activateContractWallet (knownWallet 1) endpoints
    h2 <- activateContractWallet (knownWallet 2) endpoints
    h3 <- activateContractWallet (knownWallet 3) endpoints
    h4 <- activateContractWallet (knownWallet 4) endpoints
    callEndpoint @"startSession" h1 $ StartSessionParams
        { ssStake = 10000000
        , ssJoinDeadline = slotToBeginPOSIXTime def 50
        }
    void $ Emulator.waitNSlots 10
    callEndpoint @"joinSession" h2 $ JoinSessionParams { jsStake = 2000000 }
    void $ Emulator.waitNSlots 10
    callEndpoint @"joinSession" h3 $ JoinSessionParams { jsStake = 5000000 }
    void $ Emulator.waitUntilSlot 60
    callEndpoint @"raffle" h4 ()
    void $ Emulator.waitNSlots 10
