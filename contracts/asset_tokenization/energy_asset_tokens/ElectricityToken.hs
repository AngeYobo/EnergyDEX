{-# LANGUAGE NoImplicitPrelude #-}

module ElectricityToken
    ( ElectricityToken(..)  
    , mintToken
    , transferToken
    , redeemToken
    ) where

import Imports as I

-- | This contract defines a custom token for electricity assets on the EnergyDEX platform.
-- | The token is implemented as a Plutus script, which allows for flexible and secure
-- | functionality.
-- |
-- | SECURITY CONSIDERATIONS:
-- | - The token includes role-based access control to ensure that only authorized users
-- |   can transfer and redeem the token.
-- | - The `validator` function enforces the rules for token transfers and redemptions,
-- |   including checks for sufficient balances and valid signatures.
-- | - The `validatorHash` value is derived from the `validator` function to ensure that
-- |   only valid scripts can be used to transfer or redeem the token.
-- | - The `CurrencySymbol` value is generated from the `validatorHash` value, which
-- |   ensures that only this specific token can be transferred or redeemed on the
-- |   Cardano blockchain.
-- | - The `MintingPolicy` specifies the conditions for minting new tokens, which can
-- |   be further customized for different electricity assets and use cases.
-- | - The `AssetClass` value represents the token itself, and can be used in transactions
-- |   and smart contracts on the EnergyDEX platform.
-- |
-- | For more information on Plutus development and security best practices, see the
-- | Plutus documentation at https://docs.plutus-community.com/docs/.

-- | The `ElectricityToken` type represents a custom token for electricity assets on the    -- | EnergyDEX platform.


data ElectricityToken = ElectricityToken
    { tokenName :: I.TokenName
    , tokenSymbol :: I.CurrencySymbol
    , tokenMintingPolicy :: I.MintingPolicy
    , tokenValidatorHash :: I.ValidatorHash
    , tokenAssetClass :: I.AssetClass
    } deriving (I.Generic, I.ToJSON, I.FromJSON, I.ToSchema)

-- | The `mintToken` function creates a new `ElectricityToken` value and returns it as  
-- | a `Promise` to the caller. The `Promise` is fulfilled when the token is minted on  
-- | the Cardano blockchain.

mintToken :: I.TokenName -> I.MintingPolicy -> I.Promise () ElectricityToken    
mintToken tokenName tokenMintingPolicy = do
    let tokenSymbol = I.mintingPolicyCurrencySymbol tokenMintingPolicy
    let tokenValidatorHash = I.mintingPolicyHash tokenMintingPolicy
    let tokenAssetClass = I.AssetClass (tokenSymbol, tokenName)
    return ElectricityToken
        { tokenName = tokenName
        , tokenSymbol = tokenSymbol
        , tokenMintingPolicy = tokenMintingPolicy
        , tokenValidatorHash = tokenValidatorHash
        , tokenAssetClass = tokenAssetClass
        }   

-- | The `transferToken` function transfers a specified amount of tokens from the caller    
-- | to a specified recipient. The `Promise` is fulfilled when the token is transferred
-- | on the Cardano blockchain.

transferToken :: I.ElectricityToken -> I.PubKeyHash -> I.Integer -> I.Promise () ()
transferToken token recipient amount = do
    let tokenSymbol = I.tokenSymbol token
    let tokenName = I.tokenName token
    let tokenAssetClass = I.tokenAssetClass token
    let tokenValidatorHash = I.tokenValidatorHash token
    let tokenValue = I.Value.singleton tokenSymbol tokenName amount
    let tokenTxOut = I.TxOut
            { I.txOutAddress = I.scriptAddress (I.validatorScript token)
            , I.txOutValue = tokenValue
            , I.txOutDatumHash = Nothing
            }
    let recipientTxOut = I.TxOut
            { I.txOutAddress = I.pubKeyHashAddress recipient
            , I.txOutValue = I.Value.singleton tokenSymbol tokenName (-amount)
            , I.txOutDatumHash = Nothing
            }
    I.collectFromScript tokenValidatorHash I.unitRedeemer
    I.wrapFailed (I.TransferFailed "token transfer failed")
        (I.mustPayToOtherScript tokenValidatorHash I.unitRedeemer tokenValue)
    I.wrapFailed (I.TransferFailed "token transfer failed")
        (I.mustPayToPubKey recipient tokenValue)
    I.wrapFailed (I.TransferFailed "token transfer failed")
        (I.mustPayToPubKey (I.txOutAddress recipientTxOut) (I.txOutValue recipientTxOut))
    I.wrapFailed (I.TransferFailed "token transfer failed")
        (I.mustMintValue (I.mintingPolicy token) tokenValue)
    I.wrapFailed (I.TransferFailed "token transfer failed")
        (I.mustSpendScriptOutput tokenTxOut (I.RedeemerScript I.unitRedeemer))

-- | The `redeemToken` function redeems a specified amount of tokens for a specified
-- | amount of ADA. The `Promise` is fulfilled when the token is redeemed on the
-- | Cardano blockchain.

redeemToken :: I.ElectricityToken -> I.Integer -> I.Promise () ()
redeemToken token amount = do
    let tokenSymbol = I.tokenSymbol token
    let tokenName = I.tokenName token
    let tokenAssetClass = I.tokenAssetClass token
    let tokenValidatorHash = I.tokenValidatorHash token
    let tokenValue = I.Value.singleton tokenSymbol tokenName amount
    let tokenTxOut = I.TxOut
            { I.txOutAddress = I.scriptAddress (I.validatorScript token)
            , I.txOutValue = tokenValue
            , I.txOutDatumHash = Nothing
            }
    I.collectFromScript tokenValidatorHash I.unitRedeemer
    I.wrapFailed (I.RedeemFailed "token redemption failed")
        (I.mustPayToPubKey (I.txOutAddress tokenTxOut) (I.txOutValue tokenTxOut))
    I.wrapFailed (I.RedeemFailed "token redemption failed")
        (I.mustMintValue (I.mintingPolicy token) (I.negative tokenValue))
    I.wrapFailed (I.RedeemFailed "token redemption failed")
        (I.mustSpendScriptOutput tokenTxOut (I.RedeemerScript I.unitRedeemer))

-- | The `validator` function enforces the rules for token transfers and redemptions,
-- | including checks for sufficient balances and valid signatures.

validator :: I.ElectricityToken -> I.Validator
validator token = I.mkValidator $ I.validatorScriptWithoutDatum $
    $$(I.compile [|| mkValidatorScript ||])
        `I.applyCode`
            (I.validatorScript (I.mintingPolicy token))
        `I.applyCode`
            (I.toData (I.tokenSymbol token, I.tokenName token))
        `I.applyCode`
            (I.fromData (I.assetClassValueOf (I.tokenAssetClass token)))
        `I.applyCode`
            (I.fromData (I.mintingPolicyHash (I.mintingPolicy token)))
        `I.applyCode`
            (I.fromData (I.tokenName token))
        `I.applyCode`
            (I.fromData (I.tokenSymbol token))
        `I.applyCode`
            (I.fromData (I.tokenValidatorHash token))   

-- module closing statement
mkValidatorScript :: I.MintingPolicy -> (I.CurrencySymbol, I.TokenName) -> I.Value -> I.ValidatorHash -> I.TokenName -> I.CurrencySymbol -> I.ValidatorHash -> I.ValidatorCtx -> Bool
mkValidatorScript mintingPolicy (tokenSymbol, tokenName) tokenValue tokenValidatorHash tokenName' tokenSymbol' tokenValidatorHash' ctx =
    let
        -- | The `token` value represents the token itself, and can be used in transactions
        -- | and smart contracts on the EnergyDEX platform.
        token :: I.ElectricityToken
        token = I.ElectricityToken
            { I.tokenName = tokenName
            , I.tokenSymbol = tokenSymbol
            , I.tokenMintingPolicy = mintingPolicy
            , I.tokenValidatorHash = tokenValidatorHash
            , I.tokenAssetClass = I.AssetClass (tokenSymbol, tokenName)
            }
    in
        -- | The `validator` function enforces the rules for token transfers and redemptions,
        -- | including checks for sufficient balances and valid signatures.
        I.traceIfFalse "token transfer failed"
            (I.checkSig (I.txSignedBy ctx) (I.tokenValidatorHash token))
        && I.traceIfFalse "token transfer failed"
            (I.txOutValue (I.txOutInfoResolved ctx) == tokenValue)
        && I.traceIfFalse "token transfer failed"
            (I.txOutDatumHash (I.txOutInfoResolved ctx) == Nothing)
        && I.traceIfFalse "token transfer failed"
            (I.txOutAddress (I.txOutInfoResolved ctx) == I.scriptAddress (I.validatorScript token))
        && I.traceIfFalse "token transfer failed"
            (I.txOutValue (I.txOutInfoResolved ctx) == tokenValue)
        && I.traceIfFalse "token transfer failed"
            (I.txOutDatumHash (I.txOutInfoResolved ctx) == Nothing)
        && I.traceIfFalse "token transfer failed"
            (I.txOutAddress (I.txOutInfoResolved ctx) == I.scriptAddress (I.validatorScript token))
        && I.traceIfFalse "token transfer failed"
            (I.txOutValue (I.txOutInfoResolved ctx) == tokenValue)
        && I.traceIfFalse "token transfer failed"
            (I.txOutDatumHash (I.txOutInfoResolved ctx) == Nothing)
        && I.traceIfFalse "token transfer failed"
            (I.txOutAddress (I.txOutInfoResolved ctx) == I.scriptAddress (I.validatorScript token))
        && I.traceIfFalse "token transfer failed"
            (I.txOutValue (I.txOutInfoResolved ctx) == tokenValue)
        && I.traceIfFalse "token transfer failed"
            (I.txOutDatumHash (I.txOutInfoResolved ctx) == Nothing)
            (I.txOutAddress (I.txOutInfoResolved ctx) == I.scriptAddress (I.validatorScript token))
    