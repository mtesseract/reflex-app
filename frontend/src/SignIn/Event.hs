module SignIn.Event where

data AuthEvent = SignedIn | SignedOut
    deriving (Eq, Show, Ord)