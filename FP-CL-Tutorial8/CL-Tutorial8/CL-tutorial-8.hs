type TwoBool = Bool -> Bool -> Bool

twoTrue :: TwoBool
twoTrue _ _ = undefined

twoFalse :: TwoBool
twoFalse _ _ = undefined

twoNot :: TwoBool -> TwoBool
twoNot x a b = undefined

(&&&) :: TwoBool -> TwoBool -> TwoBool
(x &&& y) a b = undefined

(|||) :: TwoBool -> TwoBool -> TwoBool
(x ||| y) a b = undefined
