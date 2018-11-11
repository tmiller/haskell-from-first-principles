module People where

type Name = String
type Age = Integer
type ValidatePerson a = Either [PersonInvalid] a

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)

nameOkay :: String -> ValidatePerson Name
nameOkay name =
  case name /= "" of
    True -> Right name
    False -> Left [NameEmpty]

ageOkay :: Integer -> ValidatePerson Age
ageOkay age =
  case age >= 0 of
    True -> Right age
    False -> Left [AgeTooLow]

mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = liftA2 (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left nameErr) (Left ageErr) = Left (nameErr ++ ageErr)
mkPerson' (Left nameErr) _             = Left nameErr
mkPerson' _              (Left ageErr) = Left ageErr
