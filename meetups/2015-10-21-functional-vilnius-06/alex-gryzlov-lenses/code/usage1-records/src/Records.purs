module Records where

  import Prelude
  import Optic.Core

  newtype Person = Person
    { firstName :: String
    , lastName  :: String
    , address   :: Address
    }

  newtype Address = Address
    { street :: StreetRec
    , city   :: String
    , country  :: Country
    }

  data Country = Lithuania
               | Belarus
               | USA

  instance showCountry :: Show Country where
    show Lithuania = "LT"
    show Belarus = "BY"
    show USA = "US"

  newtype StreetRec = StreetRec
    { number      :: Int
    , streetName  :: String
    , designation :: Designation
    }

  data Designation = Street
                   | Prospect
                   | Square

  instance showDesignation :: Show Designation where
    show Street    = "Street"
    show Prospect  = "Prospect"
    show Square    = "Square"

  instance showPerson :: Show Person where
    show (Person p) = "Person "
                   ++ "{firstName: " ++ show p.firstName
                   ++ ", lastName: " ++ show p.lastName
                   ++ ", address: " ++ show p.address
                   ++ "}"

  instance showAddress :: Show Address where
    show (Address a) = "Address "
                    ++ "{street: " ++ show a.street
                    ++ ", city: " ++ show a.city
                    ++ ", country: " ++ show a.country
                    ++ "}"

  instance showStreetRec :: Show StreetRec where
    show (StreetRec s) = "StreetRec "
                    ++ "{number: " ++ show s.number
                    ++ ", streetName: " ++ show s.streetName
                    ++ ", designation: " ++ show s.designation
                    ++ "}"

  petrIvanov :: Person
  petrIvanov = Person
    { firstName: "Petr"
    , lastName: "Ivanov"
    , address: Address
        { street: StreetRec
            { number: 12
            , streetName: "Lenina"
            , designation: Street
            }
        , city: "Minsk"
        , country: Belarus
        }
    }

  -- Without lenses

  runPerson (Person p) = p
  runAddress (Address a) = a
  runStreetRec (StreetRec s) = s

  changeFirstName :: String -> Person -> Person
  changeFirstName newName (Person p) = Person p{firstName = newName}

  changeCountry :: Country -> Person -> Person
  changeCountry newCountry (Person p) =
    Person p{address = Address (runAddress p.address){country = newCountry}}

  changeStreetNumber :: Int -> Person -> Person
  changeStreetNumber newNumber (Person p) =
    Person p{address = Address addr{street = StreetRec strt{number = newNumber}}}
    where
      addr = runAddress p.address
      strt = runStreetRec addr.street

  -- With lenses

  changeFirstNameLens :: String -> Person -> Person
  changeFirstNameLens = set (_Person..firstName)

  changeCountryLens :: Country -> Person -> Person
  changeCountryLens = set (_Person..address.._Address..country)

  changeStreetNumberLens :: Int -> Person -> Person
  changeStreetNumberLens = set (_Person..address.._Address..street.._StreetRec..number)

  -- mariaIvanov = changeFirstName "Maria" petrIvanov
  -- mariaIvanovLens = changeFirstNameLens "Maria" petrIvanov

  -- moveToLithuania = changeCountry Lithuania petrIvanov
  -- moveToLithuaniaLens = changeCountryLens Lithuania petrIvanov

  -- downTheStreet = changeStreetNumber 43 petrIvanov
  -- downTheStreetLens = changeStreetNumberLens 43 petrIvanov

  -- Lenses

  _Person :: LensP Person {firstName :: String, lastName :: String, address :: Address}
  _Person f (Person b) = Person <$> f b

  _Address :: LensP Address {street :: StreetRec, city :: String, country :: Country}
  _Address f (Address b) = Address <$> f b

  _StreetRec :: LensP StreetRec {number :: Int, streetName :: String, designation :: Designation}
  _StreetRec f (StreetRec b) = StreetRec <$> f b

  firstName :: forall b a r. Lens {firstName :: a | r} {firstName :: b | r} a b
  firstName f o = f o.firstName <#> \firstName' -> o{firstName = firstName'}

  lastName :: forall b a r. Lens {lastName :: a | r} {lastName :: b | r} a b
  lastName f o = f o.lastName <#> \lastName' -> o{lastName = lastName'}

  address :: forall b a r. Lens {address :: a | r} {address :: b | r} a b
  address f o = f o.address <#> \address' -> o{address = address'}

  street :: forall b a r. Lens {street :: a | r} {street :: b | r} a b
  street f o = f o.street <#> \street' -> o{street = street'}

  city :: forall b a r. Lens {city :: a | r} {city :: b | r} a b
  city f o = f o.city <#> \city' -> o{city = city'}

  country :: forall b a r. Lens {country :: a | r} {country :: b | r} a b
  country f o = f o.country <#> \country' -> o{country = country'}

  number :: forall b a r. Lens {number :: a | r} {number :: b | r} a b
  number f o = f o.number <#> \number' -> o{number = number'}

  streetName :: forall b a r. Lens {streetName :: a | r} {streetName :: b | r} a b
  streetName f o = f o.streetName <#> \streetName' -> o{streetName = streetName'}

  designation :: forall b a r. Lens {designation :: a | r} {designation :: b | r} a b
  designation f o = f o.designation <#> \designation' -> o{designation = designation'}
