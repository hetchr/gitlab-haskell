module GitLab.New.Utils where

-- TODO bring everything from the other files
-- functions and types

{-
-- we had a bug with empty lists and other stuff, if IIRC.
-- also ther emight be some security considerations?
marshalledQuerySpec :: Spec
marshalledQuerySpec = do
  describe "marshalledQuery" $ do
    it "quotes strings so spaces won't break stuff" $ do
      let name = "title"
          value = "there are spaces"
          eg mk = [(name, mk (Str value))]
          expectedResult = "&" <> name <> "=\"" <> value <> "\""
      marshalledQuery (eg Required) `shouldBe` expectedResult
      marshalledQuery (eg Optional) `shouldBe` expectedResult


data PossibleValue
  = Required Value
  | Optional Value

data Value
  = Pure Text
  | Str Text
  | Bool Bool
  | Num Int
  | Nullable Value

-- use this two functions
-- mkQueryKey
-- mkQueryValue
-- unRText :: RText l -> Text
--
-- QueryFlag (RText 'QueryKey)
-- QueryParam (RText 'QueryKey) (RText 'QueryValue)	


-- will be replace with something involving
-- https://hackage.haskell.org/package/modern-uri-0.3.4.0/docs/Text-URI.html#t:QueryParam
marshalledQuery :: [(Text, PossibleValue)] -> Text -- this will be Either Text Text
marshalledQuery xs =
  -- Looked at some libs for functions like this, didn't find
  -- anything at first glance but it might exists somewhere;
  -- don't if it's worthy to go after it.
  let tryMarshalling :: [(Text, Either Text Text)] = marshall <$> xs
      nameTheErrors :: [Either (Text, Text) (Text, Text)] = nameResult <$> tryMarshalling
      giveMeTheErrorsIfWeHaveAny :: Either [(Text, Text)] [(Text, Text)] = sequenceOfSorts nameTheErrors
      processTheErrorsAndResults :: Either Text [(Text, Text)] = processErrs giveMeTheErrorsIfWeHaveAny
   in case processTheErrorsAndResults of
        Left errors -> error (show errors)
        Right result -> buildQuery result -- this function is pure for now
  where
    sequenceOfSorts :: [Either (Text, Text) (Text, Text)] -> Either [(Text, Text)] [(Text, Text)]
    sequenceOfSorts = foldr shortCircuit (Right [])
      where
        shortCircuit a (Left xs) =
          -- once a left, always a left
          case a of
            Left (name, val) -> Left ((name, val) : xs)
            Right (_name, _val) -> Left xs
        shortCircuit a (Right xs) =
          case a of
            Left (name, val) -> Left [(name, val)] -- first one
            Right (name, val) -> Right ((name, val) : xs)
    processErrs :: Either [(Text, Text)] [(Text, Text)] -> Either Text [(Text, Text)]
    processErrs = \case
      Left l -> Left (T.pack $ show l)
      Right t -> Right t
    nameResult :: (Text, Either Text Text) -> Either (Text, Text) (Text, Text)
    nameResult = \case
      (name, Left l) -> Left (name, l)
      (name, Right r) -> Right (name, r)
    marshall :: (Text, PossibleValue) -> (Text, Either Text Text)
    marshall = fmap marshallPossible
    marshallPossible :: PossibleValue -> Either Text Text
    marshallPossible = \case
      Required (Pure t) -> Right t -- pure always works, if its wrong thats on the user
      Required v ->
        let x = marshallValue v
         in if x == ""
              then-- TODO we should be able to catch other
              -- problems as soon as I list them
                Left "Empty value in required parameter"
              else Right x
      Optional (Pure t) -> Right t -- pure always works, if its wrong thats on the user
      Optional v ->
        let x = marshallValue v
         in if x == ""
              then-- this should not be a failure
                Left "" -- FIXME we need a sum type for this, before it gets confusing
              else Right x
    --  curl --verbose -X POST https://gitlab.com/api/v4/projects/:id/issues--header "PRIVATE-TOKEN: <your_access_token>"
    --  curl --request POST --header "PRIVATE-TOKEN: <your_access_token>" "https://gitlab.example.com/api/v4/projects/4/issues?title=Issues%20with%20auth&labels=bug"

    marshallValue :: Value -> Text
    marshallValue = \case
      Pure t -> t
      Str s -> "\"" <> s <> "\"" -- we need to quote it before using in the request, things like whitespace might break things -- TODO this is wrong we esaping it like that won't do a thing we need to add %20 style in the encoding
      Bool True -> "true"
      Bool False -> "false"
      Num n -> T.pack $ show n
    buildQuery :: [(Text, Text)] -> Text
    buildQuery = foldr buildString ""
      where
        buildString :: (Text, Text) -> Text -> Text
        buildString (param, value) acc =
          T.pack $
            printf "&%s=%s%s" (T.unpack param) (T.unpack value) (T.unpack acc)
-}
