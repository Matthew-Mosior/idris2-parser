module XML.Types

import Data.List
import Data.List1 
import Data.String

%default total

--------------------------------------------------------------------------------
--          XMLDecl
--------------------------------------------------------------------------------

public export
record XMLDecl where
  constructor MkXMLDecl
  version    : String
  encoding   : Maybe String
  standalone : Maybe Bool

export
Show XMLDecl where
  show xmldecl =
    """
    <?xml version=\{show xmldecl.version}\
    \{maybe "" (\e => " encoding=" ++ show e) xmldecl.encoding}\
    \{maybe "" (\s => " standalone=" ++ if s then "\"yes\"" else "\"no\"") xmldecl.standalone}\
    ?>
    """

--------------------------------------------------------------------------------
--          ExternalID
--------------------------------------------------------------------------------

public export
data ExternalID = System String
                | Public String String

export
Show ExternalID where
  show (System sysID)       = "SYSTEM \{show sysID}"
  show (Public pubID sysID) = "PUBLIC \{show pubID} \{show sysID}"

--------------------------------------------------------------------------------
--          DocType
--------------------------------------------------------------------------------

public export
record DocType where
    constructor MkDocType
    name       : String
    externalID : Maybe ExternalID

export
Show DocType where
    show docType =
      """
      <!DOCTYPE \{docType.name}\
      \{maybe "" (\eID => " " ++ show eID) docType.externalID}\
      >
      """

--------------------------------------------------------------------------------
--          Name
--------------------------------------------------------------------------------

public export
data Name = MkName String

export
Show Name where
  show (MkName n) = n

export
Eq Name where
  MkName n1 == MkName n2 = n1 == n2

public export
isNameStartChar :  Char
                -> Bool
isNameStartChar c = isAlpha c || c == '_'

public export
isNameChar :  Char
           -> Bool
isNameChar c = isAlphaNum c || c == '.' || c == '-' || c == '_'

--------------------------------------------------------------------------------
--          QName
--------------------------------------------------------------------------------

public export
record QName where
  constructor MkQName
  namespaceprefix : Maybe Name
  localpart       : Name

export
Show QName where
  show (MkQName Nothing localpart)                = show localpart
  show (MkQName (Just namespaceprefix) localpart) = show namespaceprefix ++ ":" ++ show localpart

export
Eq QName where
  n1 == n2 = n1.namespaceprefix == n2.namespaceprefix && n1.localpart == n2.localpart

--------------------------------------------------------------------------------
--          Attribute
--------------------------------------------------------------------------------

public export
record Attribute where
  constructor MkAttribute
  name  : QName
  value : String

export
Show Attribute where
  show attr = "\{show attr.name}=\{show attr.value}"

public export
lookup :  (name : QName)
       -> List Attribute
       -> Maybe String
lookup name attrs = lookup name $ map (\attr => (attr.name, attr.value)) attrs

--------------------------------------------------------------------------------
--          CharData
--------------------------------------------------------------------------------

public export
record CharData where
    constructor MkCharData
    prespace  : Bool
    c         : String
    postspace : Bool

public export
maybeSpace :  Bool
           -> String
maybeSpace False = ""
maybeSpace True  = " "

public export
fromString :  String
           -> CharData
fromString str =
  case unpack str of
    [] => MkCharData False "" False
    cs@(_ :: _) =>
      let s = trim str
        in MkCharData (isSpace $ head cs) s ((not $ null s) && (isSpace $ last cs))

public export
(++) :  CharData
     -> CharData
     -> CharData
(MkCharData prespace "" midleftspace) ++ (MkCharData midrightspace d postspace) =
  MkCharData (prespace || midleftspace || midrightspace) d postspace
(MkCharData prespace c midleftspace) ++ (MkCharData midrightspace "" postspace) =
  MkCharData prespace c (midleftspace || midrightspace || postspace)
(MkCharData prespace c midleftspace) ++ (MkCharData midrightspace d postspace) =
  MkCharData prespace (c ++ maybeSpace (midleftspace || midrightspace) ++ d) postspace

export
Show CharData where
  show (MkCharData prespace c postspace) = maybeSpace prespace ++ c ++ maybeSpace postspace

public export
Semigroup CharData where
    (<+>) = (++)

public export
Monoid CharData where
    neutral = ""

--------------------------------------------------------------------------------
--          Misc
--------------------------------------------------------------------------------

public export
data Misc = Comment String
          | ProcessingInstruction String String

export
Show Misc where
  show (Comment comment) = "<!--\{comment}-->"
  show (ProcessingInstruction target instruction) = "<?\{target} \{instruction}?>"

--------------------------------------------------------------------------------
--          Element
--------------------------------------------------------------------------------

public export
data Element : Type where
  EmptyElem : QName -> List Attribute -> Element
  Elem      : QName -> List Attribute -> List (Either CharData (Either Misc Element)) -> Element

public export
(.name) :  Element
        -> QName
(EmptyElem name _).name = name
(Elem name _ _).name    = name

public export
(.attrs) :  Element
         -> List Attribute
(EmptyElem _ attrs).attrs = attrs
(Elem _ attrs _).attrs    = attrs

public export
(.content) :  Element
           -> List (Either CharData (Either Misc Element))
(EmptyElem _ _).content    = Nil 
(Elem _ _ content).content = content

public export
maybeNl :  Bool
        -> String
maybeNl False = ""
maybeNl True = "\n"

public export
showNl :  CharData
       -> String
showNl (MkCharData prespace c postspace) = maybeNl prespace ++ c ++ maybeNl postspace

public export
textContent :  Element
            -> String
textContent (EmptyElem _     _)              = ""
textContent (Elem      _     _    Nil)       = ""
textContent (Elem      qname attr (x :: xs)) =
  case x of
    (Left chardata)         =>
      (showNl chardata) ++ (textContent (Elem qname attr xs))
    (Right (Right element)) =>
      (textContent element) ++ (textContent (Elem qname attr xs))
    (Right (Left  _))       =>
      textContent (Elem qname attr xs)

public export
textContent' :  List (Either CharData (Either Misc Element))
             -> String
textContent' Nil     = ""
textContent' (x::xs) =
  case x of
    (Left chardata)         =>
      (showNl chardata) ++ (textContent' xs)
    (Right (Right element)) =>
      (textContent element) ++ (textContent' xs)
    (Right (Left misc))     =>
      (show misc) ++ (textContent' xs)

public export
find :  (Element -> Bool)
     -> Element
     -> Maybe Element
find f elem = find f (extractContent elem.content)
  where
    extractContent :  List (Either CharData (Either Misc Element))
                   -> List Element
    extractContent Nil                 = Nil
    extractContent (content::contents) =
      case content of
        (Right (Right content')) =>
          content' :: extractContent contents
        _                        =>
          extractContent contents

namespace Element

  public export
  mapContent :  (List (Either CharData (Either Misc Element)) -> List (Either CharData (Either Misc Element)))
             -> Element
             -> Element
  mapContent f (EmptyElem name attrs)    = EmptyElem name attrs
  mapContent f (Elem name attrs content) = Elem name attrs (f content)

public export
mapContentM :  Monad m
            => (List (Either CharData (Either Misc Element)) -> m (List (Either CharData (Either Misc Element))))
            -> Element
            -> m Element
mapContentM f (EmptyElem name attrs)    = pure $ EmptyElem name attrs
mapContentM f (Elem name attrs content) = pure $ Elem name attrs !(f content)

indentTail :  String
           -> String
indentTail str =
  let (x ::: xs) = split (== '\n') str
    in joinBy "\n" (x :: map indent xs)
  where
    indent :  String
           -> String
    indent "" = ""
    indent s = "    " ++ s

export
Show Element where
  show (EmptyElem name attrs)    =
    "<\{show name}\{concat $ map (\attr => " " ++ show attr) attrs}/>"
  show (Elem name attrs content) =
    """
    <\{show name}\{concat $ map (\attr => " " ++ show attr) attrs}>\
    \{indentTail $ textContent' content}\
    </\{show name}>
    """

--------------------------------------------------------------------------------
--          Prolog
--------------------------------------------------------------------------------

public export
record XMLProlog where
  constructor MkXMLProlog
  xmldecl     : Maybe XMLDecl
  xmldeclmisc : List Misc
  doctype     : Maybe DocType
  doctypemisc : List Misc

export
Show XMLProlog where
  show prolog = 
    joinBy "\n" $
      maybe [] ((:: Nil) . show) prolog.xmldecl ++
      map show prolog.xmldeclmisc ++
      maybe [] ((:: Nil) . show) prolog.doctype ++
      map show prolog.doctypemisc

--------------------------------------------------------------------------------
--          XMLDocument
--------------------------------------------------------------------------------

public export
record XMLDocument where
  constructor MkXMLDocument
  prolog : XMLProlog
  root   : Element
  misc   : List Misc

namespace XMLDocument

  public export
  mapContent :  (Element -> Element)
             -> XMLDocument
             -> XMLDocument
  mapContent f (MkXMLDocument prolog root misc) = MkXMLDocument prolog (f root) misc

export
Show XMLDocument where
  show doc =
    joinBy "\n" $
      filter (/= "") $
        show doc.prolog :: show doc.root :: map show doc.misc
