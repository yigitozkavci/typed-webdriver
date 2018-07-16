# typed-webdriver

This module is built on top of [webdriver](https://hackage.haskell.org/package/webdriver) library to provide compile time assurances that elements are selected by correct xpaths, and have the necessary capabilities for certain action.

## Examples
```haskell
-- Typechecks, because we try to click to a element of type link.
myS :: WD ()
myS = click =<< myFindElem $(mkXPath "//a/button/descendant::a[contains(text(), 'wow')]")
```

```haskell
-- Does not compile, because we try to click to a span element, which should not be clickable.
myS :: WD ()
myS = click =<< myFindElem $(mkXPath "//a/button/descendant::span[contains(text(), 'wow')]")
{-
    • (.   \
      \  |
           \ |___(\--/)
         __/    (  . . )
        "'._.    '-.O.'
             '-.  \ "|\
                '.,,/'.,,mrf
      The following XPath is not clickable:
      //a/button/descendant::span[contains(text(), 'wow')]
-}
```
