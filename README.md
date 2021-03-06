PigLet
======
![Piglet sleeping](https://lh5.googleusercontent.com/-p5UUHnasepg/UxobHZBwoAI/AAAAAAAACzk/i_mFM4zepuA/w456-h319-no/pig-sleeping-new-york-456.jpg)

Piggies love embracing templates!

PigLet is an HTML template libary that transforms HTML5 files into Blaze
HTML using template Haskell.

It's currently under initial development. Hopefully it could provide a
Haskell equivalent to Clojure's wonderful Enlive.

## Example

Let's say you have an HTML file called hello.html:

```html
<html>
  <head>
      <title>Hello World!</title>
  </head>
  <body>
      <div class="page"></div>
  </body>
</html>
```

With the following code:

```haskell
import           Text.Html.PigLet
import qualified Text.Blaze.Html5 as H


page = defTemplate [] "hello.html" [
  D "title"             >@< embedContent [| H.toHtml "Hello Piggies!" |]
, A ("class", ["page"]) >@< updateAttr [| addAttr ("id", "page-wrapper") |]
]
```

it's expected to see when the page is rendered:

```html
<html>
  <head>
      <title>Hello Piggies!</title>
  </head>
  <body>
      <div class="page" id="page-wrapper"></div>
  </body>
</html>
```

## Todo
1. More composable HTML transformations
2. Compile time error checkings
3. Better template syntax, more efficient attributes generator 
