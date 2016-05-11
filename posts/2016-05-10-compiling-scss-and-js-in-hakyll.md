---
title: Compiling SCSS and JavaScript in Hakyll
tags: haskell, hakyll
---

_Versions used:_

* _Hakyll 4.8.3.0_
* _hjsmin 0.2.0.1_
* _Sass 3.4.18_

This seems to be an often asked question, so I thought I'd try and share the approach that I've arrived at after having explored a couple of solutions to the problem. If you want to see the full code in action, check out the [repo for the codetalk.io site](https://github.com/codetalkio/codetalk.io/blob/v1.0.0/site.hs#L9) (linking to v1.0.0 is intended, in case the code changes later on).

## Compiling and minifying JavaScript
For some reason `Hakyll` does not include its own JavaScript compiler, which makes little sense. Luckily there is a package called `hjsmin` giving us `Text.Jasmine`, which we will use to both compile and minify our JavaScript files.

<blockquote>
_NOTE:_ From personal experience an earlier version of `hjsmin` (0.1.5.3) would throw a parse error on some files (like jQuery). This has later been fixed in 0.2.0.1, but unfortunately Stackage is using 0.1.5.3 in the current LTS 5.15.
</blockquote>

To get `hjsmin` 0.2.0.1 working with `stack`, you can add the following to the `stack.yaml` file in the project.


<div class="snippet-title">stack.yaml</div>
```yaml
...
extra-deps: [ hjsmin-0.2.0.1
            , language-javascript-0.6.0.4
            ]
...
```
and `hjsmin == 0.2.*` as a dependency in the projects cabal file.

Now we are ready to construct the compiler itself.

<div class="snippet-title">site.hs</div>
```haskell
import qualified Data.ByteString.Lazy.Char8 as C
import           Text.Jasmine

-- | Create a JavaScript compiler that minifies the content
compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
  let minifyJS = C.unpack . minify . C.pack . itemBody
  s <- getResourceString
  return $ itemSetBody (minifyJS s) s
```

The code is fairly straightforward. We use the `Text.Jasmine` provided function `minify` which has the signature `ByteString -> ByteString`, meaning it takes in the JavaScript code as string, and produces the result to a string.

Later on inside the main `Hakyll` function, we use it simply as we would the other compilers.

<div class="snippet-title">site.hs</div>
```haskell
-- | Define the rules for the site/hakyll compiler
main :: IO ()
main = hakyll $ do
  -- | Route for all JavaScript files found in the 'js' directory
  match "js/*" $ do
    route   idRoute
    compile compressJsCompiler
  -- The rest of your rules...
```


## Compiling and minifying SCSS (Sass)
For those who aren't aware, there are other ways to write CSS than CSS. Sass and SCSS adds a lot of niceties to CSS, such as nesting, variables and mixins, and compiles to normal CSS. You can read more about that [on their website](http://sass-lang.com).

This time we rely on external dependencies, namely the `sass` tool, which can be installed with `gem install sass`.

<blockquote>
_NOTE:_ There is a library called [`hsass`](https://hackage.haskell.org/package/hsass), which provides a Haskell interface, but I've been running into problems with linking to the underlying C API. As such, I've opted for the external dependency for now.
</blockquote>

<div class="snippet-title">site.hs</div>
```haskell
-- | Create a SCSS compiler that transpiles the SCSS to CSS and
-- minifies it (relying on the external 'sass' tool)
compressScssCompiler :: Compiler (Item String)
compressScssCompiler = do
  fmap (fmap compressCss) $
    getResourceString
    >>= withItemBody (unixFilter "sass" [ "-s"
                                        , "--scss"
                                        , "--compass"
                                        , "--style", "compressed"
                                        , "--load-path", "scss"
                                        ])
```

This time we have no library dependencies, and use the `Hakyll` provided function `unixFilter` to call the `sass` tool. An important thing is the arguments that we pass, which I'll explain briefly:

* `-s` tells `sass` to take its input from `stdin`
* `--scss` tells `sass` to use the SCSS format
* `--compass` tells `sass` to make compass imports available
* `--style compressed` tells `sass` to compress the output
* `--load-path scss` tells `sass` to look for modules in the `scss` directory (if we import stuff)

Much like with the JavaScript compiler, we use it in the main `Hakyll` function as such:

<div class="snippet-title">site.hs</div>
```haskell
-- | Define the rules for the site/hakyll compiler
main :: IO ()
main = hakyll $ do
  -- | Compile the SCSS, from 'scss/app.scss', to CSS and serve it as 'app.css'
  match "scss/app.scss" $ do
   route   $ constRoute "app.css"
   compile compressScssCompiler
  -- The rest of your rules...
```

You can now happily compile both JavaScript and SCSS in your `Hakyll` project, without much hassle :).
