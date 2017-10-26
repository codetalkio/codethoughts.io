module Site.Compiler where
import Hakyll

-- | Create a SCSS compiler that transpiles the SCSS and minifies it
-- relying on the external 'sass' tool
sassCompiler :: Compiler (Item String)
sassCompiler = loadBody (fromFilePath "resources/scss/app.scss")
                 >>= makeItem
                 >>= withItemBody (unixFilter "sass" args)
  where args = [ "--stdin"
               , "--scss"
               , "-I", "resources/scss"
               , "--compass"
               , "--style", "compressed"
               ]
