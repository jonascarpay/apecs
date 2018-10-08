{-# LANGUAGE CPP #-}
import Distribution.Simple
import Distribution.Verbosity
import Distribution.PackageDescription
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
#else
import Distribution.PackageDescription.Parse
#endif


main = do
#if defined(MIN_VERSION_Cabal) && MIN_VERSION_Cabal(2,0,0)
    pkgDescr <- readGenericPackageDescription verbose "apecs-physics.cabal"
#else
    pkgDescr <- readPackageDescription verbose "apecs-physics.cabal"
#endif
    defaultMainNoRead . addCFilesIfNeeded $ pkgDescr
  where
    addCFilesIfNeeded =
#if __GLASGOW_HASKELL__ >= 802
      id
#else
      addCFiles
#endif

    addCFiles genPkgDescr = genPkgDescr
      { condLibrary = fmap addCFiles' pkgLibrary
      }
      where
        pkgLibrary = condLibrary genPkgDescr 

        addCFiles' (CondNode lib constraints components) =
          CondNode (addCFiles'' lib) constraints components

        addCFiles'' lib = lib
          { libBuildInfo =
              let buildInfo = libBuildInfo lib in
                buildInfo
                { cSources = cSources buildInfo ++
                    [ "src/Apecs/Physics/Constraint.c"
                    , "src/Apecs/Physics/Space.c"
                    , "src/Apecs/Physics/Query.c"
                    , "src/Apecs/Physics/Body.c"
                    , "src/Apecs/Physics/Collision.c"
                    , "src/Apecs/Physics/Shape.c"
                    ]
                }
          }

