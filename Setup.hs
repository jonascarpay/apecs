{-# LANGUAGE CPP #-}
import Distribution.Simple
import Distribution.PackageDescription


main = defaultMainWithHooks simpleUserHooks
    { readDesc = (fmap . fmap) addCFilesIfNeeded (readDesc simpleUserHooks)
    }
  where
    addCFilesIfNeeded genPkgDescr =
#if MIN_VERSION_inline_c(0, 6, 0)
      genPkgDescr
#else
      addCFiles genPkgDescr
#endif

    addCFiles genPkgDescr = genPkgDescr
      { packageDescription = newPkgDescription
      }
      where
        pkgDescr = packageDescription genPkgDescr
        pkgLibrary = library pkgDescr
        newPkgDescription = pkgDescr
          { library = fmap addCFiles' pkgLibrary
          }
        addCFiles' lib = lib
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

