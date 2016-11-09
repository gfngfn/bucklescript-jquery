# BuckleScript jQuery binding

BuckleScript binding for jQuery 3.1.

Still only covers a small part of jQuery, but hopefully this repo and npm package help others understand how to do FFI and make a npm package for BuckleScript.

## How to use

### Compile

To compile, for example, a client-side script with `src/main.ml` as an entry point. 

```shell
bsc -I src -c -bs-main src/main.ml -bs-package-include bucklescript-jquery -bs-package-name $npm_package_name -bs-package-output dist
```

### Bundle

To bundle with webpack:

```shell
webpack -p dist/main.js dist/bundle.js
```

then load `dist/bundle.js` in your HTML.

## Testing

Run `npm run compile_test` then open `test/test.html`. It should show "Test success" in a browser if test is successful.

Only a small fragment of testing has been added. 

## Copyright

Copyright 2016 by Nebuta. MIT license.
(Extension to the original software: Copyright 2016 by Takashi SUWA. MIT license.)
