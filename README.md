# Deprecated

I created this package as an experiment to try to come up with a consistent
way of handling errors when converting text input to an Int, Float, Bool, or
custom type. The approach here turned out to be too cumbersome to apply in
practice. I came up with a better solution for my
[Modular UI](http://package.elm-lang.org/packages/danielnarey/elm-modular-ui/latest)
package, which was to incorporate error handling into
[constructor functions](http://package.elm-lang.org/packages/danielnarey/elm-modular-ui/1.0.1/Ui-Input#input-fields-with-built-in-encoding-decoding)
for input elements that capture numeric and custom values.
