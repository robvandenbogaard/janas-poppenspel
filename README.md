# Jana's poppenspel

Jana's doll game - a fashion exploration

## Compile and deploy

Compile: `elm make src/Main.elm --output=main.js `

The `index.html` file should not be overwritten as it contains a reference to a font
and the `main.js` file.

Development: `elm reactor --port=8010` in the project root.

Deploy: Copy `index.html`, `main.js` and the contents of `assets`.
