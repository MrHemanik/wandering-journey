Remove-item -Path ".\elm.js"
Invoke-Expression "elm make src/Main.elm --output elm.js"