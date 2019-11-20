const util = require("util")
const spawn = require("child_process").spawn
const fs = require("fs")

const env = process.env.NODE_ENV || "dev"
const envCapitalized = env.charAt(0).toUpperCase() + env.slice(1)

fs.unlinkSync("./src/env/Env.elm")

fs.copyFileSync(`./src/env/${envCapitalized}.elm`, "./src/env/Env.elm")

const elmApp = spawn("elm-app", ["build"])

elmApp.stdout.pipe(process.stdout)
elmApp.stderr.pipe(process.stderr)

elmApp.on("close", code => {
  console.log(`child process exited with code ${code}`)
})

elmApp.on("error", code => {
  console.log(`child process exited with code ${code}`)
})
