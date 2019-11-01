import "./main.css"
import { Elm } from "./Main.elm"
import * as serviceWorker from "./serviceWorker"
import { Socket, Channel } from "phoenix"

const player = JSON.parse(localStorage.getItem("player"))

console.log(player)

const app = Elm.Main.init({
  flags: player,
  node: document.getElementById("root")
})

let channel
let socket
app.ports.joinRoom.subscribe(({ playerId, player, roomId }) => {
  console.log(player)
  localStorage.setItem(
    "player",
    JSON.stringify({ player: { id: playerId, player }, roomId })
  )
  socket = new Socket("ws://192.168.0.149:4000/socket", {
    params: { player_id: playerId }
  })
  channel = socket.channel(`game:${roomId}`, { playerId })

  channel.join().receive("ok", resp => {
    console.log("kek")
    console.log(resp)
  })
  // .receive("game", console.dir)

  channel.on("game", ({ game }) => {
    console.log(game)
    app.ports.gotGame.send(game)
  })

  app.ports.move.subscribe(coords => {
    console.log(coords)
    channel.push("move", { coords, playerId })
  })

  socket.connect()

  console.log(channel)
})

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister()
