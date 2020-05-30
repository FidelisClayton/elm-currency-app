import './main.css'
import { Elm } from './Main.elm'
import * as serviceWorker from './serviceWorker'

function getCurrencies() {
  try {
    const currencies = window.localStorage.getItem('currencies')

    return JSON.parse(currencies) || []
  } catch (e) {
    return []
  }
}

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: getCurrencies(),
})

app.ports.saveCurrencies.subscribe(function (currencies) {
  window.localStorage.setItem('currencies', JSON.stringify(currencies))
})

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister()
