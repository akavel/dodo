import './main.css';
import { Main } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
//---- JS LIBRARIES ----
// localforage — offline storage for webapps, wraps IndexedDB/WebSQL/localStorage
// depending on what's available
import localForage from 'localforage';

var app = Main.embed(document.getElementById('root'));

registerServiceWorker();

//---- PORTS ----

// TODO(akavel): use JSON + Decoder in Elm, or just pass raw values as
// described in JS Interop section in the Elm Guide?

app.ports.save.subscribe(function(storage) {
    localForage.setItem('dodo-storage', storage).then(function(value) {
        console.log("saved...");
        console.log(value);
    }).catch(function(err) {
        console.log("save error!");
        // FIXME(akavel): pass error info to Elm and somehow display a
        // notification ("snack" or whatever the MDL name)
        console.log(err);
    });
});
app.ports.load.subscribe(function() {
    localForage.getItem('dodo-storage').then(function(value) {
        console.log("loaded...");
        // FIXME(akavel): how to solve upgrades purely on Elm side, without need for help in JS?
        if (value.v1 === undefined) {
            value.v1 = null;
        }
        app.ports.loaded.send(value);
        console.log(value);
    }).catch(function(err) {
        console.log("load error!");
        // FIXME(akavel): pass error info to Elm and somehow display a
        // notification ("snack" or whatever the MDL name)
        console.log(err);
    });
});

