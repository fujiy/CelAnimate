
const path = require('path')
const electron = require('electron')
const {app, BrowserWindow} = require('electron');

let mainWindow

// require('electron-reload')(__dirname, {
//     electron: path.join(__dirname, 'node_modules', '.bin', 'electron')
// });
// try { 
//     require('electron-reloader')(module, { 
//         debug: true,
//         watchRenderer: true
//     }); 
// } catch (_) { console.log('Error'); }   
// require('electron-reloade
// try {
//     require('electron-reload')(module)
// } catch (e) {
//     console.log('Error!', e)
// }

app.on('window-all-closed', function() {
    if (process.platform !== 'darwin') {
        app.quit();
    }
});

app.on('ready', function() {

    mainWindow = new BrowserWindow({
        width: 800, height: 600,
        webPreferences: {
            nodeIntegration: true
        }
        // webPreferences: {
        // webSecurity: false
        // }
    });
    mainWindow.loadURL('file://' + __dirname + '/index.html');

    mainWindow.on('closed', function() {
        mainWindow = null;
    });

});


