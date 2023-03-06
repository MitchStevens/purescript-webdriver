const { Browser, Capabilities } = require("selenium-webdriver");
const chrome = require("selenium-webdriver/chrome");

exports.chromeOptions   = chrome.Options( Capabilities.chrome())
exports.edgeOptions     = null //Capabilities.edge()
exports.firefoxOptions  = null //Capabilities.firefox()
exports.ieOptions       = null //Capabilities.ie()
exports.safariOptions   = null //Capabilities.safari()

exports.setHeadless = function(options) {
    switch(options.getBrowserName()) {
        case Browser.CHROME: return chrome.Options(options).headless()
        default: throw new Error("no headless option")
    }
}
