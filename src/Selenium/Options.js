const { Browser, Capabilities } = require("selenium-webdriver");
const chrome = require("selenium-webdriver/chrome");

exports.chromeOptions   = new chrome.Options(Capabilities.chrome())
exports.edgeOptions     = null //Capabilities.edge()
exports.firefoxOptions  = null //Capabilities.firefox()
exports.ieOptions       = null //Capabilities.ie()
exports.safariOptions   = null //Capabilities.safari()

exports.setHeadless = function(options) {
    switch(options.getBrowserName()) {
        case Browser.CHROME: return new chrome.Options(options).headless()
        default: throw new Error("no headless option")
    }
}
