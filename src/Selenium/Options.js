const { Options, Browser, Capabilities } = require("selenium-webdriver");

exports.chromeOptions = new Options(Capabilities.chrome())
exports.edgeOptions = new Options(Capabilities.edge())
exports.firefoxOptions = new Options(Capabilities.firefox())
exports.ieOptions = new Options(Capabilities.ie())
exports.safariOptions = new Options(Capabilities.safari())

exports.setHeadless = function(options) {
    switch(options.getBrowserName()) {
        case Browser.CHROME: return Options(options).headless()
        default: throw new Error("no headless option")
    }
}
