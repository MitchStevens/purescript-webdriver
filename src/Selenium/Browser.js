// module Selenium.Browser

export const _browserCapabilities = function(br) {
    return {browserName: br};
};

export const versionCapabilities = function(v) {
    return {version: v};
};

export const platformCapabilities = function(p) {
    return {platform: p};
};
