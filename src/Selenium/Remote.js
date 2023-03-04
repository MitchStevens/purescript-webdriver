// module Selenium.Remote

import remote from 'selenium-webdriver/remote';

export const fileDetector = function () {
  return new remote.FileDetector();
};
