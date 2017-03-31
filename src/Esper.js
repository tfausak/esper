'use strict';

const fs = require('fs');

module.exports = {

  effectBind: function (x) {
    return function (f) {
      return function () {
        return f(x())();
      };
    };
  },

  effectPure: function (x) {
    return function () {
      return x;
    };
  },

  inspect: function (x) {
    if (x === undefined) {
      return 'undefined';
    } else if (x === null) {
      return 'null';
    } else {
      return x.toString();
    }
  },

  log: function (x) {
    return function () {
      console.log(x);
      return {};
    };
  },

  nullable: function (y) {
    return function (f) {
      return function (x) {
        if (x === null) {
          return y;
        } else {
          return f(x);
        }
      };
    };
  },

  readFile: function (x) {
    return function (f) {
      return function () {
        fs.readFile(x, function (error, data) {
          f(error)(data)();
          return {};
        });
        return {};
      };
    };
  },

  readUInt32LE: function (buffer) {
    return function (offset) {
      return function () {
        return buffer.readUInt32LE(offset);
      };
    };
  },

  unit: {},

  warn: function (x) {
    return function () {
      console.warn(x);
      return {};
    };
  },

};
