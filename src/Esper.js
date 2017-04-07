'use strict';

const fs = require('fs');

const bufferRead = function (type) {
  return function (buffer) {
    return function (offset) {
      return function () {
        return buffer['read' + type](offset);
      };
    };
  }
};

module.exports = {

  addArray: function (x) {
    return function (y) {
      return x.concat(y);
    };
  },

  addInt: function (x) {
    return function (y) {
      return x + y;
    };
  },

  bindEffect: function (x) {
    return function (f) {
      return function () {
        return f(x())();
      };
    };
  },

  equalString: function (x) {
    return function (y) {
      return x === y;
    };
  },

  inspect: function (x) {
    return function () {
      console.dir(x, { colors: true, depth: null });
      return {};
    };
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

  pureEffect: function (x) {
    return function () {
      return x;
    };
  },

  readFile: function (x) {
    return function (f) {
      return function (g) {
        return function () {
          fs.readFile(x, function (error, buffer) {
            if (error) {
              f(error)();
            } else {
              g(buffer)();
            }
          });
          return {};
        };
      };
    };
  },

  readFloatLE: bufferRead('FloatLE'),

  readInt32LE: bufferRead('Int32LE'),

  readString: function (buffer) {
    return function (start) {
      return function (end) {
        return function () {
          return buffer.toString('utf8', start, end);
        };
      };
    };
  },

  readUInt8: bufferRead('UInt8'),

  readUInt32LE: bufferRead('UInt32LE'),

  readUInt64LE: function (buffer) {
    return function (offset) {
      return function () {
        const high = buffer.readUInt32LE(offset);
        const low = buffer.readUInt32LE(offset + 4);
        return { high: high, low: low };
      };
    };
  },

  subtractInt: function (x) {
    return function (y) {
      return x - y;
    };
  },

  throw: function (x) {
    return function () {
      throw x;
    };
  },

  toError: function (x) {
    return new Error(x);
  },

  unit: {},

  warn: function (x) {
    return function () {
      console.warn(x);
      return {};
    };
  },

};
