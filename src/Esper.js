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

exports.addArray = function (x) {
  return function (y) {
    return x.concat(y);
  };
};

exports.addInt = function (x) {
  return function (y) {
    return x + y;
  };
};

exports.bindEffect = function (x) {
  return function (f) {
    return function () {
      return f(x())();
    };
  };
};

exports.equalString = function (x) {
  return function (y) {
    return x === y;
  };
};

exports.inspect = function (x) {
  return function () {
    console.dir(x, { colors: true, depth: null });
    return {};
  };
};

exports.log = function (x) {
  return function () {
    console.log(x);
    return {};
  };
};

exports.newError = function (x) {
  return new Error(x);
};

exports.pureEffect = function (x) {
  return function () {
    return x;
  };
};

exports.readFile = function (x) {
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
};

exports.readFloatLE = bufferRead('FloatLE');

exports.readInt32LE = bufferRead('Int32LE');

exports.readString = function (buffer) {
  return function (start) {
    return function (end) {
      return function () {
        return buffer.toString('utf8', start, end);
      };
    };
  };
};

exports.readUInt8 = bufferRead('UInt8');

exports.readUInt32LE = bufferRead('UInt32LE');

exports.subtractInt = function (x) {
  return function (y) {
    return x - y;
  };
};

exports.throw = function (x) {
  return function () {
    throw x;
  };
};

exports.unit = {};

exports.warn = function (x) {
  return function () {
    console.warn(x);
    return {};
  };
};
