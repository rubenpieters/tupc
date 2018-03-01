"use strict";

exports.passNullContext = function(f) {
  return f(undefined);
};

exports.copyRecordSR = function(rec) {
  var copy = {};
  for (var key in rec) {
    if ({}.hasOwnProperty.call(rec, key)) {
      copy[key] = rec[key];
    }
  }
  return copy;
};

exports.unsafeInsertSR = function(l) {
  return function(a) {
    return function(rec) {
      rec[l] = a;
      return rec;
    };
  };
};
