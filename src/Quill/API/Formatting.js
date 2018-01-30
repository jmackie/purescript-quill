'use strict';

exports.formatImpl = function(self, name, value, source) {
    return function() {
        return self.format(name, value, source);
    };
};

exports.formatLineImpl = function(self, index, length, formats, source) {
    return function() {
        return self.formatLine(index, length, formats, source);
    };
};

exports.formatTextImpl = function(self, index, length, formats, source) {
    return function() {
        return self.formatText(index, length, formats, source);
    };
};

// Return type is stupid!
//exports.getFormatImpl = function(self, index, length) {
//    return function() {
//        return self.getFormat(index, length);
//    };
//};

exports.removeFormatImpl = function(self, index, length, source) {
    return function() {
        return self.insertEmbed(index, length, source);
    };
};
