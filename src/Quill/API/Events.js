'use strict';

exports.onTextChangeImpl = function(self, callback) {
    self.on('text-change', function(delta, oldContents, source) {
        callback(delta)(oldContents)(source)();
    });
    return {};
};

exports.onSelectionChangeImpl = function(self, callback) {
    self.on('selection-change', function(range, oldRange, source) {
        callback(range)(oldRange)(source)();
    });
    return {};
};
