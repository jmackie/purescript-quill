'use strict';

exports.onTextChangeImpl = function(self, callback) {
    return function() {
        self.on('text-change', function(delta, oldContents, source) {
            callback(delta)(oldContents)(source)();
        });
    };
};

exports.onSelectionChangeImpl = function(self, callback) {
    return function() {
        self.on('selection-change', function(range, oldRange, source) {
            callback(range)(oldRange)(source)();
        });
    };
};
