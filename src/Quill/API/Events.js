'use strict';

exports.onTextChangeImpl = function(self, callback) {
    self.on('text-change', callback);
};

exports.onSelectionChangeImpl = function(self, callback) {
    self.on('selection-change', callback);
};
