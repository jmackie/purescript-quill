'use strict';

exports.editorImpl = function(el, config) {
    return function() {
        var editor = new Quill(el, config);
        return editor;
    };
};
