"use strict";

exports.traktiInput = function (p, e) {
    let t = e.target;
    return function () {
        let rez = p(t.value);
        if (rez.value0 != null) {
            t.value = rez.value0;
            t.oldval = rez.value0;
        } else {
            t.value = t.oldval;
        }
        return rez;
    }
}