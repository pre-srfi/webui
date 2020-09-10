import * as React from 'react';
import * as ReactDOM from 'react-dom';


let e = React.createElement
let render = ReactDOM.render

function makecallback (proc) {
    let procedure = function () {
        var args = Array.prototype.slice.call(arguments);
        return g_scm_call(proc, args) 
    };

    return procedure;
};

function makeprops (obj) {
    if (obj === void 0) {
        return obj;
    }
    if (obj === null) {
        return obj;
    }
    if (typeof obj === "boolean") {
        return obj;
    }
    if (typeof obj === "number") {
        return obj;
    }
    if (obj instanceof G_Flonum) {
        return obj.val;
    }
    if (obj instanceof G_ScmString) {
        return obj.toString();
    }
    if (obj instanceof G_Pair) {
        var jsobj = {};
        var i = 0;
        while (obj instanceof G_Pair) {
            var elem = obj.car;
            if (elem instanceof G_Pair) {
                jsobj[makeprops(elem.car)] = makeprops(elem.cdr);
            } else {
                jsobj[i] = makeprops(elem);
            }
            ++i;
            obj = obj.cdr;
        }
        return jsobj;
    }
    if (obj instanceof G_Structure) {
        throw "scm2host error (cannot convert Structure)";
    }
    if (typeof obj === "function") {
        return makecallback(obj);
    }
    throw "scm2host error";
};



export default {
    e,
    render,
    makeprops
}
