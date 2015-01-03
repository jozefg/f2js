var CURRENT_CLOS = null;
var CONT_STACK = [];
var ARG_STACK = [];
var EVALED_STACK = [];


var jumpNext = function(){
    return CONT_STACK.pop();
}

var mkClosure = function(shouldUpdate, closedVariables, body) {
    return {flag : shouldUpdate,
            clos : closedVariables,
            body : body}
};

var mkLit = function(x){
    return {flag : true,
            clos : [],
            body : function(){
                EVALED_STACK.push(x);
                return jumpNext();
            }
           }
}

var enter = function(c){
    CURRENT_CLOS = c;
    return c.body();
}

var evalFirst = function(){
    CURRENT_CLOS = ARG_STACK.pop();
    return enter(CURRENT_CLOS);
}

var mkPrim = function(f){
    return function(){
        var r = EVAL_STACK.pop();
        var l = EVAL_STACK.pop();
        EVAL_STACK.push(f(l, r));
        return jumpNext();
    }
}

var plusPrim  = mkPrim(function(l, r){return l + r;});
var minusPrim = mkPrim(function(l, r){return l - r;});
var multPrim  = mkPrim(function(l, r){return l * r;});
var divPrim   = mkPrim(function(l, r){return l / r;});
var modPrim   = mkPrim(function(l, r){return l % r;});
var shlPrim   = mkPrim(function(l, r){return l << r;});
var shrPrim   = mkPrim(function(l, r){return l >> r;});
var eqPrim    = mkPrim(function(l, r){return l === r;});
var ltPrim    = mkPrim(function(l, r){return l < r;});
var ltePrim   = mkPrim(function(l, r){return l <= r;});
var gtPrim    = mkPrim(function(l, r){return l > r;});
var gtePrim   = mkPrim(function(l, r){return l >= r;});
